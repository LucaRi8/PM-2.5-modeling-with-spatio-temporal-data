# carico i dati
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(fda)
library(INLA)
library(KFAS)
library(sp)
library(leaflet)
library(igraph)
library(maps)
library(rSPDE)
library(reshape2)
library(magick)
data = tibble(read.csv("city_pollution_data.csv"))



# pre-process dei dati
#sistemare la data
tmp = strsplit(data$Date, "-")
tmp = lapply(tmp, function(x){
  if(x[3] <= 12){
    ttt = x[2]
    x[2] = x[3]
    x[3] = ttt
  }
  return(paste0(x, collapse = "-"))
})
data$Date = unlist(tmp)
data = arrange(data, City, Date)

#rimuovo variabili non di interesse
tmppm25count <- data$pm25_count
data = data %>% dplyr::select(-all_of(grep("_min|_max|_count", colnames(data), value = TRUE)))
data$past_week_avg_miles = NULL
data$pm25_count <- tmppm25count

#sistemare variabili popolazioni
data$Population.Staying.at.Home = as.numeric(gsub(pattern = ",", "", data$Population.Staying.at.Home))
data$Population.Not.Staying.at.Home = as.numeric(gsub(pattern = ",", "", data$Population.Not.Staying.at.Home))

#sistemare data
data$Date = as.POSIXct(data$Date, tz = "GMT")

#tolgo le variabili malvagie
data = data %>% dplyr::select(-c("o3_median", "o3_variance", "pp_feat", "wind.gust_median", "wind.gust_variance"))

#tolgo le città malvagie
data = data %>% filter(City != "honolulu" & City != "columbia" & City != "richmond")

# creazione della variabile popolazione totale e frazione della popolazione che esce
data$PopulationTot <- data$Population.Not.Staying.at.Home +
  data$Population.Staying.at.Home

data$PopulationNotStayFrac <- data$Population.Not.Staying.at.Home/
  data$PopulationTot

# creazione del train
train = data %>% filter(Date >= as.POSIXct("2020-01-01", tz = "GMT"))

#na per variabile
names.tmp = train %>% is.na() %>% colMeans() %>% sort(decreasing = TRUE) %>% { which(. > 0.1) } %>% names()
train = train %>% dplyr::select(-all_of(names.tmp))
data = data %>% dplyr::select(-all_of(names.tmp))

#na per città
city.index.tmp = train %>% group_by(City) %>%
  summarise_all(function(x){mean(is.na(x))}) %>%
  dplyr::select(-City) %>% rowMeans() %>% { which(. > 0.1) }

train = train %>% filter(!(City %in% unique(train$City)[city.index.tmp]))
data = data %>% filter(!(City %in% unique(data$City)[city.index.tmp]))


#creo test set
test = data %>% filter(Date < as.POSIXct("2020-01-01", tz = "GMT"))
range(test$Date)


# rimuovo le osservazini anomale 
train <- train |> 
  mutate(pressure_median = ifelse(pressure_median<800, NA, pressure_median))

test <- test |> 
  mutate(pressure_median = ifelse(pressure_median<800, NA, pressure_median))

test <- test |> 
  mutate(pressure_median = ifelse(pressure_median>1060, NA, pressure_median))

train <- train |> 
  mutate(temperature_median = ifelse(temperature_median>50, NA, temperature_median))


# imputazione degli NA
# prendo l'ultimo valore prima dell'NA ed il primo valore dopo ed interpolo
na_pos = train %>% 
  is.na %>% colSums %>% {which(. >0)} 

na_pos = na_pos

train[, na_pos] = train %>% dplyr::select(all_of(na_pos)) %>% 
  zoo::na.approx() %>% as_tibble(colnames = names(na_pos)) 

# rimuovo tutte le le osservazioni nel primo dell'anno in quanto gli NA sono 
# imputati in modo non corretto
train = train %>% filter(Date != as.POSIXct("2020-01-01", tz = "GMT"))

# valuto la presenza degli NA nel test set
test %>% is.na %>% colSums

# rimuoviamo tutte le osservazioni con NA sulla risposta in quanto non utili
# per valutare la bontà dei modelli
test = test %>% filter(!is.na(pm25_median) & !is.na(pm25_variance))

# rimanendo soli 10 valori mancanti decidiamo di eliminare tali osservazioni
test = test %>% na.omit


# seleziono alcune città che sul test non hanno problemi in modo da poter valutare 
# i modelli in tutte le date dell'anno 
# assumiamo che il fatto che non vi siano dati mancanti sia indipedente dalla risposta
val <- test
test <- test %>% filter(City %in% c("albuquerque", "atlanta", "brooklyn", "raleigh",
                                    "manhattan", "boise", "staten island",
                                    "miami", "san francisco", "tallahassee", 
                                    "springfield", "seattle", "nashville" )) 
val <- setdiff(val, test)
# rimuovo la prima e l'ultima data in quanto non presente nei dati di stima
test <- test |> filter(!(Date %in% range(Date)))

# sistemo alcune variabili nel val set
val <- val |> filter(!(Date %in% as.POSIXct(c("2019-01-01 GMT", "2019-11-30 GMT"), 
                                            tz = "GMT")))
val <- val |> mutate(temperature_median = ifelse(temperature_median == 0, NA, temperature_median))
val <- val |> mutate(pressure_median = ifelse(pressure_median < 950, NA, pressure_median))
val <- val |> mutate(wind.speed_median = ifelse(wind.speed_median >20, NA, wind.speed_median))
val <- val |> mutate(humidity_median = ifelse(humidity_median == 0, NA, humidity_median))

# cap della risposta a 120 per migliorare il fit
train$pm25_median <- ifelse(train$pm25_median > 120, 120, train$pm25_median)

# standardizzazione delle variabili
meantr <- train |> dplyr::select(where(is.numeric)) |>
  dplyr::select(-all_of(c("pm25_median", "pm25_variance", "pm25_count",
                          "longitude", "latitude"))) |>
  summarise_all(mean)

sdtr <- train |> dplyr::select(where(is.numeric)) |>
  dplyr::select(-all_of(c("pm25_median", "pm25_variance", "pm25_count",
                          "longitude", "latitude"))) |>
  summarise_all(sd)

train <- train |> mutate(across(colnames(meantr), function(x) (x - mean(x))/sd(x)))
for(n in colnames(meantr))
  test[, n] <- (test[, n] - meantr[[n]])/sdtr[[n]]

for(n in colnames(meantr))
  val[, n] <- (val[, n] - meantr[[n]])/sdtr[[n]]

# inserisco i dati in formato wide in una lista (comodo per fare lisciamenti)
var_names <- train %>% dplyr::select(where(is.numeric)) %>% colnames()
wide_data <- lapply(1:length(var_names), function(i) {
  tp = (train %>%
          pivot_wider(id_cols = City, names_from = Date, values_from = var_names[i]))[, -1] %>% 
    as.matrix %>% t %>% zoo::na.approx() %>% as_tibble
  colnames(tp) = unique(train$City)
  tp[-nrow(tp), ]
})

names(wide_data) <- var_names

#faccio la stessa cosa per i dati nel testset
wide_test_data <- lapply(1:length(var_names), function(i) {
  tp = (test %>%
          pivot_wider(id_cols = City, names_from = Date, values_from = var_names[i]))[, -1] %>% 
    as.matrix %>% t %>% zoo::na.approx() %>% as_tibble
  colnames(tp) = unique(test$City)
  tp
})

names(wide_test_data) <- var_names


#faccio la stessa cosa per i dati nel valset
wide_val_data <- lapply(1:length(var_names), function(i) {
  tp = (val %>%
          pivot_wider(id_cols = City, names_from = Date, values_from = var_names[i]))[, -1] %>% 
    as.matrix %>% t %>% zoo::na.approx() %>% as_tibble
  colnames(tp) = unique(val$City)
  tp
})

names(wide_val_data) <- var_names



# analisi esplorativa 
set.seed(1)
nn <- sample(unique(train$City), 5)
# pm 2.5
train |> filter(City %in% nn) |>
  ggplot(aes(x = Date, y = pm25_median, goup = City, color = City)) + 
  geom_line() + 
  ylab("pm 2.5") +
  labs(title = "Pm 2.5 mediano giornaliero per città")


# correlazione tra le variabili miglia e popolazione che non sta a casa
train |>
  ggplot(aes(x = mil_miles, y = Population.Not.Staying.at.Home, color = City)) + 
  geom_point() + 
  ylab("Not stay at home") +
  xlab("Miles") +
  labs(title = "") + 
  theme(legend.position="none")
# la variblità delle due osservazioni è molto differente

# grafici che postrano come miglia e popolazione che esce siano una proxy della
# dimensione della città (rappresentata dalla popo totale)
train |>
  ggplot(aes(x = mil_miles, y = Population.Not.Staying.at.Home+Population.Staying.at.Home, color = City)) + 
  geom_point() + 
  ylab("Total population") +
  xlab("Miles") +
  labs(title = "") + 
  theme(legend.position="none")


# lisciamenti e medie funzionali

# intervallo di definizione delle funzioni (i giorni) dell'anno
rangeval <- 2:(nrow(wide_data[[1]])+1)
# tolgo da rangeval l'intervallo in cui non vi sono osservazioni (abbiamo collassato
# a zero tutte le covariate numeriche)
start <- which(train$Date == as.POSIXct("2020-02-12", tz = "GMT"))[1]
end <- which(train$Date == as.POSIXct("2020-02-24", tz = "GMT"))[1]

# basi di grado lineare per interpolare
basi <- create.bspline.basis(rangeval = range(rangeval), 
                             breaks = c(2:(start-1), 
                                        rep(start, 1),
                                        rep(end, 1),
                                        (end+1):max(rangeval)),
                             norder = 2)


smooth_variable <- lapply(names(wide_data)[-c(1, 2, 4, 5, 8, 9)], function(n) {
  smooth.basis(2:(nrow(wide_data[[n]])+1), as.matrix(wide_data[[n]]), basi)
})

names(smooth_variable) <- names(wide_data)[-c(1, 2, 4, 5, 8, 9)]

# lisciamento delle due variabili Population (vogliamo usare delle basi diverse
# in quanto non vi è il buco)
basi <- create.bspline.basis(rangeval = range(rangeval), 
                             breaks = 2:max(rangeval),
                             norder = 2)
smooth_variable <- c(smooth_variable, 
                     lapply(names(wide_data)[c(1, 2)], function(n) {
                       smooth.basis(2:(nrow(wide_data[[n]])+1), as.matrix(wide_data[[n]]), basi)
                     })
)
names(smooth_variable) <- c(names(smooth_variable)[1:12], names(wide_data)[c(1, 2)])

# liscio anche le variabili nel test set (non vi sono buchi)
smooth_test_variable <- lapply(names(wide_test_data)[-c(8, 9)], function(n) {
  smooth.basis(2:(nrow(wide_test_data[[n]])+1), as.matrix(wide_test_data[[n]]), basi)
})
names(smooth_test_variable) <- names(wide_test_data)[-c(8, 9)]

# lisciamento della risposta (mi servono delle basi differenti)
basi <- create.bspline.basis(rangeval = range(rangeval), breaks = 2:max(rangeval))
D2 <- int2Lfd(2)
b_pm25 <- fdPar(basi, D2, lambda = 1e-5)

pm25_median_smooth <- smooth.basis(2:333, as.matrix(wide_data[["pm25_median"]]), b_pm25)

#inserisco nella lista anche la risposta lisciata
smooth_variable[["pm25_median"]] <- pm25_median_smooth

# analogamente per la risposta nel test
pm25_test_smooth <-  smooth.basis(2:333, as.matrix(wide_test_data[["pm25_median"]]), b_pm25)
smooth_test_variable[["pm25_median"]] <- pm25_test_smooth


# variabile risposta
m_pm25_smooth <- mean.fd(smooth_variable[["pm25_median"]]$fd)
x_seq <- 2:333
m_pm25 <- predict(m_pm25_smooth, x_seq)
p1 <- data.frame("Date" = unique(train$Date)[-length(unique(train$Date))], "mean" = m_pm25) %>%
  ggplot(aes(x = Date, y = mean)) + 
  geom_line() +
  ylab("pm 2.5") + 
  labs(title = "PM 2.5 medio")

# osservazione: anche lisciando le funzioni con gcv la 
# media fuzionale rimane poco liscia

# temperatura
Temperature_smooth = mean.fd(smooth_variable[["temperature_median"]]$fd)
x_seq <- 2:333
m_Temp <- predict(Temperature_smooth, x_seq)
p2 <- data.frame("Date" = unique(train$Date)[-length(unique(train$Date))], "mean" = m_Temp) %>%
  ggplot(aes(x = Date, y = mean)) + 
  geom_line() +
  ylab("Temperatura") + 
  labs(title = "Temperatura media")


# Vento
Wind_smooth <- mean.fd(smooth_variable[["wind.speed_median"]]$fd)
x_seq <- 2:333
m_Wind = predict(Wind_smooth, x_seq)
p3 <- data.frame("Date" = unique(train$Date)[-length(unique(train$Date))], "mean" = m_Wind) %>%
  ggplot(aes(x = Date, y = mean)) + 
  geom_line() +
  ylab("Vento") + 
  labs(title = "Vento medio") 

#PopulationNotStayFrac
PopulationNotStayFrac_smooth = mean.fd(smooth_variable[["PopulationNotStayFrac" ]]$fd)
x_seq <- 2:333
m_Mil <- predict(PopulationNotStayFrac_smooth, x_seq)
p4 <- data.frame("Date" = unique(train$Date)[-length(unique(train$Date))], "mean" = m_Mil) %>%
  ggplot(aes(x = Date, y = mean)) + 
  geom_line() +
  ylab("Frazione usciti") + 
  labs(title = "Frazione di abitanti usciti") 

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2) 


#preparazione dati per modello state-space

# insieme di dati di validation per selezionare i parametri i varianza
XVAL <- array(1, dim = c(ncol(wide_val_data[[1]]), 7, nrow(wide_val_data[[1]])))
# dim = c(funzioni (oss), covariate (inclusa intercetta), tempo)

# inserisco le covariate nell'array
XVAL[, 2, ] <- t(wide_val_data[["temperature_median"]])
XVAL[, 3, ] <- t(wide_val_data[["pressure_median"]])
XVAL[, 4, ] <- t(wide_val_data[["humidity_median"]])
XVAL[, 5, ] <- t(wide_val_data[["wind.speed_median"]])
XVAL[, 6, ] <- t(wide_val_data[["PopulationNotStayFrac"]])
XVAL[, 7, ] <- t(wide_val_data[["PopulationTot"]])

YVAL <- as.matrix(wide_val_data$pm25_median)
names(wide_data)

XMAT <- array(1, dim = c(ncol(wide_data[[1]]), 7, nrow(wide_data[[1]])))
# dim = c(funzioni (oss), covariate (inclusa intercetta), tempo)

# inserisco le covariate nell'array
XMAT[, 2, ] <- t(wide_data[["temperature_median"]])
XMAT[, 3, ] <- t(wide_data[["pressure_median"]])
XMAT[, 4, ] <- t(wide_data[["humidity_median"]])
XMAT[, 5, ] <- t(wide_data[["wind.speed_median"]])
XMAT[, 6, ] <- t(wide_data[["PopulationNotStayFrac"]])
XMAT[, 7, ] <- t(wide_data[["PopulationTot"]])

# specificazione della matrice contenente la risposta
YMAT <- as.matrix(wide_data[["pm25_median"]])

mod.tmp = lm(as.vector(YMAT[1:5, ]) ~ 1 + as.vector(XMAT[, 2, 1:5]) +
               as.vector(XMAT[, 3, 1:5]) +
               as.vector(XMAT[, 4, 1:5]) +
               as.vector(XMAT[, 5, 1:5]) +
               as.vector(XMAT[, 6, 1:5]) +
               as.vector(XMAT[, 7, 1:5]))
a0 <- coef(mod.tmp)
P0 = summary(mod.tmp)$cov.unscaled


#preparazione dati per modello funzione-funzione

# intervallo di definizione delle funzioni (i giorni) dell'anno
rangeval <- 2:(nrow(wide_data[[1]])+1)

start <- which(train$Date == as.POSIXct("2020-02-12", tz = "GMT"))[1]
end <- which(train$Date == as.POSIXct("2020-02-24", tz = "GMT"))[1]

cities = colnames(wide_data$pm25_median)
for(i in 1:length(cities)){
  wide_data$nv = wide_data$pm25_median
  wide_data$nv[,] = 0
  wide_data$nv[,which(colnames(wide_data$nv) == cities[i])] = 1
  names(wide_data)[length(wide_data)] = paste0("dummy_", cities[i])
}

for(i in 1:length(cities)){
  wide_val_data$nv = wide_val_data$pm25_median
  wide_val_data$nv[,] = 0
  wide_val_data$nv[,which(colnames(wide_val_data$nv) == cities[i])] = 1
  names(wide_val_data)[length(wide_val_data)] = paste0("dummy_", cities[i])
}

for(i in 1:length(cities)){
  wide_test_data$nv = wide_test_data$pm25_median
  wide_test_data$nv[,] = 0
  wide_test_data$nv[,which(colnames(wide_test_data$nv) == cities[i])] = 1
  names(wide_test_data)[length(wide_test_data)] = paste0("dummy_", cities[i])
}

dummy.ind = which(grepl("dummy", names(wide_data)))

# basi di grado lineare per interpolare
basi <- create.bspline.basis(rangeval = range(rangeval), 
                             breaks = c(2:(start-1), 
                                        rep(start, 1),
                                        rep(end, 1),
                                        (end+1):max(rangeval)),
                             norder = 2)

const.bas = create.bspline.basis(rangeval = range(rangeval), nbasis = 2, norder = 1)


smooth_variable <- lapply(names(wide_data)[-c(1, 2, 3, 4, 5, 8, 9, 16, 17, 18, dummy.ind)], function(n) {
  smooth.basis(2:(nrow(wide_data[[n]])+1), as.matrix(wide_data[[n]]), basi)
})

names(smooth_variable) <- names(wide_data)[-c(1, 2, 3, 4, 5, 8, 9, 16, 17, 18, dummy.ind)]

# lisciamento delle due variabili Population (vogliamo usare delle basi diverse
# in quanto non vi è il buco)
basi <- create.bspline.basis(rangeval = range(rangeval), 
                             breaks = 2:max(rangeval),
                             norder = 2)
smooth_variable <- c(smooth_variable, 
                     lapply(names(wide_data)[18], function(n) {
                       smooth.basis(2:(nrow(wide_data[[n]])+1), as.matrix(wide_data[[n]]), basi)
                     }))
names(smooth_variable) <- c(names(smooth_variable)[1:8], names(wide_data)[18])

#names(wide_data)
#lisciamento dummy
smooth_variable <- c(smooth_variable, 
                     lapply(names(wide_data)[c(17, dummy.ind)], function(n) {
                       smooth.basis(2:(nrow(wide_data[[n]])+1), as.matrix(wide_data[[n]]), const.bas)
                     }))
names(smooth_variable) <- c(names(smooth_variable[1:9]), names(wide_data)[c(17, dummy.ind)])



# liscio anche le variabili nel test set (non vi sono buchi)
smooth_test_variable <- lapply(names(wide_test_data)[-c(1, 2, 3, 4,5,8, 9,16, 17,dummy.ind)], function(n) {
  smooth.basis(2:(nrow(wide_test_data[[n]])+1), as.matrix(wide_test_data[[n]]), basi)
})
names(smooth_test_variable) <- names(wide_test_data)[-c(1, 2, 3, 4,5,8, 9,16, 17,dummy.ind)]

#codifico gli NA con -1000
for(i in 1:ncol(wide_val_data$PopulationTot)){
  wide_val_data$PopulationTot[[i]][is.na(wide_val_data$PopulationTot[[i]])] = mean(wide_val_data$PopulationTot[[i]], na.rm=TRUE)
}

wide_val_data = lapply(wide_val_data, function(x){
  for(i in 1:ncol(x)){
    x[[i]][is.na(x[[i]])] = -1000
  }
  return(x)
})
smooth_val_variable <- lapply(names(wide_val_data)[-c(1, 2, 3, 4,5,8, 9,16, 17,dummy.ind)], function(n) {
  smooth.basis(2:(nrow(wide_val_data[[n]])+1), as.matrix(wide_val_data[[n]]), basi)
})
names(smooth_val_variable) <- names(wide_val_data)[-c(1, 2, 3, 4,5,8,9,16,17,dummy.ind)]

#lisciamento dummy
smooth_test_variable <- c(smooth_test_variable, 
                          lapply(names(wide_test_data)[c(17, dummy.ind)], function(n) {
                            smooth.basis(2:(nrow(wide_test_data[[n]])+1), as.matrix(wide_test_data[[n]]), const.bas)
                          }))
names(smooth_test_variable) <- c(names(smooth_test_variable[1:9]), names(wide_test_data)[c(17, dummy.ind)])

smooth_val_variable <- c(smooth_val_variable, 
                         lapply(names(wide_val_data)[c(17, dummy.ind)], function(n) {
                           smooth.basis(2:(nrow(wide_val_data[[n]])+1), as.matrix(wide_val_data[[n]]), const.bas)
                         }))
names(smooth_val_variable) <- c(names(smooth_val_variable[1:9]), names(wide_val_data)[c(17, dummy.ind)])
plot(smooth_val_variable$PopulationTot$fd)


basi <- create.bspline.basis(rangeval = range(rangeval), breaks = 2:max(rangeval))
D2 <- int2Lfd(2)
b_pm25 <- fdPar(basi, D2, lambda = 1e-5)

pm25_median_smooth <- smooth.basis(2:333, as.matrix(wide_data[["pm25_median"]]), b_pm25)

#inserisco nella lista anche la risposta lisciata
smooth_variable[["pm25_median"]] <- pm25_median_smooth

# analogamente per la risposta nel test
pm25_test_smooth <-  smooth.basis(2:333, as.matrix(wide_test_data[["pm25_median"]]), b_pm25)
smooth_test_variable[["pm25_median"]] <- pm25_test_smooth


pm25fd = smooth_variable[["pm25_median"]]$fd
xlist = list()
xlist[[1]] = rep(1, ncol(wide_data[["pm25_median"]]))
xlist[[2]] = smooth_variable[["temperature_median"]]$fd

# espansione in basi per i parametri beta
basi_reg = create.bspline.basis(c(2, 333), 10)
D <- int2Lfd(2)
betalist = lapply(1:2, function(x) fdPar(basi_reg, D))

xval_list <- list()
xval_list[[1]] <- rep(1, ncol(wide_val_data[["pm25_median"]]))
xval_list[[2]] <- smooth_val_variable[["temperature_median"]]$fd
pm25val_fd <- smooth_val_variable[["pm25_median"]]$fd


xtest_list <- list()
xtest_list[[1]] <- rep(1, ncol(wide_test_data[["pm25_median"]]))
xtest_list[[2]] <- smooth_test_variable[["temperature_median"]]$fd
pm25test_fd <- smooth_test_variable[["pm25_median"]]$fd



#xlist[[3]] = smooth_variable[["mil_miles" ]]$fd
xlist[[3]] = smooth_variable[["wind.speed_median" ]]$fd
xlist[[4]] = smooth_variable[["humidity_median" ]]$fd
xlist[[5]] = smooth_variable[["PopulationNotStayFrac"]]$fd
#xlist[[7]] = smooth_variable[["Population.Not.Staying.at.Home"]]$fd
xlist[[6]] = smooth_variable[["pressure_median"]]$fd
xlist[[7]] = smooth_variable[["PopulationTot"]]$fd

#xtest_list[[3]] = smooth_test_variable[["mil_miles" ]]$fd
xtest_list[[3]] = smooth_test_variable[["wind.speed_median" ]]$fd
xtest_list[[4]] = smooth_test_variable[["humidity_median"]]$fd
xtest_list[[5]] = smooth_test_variable[["PopulationNotStayFrac"]]$fd
#xtest_list[[7]] = smooth_test_variable[["Population.Not.Staying.at.Home"]]$fd
xtest_list[[6]] = smooth_test_variable[["pressure_median"]]$fd
xtest_list[[7]] = smooth_test_variable[["PopulationTot"]]$fd

#xval_list[[3]] = smooth_val_variable[["mil_miles" ]]$fd
xval_list[[3]] = smooth_val_variable[["wind.speed_median" ]]$fd
xval_list[[4]] = smooth_val_variable[["humidity_median"]]$fd
xval_list[[5]] = smooth_val_variable[["PopulationNotStayFrac"]]$fd
#xval_list[[7]] = smooth_val_variable[["Population.Not.Staying.at.Home"]]$fd
xval_list[[6]] = smooth_val_variable[["pressure_median"]]$fd
xval_list[[7]] = smooth_val_variable[["PopulationTot"]]$fd


# espansione in basi per i parametri beta
basi_reg = create.bspline.basis(c(2, 333), 10)

Lbasis  <- create.constant.basis(c(2,333),
                                 axes=list("axesIntervals"))


Lcoef   <- matrix(1, ncol=2)
bfdobj  <- fd(Lcoef,Lbasis)
bwtlist <- fd2list(bfdobj)
penal <- Lfd(2, bwtlist)

D <- penal#int2Lfd(2)
betalist = lapply(1:length(xlist), function(x) fdPar(basi_reg, D))
for(i in 1:length(betalist)){betalist[[i]]$lambda = exp(-1)}


# ##inserisco dummy
# length(xlist)
# names(smooth_variable)
# dummy.ind = c(10, which(grepl("dummy", names(smooth_variable))))
# for(i in dummy.ind){
#   xlist[[length(xlist)+1]] = smooth_variable[[i]]$fd
#   xval_list[[length(xlist)+1]] = smooth_variable[[i]]$fd
#   xtest_list[[length(xlist)+1]] = smooth_variable[[i]]$fd
# }
# basi_reg_const = create.constant.basis(c(2, 333))
# length(betalist)
# length(xlist)
# for(i in (length(betalist)+1):length(xlist)){
#   betalist[[i]] = fdPar(basi_reg_const, lambda=0)
# }


modtmp = fRegress(pm25fd, xlist, betalist)


risp_fun_camp <- predict(modtmp$betaestlist[[1]]$fd, 2:333) %*% rep(1,length(xval_list[[1]]))
for(k in 2:7)
  risp_fun_camp <- risp_fun_camp + predict(modtmp$betaestlist[[k]]$fd, 2:333) %*% rep(1,length(xval_list[[1]])) * predict(xval_list[[k]], 2:333)

pred_tmp = risp_fun_camp
for(i2 in 1:ncol(pred_tmp)){
  for(i3 in 2:7)
    pred_tmp[,i2][xval_list[[i3]]$coefs[,i2]==-1000] = NA
}
nam = c("Intercept", "temp", "wind", "humidity", "population not home frac", "pressure", "PopulationTot")
par(mfrow=c(3,3))
for(i in 1:7){
  plot(modtmp$betaestlist[[i]]$fd, main=nam[i])
}
par(mfrow=c(1,1))

create_penalty = function(lambda, c0, c1){
  Lcoef   <- matrix(c(c0,c1), 1, 2)
  bfdobj  <- fd(Lcoef,Lbasis)
  bwtlist <- fd2list(bfdobj)
  penal <- Lfd(2, bwtlist)
  res = fdPar(basi_reg, penal)
  res$lambda <- lambda
  return(res)
}

lambda <- unlist(lapply(betalist, function(l) l$lambda))
coefs = unlist(lapply(betalist, function(l) {
  c(l$Lfd$bwtlist[[1]]$coefs, l$Lfd$bwtlist[[2]]$coefs)
}))

err.drop = 100
err.last = NULL
err.curr = NULL
while(err.drop > 0.001){
  for(var.ind in c(4,1,2,3)){
    opt2 <- optim(c(log(lambda[var.ind]), log(lambda[var.ind]*coefs[c(1,2)+2*(var.ind-1)])),function(x) {
      betalist[[var.ind]] = create_penalty(exp(x[1]), exp(x[2])/exp(x[1]), exp(x[3])/exp(x[1]))
      
      modtmp = fRegress(pm25fd, xlist, betalist)
      
      risp_fun_camp <- predict(modtmp$betaestlist[[1]]$fd, 2:333) %*% rep(1,length(xval_list[[1]]))
      for(k in 2:7)
        risp_fun_camp <- risp_fun_camp + predict(modtmp$betaestlist[[k]]$fd, 2:333) %*% rep(1,length(xval_list[[1]])) * predict(xval_list[[k]], 2:333)
      
      pred_tmp = risp_fun_camp
      for(i2 in 1:ncol(pred_tmp)){
        for(i3 in 2:7)
          pred_tmp[,i2][xval_list[[i3]]$coefs[,i2]==-1000] = NA
      }
      
      res_tmp <- as.matrix(wide_val_data[["pm25_median"]]) - pred_tmp
      for(i2 in 1:ncol(res_tmp)){
        res_tmp[,i2][wide_val_data[["pm25_median"]][[i2]]==-1000] = NA
      }
      apply(res_tmp, 2, function(x)max(abs(x), na.rm = T))
      print(c(exp(x)))
      print(mean(as.vector(res_tmp)**2, na.rm = TRUE))
      return(mean(as.vector(res_tmp)**2, na.rm = TRUE))
    }, control=list(maxit = 40, parscale=c(30,30,30)))
    
    betalist[[var.ind]] = create_penalty(exp(opt2$par[1]), exp(opt2$par[2])/exp(opt2$par[1]), exp(opt2$par[3])/exp(opt2$par[1]))
    err.curr = opt2$val
    print("Errore:")
    print(err.curr)
    print(var.ind)
    
    modtmp = fRegress(pm25fd, xlist, betalist)
    par(mfrow=c(3,3))
    for(i in 1:7){
      plot(modtmp$betaestlist[[i]]$fd, main=nam[i])
    }
    par(mfrow=c(1,1))
  }
  if(!is.null(err.last)){
    err.drop = err.last - err.curr
  }
  err.last = err.curr
  
  lambda <- unlist(lapply(betalist, function(l) l$lambda))
  coefs = unlist(lapply(betalist, function(l) {
    c(l$Lfd$bwtlist[[1]]$coefs, l$Lfd$bwtlist[[2]]$coefs)
  }))
}


modtmp = fRegress(pm25fd, xlist, betalist)

risp_fun_camp <- predict(modtmp$betaestlist[[1]]$fd, 2:333) %*% rep(1,length(xtest_list[[1]]))
for(k in 2:7)
  risp_fun_camp <- risp_fun_camp + predict(modtmp$betaestlist[[k]]$fd, 2:333) %*% rep(1,length(xtest_list[[1]])) * predict(xtest_list[[k]], 2:333)

pred_tmp = risp_fun_camp

res_tmp <- as.matrix(wide_test_data[["pm25_median"]]) - pred_tmp
mean(as.vector(res_tmp)**2)

par(mfrow=c(3,5))
for(i in 1:13){
  matplot(as.matrix(wide_test_data[["pm25_median"]])[,i], type="l", ylim=c(0,100), xlim=c(0,333))
  matplot(pred_tmp[,i], type="l", col="red", add=TRUE)
  # matplot(fit_test.save[,i], type="l", col="green", add=TRUE)
}


model_fun_all = modtmp

res_fun_all <- pm25fd - model_fun_all$yhatfdobj
res_fun_all_fd <- eval.fd(2:333, fdobj = res_fun_all)

sigmaE_all <- (res_fun_all_fd) %*% t(res_fun_all_fd) / (ncol(YMAT)-7)
fun.err_all <- fRegress.stderr(model_fun_all, y2cMap = smooth_variable[["pm25_median"]]$y2cMap, sigmaE_all)

estimatepar <- data.frame("Date" = unique(train$Date)[-length(unique(train$Date))]) 
for(i in 1:length(betalist))
{
  tmpdat <- data.frame(predict(model_fun_all$betaestlist[[i]], 2:333),
                       predict(model_fun_all$betaestlist[[i]]$fd - 2* fun.err_all$betastderrlist[[i]], 2:333),
                       predict(model_fun_all$betaestlist[[i]]$fd + 2* fun.err_all$betastderrlist[[i]], 2:333))
  estimatepar <- cbind.data.frame(estimatepar, tmpdat)
  
}


colnames(estimatepar) <- c("Date", "Intercetta", "Intercetta_lo", "Intercetta_up", 
                           "Temperatura", "Temperatura_lo", "Temperatura_up",
                           "Wind", "Wind_lo", "Wind_up",
                           "Humidity", "Humidity_lo", "Humidity_up",
                           "Pop.not.homeFrac", "Pop.not.homeFrac_lo", "Pop.not.homeFrac_up",
                           "Pressure", "Pressure_lo", "Pressure_up",
                           "PopulationTot", "PopulationTot_lo", "PopulationTot_up")

p1 <- estimatepar %>% 
  ggplot(aes(x = Date, y = Intercetta)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Intercetta_lo, ymax=Intercetta_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Intercetta") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")

p2 <- estimatepar %>% 
  ggplot(aes(x = Date, y = Temperatura)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Temperatura_lo, ymax=Temperatura_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Temperatura") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")

p3 <- estimatepar %>% 
  ggplot(aes(x = Date, y = Humidity)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Humidity_lo, ymax=Humidity_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Humidity") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")

p4 <- estimatepar %>% 
  ggplot(aes(x = Date, y = Wind)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Wind_lo, ymax=Wind_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Wind") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")

p5 <- estimatepar %>% 
  ggplot(aes(x = Date, y = Pressure)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Pressure_lo, ymax=Pressure_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Pressure") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")

p6 <- estimatepar %>% 
  ggplot(aes(x = Date, y = Pop.not.homeFrac)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Pop.not.homeFrac_lo, ymax=Pop.not.homeFrac_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Pop.not.homeFrac") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")


p7 <- estimatepar %>% 
  ggplot(aes(x = Date, y = PopulationTot)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=PopulationTot_lo, ymax=PopulationTot_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "PopulationTot") + geom_hline(yintercept = 0, linetype = 2, color = "darkgreen") +
  ylab("")



ggarrange(p1, p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 3, common.legend = TRUE, legend="right") 



#state space "ad errore incorrelato"
library(KFAS)

m0 <- SSModel(YMAT ~ -1 + SSMcustom(Z = XMAT, T = diag(7), Q = diag(7), a1 = a0, P1 = P0))
pm0 <- KFS(m0)

fup <- function(p, modello)
{
  modello["Q"] <- diag(c(exp(p[1:7])))
  modello["H"] <- diag(exp(p[3]), NROW(modello["H"]))
  return(modello)
}

pps = fitSSM(pm0$model, c(rep(0.5, 8), 0.5, 0.5), updatefn = fup, method = "L-BFGS-B", control=list(trace=1)) #"L-BFGS-B"
exp(pps$optim.out$par)
pps$optim.out$value


p = pps$optim.out$par

image((weather.corr + matern(dst, exp(p[9]), exp(p[10])))/2)

# stimo il modello con i parametri di varianza aggiornati
pm1 <- KFS(pps$model)
betahat <- data.frame("Estimate" = as.vector(coef(pm1)))
betahat$Date <- rep(unique(train$Date)[-length(unique(train$Date))], 7)
betanames<- c("Intercept", "temperature_median", "pressure_median", 
              "humidity_median", "wind.speed_median", 
              "PopulationNotStayFrac", "PopulationTot")
betahat$parametro <- vctrs::vec_rep_each(betanames, 332)

betahat %>% ggplot(aes(x = Date, y = Estimate, color = parametro)) + 
  geom_line() +
  facet_wrap(~ parametro, scales = "free") + 
  labs(title = "Kalman Smoother estimate")

# stime dei parametri con kalman smoother
est_beta <- coef(pm1, filtered = F)
colnames(est_beta) <- betanames

# ottengo i fitted ed i residui usando i parametri stimati con kalman smoother
fit_val <- matrix(NA, 332, nrow(XMAT[, , 1]))
for(i in 1:332)
  fit_val[i, ] <- XMAT[, , i] %*% est_beta[i, ]

# residui
res_ssmodel <- YMAT - fit_val
data.frame("Residuals" = as.vector(res_ssmodel), 
           "City" = vctrs::vec_rep_each(colnames(YMAT), 332),
           "Date" =  rep(unique(train$Date)[-length(unique(train$Date))], nrow(XMAT[, , 1]))) %>% 
  ggplot(aes(x = Date, y = Residuals, group = City)) + 
  geom_line()


#modello state space ad errore correlato
m0 <- SSModel(YMAT ~ -1 + SSMcustom(Z = XMAT, T = diag(7), Q = diag(7), a1 = a0, P1 = P0))
pm0 <- KFS(m0)
m0$P1

dst = as.matrix(dist(matrix(c(as.numeric(wide_data[["longitude"]][1,]), as.numeric(wide_data[["latitude"]][1,])), ncol=2)))


weather.corr = (cor(wide_data[["temperature_median"]]) +
                  cor(wide_data[["pressure_median"]]) +
                  cor(wide_data[["humidity_median"]]) +
                  cor(wide_data[["wind.speed_median"]]) )/4
# può avere senso utilizzare i dati per stimare la varianza dell'errore

fup <- function(p, modello)
{
  modello["Q"] <- diag(c(exp(p[1:7])))
  #cor.tmp = ifelse((plogis(p[4])-0.5)*2>=0.99, 0.99, ifelse((plogis(p[4])-0.5)*2<(-0.99), -0.99, (plogis(p[4])-0.5)*2))
  #cor.tmp = 0.5#plogis(p[9])
  #print(cor.tmp)
  #mmm = matrix(exp(p[3])*cor.tmp, ncol=NROW(modello["H"]), nrow=NROW(modello["H"])) - diag(exp(p[3])*cor.tmp, NROW(modello["H"]))
  #mmm = mmm + diag(exp(p[3]), NROW(modello["H"]))
  modello["H"] <- (weather.corr + matern(dst, exp(p[9]), exp(p[10])))/2 * exp(p[8])
  return(modello)
}

pps = fitSSM(pm0$model, c(rep(0.5, 8), 0.5, 0.5), updatefn = fup, method = "L-BFGS-B", control=list(trace=1)) #"L-BFGS-B"
exp(pps$optim.out$par)
pps$optim.out$value


p = pps$optim.out$par

image((weather.corr + matern(dst, exp(p[9]), exp(p[10])))/2)

# stimo il modello con i parametri di varianza aggiornati
pm1 <- KFS(pps$model)
betahat <- data.frame("Estimate" = as.vector(coef(pm1)))
betahat$Date <- rep(unique(train$Date)[-length(unique(train$Date))], 7)
betanames<- c("Intercept", "temperature_median", "pressure_median", 
              "humidity_median", "wind.speed_median", 
              "PopulationNotStayFrac", "PopulationTot")
betahat$parametro <- vctrs::vec_rep_each(betanames, 332)

betahat %>% ggplot(aes(x = Date, y = Estimate, color = parametro)) + 
  geom_line() +
  facet_wrap(~ parametro, scales = "free") + 
  labs(title = "Kalman Smoother estimate")

# stime dei parametri con kalman smoother
est_beta <- coef(pm1, filtered = F)
colnames(est_beta) <- betanames

# ottengo i fitted ed i residui usando i parametri stimati con kalman smoother
fit_val <- matrix(NA, 332, nrow(XMAT[, , 1]))
for(i in 1:332)
  fit_val[i, ] <- XMAT[, , i] %*% est_beta[i, ]

# residui
res_ssmodel <- YMAT - fit_val
data.frame("Residuals" = as.vector(res_ssmodel), 
           "City" = vctrs::vec_rep_each(colnames(YMAT), 332),
           "Date" =  rep(unique(train$Date)[-length(unique(train$Date))], nrow(XMAT[, , 1]))) %>% 
  ggplot(aes(x = Date, y = Residuals, group = City)) + 
  geom_line()



#regolazione state space su validation

eval.par.valid <- function(par, XMAT, YMAT, XVAL, YVAL, a0, dst)
{
  # stimo il modello
  print(par)
  
  m0 <- SSModel(YMAT ~ -1 + SSMcustom(Z = XMAT, T = diag(7), Q = diag(exp(par)), a1 = a0, P1 = P0))
  pm0 <- KFS(m0)
  
  
  fup <- function(p, modello)
  {
    modello["H"] <- (0.5*weather.corr + (1-0.5)*matern(dst, exp(p[1]), exp(p[2]))) * exp(p[3])
    return(modello)
  }
  pps = fitSSM(pm0$model, rep(0, 3), updatefn = fup)
  print(exp(pps$optim.out$par))
  image((weather.corr + matern(dst, exp(par[1]), exp(par[2])))/2)
  
  # stimo il modello con i parametri di varianza degli errori aggiornati
  pmtmp <- KFS(pps$model)
  #return(pmtmp)
  
  # calcolo l'errore sul val set
  est_beta <- coef(pmtmp, filtered = F)
  
  # ottengo i fitted ed i residui usando i parametri stimati con kalman smoother
  fit_val <- matrix(NA, 332, nrow(XVAL[, 1, ]))
  
  for(i in 1:332)
    fit_val[i, ] <- XVAL[, , i] %*% est_beta[i, ]
  
  #print(par)
  print(mean((YVAL - fit_val)^2, na.rm = T))
  return(mean((YVAL - fit_val)^2, na.rm = T))
}

opt <- optim(rep(-2, 7), eval.par.valid, XMAT = XMAT, YMAT = YMAT, 
             XVA = XVAL, YVAL = YVAL, a0 = a0, dst = dst, method="BFGS") #control=list(trace=1))

#risultato [1] -14.436489  -2.622271 -17.815600  -3.207538  -1.870813  -6.922312  -4.694066

par = opt$par
eval.par.valid(par - c(0,0,0,0,0,0,0), XMAT = XMAT, YMAT = YMAT,
               XVA = XVAL, YVAL = YVAL, a0 = a0, dst = dst)


m0 <- SSModel(YMAT ~ -1 + SSMcustom(Z = XMAT, T = diag(7), Q = diag(exp(par)), a1 = a0, P1 = P0))
pm0 <- KFS(m0)


fup <- function(p, modello)
{
  modello["H"] <- (0.5*weather.corr + (1-0.5)*matern(dst, exp(p[1]), exp(p[2]))) * exp(p[3])
  return(modello)
}
pps = fitSSM(pm0$model, rep(0, 3), updatefn = fup) 
exp(pps$optim.out$par)


pm1 <- KFS(pps$model)

vars = apply(pm1$V, 3, diag)

betahat <- data.frame("Estimate" = as.vector(pm1$alphahat))
betahat$Date <- rep(unique(train$Date)[-length(unique(train$Date))], 7)
betanames<- c("Intercept", "temperature_median", "pressure_median", 
              "humidity_median", "wind.speed_median", "PopulationNotStayFrac", 
              "PopulationTot")
betahat$parametro <- vctrs::vec_rep_each(betanames, 332)
betahat$sd = as.vector(sqrt(t(vars)))
betahat$uci = betahat$Estimate + betahat$sd * qnorm(0.975, 0, 1)
betahat$lci = betahat$Estimate - betahat$sd * qnorm(0.975, 0, 1)

betahat %>% ggplot(aes(x = Date, y = Estimate, color = parametro)) + 
  geom_line() +
  geom_line(aes(x = Date, y = uci, color = parametro), linetype = 2) +
  geom_line(aes(x = Date, y = lci, color = parametro), linetype = 2) +
  geom_hline(yintercept = 0, colour = "grey") +
  facet_wrap(~ parametro, scales = "free") +
  labs(title = "Kalman Smoother estimate") + theme(legend.position = "none")+ xlab("") + ylab("")



# calcolo l'errore sul val set
est_beta <- coef(pm1, filtered = F)

# ottengo i fitted ed i residui usando i parametri stimati con kalman smoother
fit_val <- matrix(NA, 332, nrow(XVAL[, 1, ]))
for(i in 1:332)
  fit_val[i, ] <- XVAL[, , i] %*% est_beta[i, ]

par(mfrow=c(6,6))
par(mai=c(0,0,0,0))
for(i in 1:36){
  matplot(YVAL[,i], type="l", ylim=c(0,100), xlim=c(0,333))
  matplot(fit_val[,i], type="l", col="red", add=TRUE)
}

par(mai = c(1.02, 0.82, 0.82, 0.42))


wide_test_data <- lapply(1:length(var_names), function(i) {
  tp = (test %>%
          pivot_wider(id_cols = City, names_from = Date, values_from = var_names[i]))[, -1] %>% 
    as.matrix %>% t %>% zoo::na.approx() %>% as_tibble
  colnames(tp) = unique(test$City)
  tp
})

names(wide_test_data) <- var_names

# insieme di dati di validation per selezionare i parametri i varianza
XTEST <- array(1, dim = c(ncol(wide_test_data[[1]]), 7, nrow(wide_test_data[[1]])))
# dim = c(funzioni (oss), covariate (inclusa intercetta), tempo)

# inserisco le covariate nell'array
XTEST[, 2, ] <- t(wide_test_data[["temperature_median"]])
XTEST[, 3, ] <- t(wide_test_data[["pressure_median"]])
XTEST[, 4, ] <- t(wide_test_data[["humidity_median"]])
XTEST[, 5, ] <- t(wide_test_data[["wind.speed_median"]])
XTEST[, 6, ] <- t(wide_test_data[["PopulationNotStayFrac"]])
XTEST[, 7, ] <- t(wide_test_data[["PopulationTot"]])

YTEST <- as.matrix(wide_test_data$pm25_median)



# calcolo l'errore sul test set
est_beta <- coef(pm1, filtered = F)

# ottengo i fitted ed i residui usando i parametri stimati con kalman smoother
fit_test <- matrix(NA, 332, nrow(XTEST[, 1, ]))
for(i in 1:332)
  fit_test[i, ] <- XTEST[, , i] %*% est_beta[i, ]

par(mfrow=c(2,2))
par(mai=c(0.2,0.2,0.2,0.2))
for(i in c(4,5,7,13)){
  matplot(YTEST[,i], type="l", ylim=c(0,100), xlim=c(0,333))
  matplot(fit_test[,i], type="l", col="red", add=TRUE)
  # matplot(fit_test.save[,i], type="l", col="green", add=TRUE)
}

mean((YTEST - fit_test)^2, na.rm = T)

colMeans((YTEST - fit_test)^2, na.rm = T)

data.frame("PM2.5" = c(as.vector(YTEST), as.vector(fit_test)),
           "Type" = c(rep("Observed", 332*13), rep("Predicted", 332*13)),
           "City" = rep(rep(unique(test$City), each = 332), 2),
           "Date" = rep(rep(unique(train$Date)[-length(unique(train$Date))], 13), 2)) |>
  ggplot(aes(x = Date, y = PM2.5, group = Type, color = Type)) + 
  geom_line() + 
  facet_wrap(~ City) + 
  scale_color_manual(values=c("black", "red"))


# preparazione dataset per stima modello spazio-tempo
data_ssm <- data.frame("City" = vctrs::vec_rep_each(1:50, 332),
                       "Temperature" = as.vector(as.matrix(wide_data[["temperature_median"]])),
                       "Temperature_id" = rep(2:333, 50), 
                       "Wind" = as.vector(as.matrix(wide_data[["wind.speed_median"]])),
                       "Wind_id" = rep(2:333, 50),
                       "Humidity" = as.vector(as.matrix(wide_data[["humidity_median"]])),
                       "Humidity_id" = rep(2:333, 50),
                       "Pressure" = as.vector(as.matrix(wide_data[["pressure_median"]])),
                       "Pressure_id" = rep(2:333, 50),
                       "Miles" = as.vector(as.matrix(wide_data[["mil_miles"]])),
                       "Miles_id" = rep(2:333, 50),
                       "PopulationTot" = as.vector(as.matrix(wide_data[["PopulationTot"]])),
                       "PopulationTot_id" = rep(2:333, 50),
                       "PopulationFrac" = as.vector(as.matrix(wide_data[["PopulationNotStayFrac"]])),
                       "PopulationFrac_id" = rep(2:333, 50),
                       "Time" = rep(2:333, 50),
                       "y" = as.vector(as.matrix(wide_data[["pm25_median"]])))

data_test <- data.frame("City" = vctrs::vec_rep_each(1:13, 332),
                        "Temperature" = as.vector(as.matrix(wide_test_data[["temperature_median"]])),
                        "Temperature_id" = rep(2:333, 13), 
                        "Wind" = as.vector(as.matrix(wide_test_data[["wind.speed_median"]])),
                        "Wind_id" = rep(2:333, 13),
                        "Humidity" = as.vector(as.matrix(wide_test_data[["humidity_median"]])),
                        "Humidity_id" = rep(2:333, 13),
                        "Pressure" = as.vector(as.matrix(wide_test_data[["pressure_median"]])),
                        "Pressure_id" = rep(2:333, 13),
                        "Miles" = as.vector(as.matrix(wide_test_data[["mil_miles"]])),
                        "Miles_id" = rep(2:333, 13),
                        "PopulationTot" = as.vector(as.matrix(wide_test_data[["PopulationTot"]])),
                        "PopulationTot_id" = rep(2:333, 13),
                        "PopulationFrac" = as.vector(as.matrix(wide_test_data[["PopulationNotStayFrac"]])),
                        "PopulationFrac_id" = rep(2:333, 13),
                        "Time" = rep(2:333, 13),
                        "y" = rep(NA, 332*13))

# inserisco dati di stima e verifica in modo di ottenere tutte le previsioni in modo automatico                       
data_all <- rbind.data.frame(data_ssm, data_test)  



# modello spazio temporale con coefficienti dinamici rw1
# coordinate osservate
coord <- cbind(unique(as.vector(as.matrix(wide_data[["longitude"]]))), 
               unique(as.vector(as.matrix(wide_data[["latitude"]]))))


mesh2d <- inla.mesh.2d(loc.domain = coord, max.e = c(5, 7))

# definisco la matrice di proiezione dell'effetto casuale spaziale
coord <- cbind(as.vector(as.matrix(wide_data[["longitude"]])), 
               as.vector(as.matrix(wide_data[["latitude"]])))

A_train <- inla.spde.make.A(mesh2d, loc = coord, group=rep(2:333, 50),  n.group=333)

# specificazione del modello spde
spde1 <- inla.spde2.matern(
  mesh = mesh2d, alpha = 2) 

# indici per definire lo stack
s_index <- inla.spde.make.index(name="spatial.field",
                                n.spde=spde1$n.spde,
                                n.group=333)

# preparazione dei dati
stk_train <- inla.stack(data = list(y = data_ssm$y), A = list(A_train, 1),
                   effects = list(c(s_index,list(Intercept=1)), 
                                  list(data_ssm[, 2:16])), tag = 'est')

coord_test <- cbind(as.vector(as.matrix(wide_test_data[["longitude"]])), 
                    as.vector(as.matrix(wide_test_data[["latitude"]])))

# dati del test set
A_test <- inla.spde.make.A(mesh = mesh2d, loc = coord_test,
                           group=rep(2:333, 13),
                           n.group=333)

stk_test <- inla.stack(data = list(y = rep(NA, nrow(data_test))), A = list(A_test, 1),
                        effects = list(c(s_index, list(Intercept=1)),
                                       list(data_test[, 2:16])), tag="pred")

stack_all <- inla.stack(stk_train, stk_test)

# formula del modello
formula2 <- y ~  -1 + Intercept + 
  f(spatial.field, model=spde1, group=spatial.field.group, control.group=list(model="ar1")) +
  Temperature + f(Temperature_id, Temperature, model = "rw1") + 
  Wind + f(Wind_id, Wind, model = "rw1") + 
  Humidity + 
  PopulationTot + 
  Pressure + 
  PopulationFrac 


model_spatial <- inla(formula2,  
                      data = inla.stack.data(stack, spde=spde1), 
                      family="gaussian",
                      control.predictor=list(A=inla.stack.A(stack), compute=TRUE), 
                      control.compute = list(dic = TRUE), verbose = T,
                      control.inla = list(strategy = 'laplace'))

# creo una matrice contenente tutti i parametri
betaest <- do.call(cbind, lapply(2:3, function(i) model_spatial$summary.fixed[i, "mean"]+
                                   model_spatial$summary.random[[i]][, "mean"]))
betaest <- cbind(betaest, do.call(cbind, lapply(4:7, function(i) model_spatial$summary.fixed[i, "mean"]*rep(1, 332))))

XTEST <- array(1, dim = c(ncol(wide_test_data[[1]]), 6, nrow(wide_test_data[[1]])))
# dim = c(funzioni (oss), covariate (inclusa intercetta), tempo)

# test set
XTEST[, 1, ] <- t(wide_test_data[["temperature_median"]])
XTEST[, 2, ] <- t(wide_test_data[["wind.speed_median"]])
XTEST[, 3, ] <- t(wide_test_data[["humidity_median"]])
XTEST[, 4, ] <- t(wide_test_data[["PopulationTot"]])
XTEST[, 5, ] <- t(wide_test_data[["pressure_median"]])
XTEST[, 6, ] <- t(wide_test_data[["PopulationNotStayFrac"]])

YTEST <- as.matrix(wide_test_data$pm25_median)

XVAL <- array(1, dim = c(ncol(wide_val_data[[1]]), 6, nrow(wide_val_data[[1]])))
# dim = c(funzioni (oss), covariate (inclusa intercetta), tempo)

# ival set
XVAL[, 1, ] <- t(wide_val_data[["temperature_median"]])
XVAL[, 2, ] <- t(wide_val_data[["wind.speed_median"]])
XVAL[, 3, ] <- t(wide_val_data[["humidity_median"]])
XVAL[, 4, ] <- t(wide_val_data[["PopulationTot"]])
XVAL[, 5, ] <- t(wide_val_data[["pressure_median"]])
XVAL[, 6, ] <- t(wide_val_data[["PopulationNotStayFrac"]])


YVAL <- as.matrix(wide_val_data$pm25_median)

# regolazione della complessità del lisciamento dell'intercetta mediante 
# set di validation

# effetti spazio temporali
mu_city <- matrix(A_train %*% model_spatial$summary.random$spatial.field$mean, 332, 50, byrow = F) + 
  as.numeric(model_spatial$summary.fixed[1, "mean"])
colnames(mu_city) <- colnames(wide_data[[1]])

# funzione per identificare i lisciamenti ottimi delle intercette in prevision
val_intercept_eval <- function(lambda, XVAL, YVAL, mu_city, beta, basi, D2)
{
  bpenal <- fdPar(basi, D2, lambda = exp(lambda))
  mu.fd <- smooth.basis(2:333, mu_city[, colnames(mu_city)%in%colnames(YVAL)], bpenal)
  mu_p <- predict(mu.fd$fd, 2:333)
  
  fit_val <- matrix(NA, 332, dim(XVAL)[1])
  for(i in 1:332)
    fit_val[i, ] <- XVAL[, , i] %*% beta[i, ] + mu_p[i,]
  
  return(mean(as.vector(YVAL - fit_val)**2, na.rm = TRUE))
}

# definisco le basi 
basi <- create.bspline.basis(rangeval = range(rangeval), 20)
D2 <- int2Lfd(2)

# minimizzazione dell'errore sul validation
opt <- optim(15, val_intercept_eval, XVAL = XVAL, YVAL = YVAL, mu_city = mu_city, 
             beta = betaest, basi = basi, D2 = D2, method = "BFGS")


# liscio le intercette con l'ammontare di penalizzazione ottimale
bpenal <- fdPar(basi, D2, lambda =exp(opt$par))
mu.fd <- smooth.basis(2:333, mu_city[, colnames(mu_city)%in%colnames(YTEST)], bpenal)
mu_p <- predict(mu.fd$fd, 2:333)

# previsioni sul test
fit_val <- matrix(NA, 332, 13)
for(i in 1:332)
  fit_val[i, ] <- XTEST[, , i] %*% betaest[i, ] + mu_p[i, ] 

# MSe sul test set
MSE_spatial_RW1 <- mean((YTEST - fit_val)**2)

# grafici dei parametri stimati
estpar <- data.frame("Temperature" = model_spatial$summary.random$Temperature_id[, "mean"] + model_spatial$summary.fixed$mean[2],
                     "Temperature_lo" = model_spatial$summary.random$Temperature_id[, "0.025quant"] + model_spatial$summary.fixed[2, "0.025quant"],
                     "Temperature_up" = model_spatial$summary.random$Temperature_id[, "0.975quant"] + model_spatial$summary.fixed[2, "0.975quant"],
                     "Wind" = model_spatial$summary.random$Wind_id[, "mean"] + model_spatial$summary.fixed$mean[3],
                     "Wind_lo" = model_spatial$summary.random$Wind_id[, "0.025quant"] + model_spatial$summary.fixed[3,"0.025quant"],
                     "Wind_up" = model_spatial$summary.random$Wind_id[, "0.975quant"] + model_spatial$summary.fixed[3,  "0.975quant"],
                     "Date" = unique(train$Date)[-length(unique(train$Date))]
)


p1 <- estpar %>% ggplot(aes(x = Date, y = Temperature)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Temperature_lo, ymax=Temperature_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Temperature")

p2 <- estpar %>% ggplot(aes(x = Date, y = Wind)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Wind_lo, ymax=Wind_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Wind")

mu.fd <- smooth.basis(2:333, mu_city, bpenal)
mu_p <- predict(mu.fd$fd, 2:333)

p3 <- data.frame("Intercept" = as.vector(mu_p), 
                 "City" = rep(unique(train$City), each = 332),
                 "Date" = unique(train$Date)[-length(unique(train$Date))]) %>% 
  ggplot(aes(x = Date, y = Intercept, group = City, color = City)) + 
  geom_line() + 
  labs(title = "Intercept") + 
  theme(legend.position="none")

ggarrange(p3, p1, p2, nrow = 2, ncol = 2)

#---------------
# il coefficiente associato a temperatura è molto frastagliato, si ritiene 
# che un coefficiente liscio possa migliorare il modello in termini di 
# previsioni riducendo la variabilità

# stima dei coefficienti di temperatura e vento mediante spline quadratiche
mesh1d <- inla.mesh.1d(seq(2, 333, l = 20), degree = 2) 

# matrici del modello
Atemp <- inla.spde.make.A(mesh1d, rep(2:333, 50)) * 
  kronecker(data_ssm$Temperature, t(rep(1, 19)))
Atemp_test <- inla.spde.make.A(mesh1d, rep(2:333, 13)) *
  kronecker(data_test$Temperature, t(rep(1, 19)))

Awind <- inla.spde.make.A(mesh1d, rep(2:333, 50)) * 
  kronecker(data_ssm$Wind, t(rep(1, 19)))
Awind_test <- inla.spde.make.A(mesh1d, rep(2:333, 13)) *
  kronecker(data_test$Wind, t(rep(1, 19)))

# matrice prior per gli effetti casuali delle spline 
spde2 <- inla.spde2.matern(mesh1d, constr = FALSE)

spde2.idx <- inla.spde.make.index("temp", n.spde = spde2$n.spde)
spde3.idx <- inla.spde.make.index("wind", n.spde = spde2$n.spde)


stk_train <- inla.stack(data = list(y = data_ssm$y), A = list(Awind, Atemp, A_train, 1),
                   effects = list(spde3.idx, spde2.idx, c(s_index,list(Intercept=1)), 
                                  list(data_ssm[, 2:16])), tag = 'est')

stk_test <- inla.stack(data = list(y = rep(NA, nrow(data_test))), A = list(Awind_test, Atemp_test, A_test, 1),
                        effects = list(spde3.idx, spde2.idx, c(s_index,list(Intercept=1)),
                                       list(data_test[, 2:16])), tag="pred")

stack <- inla.stack(stk_train, stk_test)

# formula del modello
formula2 <- y ~  -1 + Intercept + 
  f(spatial.field, model=spde1,
    group=spatial.field.group, control.group=list(model="ar1")) +
  f(temp, model = spde2) +
  f(wind, model = spde2) + 
  Humidity + 
  PopulationTot + 
  Pressure + 
  PopulationFrac 

model_spatial <- inla(formula2,  
                      data = inla.stack.data(stack, spde=list(spde1, spde2)), 
                      family="gaussian",
                      control.predictor=list(A=inla.stack.A(stack), compute=TRUE), 
                      control.compute = list(dic = TRUE), verbose = T,
                      control.inla = list(strategy = 'laplace'))




Ap <- inla.spde.make.A(mesh1d, 2:333) 

# grafici dei parametri stimati
estpar <- data.frame("Temperature" = as.vector(Ap %*% model_spatial$summary.random$temp[, "mean"]),
                     "Temperature_lo" = as.vector(Ap %*% model_spatial$summary.random$temp[, "0.025quant"]),
                     "Temperature_up" = as.vector(Ap %*% model_spatial$summary.random$temp[, "0.975quant"]),
                     "Wind" = as.vector(Ap %*% model_spatial$summary.random$wind[, "mean"]),
                     "Wind_lo" = as.vector(Ap %*% model_spatial$summary.random$wind[, "0.025quant"]),
                     "Wind_up" = as.vector(Ap %*% model_spatial$summary.random$wind[, "0.975quant"]),
                     "Date" = unique(train$Date)[-length(unique(train$Date))]
)


p1 <- estpar %>% ggplot(aes(x = Date, y = Temperature)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Temperature_lo, ymax=Temperature_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Temperature")

p2 <- estpar %>% ggplot(aes(x = Date, y = Wind)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=Wind_lo, ymax=Wind_up), alpha=0.1, fill = "green",  
              color = "black", linetype = "dotted") + 
  labs(title = "Wind")


ggarrange(p1, p2, nrow = 1, ncol = 2)


# parametri stimati
betaest <- cbind(estpar$Temperature, estpar$Wind)
betaest <- cbind(betaest, do.call(cbind, lapply(2:5, function(i) model_spatial$summary.fixed[i, "mean"]*rep(1, 332))))


mu_city <- matrix(A_train %*% model_spatial$summary.random$spatial.field$mean, 332, 50, byrow = F) + 
  as.numeric(model_spatial$summary.fixed[1, "mean"])
colnames(mu_city) <- colnames(wide_data[[1]])

# minimizzazione dell'errore sul validation
opt <- optim(15, val_intercept_eval, XVAL = XVAL, YVAL = YVAL, mu_city = mu_city, 
             beta = betaest, basi = basi, D2 = D2, method = "BFGS")


# liscio le intercette con l'ammontare di penalizzazione ottimale
bpenal <- fdPar(basi, D2, lambda =exp(opt$par))
mu.fd <- smooth.basis(2:333, mu_city[, colnames(mu_city)%in%colnames(YTEST)], bpenal)
mu_p <- predict(mu.fd$fd, 2:333)

# previsioni sul test
fit_val2 <- matrix(NA, 332, 13)
for(i in 1:332)
  fit_val2[i, ] <- XTEST[, , i] %*% betaest[i, ] + mu_p[i, ] 

# MSe sul test set
MSE_spatial_spline <- mean((YTEST - fit_val2)**2)


data.frame("PM2.5" = c(as.vector(YTEST), as.vector(fit_val), as.vector(fit_val2)),
           "Type" = c(rep("Observed", 332*13), rep("RW1", 332*13), rep("Spline", 332*13)),
           "City" = rep(rep(unique(test$City), each = 332), 3),
           "Date" = rep(rep(unique(train$Date)[-length(unique(train$Date))], 13), 3)) |>
  ggplot(aes(x = Date, y = PM2.5, group = Type, color = Type)) + 
  geom_line() + 
  facet_wrap(~ City) + 
  scale_color_manual(values=c("black", "green", "red"))
  

# summary del modello
model_spatial$summary.fixed

# 

#La matrice di correlazione tra le osservazioni stimata mediante 
# l'effetto casuale spaziale è rappresentata dalla seguente matrice
k <- exp(model_spatial$summary.hyperpar["Theta2 for spatial", "mean"])
tau <- exp(model_spatial$summary.hyperpar["Theta1 for spatial", "mean"])
sigma_2 <- 1/(4*pi * k**2 * tau**2)
r <- sqrt(8)/k
corrmat <- matern.covariance(dist(unique(coord)), k, 1, sqrt(sigma_2))/sigma_2 + diag(50)
melted_cormat <- melt(corrmat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()



# grafico che mostra come correlano le città
# due città solo collegate da un segmento se la correlazione è in valore
# assoluto almeno pari a 0.5
ind <- which(melted_cormat$value > 0.5 | melted_cormat$value< -0.5)
corr_plus <- melted_cormat[ind, ]
corr_plus$Var1 <- colnames(wide_data[[1]])[corr_plus$Var1]
corr_plus$Var2 <- colnames(wide_data[[1]])[corr_plus$Var2]

edgelist <- data.frame(corr_plus$Var1 , corr_plus$Var2)
node <- sort(unique(c(corr_plus$Var1 , corr_plus$Var2)))
indcity <- colnames(wide_data[[1]]) %in% node
coordinate <- unique(coord)[indcity, ]

cities <- data.frame("city" = node, "lon" = coordinate[, 1], 
                     "lat" = coordinate[, 2])

# costruzione di un grafo
graph_city <- graph_from_data_frame(d = corr_plus[, -3], directed = FALSE)

# estraggo la mappa usa
usa_map <- map_data("state")

edges <- as_data_frame(graph_city, what = "edges")
vertices <- as_data_frame(graph_city, what = "vertices")

# Aggiungi le coordinate ai vertici
edges <- merge(edges, cities, by.x = "from", by.y = "city")
edges$random <- NULL
edges <- unique(edges)
edges <- merge(edges, cities, by.x = "to", by.y = "city", suffixes = c(".from", ".to"))

# grafico delle connessioni
ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_segment(data = edges, aes(x = lon.from, y = lat.from, xend = lon.to, yend = lat.to),
               color = "blue", linewidth = 1.1 ) +
  geom_point(data = cities, aes(x = lon, y = lat), color = "red", shape = 21, stroke = 1.2) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city)) +
  theme_minimal()


# gif per gli effetti spazio-temporali
coef <- model_spatial$summary.random$spatial.field[, "mean"]
coef.mat = matrix(coef, ncol=229, byrow=TRUE)

longitude.range = range(usa_map$long)
latitude.range = range(usa_map$lat)

coord <- as.matrix(expand.grid(seq(longitude.range[1], longitude.range[2], length.out=250),
                               seq(latitude.range[1], latitude.range[2], length.out=250)))


A_proj <- inla.spde.make.A(mesh2d, loc = coord)
pt = unique(data.frame(train$longitude, train$latitude))

br = c(-Inf,0:120,Inf)
j = 1
for(i in seq(1, 333)){
  pr1 = A_proj%*%coef.mat[i,]
  
  #pr1.mat = matrix(pr1, ncol=250, byrow=FALSE)
  df = data.frame(long = coord[,1], lat = coord[,2], val = as.vector(pr1))

  ggsave(filename = paste0("plots1/plt-", j, ".jpg", collapse = ""), ggplot() +
           geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
           geom_contour_filled(aes(long, lat, z = val), data=df, alpha = 0.5, breaks = br) + theme(legend.position="none") +
           geom_point(aes(train.longitude, train.latitude), data=pt, col="purple"))
  print(i)
  #Sys.sleep(1/5)  
  j = j+1
}

setwd("plots2")
imm <- image_read(path = dir())
gf <- image_animate(image_scale(imm, "1920x1080"), delay = 0.1, dispose = "previous")
gf




#simulazioni 
t=1:100
library(mvtnorm)
par(mfrow=c(2,3))
par(mai=c(0.3,0.3,0.3,0.3))
#EFFETTO LATENTE INCORRELATO TEMPORALMENTE CON LA COVARIATA OSSERVATA
#MA CON LO STESSO SCHEMA DI CORRELAZIONE TRA LE CITTA' RISPETTO ALLA COVARIATA

cor.mat = cor(wide_data$temperature_median)
image(cor.mat, main="matrice di correlazione")

temp = rmvnorm(n=100, mean=rep(0,50), cor.mat)
matplot(temp, type="l", main="effetto osservato")

latent = rmvnorm(n=100, mean=rep(0,50), cor.mat)
matplot(latent, type="l", main="effetto latente")

diag(cor(temp, latent)) #incorrelati, ma con la stessa struttura di correlazione tra città
# image(cor(temp))
# image(cor(latent))

resp = 5 * temp + 5 * latent + 1 * matrix(rnorm(100*50, 0, 3), nrow=100, ncol=50)

matplot(t, resp, type="l", main="risposta generata")

library(KFAS)

tmp = array(NA, dim=c(50,2,100))
tmp[,1,] = matrix(1, nrow=50, ncol=100)
tmp[,2,] = t(temp)
tmpY = resp

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- diag(50)*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value
pm1 <- KFS(pps$model)

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

cormat = cor(temp)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- cormat*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value


pm1.2 <- KFS(pps$model)

#par(mfrow=c(2,1))
plot(coef(pm1)[,1], type="l", ylim=c(-60, +60), main="Intercetta")
lines(coef(pm1.2)[,1], col="red")

plot(coef(pm1)[,2], type="l", ylim=c(-5, 15), main="Coefficiente effetto osservato")
lines(coef(pm1.2)[,2], col="red") #questo andrà meglio anche in previsione
#perchè l'effetto latente è incorrelato
#quindi tutti gli andamenti frastagliati non sono "replicati" in futuro
#nonostante questo, la verosimiglianza è migliore nel modello senza correlazione... ma conta poco, sono scorrettamente specificati...

###
par(mfrow=c(2,3))
#par(mfrow=c(1,1))

#EFFETTO LATENTE CORRELATO TEMPORALMENTE CON LA COVARIATA OSSERVATA
#E CON LO STESSO SCHEMA DI CORRELAZIONE TRA LE CITTA' RISPETTO ALLA COVARIATA

cor.mat = cor(wide_data$temperature_median)
image(cor.mat, main="matrice di correlazione")

temp = rmvnorm(n=100, mean=rep(0,50), cor.mat)
matplot(temp, type="l", main="effetto osservato")

latent = temp + rmvnorm(n=100, mean=rep(0,50), diag(50)/5)
temp = temp + rmvnorm(n=100, mean=rep(0,50), diag(50)/5)
matplot(latent, type="l", main="effetto latente")

mean(diag(cor(temp, latent)) )#correlati
#image(cor(temp))
#image(cor(latent)) #ma ancora stessa struttura di correlazione tra le città


resp = 5 * temp + 5 * latent + 1 * matrix(rnorm(100*50, 0, 3), nrow=100, ncol=50)

matplot(t, resp, type="l", main="risposta generata")


library(KFAS)

tmp = array(NA, dim=c(50,2,100))
tmp[,1,] = matrix(1, nrow=50, ncol=100)
tmp[,2,] = t(temp)
tmpY = resp

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- diag(50)*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value
pm1 <- KFS(pps$model)

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

cormat = cor(temp)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- cormat*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value


pm1.2 <- KFS(pps$model)

#par(mfrow=c(2,1))
plot(coef(pm1)[,1], type="l", ylim=c(-60, +60), main="Intercetta")
lines(coef(pm1.2)[,1], col="red")

plot(coef(pm1)[,2], type="l", ylim=c(-5, 15), main="Coefficiente effetto osservato")
lines(coef(pm1.2)[,2], col="red")

#anche qui, la verosimiglianza è migliore nel modello senza correlazione... ma conta poco, sono scorrettamente specificati...


###
#io cerco di stimare quali sono le città correlate metereologicamente
#attraverso la media della correlazione tra le covariate meteo osservate
#la correlazione potrebbe non essere solo dipendente dalla distanza
#penso a due città vicine con una montagna in mezzo (oppure una in montagna e una in pianura)
#o due città distanti ma con clima condiviso per via di macro-correnti

#se ci sono effetti latenti meteo (potenzialmente incorrelati o correlati con quelli osservati,
# ma con la stessa struttura di correlazione tra città)
#inserire una matrice di correlazione "meteo" sarà più efficace di inserire una matern

#più banalmente, anche senza scomodare gli effetti latenti
#le oscillazioni di pm2.5 al netto delle covariate (quindi termine di errore) non sono incorrelate tra le città
#proponiamo che stimare la correlazione dell'errore tramite correlazioni "meteo" sia potenzialmente più efficace di usare la matern
#proprio per gli esempi scritti sopra (catene montuose, correnti meteo che spaziano centinaia di km, città distanti con macro-clima simili...)
#correnti meteo che governano clima e spostamenti d'aria (e plausibilmente il PM) non necessariamente considerano solo distanze euclidee

#è da capire se vogliamo che città come ad es. miami e los angeles siano considerate correlate (anche se molto distanti)
#perchè hanno macro-trend di temperatura simili (ma non oscillazioni simili al netto del trend)
#se ciò ci disturba, si potrebbero stimare le correlazioni tra città al netto dei trend delle variabili
#oppure, volgiamo considerarle correlate
#perchè pensiamo ad effetti latenti che abbiano trend simili tra le due città (es. piogge...)
#dato che il macro-clima delle città è simile, il periodo piovoso tra le due città sarà simile
#anche se le piogge al netto del trend di piovosità generale non saranno correlate
#e quindi il pm2.5, al netto delle covariate, sarà correlato tra due città distanti, perchè hanno macro-clima simile...


#COVARIATA ED EFFETTO LATENTE CON LA STESSA CORRELAZIONE TRA CITTA'
#PER MOTIVO DI TREND COMUNI, MA CORRELAZIONE TRA CITTA' NULLA AL NETTO DEL TREND
#E CORRELAZIONE NULLA TEMPORALMENTE TRA I TREND
par(mfrow=c(1,1))
tempTrend = (2500-(t-50)^2)/2500 * 10
latentTrend = sin((t/50)*pi) * 5
plot(tempTrend)
plot(latentTrend)

temp = matrix(NA, ncol=50, nrow=100)
temp[,1] = tempTrend
for(i in 2:50){
  temp[,i] = c(tempTrend[(1+(i-1)*2):100], tempTrend[1:((i-1)*2)])
}
matplot(temp, type="l")

latent = matrix(NA, ncol=50, nrow=100)
latent[,1] = latentTrend
for(i in 2:50){
  latent[,i] = c(latentTrend[(1+(i-1)*2):100], latentTrend[1:((i-1)*2)])
}
matplot(latent, type="l")


temp[,1] = temp[,1] + rnorm(100, 0, 1)
for(i in 2:50){
  temp[,i] = temp[,i] + rnorm(100, 0, 1)
}
matplot(temp, type="l")

latent[,1] = latent[,1] + rnorm(100, 0, 1)
for(i in 2:50){
  latent[,i] = latent[,i] + rnorm(100, 0, 1)
}
matplot(latent, type="l")


diag(cor(temp, latent)) #incorrelati temporalmente
image(cor(temp))
image(cor(latent))

resp = 5 * temp + 5 * latent + 1 * matrix(rnorm(100*50, 0, 1), nrow=100, ncol=50)

matplot(t, resp, type="l")


library(KFAS)

tmp = array(NA, dim=c(50,2,100))
tmp[,1,] = matrix(1, nrow=50, ncol=100)
tmp[,2,] = t(temp)
tmpY = resp

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- diag(50)*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value
pm1 <- KFS(pps$model)

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

cormat = cor(temp)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- cormat*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value


pm1.2 <- KFS(pps$model)

par(mfrow=c(2,1))
plot(coef(pm1)[,1], type="l", ylim=c(-60, +60))
lines(coef(pm1.2)[,1], col="red")

plot(coef(pm1)[,2], type="l", ylim=c(-5, 15)) 
lines(coef(pm1.2)[,2], col="red")

#la covariata in questo caso non coglie l'effetto latente
matplot( (fitted(pm1)-resp)[,1] , type="l")
#però i residui sono correlati, quindi la verosimiglianza è migliore nel modello che considera la correlazione tra città


par(mfrow=c(2,1))
#COVARIATA ED EFFETTO LATENTE CON LA STESSA CORRELAZIONE TRA CITTA'
#PER MOTIVO DI TREND COMUNI, MA CORRELAZIONE TRA CITTA' NULLA AL NETTO DEL TREND
#E CORRELAZIONE PRESENTE TEMPORALMENTE TRA I TREND
tempTrend = (2500-(t-50)^2)/2500 * 10
latentTrend = sin((t/50-0.5)*pi) * 5
plot(tempTrend)
plot(latentTrend)

temp = matrix(NA, ncol=50, nrow=100)
temp[,1] = tempTrend
for(i in 2:50){
  temp[,i] = c(tempTrend[(1+(i-1)*2):100], tempTrend[1:((i-1)*2)])
}
matplot(temp, type="l")

latent = matrix(NA, ncol=50, nrow=100)
latent[,1] = latentTrend
for(i in 2:50){
  latent[,i] = c(latentTrend[(1+(i-1)*2):100], latentTrend[1:((i-1)*2)])
}
matplot(latent, type="l")


temp[,1] = temp[,1] + rnorm(100, 0, 1)
for(i in 2:50){
  temp[,i] = temp[,i] + rnorm(100, 0, 1)
}
matplot(temp, type="l")

latent[,1] = latent[,1] + rnorm(100, 0, 1)
for(i in 2:50){
  latent[,i] = latent[,i] + rnorm(100, 0, 1)
}
matplot(latent, type="l")


diag(cor(temp, latent)) #correlati temporalmente
image(cor(temp))
image(cor(latent))

resp = 5 * temp + 5 * latent + 1 * matrix(rnorm(100*50, 0, 1), nrow=100, ncol=50)

matplot(t, resp, type="l")


library(KFAS)

tmp = array(NA, dim=c(50,2,100))
tmp[,1,] = matrix(1, nrow=50, ncol=100)
tmp[,2,] = t(temp)
tmpY = resp

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- diag(50)*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value
pm1 <- KFS(pps$model)

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

cormat = cor(temp)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- cormat*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value


pm1.2 <- KFS(pps$model)

par(mfrow=c(2,1))
plot(coef(pm1)[,1], type="l", ylim=c(-60, +60))
lines(coef(pm1.2)[,1], col="red")

plot(coef(pm1)[,2], type="l", ylim=c(-5, 15)) 
lines(coef(pm1.2)[,2], col="red")


matplot( (fitted(pm1)-resp)[,1] , type="l")
matplot( (fitted(pm1)-resp)[,25] , type="l") #qui il modello ha provato a togliere la correlazione tra i residui...

matplot( (fitted(pm1.2)-resp)[,1] , type="l")
matplot( (fitted(pm1.2)-resp)[,25] , type="l") #qui i residui hanno la correlazione che il modello si aspetta...

#verosimiglianza migliore nel modello con la correlazione inserita




#SIMULAZIONE PIU' STRANA, UN MIX DI UN PO' DI COSE...
#trend temporali incorrelati temporalmente tra variabili
#oscillazioni additive sul trend correlate temporalmente tra variabili
#ciò porta alcune città ad essere molto correlate temporalmente (quelle che hanno poco trend), altre no
#correlazione tra città simile (ma non esattamente uguale) tra effetto latente e effetto osservato

t = seq(0,100, length.out = 100) #indice di tempo
#50 città

cities.tempRank = sample(1:50) #ranking di temperatura delle città -> più alto, temperature più alte
cities.tempPos = rank(cities.tempRank+rnorm(50, 0, 2)) #"posizione" della città -> più "vicine", oscillazioni sul trend più simili
#cities.tempRank e cities.tempPos sono simili

tempTrend = (2500-(t-50)^2)/2500*10 #trend "tipo" della temperatura
plot(t, tempTrend, type="l")


tempMat = t(matrix(cities.tempRank/50, ncol=1) %*% matrix(tempTrend, nrow=1)) + 
  sin(t %*% matrix(1, nrow=1, ncol=50) + pi/2 * matrix(cities.tempPos, nrow=100, ncol=50, byrow=TRUE)/50) +
  matrix(rnorm(100*50, 0, 0.3), nrow=100, ncol=50)
matplot(tempMat, type="l") #temperature osservate
#le temperature generali sono correlate tra città "vicine" (in base a cities.tempRank)
#le oscillazioni più piccole sono correlate tra città "vicine" (in base a cities.tempPos)
plot(tempMat[,cities.tempRank==1], tempMat[,cities.tempRank==2])
plot(tempMat[,cities.tempRank==1], tempMat[,cities.tempRank==50])

#faccio la stessa cosa per l'effetto latente (con cities.tempRank e cities.tempPos simili alle controparti -> vengono correlate simili)
latentTrend = sin(t/50*pi)
plot(t, latentTrend, type="l")

#stessa cosa per "ranking" e "posizione" delle città per quanto riguarda la var. latente (simili a quelle di temp -> la correlazione di latent sarà simile a quella di temp)
cities.latentRank = rank(cities.tempRank+rnorm(50, 0, 2)) 
cities.latentPos = rank(cities.tempPos+rnorm(50, 0, 2))
#plot(cities.tempRank, cities.latentPos)

latentMat = (matrix(latentTrend, ncol=1) %*% matrix(cities.latentRank, nrow=1))/5 + 
  sin(t %*% matrix(1, nrow=1, ncol=50) + pi/2 * matrix(cities.latentPos, nrow=100, ncol=50, byrow=TRUE)/50) +
  matrix(rnorm(100*50, 0, 0.3), nrow=100, ncol=50)
matplot(latentMat, type="l")

image(cor(latentMat), breaks=c(-1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), col = gray.colors(n=11))
image(cor(tempMat), breaks=c(-1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), col = gray.colors(n=11))


plot(diag(cor(latentMat, tempMat))) #alcune città molto correlate...
plot(diag(cor(latentMat, tempMat)), cities.tempRank) #quelle che hanno poco trend sono molto correlate
#tutte sono molto correlate al netto del trend 


resp = 5*(tempMat) + 5*latentMat + matrix(rnorm(100*50, 0, 3), nrow=100, ncol=50)

##
matplot(5*(tempMat) + 5*latentMat, type="l")
matplot(t, (resp), type="l")


library(KFAS)

tmp = array(NA, dim=c(50,2,100))
tmp[,1,] = matrix(1, nrow=50, ncol=100)
tmp[,2,] = t(tempMat)
tmpY = resp


m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- diag(50)*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value
pm1 <- KFS(pps$model)

m0 <- SSModel(tmpY ~ -1 + SSMcustom(Z = tmp, T = diag(2), Q = diag(2), a1 = c(0,5)))
pm0 <- KFS(m0)

cormat = cor(tempMat)
#cormat = cor(latentMat)

fup <- function(p, modello)
{
  modello["Q"] <- exp(c(p[1], p[2]))
  
  modello["H"] <- cormat*exp(p[3])
  return(modello)
}

pps = fitSSM(pm0$model, c(0.5, 0.5, 0.5), updatefn = fup)
pps$optim.out$value


pm1.2 <- KFS(pps$model)

par(mfrow=c(2,1))
plot(coef(pm1)[,1], type="l", ylim=c(-80, +80))
lines(coef(pm1.2)[,1], col="red")

plot(coef(pm1)[,2], type="l", ylim=c(-10, 15)) 
lines(coef(pm1.2)[,2], col="red")

