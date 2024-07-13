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

