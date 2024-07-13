
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
