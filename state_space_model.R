
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
