
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
