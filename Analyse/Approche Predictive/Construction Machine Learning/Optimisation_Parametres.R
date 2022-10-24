source("Z:/RStudio/Packages.R", encoding = "UTF-8")

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# -------------------------------- Data PREPARATION-----------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

load("Z:/Analyse Financiere Reservoir/Data/Facturation Groupe.RData")

data.machinelearning <- data.machinelearning[data.machinelearning$date_invoice_month<"2022-08-01",]

data.machinelearning.ts <- ts(data.machinelearning$net_sales, frequency = 12, start = year(min(as.Date(data$date_invoice))))

model.train <- window(data.machinelearning.ts, start = 2013, end = c(2021,12))
model.test <- window(data.machinelearning.ts, start = c(2022,1))

NbMoisPred <- 6

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------------------- ARIMA ----------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
jeu_de_donnees.fit.ARIMA <- auto.arima(model.train, trace=TRUE,ic = "aic")

for (NbMoisPred in c(1,3,6)){
  jeu_de_donnees.fcst.ARIMA <- forecast(jeu_de_donnees.fit.ARIMA, h = NbMoisPred)
  print(MAE(jeu_de_donnees.fcst.ARIMA$mean, model.test[1:NbMoisPred]))
  print(MAPE(jeu_de_donnees.fcst.ARIMA$mean, model.test[1:NbMoisPred])*100)
}


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------------------- LSTM -----------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
source_python("Z:/Analyse Financiere Reservoir/Analyse/Approche Predictive/MachineLearning.py")

grid <- expand.grid(n_epoch=seq(from=1,to=10,by=1))
grid <- expand.grid(n_epoch=seq(from=10,to=100,by=10))
grid <- expand.grid(n_epoch=seq(from=100,to=1000,by=100))

for(i in 1:nrow(grid)){
  res <- LSTM(data.machinelearning,NbMoisPred,grid$n_epoch[i],"2021-12-01")
  print(MAE(res$net_sales, model.test[1:NbMoisPred]))
  print(MAPE(res$net_sales, model.test[1:NbMoisPred])*100)
  print(i)
}

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------------------- NNETAR ---------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

grid <- expand.grid(p=seq(from=15,to=25,by=5),P=c(1,2,3),size=seq(from=15,to=25,by=5),repeats=seq(from=10,to=20,by=5))
resultats <- data.frame(grid)

for(NbMoisPred in c(1,3,6)){
  results_grid <- data.frame(matrix(0, ncol = 2, nrow = nrow(grid)))
  names(results_grid) <- c("MAE","MAPE")
  for(i in 1:nrow(grid)){
    jeu_de_donnees.fit.nnetar <- nnetar(model.train,p=grid$p[i],P=grid$P[i],size=grid$size[i],repeats = grid$repeats[i])
    jeu_de_donnees.fcst.nnetar <- forecast(jeu_de_donnees.fit.nnetar, h = NbMoisPred)
    results_grid$MAE[i] <- MAE(jeu_de_donnees.fcst.nnetar$mean, model.test)
    results_grid$MAPE[i] <- MAPE(jeu_de_donnees.fcst.nnetar$mean, model.test)*100
    print(i)
  }
  resultats <- data.frame(cbind(resultats,results_grid))
}

names(resultats) <- c("p","P","size","repeats","MAE_1pred","MAPE_1pred","MAE_3pred","MAPE_3pred","MAE_6pred","MAPE_6pred")

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------------------- MLP ------------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
grid <- expand.grid(hd=seq(from=5,to=30,by=5),reps=seq(from=5,to=30,by=5))
resultats <- data.frame(grid)

for(NbMoisPred in c(1,3,6)){
  results_grid <- data.frame(matrix(0, ncol = 2, nrow = nrow(grid)))
  names(results_grid) <- c("MAE","MAPE")
  for(i in 1:nrow(grid)){
    jeu_de_donnees.fit.mlp <- mlp(model.train, hd=grid$hd[i],reps=grid$reps[i])
    jeu_de_donnees.fcst.mlp <- forecast(jeu_de_donnees.fit.mlp, h = NbMoisPred)
    results_grid$MAE[i] <- MAE(jeu_de_donnees.fcst.mlp$mean, model.test)
    results_grid$MAPE[i] <- MAPE(jeu_de_donnees.fcst.mlp$mean, model.test)*100
    print(i)
  }
  resultats <- data.frame(cbind(resultats,results_grid))
}

names(resultats) <- c("hd","reps","MAE_1pred","MAPE_1pred","MAE_3pred","MAPE_3pred","MAE_6pred","MAPE_6pred")

# write.csv(resultats,"C:/Users/gft/Desktop/ML/MLP.csv", row.names = FALSE)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------------------- HW -------------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

grid <- expand.grid(alpha=seq(from=0.1,to=1,by=0.1),beta=seq(from=0.1,to=1,by=0.1),gamma=seq(from=0.1,to=1,by=0.1))
resultats <- data.frame(grid)

for(NbMoisPred in c(1,3,6)){
  results_grid <- data.frame(matrix(0, ncol = 2, nrow = nrow(grid)))
  names(results_grid) <- c("MAE","MAPE")
  for(i in 1:nrow(grid)){
    jeu_de_donnees.fit.HW <- HoltWinters(model.train, alpha=grid$alpha[i], beta=grid$beta[i], gamma=grid$gamma[i])
    jeu_de_donnees.fcst.HW <- forecast(jeu_de_donnees.fit.HW, h = NbMoisPred)
    results_grid$MAE[i] <- MAE(jeu_de_donnees.fcst.HW$mean, model.test)
    results_grid$MAPE[i] <- MAPE(jeu_de_donnees.fcst.HW$mean, model.test)*100
    print(i)
  }
  resultats <- data.frame(cbind(resultats,results_grid))
}

names(resultats) <- c("alpha","beta","gamma","MAE_1pred","MAPE_1pred","MAE_3pred","MAPE_3pred","MAE_6pred","MAPE_6pred")
# write.csv(resultats,"C:/Users/gft/Desktop/ML/HW.csv", row.names = FALSE)
