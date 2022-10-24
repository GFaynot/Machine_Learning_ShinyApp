source("A:/Analyse Financiere Reservoir/RStudio/Packages.R", encoding = "UTF-8")

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------ Data -----------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

load("A:/Analyse Financiere Reservoir/Data/Facturation Groupe.RData")


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------ Préparation ----------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# data.machinelearning <- data.machinelearning[data.machinelearning$date_invoice_month<"2022-08-01",]

data.machinelearning.ts <- ts(data.machinelearning$net_sales_cum, frequency = 12, start = year(min(as.Date(data$date_invoice))))

model.train <- window(data.machinelearning.ts, start = 2013, end = c(2022,4))
model.test <- window(data.machinelearning.ts, start = c(2022,5))
NbMoisPred <- 3


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------ Validation ----------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

arima.mape <- c()
LSTM.mape <- c()
NNETAR.mape <- c()
MLP.mape <- c()
HW.mape <- c()

for(i in 1:10){
  # ARIMA model
  model.fit.ARIMA <- auto.arima(model.train, trace=FALSE, ic="aic")
  model.fcst.ARIMA <- forecast(model.fit.ARIMA, h = NbMoisPred)
  arima.mape <- c(arima.mape,MAPE(model.fcst.ARIMA$mean, model.test)*100)
  print(i)
}

for(i in 1:10){
  # LSTM
  source_python("A:/Analyse Financiere Reservoir/Analyse/Approche Predictive/MachineLearning.py")
  model.fcst.LSTM <- LSTM(data.machinelearning,NbMoisPred,100)
  LSTM.mape <- c(LSTM.mape,MAPE(model.fcst.LSTM$pred_value, model.test)*100)
  print(i)
}
for(i in 1:10){
  # NNETAR model
  model.fit.nnetar <- nnetar(model.train,P=1, p=20,size=20, repeats = 20)
  model.fcst.nnetar <- forecast(model.fit.nnetar, h = NbMoisPred)
  NNETAR.mape <- c(NNETAR.mape,MAPE(model.fcst.nnetar$mean, model.test)*100)
  print(i)
}
for(i in 1:10){
  # MLP model
  model.fit.mlp <- mlp(model.train, hd=20,reps=20)
  model.fcst.mlp <- forecast(model.fit.mlp, h = NbMoisPred)
  MLP.mape <- c(MLP.mape,MAPE(model.fcst.mlp$mean, model.test)*100)
  print(i)
}
for(i in 1:10){
  # Holt-Winters model
  model.fit.HW <- HoltWinters(model.train, alpha=0.3, beta=0.2, gamma=0.7)
  model.fcst.HW <- forecast(model.fit.HW, h = NbMoisPred)
  HW.mape <- c(HW.mape,MAPE(model.fcst.HW$mean, model.test)*100)
  print(i)
}

mean(arima.mape)
mean(LSTM.mape)
mean(NNETAR.mape)
mean(MLP.mape)
mean(HW.mape)
