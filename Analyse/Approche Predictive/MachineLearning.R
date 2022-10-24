#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# -------------------------------- OBJECTIF ------------------------------------
# Ce programme permet de générer des fonctions allant prédire des données d'entrées
# grâce à 4 algorithmes.
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ------------------------------ FONCTION NUMERO 1 -----------------------------
#                             JEU DE DONNEES COMPLET
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  
machinelearning_complet <- function(data,NbMoisPred){
  
  load("/media/sf_Z_DRIVE/Analyse Financiere Reservoir/Analyse/Approche Predictive/PreparationModeles.RData")
  
  # Supprimer les données poour les mois incomplets (mois en cours)
  data <- data[data$date_invoice_month<floor_date(Sys.Date(), "month"),]
  
  # Paramètres des dates pour les modèles
  DateDebutApprentissage <- as.Date(floor_date(min(data$date_invoice_month),"month"))
  DateFinApprentissage <- as.Date(floor_date(max(data$date_invoice_month),"month"))
  
  if(DateFinApprentissage==as.Date(floor_date(Sys.Date(),"month"))){
    DateFinApprentissage <- as.Date(floor_date(max(data$date_invoice_month)-months(1),"month"))
  }
  
  NbMoisPred <- as.integer(NbMoisPred)
  
  if(NbMoisPred<3){
    # model.fit.LSTM <- model.fit.LSTM.CT
    model.fit.nnetar <- model.fit.nnetar.CT
    model.fit.mlp <- model.fit.mlp.CT
    model.fit.HW <- model.fit.HW.CT
  }else{
    if(NbMoisPred<6){
      # model.fit.LSTM <- model.fit.LSTM.MT
      model.fit.nnetar <- model.fit.nnetar.MT
      model.fit.mlp <- model.fit.mlp.MT
      model.fit.HW <- model.fit.HW.MT
    }else{
      # model.fit.LSTM <- model.fit.LSTM.LT
      model.fit.nnetar <- model.fit.nnetar.LT
      model.fit.mlp <- model.fit.mlp.LT
      model.fit.HW <- model.fit.HW.LT
    }
  }

  # Début des modèles de prédiction
  # ARIMA model
  model.fcst.ARIMA <- forecast(model.fit.ARIMA.CT.MT.LT, h = NbMoisPred)
  
  # LSTM
  # source_python("/media/sf_Z_DRIVE/Analyse Financiere Reservoir/Analyse/Approche Predictive/MachineLearning.py")
  # model.fcst.LSTM <- LSTM_PRED(data = data,Regressor = unserialize_model(model.fit.LSTM),NbPred = NbMoisPred)
  
  
  # NNETAR model
  model.fcst.nnetar <- forecast(model.fit.nnetar, h = NbMoisPred)
  
  # MLP model (package nnfor)
  model.fcst.mlp <- forecast(model.fit.mlp, h = NbMoisPred)
  
  # Holt-Winters model
  model.fcst.HW <- forecast(model.fit.HW, h = NbMoisPred)
  
  # Transformation exponentielle
  model.fcst.ARIMA$mean <- exp(model.fcst.ARIMA$mean)
  # model.fcst.LSTM$net_sales <- exp(model.fcst.LSTM$net_sales)
  model.fcst.nnetar$mean <- exp(model.fcst.nnetar$mean)
  model.fcst.mlp$mean <- exp(model.fcst.mlp$mean)
  model.fcst.HW$mean <- exp(model.fcst.HW$mean)
  
  # Graphique
  data.models.pred <- list(data.frame(date=as.Date(as.yearmon(time(model.fcst.ARIMA$mean))),ARIMA=as.matrix(model.fcst.ARIMA$mean)),
                           # data.frame(date=as.Date(model.fcst.LSTM$date_invoice_month),LSTM=model.fcst.LSTM$net_sales),
                           data.frame(date=as.Date(as.yearmon(time(model.fcst.nnetar$mean))),NNETAR=as.matrix(model.fcst.nnetar$mean)),
                           data.frame(date=as.Date(as.yearmon(time(model.fcst.mlp$mean))),MLP=as.matrix(model.fcst.mlp$mean)),
                           data.frame(date=as.Date(as.yearmon(time(model.fcst.HW$mean))),HW=as.matrix(model.fcst.HW$mean))) %>% 
    reduce(full_join, by='date') %>% 
    mutate(date = as.Date(date))
  
  
  resultats <- data.frame(date=c(data$date_invoice_month,data.models.pred$date),net_sales=NA,ARIMA =NA,NNETAR =NA,MLP=NA,HW=NA ) %>% 
    unique()
  
  resultats <- resultats %>% 
    mutate(net_sales = data$net_sales[match(resultats$date, data$date_invoice_month)],
           ARIMA = data.models.pred$ARIMA[match(resultats$date, data.models.pred$date)],
           # LSTM = data.models.pred$LSTM[match(resultats$date, data.models.pred$date)],
           NNETAR = data.models.pred$NNETAR[match(resultats$date, data.models.pred$date)],
           MLP = data.models.pred$MLP[match(resultats$date, data.models.pred$date)],
           HW = data.models.pred$HW[match(resultats$date, data.models.pred$date)])
  

  # Prédictions cumulées
  resultats.cumul <- resultats
  resultats.cumul[resultats.cumul$date<= DateFinApprentissage,c("ARIMA","NNETAR","MLP","HW")] <- resultats.cumul$net_sales[resultats.cumul$date<= DateFinApprentissage]
  resultats.cumul <- resultats.cumul %>% 
    filter(date >= floor_date(floor_date(DateFinApprentissage-months(1),"month"), "year")) %>% 
    mutate(net_sales=as.numeric(unlist(tapply(net_sales, year(date), cumsum))),
           ARIMA = as.numeric(unlist(tapply(ARIMA, year(date), cumsum))),
           NNETAR = as.numeric(unlist(tapply(NNETAR, year(date), cumsum))),
           MLP = as.numeric(unlist(tapply(MLP, year(date), cumsum))),
           HW = as.numeric(unlist(tapply(HW, year(date), cumsum))))
  # resultats.cumul <- resultats.cumul %>% 
  #   filter(date >= floor_date(floor_date(DateFinApprentissage-months(1),"month"), "year")) %>% 
  #   mutate(net_sales=cumsum(net_sales),
  #          ARIMA = cumsum(ARIMA),
  #          NNETAR = cumsum(NNETAR),
  #          MLP = cumsum(MLP),
  #          HW = cumsum(HW))
  resultats.cumul[resultats.cumul$date<= DateFinApprentissage-months(1),c("ARIMA","NNETAR","MLP","HW")] <- NA
  
  indicateurs <- NULL 
  if(nrow(na.omit(resultats))>0){
    indicateurs <- round(matrix(c(MAE(model.fcst.ARIMA$mean, resultats$net_sales[!is.na(resultats$ARIMA)]),
                                  # MAE(model.fcst.LSTM$net_sales, resultats$net_sales[!is.na(resultats$LSTM)]),
                                  MAE(model.fcst.nnetar$mean, resultats$net_sales[!is.na(resultats$NNETAR)]),
                                  MAE(model.fcst.mlp$mean, resultats$net_sales[!is.na(resultats$MLP)]),
                                  MAE(model.fcst.HW$mean, resultats$net_sales[!is.na(resultats$HW)]),
                                  MAPE(model.fcst.ARIMA$mean, resultats$net_sales[!is.na(resultats$ARIMA)])*100,
                                  # MAPE(model.fcst.LSTM$net_sales, resultats$net_sales[!is.na(resultats$LSTM)])*100,
                                  MAPE(model.fcst.nnetar$mean, resultats$net_sales[!is.na(resultats$NNETAR)])*100,
                                  MAPE(model.fcst.mlp$mean, resultats$net_sales[!is.na(resultats$MLP)])*100,
                                  MAPE(model.fcst.HW$mean, resultats$net_sales[!is.na(resultats$HW)])*100),
                                nrow = 2,
                                ncol=4,
                                byrow=TRUE,
                                dimnames = list(c("MAE","MAPE"), c("ARIMA", "NNETAR","MLP","HW"))),2)
  }
  
  
  prediction.plot <- resultats %>% 
    filter(date >= floor_date(floor_date(Sys.Date()-months(1),"month"), "year")) %>% 
    plot_ly(x = ~date, y = ~net_sales, name = 'Valeurs observées', type = 'bar') %>%
    add_trace(y = ~ARIMA, name = 'Prédiction ARIMA', type = 'scatter', mode = 'lines+markers') %>%
    # add_trace(y = ~LSTM, name = 'Prédiction LSTM', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~NNETAR, name = 'Prédiction NNETAR', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~MLP, name = 'Prédiction MLP', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~HW, name = 'Prédiction HW', type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis = list(title = ''),yaxis = list (title = "Chiffre d'affaires (en euros)"),title = paste("Prédiction du chiffre d'affaires sur",NbMoisPred, "mois (en euros)"))
  
  
  
  prediction.cumul.plot <- resultats.cumul %>% 
    plot_ly(x = ~date, y = ~net_sales, name = 'Valeurs observées', type = 'bar') %>%
    add_trace(y = ~ARIMA, name = 'Prédiction ARIMA', type = 'scatter', mode = 'lines+markers') %>%
    # add_trace(y = ~LSTM, name = 'Prédiction LSTM', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~NNETAR, name = 'Prédiction NNETAR', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~MLP, name = 'Prédiction MLP', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~HW, name = 'Prédiction HW', type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis = list(title = ''),yaxis = list (title = "Chiffre d'affaires (en euros)"),title = paste("Prédiction du chiffre d'affaires sur",NbMoisPred, "mois (en euros)"))
  
  data.models.pred$date <- as.character(data.models.pred$date)
  
  resultats.cumul <- resultats.cumul[resultats.cumul$date> DateFinApprentissage,c("date","ARIMA","NNETAR","MLP","HW")]
  resultats.cumul$date <- as.character(resultats.cumul$date)
  
  # Graphique de la série temporelle
  plot.tsl <- stl(x = data.ts, s.window = "periodic") %>% 
    autoplot(ts.colour = 'blue',xlab = "", ylab = "Chiffre d'affaires des ventes (en €)", main = "Statistiques série temporelle")
  
  return(list(data.models.pred,prediction.plot,prediction.cumul.plot,resultats.cumul,plot.tsl,indicateurs))
  
}


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ------------------------------ FONCTION NUMERO 2 -----------------------------
#                         JEU DE DONNEES PERSONNALISE
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

machinelearning_personnalise <- function(data,NbMoisPred){
  
  # Vérification des paramètres
  NbMoisPred <- as.integer(NbMoisPred)
  
  # Supprimer les données poour les mois incomplets (mois en cours)
  if (max(data$date_invoice_month)>=floor_date(Sys.Date(), "month")){
    data <- data[data$date_invoice_month<floor_date(Sys.Date(), "month"),]
  }
  
  # Aggrégation des données
  data <- aggregate(list(net_sales = data$net_sales),by=list(date_invoice_month = as.Date(data$date_invoice_month)),sum)

  # Paramètres des dates pour les modèles
  DateDebutApprentissage <- as.Date(floor_date(min(data$date_invoice_month),"month"))
  DateFinApprentissage <- as.Date(floor_date(max(data$date_invoice_month),"month"))
  
  if(DateFinApprentissage==as.Date(floor_date(Sys.Date(),"month"))){
    DateFinApprentissage <- as.Date(floor_date(max(data$date_invoice_month)-months(1),"month"))
  }
  
  
  # Dataframe en série temporelle
  data.ts <- ts(data$net_sales, frequency = 12, start = year(min(as.Date(DateDebutApprentissage))))
  
  log_transf <- FALSE
  if(nrow(data[data$net_sales<=0,])==0){
    data.ts <- log(data.ts)
    log_transf <- TRUE
  }
  
  # Définir les données d'apprentissage et de test
  
  DateDebutTest <- floor_date(DateFinApprentissage+months(1), "month")
  
  model.train <- window(data.ts, start = year(DateDebutApprentissage), end = c(year(DateFinApprentissage),month(DateFinApprentissage)))
  
  
  
  if(NbMoisPred<3){
    # ARIMA model
    model.fit.ARIMA <- auto.arima(model.train, trace=FALSE, ic="aic")
    # NNETAR model
    model.fit.nnetar <- nnetar(model.train,P=1, p=20,size=20, repeats = 10)
    # MLP model (package nnfor)
    model.fit.mlp <- mlp(model.train, hd=30,reps=5)
    # Holt-Winters model
    model.fit.HW <- HoltWinters(model.train, alpha=0.1, beta=0.2, gamma=1)
  }else{
    if(NbMoisPred<6){
      # ARIMA model
      model.fit.ARIMA <- auto.arima(model.train, trace=FALSE, ic="aic")
      # NNETAR model
      model.fit.nnetar <- nnetar(model.train,P=2, p=25,size=20, repeats = 15)
      # MLP model (package nnfor)
      model.fit.mlp <- mlp(model.train, hd=30,reps=25)
      # Holt-Winters model
      model.fit.HW <- HoltWinters(model.train, alpha=0.5, beta=0.4, gamma=0.9)
    }else{
      # ARIMA model
      model.fit.ARIMA <- auto.arima(model.train, trace=FALSE, ic="aic")
      # NNETAR model
      model.fit.nnetar <- nnetar(model.train,P=2, p=25,size=25, repeats = 15)
      # MLP model (package nnfor)
      model.fit.mlp <- mlp(model.train, hd=25,reps=25)
      # Holt-Winters model
      model.fit.HW <- HoltWinters(model.train, alpha=0.5, beta=0.3, gamma=0.7)
    }
  }
  
  # Début des modèles de prédiction

  # ARIMA model
  model.fcst.ARIMA <- forecast(model.fit.ARIMA, h = NbMoisPred)
  # NNETAR model
  model.fcst.nnetar <- forecast(model.fit.nnetar, h = NbMoisPred)
  # MLP model (package nnfor)
  model.fcst.mlp <- forecast(model.fit.mlp, h = NbMoisPred)
  # Holt-Winters model
  model.fcst.HW <- forecast(model.fit.HW, h = NbMoisPred)
  
  if(log_transf){
    model.fcst.ARIMA$mean <- exp(model.fcst.ARIMA$mean)
    model.fcst.nnetar$mean <- exp(model.fcst.nnetar$mean)
    model.fcst.mlp$mean <- exp(model.fcst.mlp$mean)
    model.fcst.HW$mean <- exp(model.fcst.HW$mean)
  }
  
  
  # Graphique
  data.models.pred <- list(data.frame(date=as.Date(as.yearmon(time(model.fcst.ARIMA$mean))),ARIMA=as.matrix(model.fcst.ARIMA$mean)),
                           data.frame(date=as.Date(as.yearmon(time(model.fcst.nnetar$mean))),NNETAR=as.matrix(model.fcst.nnetar$mean)),
                           data.frame(date=as.Date(as.yearmon(time(model.fcst.mlp$mean))),MLP=as.matrix(model.fcst.mlp$mean)),
                           data.frame(date=as.Date(as.yearmon(time(model.fcst.HW$mean))),HW=as.matrix(model.fcst.HW$mean))) %>% 
    reduce(full_join, by='date') %>% 
    mutate(date = as.Date(date))
  
  
  resultats <- data.frame(date=c(data$date_invoice_month,data.models.pred$date),net_sales=NA,ARIMA =NA,NNETAR =NA,MLP=NA,HW=NA ) %>% 
    unique()
  
  resultats <- resultats %>% 
    mutate(net_sales = data$net_sales[match(resultats$date, data$date_invoice_month)],
           ARIMA = data.models.pred$ARIMA[match(resultats$date, data.models.pred$date)],
           NNETAR = data.models.pred$NNETAR[match(resultats$date, data.models.pred$date)],
           MLP = data.models.pred$MLP[match(resultats$date, data.models.pred$date)],
           HW = data.models.pred$HW[match(resultats$date, data.models.pred$date)])
  
  # resultats$Moyenne <- rowMeans(resultats[c("ARIMA","LSTM","NNETAR","MLP","HW")])
  
  # Prédictions cumulées
  resultats.cumul <- resultats
  resultats.cumul[resultats.cumul$date<= DateFinApprentissage,c("ARIMA","NNETAR","MLP","HW")] <- resultats.cumul$net_sales[resultats.cumul$date<= DateFinApprentissage]
  # resultats.cumul <- resultats.cumul %>% 
  #   filter(date >= floor_date(floor_date(DateFinApprentissage-months(1),"month"), "year")) %>% 
  #   mutate(net_sales=cumsum(net_sales),
  #          ARIMA = cumsum(ARIMA),
  #          NNETAR = cumsum(NNETAR),
  #          MLP = cumsum(MLP),
  #          HW = cumsum(HW))
  
  
  
  resultats.cumul <- resultats.cumul %>% 
    filter(date >= floor_date(floor_date(DateFinApprentissage-months(1),"month"), "year")) %>% 
    mutate(net_sales=as.numeric(unlist(tapply(net_sales, year(date), cumsum))),
           ARIMA = as.numeric(unlist(tapply(ARIMA, year(date), cumsum))),
           NNETAR = as.numeric(unlist(tapply(NNETAR, year(date), cumsum))),
           MLP = as.numeric(unlist(tapply(MLP, year(date), cumsum))),
           HW = as.numeric(unlist(tapply(HW, year(date), cumsum))))
  
  resultats.cumul[resultats.cumul$date<= DateFinApprentissage-months(1),c("ARIMA","NNETAR","MLP","HW")] <- NA
  
  
  
  indicateurs <- NULL 
  if(nrow(na.omit(resultats))>0){
    indicateurs <- round(matrix(c(MAE(model.fcst.ARIMA$mean, resultats$net_sales[!is.na(resultats$ARIMA)]),
                                  MAE(model.fcst.nnetar$mean, resultats$net_sales[!is.na(resultats$NNETAR)]),
                                  MAE(model.fcst.mlp$mean, resultats$net_sales[!is.na(resultats$MLP)]),
                                  MAE(model.fcst.HW$mean, resultats$net_sales[!is.na(resultats$HW)]),
                                  MAPE(model.fcst.ARIMA$mean, resultats$net_sales[!is.na(resultats$ARIMA)])*100,
                                  MAPE(model.fcst.nnetar$mean, resultats$net_sales[!is.na(resultats$NNETAR)])*100,
                                  MAPE(model.fcst.mlp$mean, resultats$net_sales[!is.na(resultats$MLP)])*100,
                                  MAPE(model.fcst.HW$mean, resultats$net_sales[!is.na(resultats$HW)])*100),
                                nrow = 2,
                                ncol=4,
                                byrow=TRUE,
                                dimnames = list(c("MAE","MAPE"), c("ARIMA", "NNETAR","MLP","HW"))),2)
  }
  
  
  prediction.plot <- resultats %>% 
    filter(date >= floor_date(floor_date(DateFinApprentissage-months(1),"month"), "year")) %>% 
    plot_ly(x = ~date, y = ~net_sales, name = 'Valeurs observées', type = 'bar') %>%
    add_trace(y = ~ARIMA, name = 'Prédiction ARIMA', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~NNETAR, name = 'Prédiction NNETAR', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~MLP, name = 'Prédiction MLP', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~HW, name = 'Prédiction HW', type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis = list(title = ''),yaxis = list (title = "Chiffre d'affaires (en euros)"),title = paste("Prédiction du chiffre d'affaires sur",NbMoisPred, "mois (en euros)"))
  

  # Graphique des prédictions cumulées
  prediction.cumul.plot <- resultats.cumul %>% 
    plot_ly(x = ~date, y = ~net_sales, name = 'Valeurs observées', type = 'bar') %>%
    add_trace(y = ~ARIMA, name = 'Prédiction ARIMA', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~NNETAR, name = 'Prédiction NNETAR', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~MLP, name = 'Prédiction MLP', type = 'scatter', mode = 'lines+markers') %>%
    add_trace(y = ~HW, name = 'Prédiction HW', type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis = list(title = ''),yaxis = list (title = "Chiffre d'affaires (en euros)"),title = paste("Prédiction du chiffre d'affaires sur",NbMoisPred, "mois (en euros)"))
  
  
  data.models.pred$date <- as.character(data.models.pred$date)
  
  resultats.cumul <- resultats.cumul[resultats.cumul$date> DateFinApprentissage,c("date","ARIMA","NNETAR","MLP","HW")]
  resultats.cumul$date <- as.character(resultats.cumul$date)
  
  
  # Graphique de la série temporelle
  plot.tsl <- stl(x = data.ts, s.window = "periodic") %>% 
    autoplot(ts.colour = 'blue',xlab = "", ylab = "Chiffre d'affaires des ventes (en €)", main = "Statistiques série temporelle")
  
  
  return(list(data.models.pred,prediction.plot,prediction.cumul.plot,resultats.cumul,plot.tsl,indicateurs))
  
}


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ------------------------------ PARTIE TEST -----------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# data <- data.machinelearning
# NbMoisPred <- 3
# DateFinApprentissage <- "2022-07-15"
# res <- machinelearning(data,NbMoisPred,DateFinApprentissage)
