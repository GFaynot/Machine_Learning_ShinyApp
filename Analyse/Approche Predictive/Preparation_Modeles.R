#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# =============================== INTRODUCTION =================================
# Ce programme prépare les modèles à l'avance pour pouvoir prédire plus 
# rapidement dans l'application
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------ Packages -------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

source("Z:/Analyse Financiere Reservoir/RStudio/PackagesShiny.R", encoding = "UTF-8")

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
# ------------------------ Data -----------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

load("Z:/Analyse Financiere Reservoir/Data/Facturation Groupe.RData")

source_python("Z:/Analyse Financiere Reservoir/Analyse/Approche Predictive/MachineLearning.py")

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ================================ PREPARATION =================================
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



DateDebutApprentissage <- as.Date(floor_date(min(data$date_invoice_month),"month"))
DateFinApprentissage <- as.Date(floor_date(Sys.Date()-months(1),"month"))

# Supprimer les données poour les mois incomplets (mois en cours)
data <- data.machinelearning[data.machinelearning$date_invoice_month<floor_date(Sys.Date(), "month"),]

# Dataframe en série temporelle
data.ts <- ts(data$net_sales, frequency = 12, start = year(min(as.Date(DateDebutApprentissage))))

# Transformation logarithmique
data.ts <- log(data.ts) # eviter les prédictions négatives

# Définir les données d'apprentissage et de test
DateDebutTest <- floor_date(DateFinApprentissage+months(1), "month")

model.train <- window(data.ts, start = year(DateDebutApprentissage), end = c(year(DateFinApprentissage),month(DateFinApprentissage)))



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ================================ COURT TERME =================================
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# ARIMA model
model.fit.ARIMA.CT.MT.LT <- auto.arima(model.train, trace=FALSE, ic="aic")

# LSTM
model.fit.LSTM.CT <- serialize_model(LSTM_PREP(data = data,n_epoch = 6))


# NNETAR model
model.fit.nnetar.CT <- nnetar(model.train,P=1, p=20,size=20, repeats = 10)

# MLP model (package nnfor)
model.fit.mlp.CT <- mlp(model.train, hd=30,reps=5)

# Holt-Winters model
model.fit.HW.CT <- HoltWinters(model.train, alpha=0.1, beta=0.2, gamma=1)



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ================================ MOYEN TERME =================================
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


# LSTM
model.fit.LSTM.MT <- serialize_model(LSTM_PREP(data = data,n_epoch = 400))


# NNETAR model
model.fit.nnetar.MT <- nnetar(model.train,P=2, p=25,size=20, repeats = 15)

# MLP model (package nnfor)
model.fit.mlp.MT <- mlp(model.train, hd=30,reps=25)

# Holt-Winters model
model.fit.HW.MT <- HoltWinters(model.train, alpha=0.5, beta=0.4, gamma=0.9)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ================================ LONG TERME ==================================
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


# LSTM
model.fit.LSTM.LT <- serialize_model(LSTM_PREP(data = data,n_epoch = 10))


# NNETAR model
model.fit.nnetar.LT <- nnetar(model.train,P=2, p=25,size=25, repeats = 15)

# MLP model (package nnfor)
model.fit.mlp.LT <- mlp(model.train, hd=25,reps=25)

# Holt-Winters model
model.fit.HW.LT <- HoltWinters(model.train, alpha=0.5, beta=0.3, gamma=0.7)


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ================================== EXPORT ====================================
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

save(data.machinelearning,
     data.ts,
     model.fit.ARIMA.CT.MT.LT,
     model.fit.LSTM.CT,
     model.fit.nnetar.CT,
     model.fit.mlp.CT,
     model.fit.HW.CT,
     model.fit.LSTM.MT,
     model.fit.nnetar.MT,
     model.fit.mlp.MT,
     model.fit.HW.MT,
     model.fit.LSTM.LT,
     model.fit.nnetar.LT,
     model.fit.mlp.LT,
     model.fit.HW.LT,
     file="Z:/Analyse Financiere Reservoir/Analyse/Approche Predictive/PreparationModeles.RData")

