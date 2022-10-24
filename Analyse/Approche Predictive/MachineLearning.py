# -*- coding: utf-8 -*-
"""
Created on Wed Aug 17 11:17:46 2022

@author: GFT
"""

#♦ https://blog.engineering.publicissapient.fr/2020/09/23/long-short-term-memory-lstm-networks-for-time-series-forecasting/


def LSTM_PREP(data=None,n_epoch=None):
    
    ###############################################################################
    # ===================== Tache 1 : Import des packages ========================= 
    ###############################################################################
     
    import pandas as pd
    import numpy as np
     
     
    from keras.models import Sequential
    from keras.layers import Dense, LSTM
     
    from sklearn.preprocessing import MinMaxScaler
     
    from dateutil.relativedelta import relativedelta

    from datetime import date
    from pandas.tseries.offsets import MonthBegin

    ###############################################################################
    # =================== Tache 2 : Load and prepare data ========================= 
    ###############################################################################

    
    data = data.set_index('date_invoice_month')
    data.index = pd.to_datetime(data.index)

    series = pd.DataFrame(data)

    # verify the parameters
    n_epoch = int(n_epoch)
    DateFinApprentissage = str(pd.to_datetime(date.today()) - MonthBegin(1))

    # Divide data into train and test sets
    train = series.loc[series.index < pd.to_datetime(DateFinApprentissage)]

    # Normalize training data
    sc = MinMaxScaler(feature_range=(-1,1))
    train_scaled = sc.fit_transform(train)
     
    # Create supervised data with 12 inputs and 1 output
    n_lag = 12
    X_train = []
    y_train = []
    for i in range(n_lag, len(train)):
        X_train.append(train_scaled[i-n_lag:i, 0])
        y_train.append(train_scaled[i, 0])
     
    X_train, y_train = np.array(X_train), np.array(y_train)
     
    # Reshape train set
    X_train = np.reshape(X_train, (X_train.shape[0], X_train.shape[1], 1))


    ###############################################################################
    # =================== Tache 3 : Build and train model ========================= 
    ###############################################################################

    # Initiate model
    regressor = Sequential()
     
    # Add one LSTM layer
    regressor.add(LSTM(units=4, input_shape=(X_train.shape[1], 1)))
     
    # Add an output layer
    regressor.add(Dense(units=1))
     
    # Compile the model
    regressor.compile(loss='mean_squared_error', optimizer='adam')
     
    # Fit LSTM to the training set with a split for validation
    regressor.fit(X_train, y_train, epochs=50, batch_size=1, verbose=0, shuffle=False)

    
    return regressor


def LSTM_PRED(data=None,Regressor=None, NbPred=None):
    
    ###############################################################################
    # ===================== Tache 1 : Import des packages ========================= 
    ###############################################################################
     
    import pandas as pd
    import numpy as np
     
     
    from keras.models import Sequential
    from keras.layers import Dense, LSTM
     
    from sklearn.preprocessing import MinMaxScaler
     
    from dateutil.relativedelta import relativedelta

    from datetime import date
    from pandas.tseries.offsets import MonthBegin


 
    ###############################################################################
    # =================== Tache 2 : Forecast future values ======================== 
    ###############################################################################
    
    data = data.set_index('date_invoice_month')
    data.index = pd.to_datetime(data.index)

    series = pd.DataFrame(data)

    # verify the parameters
    NbPred = int(NbPred)
    DateFinApprentissage = str(pd.to_datetime(date.today()) - MonthBegin(1))
    
    train = series.loc[series.index < pd.to_datetime(DateFinApprentissage)]

    test = pd.DataFrame(index=pd.date_range(start=pd.to_datetime(DateFinApprentissage), end=pd.to_datetime(DateFinApprentissage)+relativedelta(months=+(NbPred-1)), freq='MS'), columns=['net_sales'])

    n_lag = 12

    # Normalize training data
    sc = MinMaxScaler(feature_range=(-1,1))
    train_scaled = sc.fit_transform(train)
    
        
    # Prepare test set
    inputs = series[len(series) - len(test) - n_lag:].values
    inputs = inputs.reshape(-1,1)
    inputs = sc.transform(inputs)
    X_test = []
    for i in range(n_lag, n_lag+len(test)):
        X_test.append(inputs[i-n_lag:i, 0])
         
    X_test = np.array(X_test)
    X_test = np.reshape(X_test, (X_test.shape[0], X_test.shape[1], 1))
     
    # Forecast
    regressor = Regressor
    predict_scaled = regressor.predict(X_test)
    predict = sc.inverse_transform(predict_scaled)
     
    predict = pd.DataFrame(predict)
    predict.columns = ['net_sales']
    predict['date_invoice_month'] = test.index
    # predict.index = test.index
    predict['date_invoice_month'] = predict['date_invoice_month'].astype(str)
    
    return predict

