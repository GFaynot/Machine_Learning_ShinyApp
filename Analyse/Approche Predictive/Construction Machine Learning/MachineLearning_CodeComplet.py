# -*- coding: utf-8 -*-
"""
Created on Fri Apr 15 12:09:11 2022

@author: GFT
"""

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ---------------------------- Introduction --------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# https://towardsdatascience.com/predicting-sales-611cb5a252de

#%%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ---------------------------- Import packages --------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
from __future__ import division
from datetime import datetime, timedelta,date
import pandas as pd, datetime as dt
# matplotlib inline
import matplotlib.pyplot as plt
import numpy as np
from sklearn.metrics import mean_absolute_error

import warnings
warnings.filterwarnings("ignore")

from chart_studio import plotly
import plotly.offline as pyoff
import plotly.graph_objs as go
import plotly.io as pio


#import Keras
import keras
from keras.layers import Dense
from keras.models import Sequential
#from keras.optimizers import Adam
from tensorflow.keras.optimizers import Adam
from keras.callbacks import EarlyStopping
from keras.utils import np_utils
from keras.layers import LSTM
from sklearn.model_selection import KFold, cross_val_score, train_test_split

######RMSE#######
from sklearn.metrics import mean_squared_error 
from statsmodels.tools.eval_measures import rmse 

#%%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ---------------------------- Import Data ------------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#initiate plotly
pyoff.init_notebook_mode()
pio.renderers.default = 'browser'

#read the data in csv
data = pd.read_excel('A:/Memoire GFT/Data/Data 2013-2021.xlsx')


#%%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# -------------------------- Data Formatting ----------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#convert date field from string to datetime

data['date_invoice'] = pd.to_datetime(data['date_invoice'])
#data['date_invoice_week'] = data.apply(lambda row: row['date_invoice'] - dt.timedelta(days=row['date_invoice'].weekday()), axis=1)
data['date_invoice_month'] = data['date_invoice'].dt.year.astype('str') + '-' + data['date_invoice'].dt.month.astype('str') + '-01'#represent month in date field as its first day
data['date_invoice_month'] = pd.to_datetime(data['date_invoice_month'])
data['year'] = pd.DatetimeIndex(data['date_invoice']).year


#groupby date and sum the sales
df_sales = data.groupby(['date_invoice_month','year']).net_sales.sum().reset_index()
df_sales['net_sales_cum'] = df_sales.groupby('year')['net_sales'].transform(pd.Series.cumsum)
df_sales = df_sales.drop('net_sales', 1)
df_sales = df_sales.drop('year', 1)

#plot monthly sales
plot_data = [
    go.Scatter(
        x=df_sales['date_invoice_month'],
        y=df_sales['net_sales_cum'],
    )
]
plot_layout = go.Layout(
        title='Monthly Sales'
    )
fig = go.Figure(data=plot_data, layout=plot_layout)
pyoff.iplot(fig)




#create a new dataframe to model the difference
df_diff = df_sales.copy()

#add previous sales to the next row
df_diff['prev_sales'] = df_diff['net_sales_cum'].shift(1)

#drop the null values and calculate the difference
df_diff = df_diff.dropna()
df_diff['diff'] = (df_diff['net_sales_cum'] - df_diff['prev_sales'])



#plot sales diff
plot_data = [
    go.Scatter(
        x=df_diff['date_invoice_month'],
        y=df_diff['diff'],
    )
]
plot_layout = go.Layout(
        title='Monthly Sales Diff'
    )
fig = go.Figure(data=plot_data, layout=plot_layout)
pyoff.iplot(fig)




#create dataframe for transformation from time series to supervised
df_supervised = df_diff.drop(['prev_sales'],axis=1)
#adding lags
for inc in range(1,14):
    field_name = 'lag_' + str(inc)
    df_supervised[field_name] = df_supervised['diff'].shift(inc)
#drop null values
df_supervised = df_supervised.dropna().reset_index(drop=True)


# Import statsmodels.formula.api
import statsmodels.formula.api as smf
# Define the regression formula
model = smf.ols(formula='diff~lag_1+lag_2+lag_3+lag_4+ lag_5+ lag_6+ lag_7+ lag_8+ lag_9+ lag_10+lag_11+ lag_12', data=df_supervised)
#model = smf.ols(formula='diff~lag_1+lag_2+lag_3+lag_4+ lag_5+ lag_6+ lag_7+ lag_8+ lag_9+ lag_10+lag_11+ lag_12+ lag_13+ lag_14+ lag_15+ lag_16+ lag_17+lag_18+ lag_19+ lag_20+ lag_21+ lag_22+ lag_23+ lag_24+lag_25', data=df_supervised)
#model = smf.ols(formula='diff~lag_1+lag_2+lag_3+lag_4+ lag_5+ lag_6+ lag_7+ lag_8+ lag_9+ lag_10+lag_11+ lag_12+ lag_13+ lag_14+ lag_15+ lag_16+ lag_17+lag_18+ lag_19+ lag_20+ lag_21+ lag_22+ lag_23+ lag_24+lag_25+ lag_26+ lag_27+ lag_28+ lag_29+ lag_30+ lag_31+lag_32+ lag_33+ lag_34+ lag_35+ lag_36+ lag_37+ lag_38+lag_39+ lag_40+ lag_41+ lag_42+ lag_43+ lag_44+ lag_45+lag_46+ lag_47+ lag_48+ lag_49+ lag_50+ lag_51+ lag_52', data=df_supervised)
# Fit the regression
model_fit = model.fit()
# Extract the adjusted r-squared
regression_adj_rsq = model_fit.rsquared_adj
print(regression_adj_rsq)


#%%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ---------------- Data formatting for LSTM model -----------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


#import MinMaxScaler and create a new dataframe for LSTM model
from sklearn.preprocessing import MinMaxScaler
df_model = df_supervised.drop(['net_sales_cum','date_invoice_month'],axis=1)
#split train and test set
train_set, test_set = df_model[0:-11].values, df_model[-11:].values

#apply Min Max Scaler
scaler = MinMaxScaler(feature_range=(-1, 1))
scaler = scaler.fit(train_set)
# reshape training set
train_set = train_set.reshape(train_set.shape[0], train_set.shape[1])
train_set_scaled = scaler.transform(train_set)
# reshape test set
test_set = test_set.reshape(test_set.shape[0], test_set.shape[1])
test_set_scaled = scaler.transform(test_set)


#%%
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# ---------------------------- LSTM model -------------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


X_train, y_train = train_set_scaled[:, 1:], train_set_scaled[:, 0:1]
X_train = X_train.reshape(X_train.shape[0], 1, X_train.shape[1])
X_test, y_test = test_set_scaled[:, 1:], test_set_scaled[:, 0:1]
X_test = X_test.reshape(X_test.shape[0], 1, X_test.shape[1])

model = Sequential()
model.add(LSTM(4, batch_input_shape=(1, X_train.shape[1], X_train.shape[2]), stateful=True))
model.add(Dense(1))
model.compile(loss='mean_squared_error', optimizer='adam')
model.fit(X_train, y_train, epochs=100, batch_size=1, verbose=1, shuffle=False)

y_pred = model.predict(X_test,batch_size=1)
#for multistep prediction, you need to replace X_test values with the predictions coming from t-1

print(model.predict(X_test,batch_size=1))

#reshape y_pred
y_pred = y_pred.reshape(y_pred.shape[0], 1, y_pred.shape[1])
#rebuild test set for inverse transform
pred_test_set = []
for index in range(0,len(y_pred)):
    print (np.concatenate([y_pred[index],X_test[index]],axis=1))
    pred_test_set.append(np.concatenate([y_pred[index],X_test[index]],axis=1))
#reshape pred_test_set
pred_test_set = np.array(pred_test_set)
pred_test_set = pred_test_set.reshape(pred_test_set.shape[0], pred_test_set.shape[2])
#inverse transform
pred_test_set_inverted = scaler.inverse_transform(pred_test_set)


#create dataframe that shows the predicted sales
result_list = []
sales_dates = list(df_sales[-12:].date_invoice_month)
act_sales = list(df_sales[-12:].net_sales_cum)
for index in range(0,len(pred_test_set_inverted)):
    result_dict = {}
    result_dict['pred_value'] = int(pred_test_set_inverted[index][0] + act_sales[index])
    result_dict['date_invoice_month'] = sales_dates[index+1]
    result_list.append(result_dict)
df_result = pd.DataFrame(result_list)
#for multistep prediction, replace act_sales with the predicted sales


#merge with actual sales dataframe
df_sales_pred = pd.merge(df_sales,df_result,on='date_invoice_month',how='left')
#plot actual and predicted
plot_data = [
    go.Scatter(
        x=df_sales_pred['date_invoice_month'],
        y=df_sales_pred['net_sales_cum'],
        name='actual'
    ),
        go.Scatter(
        x=df_sales_pred['date_invoice_month'],
        y=df_sales_pred['pred_value'],
        name='predicted'
    )
    
]
plot_layout = go.Layout(
        title='Sales Prediction'
    )
fig = go.Figure(data=plot_data, layout=plot_layout)
pyoff.iplot(fig)

#%%
#RMSE for LSTM Model
err_LSTM = rmse(df_sales_pred['net_sales_cum'][95:107], df_sales_pred['pred_value'][95:107])
print('RMSE with LSTM', err_LSTM)


#çdf_sales_pred['pred_value'][96] = 250000


#Defining MAPE function
def MAPE(Y_actual,Y_Predicted):
    mape = np.mean(np.abs((Y_actual - Y_Predicted)/Y_actual))*100
    return mape
 
print("MAPE: ",MAPE(df_sales_pred['net_sales_cum'][97:107], df_sales_pred['pred_value'][97:107]))