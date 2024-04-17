import numpy as np
import pandas as pd
from os import listdir;
from os.path import isfile, join;
import os
os.sys.path.append("/home/kanli/cmem/src/")
from config import *
'''
this file should be wrong
as the loc in the xtrain ytrian is wrong

'''

def tryMkdir(path):
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0
_,_ = map(tryMkdir,[path05,path0600])

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path01Files, path01_1Files, path02Files, path04Files, path05Files, path0600Files =\
    map(readFromPath, [path01, path01_1, path02, path04, path05, path0600])

array1 = np.concatenate( [np.arange(1,10,0.01), np.arange(10,50,0.1) ])
array2 = np.arange(1,0.001,-0.001)
combined_array = np.array(list(zip(array1, array2))).flatten()
# used for alphas

def regularity_ols(X_train, y_train, X_test, regulator):
    if regulator == "None":
        y_pred = X_test.to_numpy().flatten()
        return y_pred
    elif regulator == "OLS":
        # print("OLS")
        import statsmodels.api as sm
        def ols_with_summary(X, y):
            X = sm.add_constant(X, has_constant='add')
            results = sm.OLS(y, X).fit()
            return results

        model = ols_with_summary(X_train, y_train)
        X = sm.add_constant(X_test, has_constant='add')
        y_pred = model.predict(X).values
        # assert type(y_pred) == np.float64
        return y_pred
    elif regulator in ["Lasso", "Ridge"]:
        # print("LASSO / RIDGE")
        def find_best_regularity_alpha(X_train, y_train):
            if regulator == "Lasso":
                from sklearn.linear_model import LassoCV
                model = LassoCV(random_state=0, max_iter=10000000)
            if regulator == "Ridge":
                from sklearn.linear_model import RidgeCV
                model = RidgeCV(alphas=combined_array)
            model.fit(X_train, y_train)
            return model.alpha_

        best_regularity_alpha = find_best_regularity_alpha(X_train, y_train)
        # print(best_regularity_alpha) #$
        if regulator == "Lasso":
            from sklearn.linear_model import Lasso
            reg = Lasso(alpha=best_regularity_alpha, max_iter=10000000, tol=1e-2)
        if regulator == "Ridge":
            from sklearn.linear_model import Ridge
            reg = Ridge(alpha=best_regularity_alpha, max_iter=10000000, tol=1e-2)
        reg.fit(X_train, y_train)
        # X = pd.DataFrame(X_test).T
        # y_pred = reg.predict(X)
        y_pred = reg.predict(X_test)
        y_pred = y_pred.flatten()
        return y_pred
    else:
        raise NotImplementedError


dflst = []
dflst2 =[]
from tqdm import tqdm
for i in tqdm(range(len(path0600Files))):
    print(f">>> i: {i}")
    df = pd.read_pickle(path0600+path0600Files[i])
    symbol = path0600Files[i][:-4]
    bin_size = 26
    train_size = 10 * 26
    test_size = 1 * 26
    index_max  = int((df.shape[0] -(train_size + test_size))/bin_size)
    r2_list = []
    mse_list = []
    # index = 0 for index in range(0, index_max+1)
    # index = 0 for index in range(0, index_max+0) # not sure

    # x_list = ['x', 'eta*seas', 'eta', 'seas', 'mu']
    # y_list = ['turnover']

    # x_list = ['eta','seas','mu']
    # y_list = ['turnover']

    # our_log_features = ['log_ntn', 'log_volBuyNotional', 'log_volSellNotional', 'log_nrTrades', 'log_ntr',
    #                     'log_volBuyNrTrades_lit', 'log_volSellNrTrades_lit', 'log_volBuyQty', 'log_volSellQty',
    #                     'log_daily_ntn', 'log_daily_volBuyNotional', 'log_daily_volSellNotional', 'log_daily_nrTrades',
    #                     'log_daily_ntr', 'log_daily_volBuyNrTrades_lit', 'log_daily_volSellNrTrades_lit',
    #                     'log_daily_volBuyQty', 'log_daily_volSellQty', 'log_daily_qty', 'log_intraday_ntn',
    #                     'log_intraday_volBuyNotional', 'log_intraday_volSellNotional', 'log_intraday_nrTrades',
    #                     'log_intraday_ntr', 'log_intraday_volBuyNrTrades_lit', 'log_intraday_volSellNrTrades_lit',
    #                     'log_intraday_volBuyQty', 'log_intraday_volSellQty', 'log_intraday_qty', 'log_ntn_2',
    #                     'log_volBuyNotional_2', 'log_volSellNotional_2', 'log_nrTrades_2', 'log_ntr_2',
    #                     'log_volBuyNrTrades_lit_2', 'log_volSellNrTrades_lit_2', 'log_volBuyQty_2', 'log_volSellQty_2',
    #                     'log_ntn_8', 'log_volBuyNotional_8', 'log_volSellNotional_8', 'log_nrTrades_8', 'log_ntr_8',
    #                     'log_volBuyNrTrades_lit_8', 'log_volSellNrTrades_lit_8', 'log_volBuyQty_8', 'log_volSellQty_8']
    # x_list = ['log_x', 'log_eta*seas', 'log_eta', 'log_seas', 'log_mu']
    # x_list = x_list +  our_log_features
    x_list = ['log_eta', 'log_seas', 'log_mu']
    y_list = ['log_turnover']


    x_list = ['x']
    y_list = ['turnover']

    # x_list = ['log_x']
    # y_list = ['log_turnover']


    original_space = ['turnover']
    for index in range(0, index_max + 1):
        train_end_index = index * bin_size + train_size
        def get_trainData(df):
            x_train = df.loc[index * bin_size : index * bin_size + train_size,x_list]
            y_train = df.loc[index * bin_size : index * bin_size + train_size,y_list]
            return x_train, y_train
        def get_testData(df):
            x_test = df.loc[train_end_index:train_end_index+test_size,x_list]
            y_test = df.loc[train_end_index:train_end_index+test_size,y_list]
            return x_test, y_test
        X_train, y_train = get_trainData(df)
        X_test, y_test = get_testData(df)

        # regulator = "OLS"
        # regulator = "Lasso"
        # regulator = "Ridge"
        regulator = "None"
        y_pred = regularity_ols(X_train, y_train, X_test, regulator)
        min_limit, max_limit = y_train.min(), y_train.max()
        broadcast = lambda x:np.full(y_pred.shape[0], x.to_numpy())
        min_limit, max_limit= map(broadcast, [min_limit, max_limit])
        y_pred_clipped = np.clip(y_pred, min_limit, max_limit)
        if any('log' in x for x in x_list):
            y_pred_clipped = np.exp(y_pred_clipped)
        test_date = df.date[train_end_index]


        from sklearn.metrics import r2_score
        original_images = df.loc[train_end_index:train_end_index+test_size,original_space]
        # r2 = r2_score(y_test, y_pred_clipped)
        r2 = r2_score(original_images, y_pred_clipped)
        from sklearn.metrics import mean_squared_error
        mse = mean_squared_error(original_images, y_pred_clipped)


        r2_list.append([test_date,r2])
        mse_list.append([test_date,mse])
        # y_list.append([test_date, y_test, y_pred_clipped])
    r2arr = np.array(r2_list)
    df = pd.DataFrame(r2arr)
    # r2arr[:,1].mean()
    df.columns = ['test_date','r2']
    df['symbol'] = symbol
    df = df[['symbol','test_date','r2']]
    df.test_date = df.test_date.astype(int)
    pivot_df = df.pivot(index='test_date', columns='symbol', values='r2')
    dflst.append(pivot_df)

    msearr = np.array(mse_list)
    df2 = pd.DataFrame(msearr)
    df2.columns = ['test_date','mse']
    df2['symbol'] = symbol
    df2 = df2[['symbol','test_date','mse']]
    df2.test_date = df2.test_date.astype(int)
    pivot_df2 = df2.pivot(index='test_date', columns='symbol', values='mse')
    dflst2.append(pivot_df2)
r2df = pd.concat(dflst,axis =1)
msedf = pd.concat(dflst2,axis =1)


import datetime
# Get the current date and time
current_time = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
# Construct the filename with the timestamp
filename = path00 + "07_r2df_" + regulator + "_" + current_time + ".csv"
# Save the DataFrame to the CSV file with the specified filename
r2df.to_csv(filename, mode='w')

import datetime
# Get the current date and time
current_time = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
# Construct the filename with the timestamp
filename = path00 + "07_msedf_" + regulator + "_" + current_time + ".csv"
# Save the DataFrame to the CSV file with the specified filename
r2df.to_csv(filename, mode='w')
r2df_copy = r2df.copy()

r2df.mean(axis=1)
r2df.mean(axis=1).mean()

# r2df = r2df_copy.copy()
def select_quantile(r2df,quantile):
    '''
    r2df rows are date, cols are stock
    r2df.mean(axis=0) get the mean of each stock,
    '''
    mean_values = r2df.mean(axis=0)
    threshold = mean_values.quantile(quantile)
    selected_r2df = r2df.loc[:, mean_values >= threshold]
    print(quantile, selected_r2df.shape[1]/r2df.shape[1])
    return selected_r2df

select_quantile(r2df,0.20).mean(axis=1).mean()

r2df = select_quantile(r2df,0.20)




# r2df = r2dff1

# '''
type='r2'
df3 = msedf if type =='mse' else r2df
df3.index = df3.index.astype(int).astype(str)
m = df3.mean(axis=1) # by date
s = df3.std(axis=1) # by date
df3.mean(axis=1).mean() # all mean
# df3.to_csv(path00 + "07_r2df_universal_day_483_"+"lasso"+"_.csv", mode = 'w')
# start plotting
a = (m-s).values
b = m.values
c = (m+s).values
mean = b.mean()
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import timedelta, datetime
font = 20# Font size variable
plt.figure(figsize=(16, 12))# Plotting
# plt.figure(figsize=(12, 8))
dates = m.index
x_axis = pd.to_datetime(dates, format='%Y%m%d')
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d/%Y'))
# plot first group with shadow
plot_label = 'Mean of R Squared' if type =='r2' else 'Mean of Mean Sqaured Error'
plt.plot(x_axis, b, label=plot_label, color='blue')
plt.fill_between(x_axis, a, c, color='blue', alpha=0.1)
plt.axhline(mean, color='red', linestyle='-', label='Mean across all dates')
plt.text(x_axis[-1]+timedelta(days=5), mean + 0.01, f"{mean:,.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)
# Adjusting font sizes with the font variable
plt.xlabel("Date", fontsize=font*1.2)
ylabel = "Out of sample R squared" if type=='r2' else "Out of sample Mean Squared Error"
plt.ylabel(ylabel, fontsize=font*1.2)
plt.xticks(fontsize=font*1.2)
plt.yticks(fontsize=font*1.2)
plt.legend(fontsize=font*1.2)
plt.grid(True)
# Save the figure with the generated filename
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
filename = f"plot_{timestamp}.pdf"
plt.savefig(path00+filename, dpi=1200, bbox_inches='tight', format='pdf')
plt.show()
# '''
