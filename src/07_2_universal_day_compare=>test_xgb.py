from tqdm import tqdm
import numpy as np
import pandas as pd
from os import listdir;
from os.path import isfile, join;
from sklearn.metrics import r2_score
import os
os.sys.path.append("/home/kanli/cmem/src/")
from config import *
# import sys;sys.path.append("/homes/80/kang/cmem/");from src.config import *
'''
I guess 
for testing we should use 
07compare=>test.py for single assets
07_2_universal_day_compare=>test.py for universal models but more general
but still not sure what is the difference between 07_2_universal_day_compare=>test.py and 07_2_universal_compare=>test.py

for 07_2_universal_compare=>test.py
should be used for universal
but the 07_2_universal_day_compare=>test.py one is more general

such as:
df3lst = []
# for start_index in tqdm(range(1)):
# list(range(0, 100, 50))
num_stock_per_group = 1
# num_stock_per_group = 2
# num_stock_per_group = 5
# num_stock_per_group = 10
# num_stock_per_group = 20
# num_stock_per_group = 50
# num_stock_per_group = 100
# for start_index in tqdm(range(0, 100, num_stock_per_group)):
# for start_index in tqdm(range(20, 100, num_stock_per_group)):
# for start_index in tqdm(range(40, 60, num_stock_per_group)):
# for start_index in tqdm(range(60, 80, num_stock_per_group)):
# for start_index in tqdm(range(80, 100, num_stock_per_group)):
# for start_index in tqdm(range(50, 60, num_stock_per_group)):
# for start_index in tqdm(range(70, 80, num_stock_per_group)):
# for start_index in tqdm(range(90, 100, num_stock_per_group)):
# for start_index in tqdm(range(30, 40, num_stock_per_group)):
for start_index in tqdm(range(10, 20, num_stock_per_group)):
    df3 = get_universal(start_index,num_stock_per_group)
    df3lst.append(df3)
df3lst1 = df3lst[0:10]
'''

def tryMkdir(path):
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0
_,_ = map(tryMkdir,[path05,path06])

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path01Files, path01_1Files, path02Files, path04Files, path05Files, path06Files =\
    map(readFromPath, [path01, path01_1, path02, path04, path05, path06])




def get_universal(start_index, num_of_stocks):
# def get_universal(start_index, num_of_stocks, end_index):
    # assert (end_index - start_index) % num_of_stocks == 0
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
        elif regulator == "XGB":
            import xgboost as xgb
            model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160)
            model.fit(X_train, y_train)
            y_pred = model.predict(X_test)
            y_pred = y_pred.flatten()
            return y_pred
        else:
            raise NotImplementedError
    # ================================
    num = num_of_stocks
    # num = 100
    # num = 2 # for 2 stocks
    # num = 1 # for single stock
    def get_universal_df():
        df_lst = []
        from tqdm import tqdm
        for i in tqdm(range(start_index,start_index+num)): # on mac4
            df = pd.read_pickle(path06+path06Files[i])
            df_lst.append(df)

        new_dflst_lst = []
        for index, dflst in enumerate(df_lst):
            # assert dflst.shape[0] == 3172, f"index, {index}"
            if dflst.shape[0] == 3146:
                new_dflst_lst.append(dflst)

        gs = [dflst.iterrows() for dflst in new_dflst_lst]
        dff = []
        for i in tqdm(range(dflst.shape[0])):
            for g in gs:
                elem = next(g)[1].T
                dff.append(elem)
        df = pd.concat(dff, axis=1).T
        df.reset_index(inplace=True,drop=True)
        return df
    df = get_universal_df()
    # symbol = path06Files[i][:-4]
    # # ================================
    # from sklearn.cluster import KMeans
    #
    # km = KMeans(
    #     n_clusters=3, init='random',
    #     n_init=10, max_iter=300,
    #     tol=1e-04, random_state=0
    # )
    # y_km = km.fit_predict(X)
    # # ================================



    # ================================
    bin_size = 26
    train_size = 10 * 26
    test_size = 1 * 26
    index_max  = int((df.shape[0] -(train_size + test_size))/bin_size)
    # index = 0 for index in range(0, index_max+1)
    # index = 0 for index in range(0, index_max+0) # not sure

    # x_list = ['x', 'eta*seas', 'eta', 'seas', 'mu']
    # y_list = ['turnover']

    # x_list = ['eta','seas','mu']
    # y_list = ['turnover']

    our_log_features = ['log_ntn', 'log_volBuyNotional', 'log_volSellNotional', 'log_nrTrades', 'log_ntr',
                        'log_volBuyNrTrades_lit', 'log_volSellNrTrades_lit', 'log_volBuyQty', 'log_volSellQty',
                        'log_daily_ntn', 'log_daily_volBuyNotional', 'log_daily_volSellNotional', 'log_daily_nrTrades',
                        'log_daily_ntr', 'log_daily_volBuyNrTrades_lit', 'log_daily_volSellNrTrades_lit',
                        'log_daily_volBuyQty', 'log_daily_volSellQty', 'log_daily_qty', 'log_intraday_ntn',
                        'log_intraday_volBuyNotional', 'log_intraday_volSellNotional', 'log_intraday_nrTrades',
                        'log_intraday_ntr', 'log_intraday_volBuyNrTrades_lit', 'log_intraday_volSellNrTrades_lit',
                        'log_intraday_volBuyQty', 'log_intraday_volSellQty', 'log_intraday_qty', 'log_ntn_2',
                        'log_volBuyNotional_2', 'log_volSellNotional_2', 'log_nrTrades_2', 'log_ntr_2',
                        'log_volBuyNrTrades_lit_2', 'log_volSellNrTrades_lit_2', 'log_volBuyQty_2', 'log_volSellQty_2',
                        'log_ntn_8', 'log_volBuyNotional_8', 'log_volSellNotional_8', 'log_nrTrades_8', 'log_ntr_8',
                        'log_volBuyNrTrades_lit_8', 'log_volSellNrTrades_lit_8', 'log_volBuyQty_8', 'log_volSellQty_8']
    x_list = ['log_x', 'log_eta*seas', 'log_eta', 'log_seas', 'log_mu']
    x_list = x_list + our_log_features
    y_list = ['log_turnover']
    # x_list = ['log_eta', 'log_seas', 'log_mu']
    # y_list = ['log_turnover']
    # x_list = ['x']
    # y_list = ['turnover']

    # x_list = ['log_x']
    # y_list = ['log_turnover']
    original_space = ['turnover']
    # ================================


    # for index in tqdm(range(0, index_max + 1)):
    #     print(index)

    from tqdm import tqdm
    r2_list = []
    for index in tqdm(range(111)):

        train_start_index = (index * bin_size ) * num
        train_end_index = (index * bin_size + train_size ) * num
        test_start_index = train_end_index
        test_end_index = train_end_index + test_size * num
        def get_trainData(df):
            x_train = df.loc[:, x_list].iloc[train_start_index: train_end_index, :]
            y_train = df.loc[:, y_list].iloc[train_start_index: train_end_index, :]
            # x_train = df.iloc[train_start_index : train_end_index, x_list]
            # y_train = df.loc[train_start_index : train_end_index, y_list]
            return x_train, y_train
        def get_testData(df):
            x_test = df.loc[:, x_list].iloc[train_end_index:  test_end_index, :]
            y_test = df.loc[:, y_list].iloc[train_end_index: test_end_index, :]
            return x_test, y_test
        X_train, y_train = get_trainData(df)
        X_test, y_test = get_testData(df)
        original_images = df.loc[:, original_space].iloc[train_end_index:test_end_index,:]

        # regulator = "Lasso"
        regulator = "XGB"


        # regulator = "OLS"
        # regulator = "Ridge"
        # regulator = "None"
        y_pred = regularity_ols(X_train, y_train, X_test, regulator)
        min_limit, max_limit = y_train.min(), y_train.max()
        broadcast = lambda x:np.full(y_pred.shape[0], x.to_numpy())
        min_limit, max_limit= map(broadcast, [min_limit, max_limit])
        y_pred_clipped = np.clip(y_pred, min_limit, max_limit)
        if any('log' in x for x in x_list):
            y_pred_clipped = np.exp(y_pred_clipped)
        test_date = df.date[train_end_index]


        # r2 = r2_score(y_test, y_pred_clipped)
        y_pred_clipped = pd.DataFrame(y_pred_clipped)
        y_pred_clipped.columns = ['pred']
        original_images.reset_index(inplace=True,drop=True)
        original_images.columns = ['true']


        original_images['date'] = test_date
        stock_index = np.tile(np.arange(num),26)
        original_images['stock_index']= stock_index
        oneday_df = pd.concat([original_images,y_pred_clipped],axis=1)
        lst = []
        g = oneday_df.groupby(stock_index)
        for stock, item in g:
            pass
            r2value = r2_score(item['true'], item['pred'])
            lst.append([test_date, stock, r2value])
        r2_list.extend(lst)


    r2arr = np.array(r2_list)
    df1 = pd.DataFrame(r2arr)
    df1.columns = ['test_date','stock_index','r2']
    df2 = df1.pivot(index="test_date",columns="stock_index",values="r2")



    # r2arr[:,1].mean()
    print(df2)
    df2.mean(axis=0) # stock
    df2.mean(axis=1) # date
    df2.mean(axis=1).mean()
    #
    #
    # df.test_date = df.test_date.astype(int)
    # pivot_df = df.pivot(index='test_date', columns='symbol', values='r2')
    # dflst.append(pivot_df)
    #
    #
    #
    # r2df = pd.concat(dflst,axis =1)
    # r2df.to_csv(path00 + "07_r2df_"+regulator+"_.csv", mode = 'w')
    return df2
def print_mean(df3):
    print(f">>>> stock mean: \n",df3.mean(axis=0))  # stock
    print(f">>>> date mean: \n",df3.mean(axis=1))   # date
    print(f">>>> aggregate mean: \n",df3.mean(axis=1).mean())

# for start_index in tqdm(range(1)):
# list(range(0, 100, 50))
# num_stock_per_group = 2
# num_stock_per_group = 5
# num_stock_per_group = 10
# num_stock_per_group = 20
# num_stock_per_group = 50
# num_stock_per_group = 100
# for start_index in tqdm(range(0, 100, num_stock_per_group)):
# for start_index in tqdm(range(20, 100, num_stock_per_group)):
# for start_index in tqdm(range(40, 60, num_stock_per_group)):
# for start_index in tqdm(range(60, 80, num_stock_per_group)):
# for start_index in tqdm(range(80, 100, num_stock_per_group)):
# for start_index in tqdm(range(50, 60, num_stock_per_group)):
# for start_index in tqdm(range(70, 80, num_stock_per_group)):
# for start_index in tqdm(range(90, 100, num_stock_per_group)):
# for start_index in tqdm(range(30, 40, num_stock_per_group)):

# def get_universal(start_index, num_of_stocks):

df3 = get_universal(start_index=0,num_of_stocks=len(path06Files))


import pandas as pd
df3 = pd.read_csv("/home/kanli/cmem/07_r2df_universal_day_483_lasso_.csv",index_col=0)
df3
df3.index = df3.index.astype(int).astype(str)
m = df3.mean(axis=1) # by date
s = df3.std(axis=1) # by date





df3.mean(axis=1).mean() # all mean
# df3.to_csv(path00 + "07_r2df_universal_day_483_"+"lasso"+"_.csv", mode = 'w')

'''
start plotting
'''

a = (m-s).values
b = m.values
c = (m+s).values
mean = b.mean()


import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import timedelta, datetime

# Font size variable
font = 20

# Plotting
plt.figure(figsize=(16, 12))
# plt.figure(figsize=(12, 8))

dates = m.index
x_axis = pd.to_datetime(dates, format='%Y%m%d')
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d/%Y'))

# plot first group with shadow
plt.plot(x_axis, b, label='Mean of R Squared', color='blue')
plt.fill_between(x_axis, a, c, color='blue', alpha=0.1)
plt.axhline(mean, color='red', linestyle='-', label='Mean across all dates')
plt.text(x_axis[-1]+timedelta(days=5), mean + 0.01, f"{mean:.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)

# Adjusting font sizes with the font variable
plt.xlabel("Date", fontsize=font*1.2)
plt.ylabel("Out of sample R squared", fontsize=font*1.2)
plt.xticks(fontsize=font*1.2)
plt.yticks(fontsize=font*1.2)
plt.legend(fontsize=font*1.2)

plt.grid(True)

# Save the figure with the generated filename
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
filename = f"plot_{timestamp}.pdf"
plt.savefig(path00+filename, dpi=1200, bbox_inches='tight', format='pdf')
plt.show()



'''
end   plotting
'''




'''
The start_index here is actually the index of choosing which stock to start first to be selected for merged 
as universal df
there is no relationship with the forecasting date index
'''

'''
df3lst = []
for start_index in tqdm(range(10, 20, num_stock_per_group)):
    df3 = get_universal(start_index,num_stock_per_group)
    df3lst.append(df3)
df3lst1 = df3lst[0:10]
df4 = pd.concat(df3lst1, axis=1)
# df4 = pd.concat(df3lst, axis=1)
print_mean(df4)
'''
# df4.to_csv("100stocks_2stocksPerGroup"+".csv")
# s,e=0,10
# s,e=20,30
# s,e=40,50
# s,e=60,70
# s,e=80,90
# s,e=50,60
# s,e=70,80
# s,e=90,100
# s,e=30,40
# s,e=10,20




'''
df4.to_csv("/home/kanli/cmem/07_output_universal/"+f"100stocks_{num_stock_per_group}_stocksPerGroup_{s}_{e}"+".csv")
# df4.to_csv("/homes/80/kang/cmem/07_output_universal/"+f"100stocks_{num_stock_per_group}_stocksPerGroup_{s}_{e}"+".csv")

# df4.to_csv("100stocks_5stocksPerGroup"+".csv")
# df4.to_csv("100stocks_10stocksPerGroup"+".csv")
# df4.to_csv("100stocks_20stocksPerGroup"+".csv")
# df4.to_csv("100stocks_20stocksPerGroup"+".csv")


path100 = "/home/kanli/cmem/07_output_universal/"
# path100 = "/homes/80/kang/cmem/07_output_universal/"
readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path01Files, path100Files =\
    map(readFromPath, [path01, path100])

dflst = []
for i in range(len(path100Files)):
    df = pd.read_csv(path100+path100Files[i])
    df = df.set_index('test_date')
    dflst.append(df)

df5 = pd.concat(dflst,axis =1)
print_mean(df5)
'''
