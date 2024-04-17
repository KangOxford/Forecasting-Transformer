import numpy as np
import pandas as pd
from os import listdir;
import time
from os.path import isfile, join;

import os
os.sys.path.append("/home/kanli/cmem/src/")
os.sys.path.append("/Users/kang/CMEM/src/")
os.sys.path.append("/homes/80/kang/cmem/src/")
from config import *



def tryMkdir(path):
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0
_,_ = map(tryMkdir,[path05,path06])

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path01Files, path01_1Files, path02Files, path04Files, path05Files, path06Files =\
    map(readFromPath, [path01, path01_1, path02, path04, path05, path06])

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
        import warnings
        from sklearn.exceptions import DataConversionWarning
        warnings.filterwarnings("ignore", category=DataConversionWarning)
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

if __name__=="__main__":
    dflst = []
    dflst2 =[]
    from tqdm import tqdm
    # for i in tqdm(range(len(path06Files))):
    def process_file(i):
        start = time.time()
        df = pd.read_pickle(path06+path06Files[i])
        symbol = path06Files[i][:-4]
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
        if_log = False
        if if_log:
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
            x_list = ['log_x', 'log_eta*seas', 'log_eta', 'log_seas', 'log_mu']
            # x_list = x_list +  our_log_features
            y_list = ['log_turnover']
            # x_list = ['log_eta', 'log_seas', 'log_mu']
            # y_list = ['log_turnover']
            # x_list = ['x']
            # y_list = ['turnover']

            # x_list = ['log_x']
            # y_list = ['log_turnover']
            # breakpoint()
        else:

            # x_list =   ["x", "eta*seas", "eta", "seas", "mu", "ntn", "volBuyNotional", "volSellNotional", "nrTrades",
            #               "ntr", "volBuyNrTrades_lit", "volSellNrTrades_lit", "volBuyQty", "volSellQty", "daily_ntn",
            #               "daily_volBuyNotional", "daily_volSellNotional", "daily_nrTrades", "daily_ntr",
            #               "daily_volBuyNrTrades_lit", "daily_volSellNrTrades_lit", "daily_volBuyQty", "daily_volSellQty",
            #               "daily_qty", "intraday_ntn", "intraday_volBuyNotional", "intraday_volSellNotional",
            #               "intraday_nrTrades", "intraday_ntr", "intraday_volBuyNrTrades_lit", "intraday_volSellNrTrades_lit",
            #               "intraday_volBuyQty", "intraday_volSellQty", "intraday_qty", "ntn_2", "volBuyNotional_2",
            #               "volSellNotional_2", "nrTrades_2", "ntr_2", "volBuyNrTrades_lit_2", "volSellNrTrades_lit_2",
            #               "volBuyQty_2", "volSellQty_2", "ntn_8", "volBuyNotional_8", "volSellNotional_8", "nrTrades_8",
            #               "ntr_8", "volBuyNrTrades_lit_8", "volSellNrTrades_lit_8", "volBuyQty_8", "volSellQty_8", "qty"]

            # x_list = ["x", "eta*seas", "eta", "seas", "mu"]
            x_list = ["x"]
            y_list = ['turnover']


        original_space = ['turnover']



        # # ---------- test -----------
        #
        # dff = df[['date','turnover','x']]
        # g =dff.groupby('date')
        # lst = []
        # for index, item in g:
        #     pass
        #     from sklearn.metrics import r2_score
        #     lst.append((int(index),r2_score(item.turnover, item.x)))
        # df0 = pd.DataFrame(lst)
        # df0.mean()
        # # df['x']
        # # df['eta']
        # # df['seas']
        # # df['mu']
        # # df['eta']*df['seas']*df['mu']
        # # ---------- test -----------

        for index in range(0, index_max + 1):

            # train_end_index = index * bin_size + train_size
            num_of_universal_stocks = 1
            train_start_index = (index * bin_size) * num_of_universal_stocks
            train_end_index = (index * bin_size + train_size) * num_of_universal_stocks
            test_start_index = train_end_index
            test_end_index = train_end_index + test_size * num_of_universal_stocks

            def get_trainData(df):
                x_train = df.loc[:, x_list].iloc[train_start_index: train_end_index, :]
                y_train = df.loc[:, y_list].iloc[train_start_index: train_end_index, :]
                return x_train, y_train
            def get_testData(df):
                x_test = df.loc[:, x_list].iloc[train_end_index:  test_end_index, :]
                y_test = df.loc[:, y_list].iloc[train_end_index:  test_end_index, :]
                return x_test, y_test
            X_train, y_train = get_trainData(df)
            X_test, y_test = get_testData(df)
            # original_images = df.loc[train_end_index:train_end_index+test_size,original_space]
            original_images = df.loc[:, original_space].iloc[train_end_index:test_end_index, :]


            # regulator = "OLS"
            # regulator = "Lasso"
            # regulator = "Ridge"
            regulator = "None"

            y_pred = regularity_ols(X_train, y_train, X_test, regulator)
            # 1st
            # min_limit, max_limit = y_train.min(), y_train.max()
            # broadcast = lambda x:np.full(y_pred.shape[0], x.to_numpy())
            # min_limit, max_limit= map(broadcast, [min_limit, max_limit])
            # y_pred_clipped = np.clip(y_pred, min_limit, max_limit)
            # 2nd
            y_pred_clipped = y_pred
            if if_log:
                y_pred_clipped = np.exp(y_pred_clipped)
            test_date = df.date[train_end_index]


            from sklearn.metrics import r2_score
            # r2 = r2_score(y_test, y_pred_clipped)
            r2 = r2_score(original_images, y_pred_clipped)
            from sklearn.metrics import mean_squared_error
            mse = mean_squared_error(original_images, y_pred_clipped)


            r2_list.append([test_date,r2])
            mse_list.append([test_date,mse])

        r2arr = np.array(r2_list)
        df = pd.DataFrame(r2arr)
        # r2arr[:,1].mean()
        df.columns = ['test_date','r2']
        df['symbol'] = symbol
        df = df[['symbol','test_date','r2']]
        df.test_date = df.test_date.astype(int)
        pivot_df = df.pivot(index='test_date', columns='symbol', values='r2')
        # dflst.append(pivot_df)

        msearr = np.array(mse_list)
        df2 = pd.DataFrame(msearr)
        df2.columns = ['test_date','mse']
        df2['symbol'] = symbol
        df2 = df2[['symbol','test_date','mse']]
        df2.test_date = df2.test_date.astype(int)
        pivot_df2 = df2.pivot(index='test_date', columns='symbol', values='mse')
        # dflst2.append(pivot_df2)
        print(f">>> i: {i} finished, takes {(time.time()-start)/60:.2f} min")
        return pivot_df, pivot_df2

    process_file(0)



    import time
    import multiprocessing
    # 2. Map the function to the data
    start_ = time.time()
    num_processes = multiprocessing.cpu_count()  # Get the number of available CPU cores
    import os; home = os.path.expanduser("~")
    if home == '/homes/80/kang':
        num_processes = 112

    # import socket
    # time_i = int(socket.gethostname()[-1])
    # time_i = 1

    # if time_i==1:
    #     with multiprocessing.Pool(processes=num_processes) as pool:
    #         results = pool.map(process_file, range(len(path06Files))[:100])
    # elif time_i==2:
    #     with multiprocessing.Pool(processes=num_processes) as pool:
    #         results = pool.map(process_file, range(len(path06Files))[100:200])
    # elif time_i==3:
    #     with multiprocessing.Pool(processes=num_processes) as pool:
    #         results = pool.map(process_file, range(len(path06Files))[200:300])
    # elif time_i==4:
    #     with multiprocessing.Pool(processes=num_processes) as pool:
    #         results = pool.map(process_file, range(len(path06Files))[300:400])
    # elif time_i==5:
    #     with multiprocessing.Pool(processes=num_processes) as pool:
    #         results = pool.map(process_file, range(len(path06Files))[400:])


    with multiprocessing.Pool(processes=num_processes) as pool:
        # results = pool.map(process_file, range(len(path06Files))[400:410])
        # results = pool.map(process_file, range(len(path06Files))[380:402])
        results = pool.map(process_file, range(len(path06Files)))

    end_ = time.time()
    #
    # Post-process the results


    r2_list_combined = []
    mse_list_combined = []
    for r2, mse in results:
        r2_list_combined.append(r2)
        mse_list_combined.append(mse)
    r2df = pd.concat(r2_list_combined,axis=1)
    msedf = pd.concat(mse_list_combined,axis=1)

    print(r2df.mean(axis=1).mean())

    # r2df.to_csv(f"r2df_"+str(time.time())+"_{time_i}.csv")


    #
    print(f"time {(end_ - start_) / 60}")
    #
    #
    #
    # # r2arr = np.array(r2_list)
    # df = pd.DataFrame(r2arr)
    # # r2arr[:,1].mean()
    # df.columns = ['test_date','r2']
    # df['symbol'] = symbol
    # df = df[['symbol','test_date','r2']]
    # df.test_date = df.test_date.astype(int)
    # pivot_df = df.pivot(index='test_date', columns='symbol', values='r2')
    # dflst.append(pivot_df)







    #     msearr = np.array(mse_list)
    #     df2 = pd.DataFrame(msearr)
    #     df2.columns = ['test_date','mse']
    #     df2['symbol'] = symbol
    #     df2 = df2[['symbol','test_date','mse']]
    #     df2.test_date = df2.test_date.astype(int)
    #     pivot_df2 = df2.pivot(index='test_date', columns='symbol', values='mse')
    #     dflst2.append(pivot_df2)
    #
    #
    #
    # r2df = pd.concat(dflst,axis =1)
    # msedf = pd.concat(dflst2,axis =1)
    #
    #
    # import datetime
    # # Get the current date and time
    # current_time = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    # # Construct the filename with the timestamp
    # filename = path00 + "07_r2df_" + regulator + "_" + current_time + ".csv"
    # # Save the DataFrame to the CSV file with the specified filename
    # r2df.to_csv(filename, mode='w')
    #
    # import datetime
    # # Get the current date and time
    # current_time = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    # # Construct the filename with the timestamp
    # filename = path00 + "07_msedf_" + regulator + "_" + current_time + ".csv"
    # # Save the DataFrame to the CSV file with the specified filename
    # r2df = r2df.astype(float)
    # r2df.to_csv(filename, mode='w')
    #
    #
    # r2df.mean(axis=1)
    # r2df.mean(axis=1).mean()
    #
    #





    '''
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
    '''
