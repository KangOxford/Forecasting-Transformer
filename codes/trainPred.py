from model import *
import pandas as pd
from sklearn.metrics import r2_score

BIN_SIZE = 2
TRAIN_DAYS = 50

def train_and_pred(index,df,config):

    
    
    num,regulator = config["num"],config["regulator"]
    
    

    total_test_days, bin_size, train_size, test_size, x_list, y_list, original_space = param_define(df,num) # here already wrong, need to check the codes
    
    bin_size = BIN_SIZE
    

    def get_X_train_y_train_X_test_original_images(df,num):
        train_start_index = (index * bin_size) * num
        train_end_index = (index * bin_size + train_size) * num
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
        original_images = df.loc[:, original_space].iloc[train_end_index:test_end_index, :]
        return X_train,y_train,X_test,y_test,original_images,train_end_index
    X_train,y_train,X_test,y_test,original_images,train_end_index=get_X_train_y_train_X_test_original_images(df,num)

    # already wrong at this stage, as the X_test is empty


    # breakpoint()
    # print(regulator)
    if regulator == "Inception":
        y_pred = model_nn(X_train, y_train, X_test, y_test, config)
        # y_pred = regularity_nn(X_train, y_train, X_test,y_test, regulator,num)
    else:
        y_pred = regularity_ols(X_train, y_train, X_test, config)
    # print(regulator+"_finished")
    min_limit, max_limit = y_train.min(), y_train.max()
    broadcast = lambda x: np.full(y_pred.shape[0], x.to_numpy())
    min_limit, max_limit = map(broadcast, [min_limit, max_limit])
    y_pred_clipped = np.clip(y_pred, min_limit, max_limit)
    if any('log' in x for x in x_list):
        y_pred_clipped = np.exp(y_pred)
        # y_pred_clipped = np.exp(y_pred_clipped) # TODO caution 
    test_date = df.date.iloc[train_end_index]
    '''prob in the y_pred shapes'''

    # r2 = r2_score(y_test, y_pred_clipped)
    y_pred_clipped = pd.DataFrame(y_pred_clipped)
    y_pred_clipped.columns = ['pred']
    original_images.reset_index(inplace=True, drop=True)
    original_images.columns = ['true']

    original_images['date'] = test_date
    # stock_index = np.tile(tile_array, 26) # original [[bin,483] 26]
    stock_index = np.arange(num).repeat(bin_size)
    original_images['stock_index'] = stock_index
    oneday_df = pd.concat([original_images, y_pred_clipped], axis=1)[['date','stock_index','true','pred']]
    lst = []
    g = oneday_df.groupby(stock_index)
    for stock, item in g:
        pass
        try:
            r2value = r2_score(item['true'], item['pred'])
            lst.append([test_date, stock, r2value])
        except:
            print()
    test_df = pd.DataFrame(lst,columns=["test_date", "stock", "r2value"])
    test_df = test_df.pivot(index='test_date',columns='stock')
    print(test_df)
    from datetime import datetime

    # Get current date and time
    current_time = datetime.now()

    # Format date and time to be used in the file name
    date_str = current_time.strftime("%Y-%m-%d")
    time_str = current_time.strftime("%H")

    # Combine date and time to form the file name
    # Get the short Git hash
    short_hash = config["short_hash"]
    idnetificator = f"_{date_str}_{time_str}_{short_hash}"
    # In subsequent iterations, append without the header
    test_df.to_csv('/homes/80/kang/cmem/'+'data_summary_'+regulator+idnetificator+'.csv', mode='a', header=False, index=True)
    # test_df.to_csv('/homes/80/kang/cmem/'+'data_summary.csv', mode='a', header=False, index=True)
    # print(index,test_date,test_df.r2value.mean())
    oneday_df.to_csv('/homes/80/kang/cmem/'+'data_allValues_'+regulator+idnetificator+'.csv', mode='a', header=False, index=False)
    # oneday_df.to_csv('/homes/80/kang/cmem/'+'data_all_values.csv', mode='a', header=False, index=False)
    return lst,oneday_df



def param_define(df,num):
    # bin_size = 26
    bin_size = BIN_SIZE
    train_days = TRAIN_DAYS
    # bin_size = 2
    # train_days = 20
    # train_days = 50
    # train_days = 50
    # train_days = 20
    # train_days = 10
    train_size = train_days * bin_size
    test_size = 1 * bin_size
    # breakpoint()
    if type(df) == list:
        total_test_days = df[0].shape[0]//bin_size - train_days
    else:
        total_test_days = (df.shape[0]//num - train_size)//bin_size # reached
    # index_max  = int((df.shape[0] -(train_size + test_size))/bin_size)
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
    return total_test_days, bin_size, train_size, test_size, x_list, y_list, original_space



