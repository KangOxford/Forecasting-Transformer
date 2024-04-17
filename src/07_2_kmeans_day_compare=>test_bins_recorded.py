from tqdm import tqdm
import numpy as np
import pandas as pd
from os import listdir;
from os.path import isfile, join;
from sklearn.metrics import r2_score
import sys;sys.path.append("/homes/80/kang/cmem/");sys.path.append("/home/kanli/cmem/");from src.config import *


def tryMkdir(path):
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0
_,_ = map(tryMkdir,[path05,path06])

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path01Files, path01_1Files, path02Files, path04Files, path05Files, path06Files =\
    map(readFromPath, [path01, path01_1, path02, path04, path05, path06])




# n_clusters = 2
# n_clusters = 5
# n_clusters = 10
n_clusters = 20
# n_clusters = 50


ratio_cumsum = 0.80
# ratio_cumsum = 0.99
# ratio_cumsum = 0.9999
# ratio_cumsum = 1.00

# def get_universal(start_index, num_of_stocks, end_index):
    # assert (end_index - start_index) % num_of_stocks == 0
start_index, num_of_stocks = 0, 100
array1 = np.concatenate( [np.arange(1,10,0.01), np.arange(10,50,0.1) ])
array2 = np.arange(1,0.001,-0.001)
combined_array = np.array(list(zip(array1, array2))).flatten()
# used for alphas


# ================================
num = num_of_stocks
# num = 100
# num = 2 # for 2 stocks
# num = 1 # for single stock`
# def get_universal_df():
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


one_stock_shape = 3146
bin_size = 26
train_days = 10
train_size = train_days * 26
test_size = 1 * 26
total_num_stocks = len(new_dflst_lst)
total_test_days = new_dflst_lst[0].shape[0]//bin_size - train_days

# ================================
index_max  = int((df.shape[0] -(train_size + test_size))/bin_size)

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
original_space = ['turnover']
# ================================

def get_features(new_dflst_lst,type="volume"):
    if type =="volume":
        return get_volume_features(new_dflst_lst)
    elif type =="features":
        return get_features_features(new_dflst_lst)
    else:
        raise NotImplementedError

def get_volume_features(new_dflst_lst):
    feature_list = []
    for index, item in enumerate(new_dflst_lst):
        pass
        item.date = item.date.astype(np.int32)
        item = item.set_index('date')
        value = item.turnover
        value.name = path06Files[index][:-4]
        feature_list.append(value)
    features = pd.concat(feature_list,axis = 1)
    return features

def get_features_features(new_dflst_lst):
    nfeatures = []
    for col in x_list:
        feature_list = []
        for index, item in enumerate(new_dflst_lst):
            pass
            item.columns
            item.date = item.date.astype(np.int32)
            item = item.set_index('date')
            value = item.loc[:,col]
            assert item.shape[1] == 108
            value.name = path06Files[index][:-4]
            feature_list.append(value)
        features = pd.concat(feature_list,axis = 1)
        nfeatures.append(features)
    nfeatures = np.stack(nfeatures)
    len(nfeatures.shape)
    nfeatures.shape
    return nfeatures


features = get_features(new_dflst_lst,type="features")
features.shape


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
        import warnings
        from sklearn.exceptions import DataConversionWarning
        warnings.filterwarnings("ignore", category=DataConversionWarning)
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


def return_lst(list_, index):
    print(f"\n+++@ return_lst() called\n")
    groupped_dfs = [new_dflst_lst[i] for i in list_]
    gs = [dflst.iterrows() for dflst in groupped_dfs]
    dff = []
    for i in range(one_stock_shape):
    # for i in tqdm(range(one_stock_shape)):
        for g in gs:
            elem = next(g)[1].T
            dff.append(elem)
    df = pd.concat(dff, axis=1).T
    df.reset_index(inplace=True, drop=True)
    # print(df.shape)

    num = len(groupped_dfs)

    train_start_index = (index * bin_size) * num
    train_end_index = (index * bin_size + train_size) * num
    # print(f"train_end_index: {train_end_index} = (index: {index} * bin_size: {bin_size}+ train_size: {train_size}) * num: {num}")
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

    # regulator = "OLS"
    regulator = "Lasso"
    # regulator = "Ridge"
    # regulator = "None"
    y_pred = regularity_ols(X_train, y_train, X_test, regulator)
    min_limit, max_limit = y_train.min(), y_train.max()
    broadcast = lambda x: np.full(y_pred.shape[0], x.to_numpy())
    min_limit, max_limit = map(broadcast, [min_limit, max_limit])
    y_pred_clipped = np.clip(y_pred, min_limit, max_limit)
    if any('log' in x for x in x_list):
        y_pred_clipped = np.exp(y_pred_clipped)
    test_date = df.date.iloc[train_end_index]
    # print(f"test_date:{test_date} = df: {df.shape}.date[train_end_index: {train_end_index}]")
    # print(df.index)
    # print(df.date.iloc[train_end_index])
    # print(df)

    # r2 = r2_score(y_test, y_pred_clipped)
    y_pred_clipped = pd.DataFrame(y_pred_clipped)
    y_pred_clipped.columns = ['pred']
    original_images.reset_index(inplace=True, drop=True)
    original_images.columns = ['true']

    original_images['date'] = test_date
    stock_index = np.tile(list_, 26)
    # stock_index = np.tile(np.arange(num),26)
    original_images['stock_index'] = stock_index
    oneday_df = pd.concat([original_images, y_pred_clipped], axis=1)
    lst = []
    g = oneday_df.groupby(stock_index)
    for stock, item in g:
        pass
        r2value = r2_score(item['true'], item['pred'])
        lst.append([int(test_date), stock, r2value])
    return lst


date_index =0
def process_data(date_index):
    print(f"index, {date_index}")


    # def classifyStocks(features):
    train_start_Index = (date_index * bin_size ) # for classification of stocks
    train_end_Index = (date_index * bin_size + train_size)  # for classification
    def get_corr_matrix(train_start_Index, train_end_Index, features):
        if len(features.shape) ==2:
            print(f"shape of features: {features.shape}")
            f = features.iloc[train_start_Index:train_end_Index,:]
            fv=f.values
            corr_matrix = np.corrcoef(fv, rowvar=False)
            # Print the shape of the correlation matrix
            print("Shape of correlation matrix:", corr_matrix.shape)
            return corr_matrix
        elif len(features.shape) ==3:
            print(f"shape of features: {features.shape}")
            nfeatures = features
            f = np.array([nfeatures[i,train_start_Index:train_end_Index,:] for i in range(nfeatures.shape[0])])
            f.shape
            ncorr_matrix = np.array([np.corrcoef(fv, rowvar=False) for fv in f])
            ncorr_matrix.shape
            corr_matrix = np.mean(ncorr_matrix,axis=0)
            # Print the shape of the correlation matrix
            # print("Shape of correlation matrix:", corr_matrix.shape)
            return corr_matrix
        else:
            raise NotImplementedError

    corr_matrix = get_corr_matrix(train_start_Index, train_end_Index, features)
    from sklearn.decomposition import PCA
    pca = PCA()
    # Fit PCA on the correlation matrix
    # print(corr_matrix)
    pca.fit(corr_matrix)
    
    # Obtain the principal
    component=pca.components_
    ratio=pca.explained_variance_ratio_
    # pca=PCA(n_components=40)
    # pca=PCA(n_components=100)
    # pca=PCA(n_components=np.argmax(ratio.cumsum() >= 0.9999))
    # pca=PCA(n_components=np.argmax(ratio.cumsum() >= 0.99))
    if ratio_cumsum == 1.00:
        pca = PCA(n_components=100)
        print("n_components 100")
    else:
        pca = PCA(n_components=np.argmax(ratio.cumsum() >= ratio_cumsum))
        print(f"n_components {np.argmax(ratio.cumsum() >= ratio_cumsum)}")
    pca.fit(corr_matrix)
    scores_pca = pca.transform(corr_matrix)

    from sklearn.cluster import KMeans
    kmeans_pca = KMeans(n_clusters=n_clusters, init="k-means++",random_state=42)
    kmeans_pca.fit(scores_pca)
    labels = kmeans_pca.labels_
    assert kmeans_pca.labels_.shape == (100,)


    v = pd.DataFrame({"a":labels,"b": np.arange(100)})
    g =v.groupby("a")
    lst2 = []
    for i1,item in g:
        lst2.append([i1,item.b.values])

    sub_r2_list = []
    # index2, list_ =  lst2[0]
    for i2, list_ in lst2:
        lst = return_lst(list_, date_index)
        sub_r2_list+=lst

    return sub_r2_list


if __name__ == '__main__':
    import multiprocessing
    import time
    num_processes = multiprocessing.cpu_count()  # Get the number of available CPU cores
    import os; home = os.path.expanduser("~")
    if home == '/homes/80/kang':
        num_processes = 80
    from tqdm import tqdm
    results = []


    start = time.time()
    with multiprocessing.Pool(processes=num_processes) as pool:
         results = pool.map(process_data,range(total_test_days))
    end = time.time()
    print(f"time {(end-start)/60}")


    r2arr = np.array(results).reshape(-1,3)
    df1 = pd.DataFrame(r2arr)
    df1.columns = ['test_date','stock_index','r2']
    assert np.unique(df1.stock_index).shape == (100,)
    df2 = df1.pivot(index="test_date",columns="stock_index",values="r2")

    print(df2)
    df2finalr2 = str(df2.mean(axis=1).mean())[:6]
    df2.to_csv("n_clusters_"+str(n_clusters)+"_"+df2finalr2+"_.csv")
    df2.mean(axis=0) # stock
    df2.mean(axis=1) # date
    print(df2.mean(axis=1).mean())


    '''
    start = time.time()
    results = [process_data(i) for i in range(total_test_days)]
    end = time.time()
    print(f"time {(end-start)/60}")


    r2arr = np.array(results).reshape(-1,3)
    df1 = pd.DataFrame(r2arr)
    df1.columns = ['test_date','stock_index','r2']
    assert np.unique(df1.stock_index).shape == (100,)
    df2 = df1.pivot(index="test_date",columns="stock_index",values="r2")

    print(df2)
    df2finalr2 = str(df2.mean(axis=1).mean())[:6]
    df2.to_csv("n_clusters_"+str(n_clusters)+"_"+df2finalr2+"_.csv")
    df2.mean(axis=0) # stock
    df2.mean(axis=1) # date
    print(df2.mean(axis=1).mean())
    '''
