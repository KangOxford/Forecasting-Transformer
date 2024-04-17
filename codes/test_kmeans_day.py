from tqdm import tqdm
import numpy as np
import pandas as pd
from os import listdir;
from os.path import isfile, join;
from sklearn.metrics import r2_score

import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/codes/")
import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/codes/")
from utils import *
from model import *
from params import *
from kmeans import *
from universal import *
from trainPred import *
import multiprocessing
import time

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path0600_1Flies =readFromPath(path0600_1)


def return_lst(list_, index,regulator):
    # print(f"\n+++@ return_lst() called\n")
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
    result=train_and_pred(index,df,num,regulator,tile_array=list_)
    return result

def process_data(date_index,regulator):
    print(f"+++ index, {date_index}")


    # def classifyStocks(features):
    train_start_Index = (date_index * bin_size ) # for classification of stocks
    train_end_Index = (date_index * bin_size + train_size)  # for classification

    corr_matrix = get_corr_matrix(train_start_Index, train_end_Index, features)
    labels =get_labels_byPCA(corr_matrix,ratio_cumsum,n_components=len(path0600_1Flies),n_clusters=n_clusters)


    v = pd.DataFrame({"a":labels,"b": np.arange(len(path0600_1Flies))})
    g =v.groupby("a")
    lst2 = []
    for i1,item in g:
        lst2.append([i1,item.b.values])

    sub_r2_list = []
    # index2, list_ =  lst2[0]
    for i2, list_ in lst2:
        lst = return_lst(list_, date_index,regulator)
        sub_r2_list+=lst

    return sub_r2_list

def multiprocessing():
    import multiprocessing
    num_processes = multiprocessing.cpu_count()  # Get the number of available CPU cores
    import os; home = os.path.expanduser("~")
    if home == '/homes/80/kang':
        num_processes = 112
    with multiprocessing.Pool(processes=num_processes) as pool:
        results = pool.map(process_data,range(total_test_days))


def get_r2df(num,n_clusters,ratio_cumsum,regulator):

    start = time.time()
    results = []
    for i in range(total_test_days):
        results.append(process_data(i,regulator))
    end = time.time()
    print(f"time {(end-start)/60}")


    r2arr = np.array(results).reshape(-1,3)
    df1 = pd.DataFrame(r2arr)
    df1.columns = ['test_date','stock_index','r2']
    assert np.unique(df1.stock_index).shape == (len(path0600_1Flies),)
    df2 = df1.pivot(index="test_date",columns="stock_index",values="r2")
    return df2

if __name__ == '__main__':
    import time
    from tqdm import tqdm
    # regulator = "OLS"
    regulator = "XGB"
    # n_clusters = 2
    # n_clusters = 5
    n_clusters = 10
    # n_clusters = 20
    # n_clusters = 50


    ratio_cumsum = 0.80
    # ratio_cumsum = 0.99
    # ratio_cumsum = 0.9999
    # ratio_cumsum = 1.00
    
    new_dflst_lst,dflst = get_df_list(start_index=0, num = len(path0600_1Flies))
    total_num_stocks = len(new_dflst_lst)
    total_test_days, bin_size, train_size, test_size, x_list, y_list, original_space = param_define(new_dflst_lst,total_num_stocks)

    # one_stock_shape = 3146
    one_stock_shape = 109*26
    
    features = get_features(new_dflst_lst,x_list,type="volume")
    # features = get_features(new_dflst_lst,type="features")
    print(features.shape)
    
    df2 = get_r2df(num=len(path0600_1Files),n_clusters=n_clusters,ratio_cumsum=ratio_cumsum,regulator=regulator)
    

    print(df2)
    df2finalr2 = str(df2.mean(axis=1).mean())[:6]
    df2.to_csv("n_clusters_"+str(n_clusters)+"_"+df2finalr2+"_.csv")
    df2.mean(axis=0) # stock
    df2.mean(axis=1) # date
    print(df2.mean(axis=1).mean())

    filename = "07_2_kmeans_day_compare_test.py_"+str(len(df2.index))+'_'+str(int(df2.index[0]))+"_"+str(int(df2.index[-1]))+"_"+str(int(time.time()))+"_.csv"
    df2.to_csv(filename)

