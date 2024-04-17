import numpy as np
from tqdm import tqdm
import pandas as pd
from os import listdir;
from os.path import isfile, join;
from sklearn.metrics import r2_score


import numpy as np
import pandas as pd
from tqdm import tqdm
import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/codes/")
import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/codes/")
from utils import *
from model import *
from dates import *
from trainPred import BIN_SIZE, TRAIN_DAYS
import multiprocessing
import time

path060000Files = readFromPath(path060000_fractional_shares)
# path060000Files = readFromPath(path060000)
print(len(path060000Files))

def get_df_list(start_index, num):
    df_lst = []
    new_dflst_lst = []
    
    from tqdm import tqdm
    for i in tqdm(range(start_index, start_index + num)):  # on mac4
        df = pd.read_csv(path060000 + path060000Files[i], index_col=0)
        df_lst.append(df)

    d = generate_unusual_date(year=2017)
    shape_lst = [df.shape[0] for df in df_lst]
    from statistics import mode
    try:
        mode_value = mode(shape_lst)
        print(f"The mode of the list is {mode_value}")
    except:
        print("No unique mode found")
    
    for index, dflst in enumerate(df_lst):
        if dflst.shape[0] == mode_value:
        # if dflst.shape[0] == 2834:
            dflst_filtered = dflst[~dflst['date'].isin(d)]
            new_dflst_lst.append(dflst_filtered)
    '''what is the meaning of dflst.shape to be 3146*109
    109 is the num of features
    dflst is for one single stock
    and across different dates
    26bins*121days==3146rows
    up to here, it is all right'''
    return new_dflst_lst, dflst_filtered



def get_universal_df(start_index, num):
    new_dflst_lst,dflst_filtered = get_df_list(start_index, num)
    gs=[[df for date, df in list(dflst.groupby("date") )] for dflst in new_dflst_lst]
    dff = []
    def truncate_df_wrt_bin_size(dflst_filtered):
        dflst_filtered = pd.concat([itm.iloc[-BIN_SIZE:,:] for idx,itm in dflst_filtered.groupby('date')])
        return dflst_filtered
    dflst_filtered =  truncate_df_wrt_bin_size(dflst_filtered)
    num_days = dflst_filtered.shape[0]//BIN_SIZE
    assert dflst_filtered.shape[0]//BIN_SIZE == dflst_filtered.shape[0]/BIN_SIZE
    # num_days = dflst_filtered.shape[0]//26
    num_stocks = len(new_dflst_lst)
    # for i in tqdm(range(num_days+1)):
    for i in tqdm(range(num_days)):
        for j in range(num_stocks):
            group = gs[j][i].iloc[-BIN_SIZE:,:]    
            dff.append(group)
    df = pd.concat(dff, axis=0)
    df.reset_index(inplace=True, drop=True)
    print(">>> finish preparing the universal df")
    return df


    
        
    # gs = [dflst.iterrows() for dflst in new_dflst_lst]
    # dff = []
    # '''the way of stack is wrong, here it is stacked by perrows/bins.
    # but actually it should be stacked by per day'''
    # for i in tqdm(range(dflst_filtered.shape[0])):
    #     for g in gs:
    #         elem = next(g)[1].T
    #         dff.append(elem)
    # df = pd.concat(dff, axis=1).T
    # df.reset_index(inplace=True, drop=True)
    # print(">>> finish preparing the universal df")
    # return df



# if __name__=="__main__":    
#     df = get_universal_df(start_index=0, num=len(path060000Files))
#     tryMkdir(path0700)
#     df.to_csv(path0700+"universal.csv")
#     df.to_pickle(path0700+"universal.pkl")
def main1():
    df = get_universal_df(start_index=0, num=len(path060000Files))
    # breakpoint()
    print("len(path060000Files):",len(path060000Files))
    tryMkdir(path0701)
    print("path0701:",path0701)
    df.to_csv(path0701+"one_file.csv")
    df.to_pickle(path0701+"one_file.pkl")
def main2():
    dfs,_ = get_df_list(start_index=0, num=len(path060000Files))
    print("len(path060000Files):",len(path060000Files))
    tryMkdir(path0702)
    print("path0702:",path0702)
    # breakpoint()
    # for idx,df in enumerate(dfs):
    for idx, df in tqdm(enumerate(dfs), desc="Processing files", total=len(dfs)):
        df.to_csv(path0702 + path060000Files[idx][:-4] + ".csv")
        df.to_pickle(path0702 + path060000Files[idx][:-4] + ".pkl")


if __name__=="__main__":    
    main1()
    # main2()
    
    
    