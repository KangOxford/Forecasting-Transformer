from tqdm import tqdm
import numpy as np
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
# from params import *
from trainPred import *
from get_results import get_r2df
import multiprocessing
import time

path060000Files = readFromPath(path060000)
print(len(path060000Files))


def getUniversalDf():
    # df = pd.read_csv(path0700+"universal.csv",index_col=0)
    return pd.read_pickle(path0700+"universal.pkl")
    # return pd.read_pickle(path0701+"one_file.pkl")

def print_mean(df3):
    print(f">>>> stock mean: \n",df3.mean(axis=0))  # stock
    print(f">>>> date mean: \n",df3.mean(axis=1))   # date
    print(f">>>> aggregate mean: \n",df3.mean(axis=1).mean())

if __name__=="__main__":
    regulator = "Lasso"
    # regulator = "XGB"

    # regulator = "cnnLstm"
    # regulator = "CNN"
    # regulator = "OLS"
    # regulator = "Ridge"
    # regulator = "None"
    
    # df3 = get_r2df(num=1,regulator=regulator)
    num_of_stocks = len(path060000Files)
    df3,df33 = get_r2df(num=num_of_stocks,regulator=regulator,df = getUniversalDf())




    total_r2 = df3.mean(axis=1).mean()
    print('total r2: ',df3.mean(axis=1).mean()) # all mean
    df3.to_csv(path00 + "08_r2df_universal_day_"+str(num_of_stocks)+"_"+regulator+"_"+str(total_r2)[:6]+".csv", mode = 'w')
    df33.to_csv(path00 + "08_r2df_universal_day_"+str(num_of_stocks)+"_"+regulator+"_"+str(total_r2)[:6]+'_values_'+".csv", mode = 'w')
    
    