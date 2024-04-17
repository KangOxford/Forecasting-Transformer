import os
os.sys.path.append("/home/kanli/cmem/src/")
try: from config import *
except: from src.config import *

def tryMkdir(path):
    from os import listdir
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0
_,_ = map(tryMkdir,[path01,path05])


from os import listdir;
from os.path import isfile, join;
import pandas as pd
import numpy as np


readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
# path04Files, path04_1Files, path05Files = map(readFromPath, [path04, path04_1, path05])
path0400Files, path0400_1Files, path04_2Files = map(readFromPath, [path0400, path0400_1, path04_2])

# common_dates = np.load(path00 + 'common_dates.npy')[1:] # only the r output parts is in need
sample=pd.read_csv("/home/kanli/cmem/data/02_r_input/A.txt",sep="\t")
sample1=sorted(list(set(sample.date)))
common_dates0 = sample1[1:]
common_dates = [str(date) for date in common_dates0]
# len(common_dates)

from tqdm import tqdm
for i in tqdm(range(480,len(path0400Files))):
    df = pd.read_csv(path04 + path0400Files[i],header=None, index_col=0).dropna(axis=1).reset_index(drop=True)
    df = df.apply(abs)
    df.columns = ['eta','seas','mu','x']
    df['eta*seas'] = df['eta'] * df['seas']
    # ============= milestone here ============
    df['log_eta'] = df['eta'].apply(np.log)
    df['log_seas'] = df['seas'].apply(np.log)
    df['log_mu'] = df['mu'].apply(np.log)
    df['log_x'] = df['x'].apply(np.log)
    df['log_eta*seas'] = df['eta*seas'].apply(np.log)
    new_df = df[['log_x','log_eta*seas','log_eta','log_seas','x','eta*seas','log_mu', 'eta','seas','mu']]
    # ============= milestone here ============
    try:
        name = path0400Files[i][10:-4] +".pkl"
        ft = pd.read_pickle(path04_2 + name).reset_index(drop=True)
        # Select rows from the DataFrame based on common dates
        selected_rows = ft[ft['date'].isin(common_dates)].reset_index(drop=True)
    except:
        print(f"no {name} file, continue")
        continue
    try:
        assert len(list(set(selected_rows.date))) == len(common_dates)
        assert selected_rows.shape[0] == 122 * 26 # 3172
    except:
        print(f"{path0400Files[i]} exist error, continue")
        continue
    features = list(selected_rows.columns[5:-2]) + ['qty']
    # features = list(selected_rows.columns[5:-2])
    assert type(features) == list
    for feature in features:
        new_ft = "log_"+feature
        selected_rows[new_ft] = selected_rows[feature].apply(np.log)
    new_features = features + ["log_"+feature for feature in features]
    df_with_newFeatures = selected_rows[new_features]
    merged_df = pd.concat([new_df, df_with_newFeatures],axis = 1)
    # ============= milestone here ============
    merged_df.to_csv(path04_1 + path0400Files[i], mode = 'w+')
