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

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
path0400_1Files, path0500Files = map(readFromPath, [path0400_1, path0500])


# r2 check
def r2_check_0400_1():
    lst = []
    i=0
    for i in tqdm(range(len(path0400_1Files))):
        item = pd.read_csv(path0400_1 + path0400_1Files[i], index_col=0)
        item['qty_shift1']=item.qty.shift(-1)
        item[['qty_shift1','x']]
        import numpy as np
        item['date'] = np.array([[i]*26 for i in range(item.shape[0]//26)]).reshape(-1)
        itm = item[:-26]
        g = itm.groupby("date")
        for index, it in g:
            pass
            true = it.qty_shift1
            pred = it.x
            from sklearn.metrics import r2_score
            r2 = r2_score(true, pred)
            lst.append([index, path0400_1Files[i][:-4],r2])

    df = pd.DataFrame(lst,columns=['date','stock','r2'])
    dff=df.pivot(index = 'date',columns='stock')
    r2 = dff.mean(axis=1).mean()
    print(f"the kf-cmem have a oos r2:{r2}")
    # assert r2 >= 0.45, f"the kf-cmem should have a oos r2:{r2} over 0.45"
# r2_check_0400_1()


from tqdm import tqdm
i=0
for i in tqdm(range(len(path0400_1Files))):
    item = pd.read_csv(path0400_1+path0400_1Files[i],index_col=0)
    item.to_pickle(path0500 + path0400_1Files[i][:-3] +"pkl")
    # assert item.shape[0] == 109*26


def r2_check_(path,pathFiles):
    lst = []
    for i in tqdm(range(len(path0500Files))):
        item = pd.read_pickle(path0500 + path0400_1Files[i][:-3] +"pkl")
        item['log_qty_shift1']=item.log_qty.shift(-1)
        import numpy as np
        item['date'] = np.array([[i]*26 for i in range(item.shape[0]//26)]).reshape(-1)
        itm = item[:-26]
        g = itm.groupby("date")
        for index, it in g:
            pass
            true = it.log_qty_shift1.apply(np.exp)
            pred = it.log_x.apply(np.exp)
            from sklearn.metrics import r2_score
            r2 = r2_score(true, pred)
            lst.append([index, path0400_1Files[i][:-4],r2])

    df = pd.DataFrame(lst,columns=['date','stock','r2'])
    dff=df.pivot(index = 'date',columns='stock')
    r2 = dff.mean(axis=1).mean()
    print(f"the kf-cmem have a oos r2:{r2}")
    # assert r2 >= 0.45, f"the kf-cmem should have a oos r2:{r2} over 0.45"

r2_check_(path0500,path0500Files)








