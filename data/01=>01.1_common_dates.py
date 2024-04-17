import numpy as np

import os; os.sys.path.append("/home/kanli/cmem/")

path00 = "/home/kanli/cmem/"
path01 = "/home/kanli/cmem/data/01_raw/"

path01_1 = "/home/kanli/cmem/data/01.1_raw/"
path02 = "/home/kanli/cmem/data/02_r_input/"
path04 = "/home/kanli/cmem/r_output/04_r_output_raw_data/"
path04_1 = "/home/kanli/cmem/r_output/04_1_rOuputFeatured/"
path05 = "/home/kanli/cmem/r_output/05_r_output_raw_pkl/"
path06 = '/home/kanli/cmem/r_output/06_r_output_raw_pkl/'

# path00 = "/Users/kang/CMEM/"
# path01 = "/Users/kang/CMEM/data/01_raw/"
# path01_1 = "/Users/kang/CMEM/data/01.1_raw/"
# path02 = "/Users/kang/CMEM/data/02_r_input/"
# path04 = "/Users/kang/CMEM/r_output/04_r_output_raw_data/"
# path04_1 = "/Users/kang/CMEM/r_output/04_1_rOuputFeatured/"
# path05 = "/Users/kang/CMEM/r_output/05_r_output_raw_pkl/"
# path06 = '/Users/kang/CMEM/r_output/06_r_output_raw_pkl/'


from os import listdir;
from os.path import isfile, join;
import pandas as pd

readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f))])
path01Files, path02Files = map(readFromPath, [path01, path02])
date_list = []
for i in range(len(path01Files)):
    item = pd.read_pickle(path01+path01Files[i])
    g = item.groupby("date")
    lst = []
    for index, df in g:
        print(index, df.shape[0])
        if df.shape[0] == 26:
            lst.append(index)
    date_list.append(lst)
#import numpy as np
# np.array(date_list)


common_elements = lambda nested_list: sorted(list(set(nested_list[0]).intersection(*nested_list[1:])))
common_dates = common_elements(date_list)

np.save(path00 + 'common_dates.npy', common_dates)

def tryMkdir(path):
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0
_,_ = map(tryMkdir,[path01,path01_1])

for i in range(len(path01Files)):
    print(f">>> {i} th")
    item = pd.read_pickle(path01 + path01Files[i])
    new_item = item[item['date'].isin(common_dates)]
    name = path01Files[i]
    new_item.to_pickle(path01_1+name)


