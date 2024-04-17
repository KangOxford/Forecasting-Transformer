import pandas as pd
import numpy as np
from tqdm import tqdm
import warnings;

warnings.simplefilter("ignore", category=FutureWarning)
from os import listdir;
from os.path import isfile, join
from r_output import Config
from sklearn.metrics import r2_score

pd.set_option('display.max_columns', None)


def platform():
    import pandas as pd
    import numpy as np
    from tqdm import tqdm
    import warnings;
    warnings.simplefilter("ignore", category=FutureWarning)
    from os import listdir;
    from os.path import isfile, join
    from data import Config

    pd.set_option('display.max_columns', None)

    import platform  # Check the system platform
    if platform.system() == 'Darwin':
        print("Running on MacOS")
        data_path = "/r_output/04_r_output_raw_data_10/"
    elif platform.system() == 'Linux':
        print("Running on Linux")
        raise NotImplementedError
    else:
        print("Unknown operating system")

    onlyfiles = sorted([f for f in listdir(data_path) if isfile(join(data_path, f))])
    return data_path, onlyfiles

data_path, onlyfiles = platform()

for i in range(len(onlyfiles)):
    df = pd.read_csv(data_path + onlyfiles[i])




a = "/Users/kang/CMEM/data/02_r_input"
b = "/Users/kang/CMEM/r_output/04_r_output_raw_data"
c = '/Users/kang/CMEM/data/02_r_input_remained'
from os import listdir;from os.path import isfile, join
onlyfiles = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f))])
a1 = onlyfiles(a)
a2 = [file[:-4] for file in a1]
a2
b1 = onlyfiles(b)
b2 = [file[10:-4] for file in b1]
b2
c2 = [file  for file in a2 if file not in b2]
c1 = [file+".txt" for file in c2]
c1
# for file name in list c1 copy the file in dir a into dir c

import os
import shutil
for file_name in c1:
    try:
        src_file_path = os.path.join(a, file_name)
        dest_file_path = os.path.join(c, file_name)
        shutil.copy(src_file_path, dest_file_path)
    except:
        print(f"{file_name} not found")




import pandas as pd
a = "/home/kanli/cmem/r_output/04_1_rOuputFeatured_100/forecasts_A.csv"
a1 = pd.read_csv(a)
a1
b="/home/kanli/cmem/data/01.1_raw/A.pkl"
b1 = pd.read_pickle(b)
c = "/home/kanli/volume/02_raw_component/A.pkl"
c1 = pd.read_pickle(c)
c1

import pandas as pd
a ="/homes/80/kang/cmem/07_2_kmeans_day_compare=>test.py_1690267558_.csv"
a1 = pd.read_csv(a)







import pandas as pd
a ="/Users/kang/CMEM/07_2_kmeans_day_compare=>test.py_1690267558_.csv"
a ="/Users/kang/CMEM/xgb_07_2_kmeans_day_compare_test.py_10_20170721_20170803_1690272692_.csv"
a1 = pd.read_csv(a,index_col=0)
a1
a1.mean(axis=1)


b ="/Users/kang/CMEM/xgb_07_2_kmeans_day_compare_test.py_10_20170804_20170817_1690272801_.csv"
b1 = pd.read_csv(b,index_col=0)
a1
a1.mean(axis=1)


c ="/Users/kang/CMEM/xgb_07_2_kmeans_day_compare_test.py_10_20170818_20170831_1690272793_.csv"
c1 = pd.read_csv(c,index_col=0)
a1
a1.mean(axis=1)



df = pd.concat([a1,b1,c1])
df
df3 = df
df3
mean_per_stock = df3.mean(axis=0)
num_top_stocks = int(len(mean_per_stock) * 0.5)
top_performing_stocks = mean_per_stock.nlargest(num_top_stocks)
top_performing_stocks.mean()




# Assuming you have already calculated the mean_per_stock using df3.mean(axis=0)
mean_per_stock = df3.mean(axis=0)

# Calculate the number of bottom-performing stocks you want (e.g., bottom 50%)
num_bottom_stocks = int(len(mean_per_stock) * 0.5)

# Get the bottom-performing stocks based on the mean values
bottom_performing_stocks = mean_per_stock.nsmallest(num_bottom_stocks)

# Calculate the mean of the bottom-performing stocks
bottom_performing_stocks_mean = bottom_performing_stocks.mean()

print(bottom_performing_stocks_mean)



path00 = '/home/kanli/cmem/'
import pandas as pd
import numpy as np
from os import listdir;from os.path import isfile,join
files = lambda path: sorted([f for f in listdir(path) if isfile(join(path,f)) and f != '.DS_Store'])
path21='/home/kanli/cmem/r_output/0400_r_kl_output_raw_data/'
files21 = files(path21)
i=0
print(len(files21))
r2lst = []
for i in range(len(files21)):
    df = pd.read_csv(path21+files21[i])
    r2=df.r2.mean()
    r2lst.append(r2)
r2arr= np.array(r2lst)
r2arr.mean()




path00 = '/home/kanli/cmem/'
import pandas as pd; import numpy as np
from os import listdir;from os.path import isfile,join
files = lambda path: sorted([f for f in listdir(path) if isfile(join(path,f)) and f != '.DS_Store'])
path21='/home/kanli/cmem/r_output/0400_r_kl_output_raw_data/'
files21 = files(path21)
path22='/home/kanli/cmem/data/02.2_data_r_input_kf/'
files22 = files(path22)
files23 = [file for file in files22 if file not in files21]
len(files23)
assert len(files23)+len(files21)==len(files22)

path23=path22[:-2]+"_remained/"
path23
import os
os.system(f'mkdir {path23}')


import shutil
for file in files23:
    source_file = os.path.join(path22,file)
    destination_file = os.path.join(path23, file)
    shutil.copy(source_file, destination_file)



# len(files(path23))



import pandas as pd
b ="/home/kanli/cmem/r_output/06_r_output_raw_pkl/A.pkl"
b1 = pd.read_pickle(b)
b1




df3 = df2
df3
mean_per_stock = df3.mean(axis=0)
num_top_stocks = int(len(mean_per_stock) * 0.5)
top_performing_stocks = mean_per_stock.nlargest(num_top_stocks)
top_performing_stocks.mean()
