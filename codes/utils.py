import os
home_path = "/homes/80/kang/"
# home_path = os.path.expanduser('~') + "/"
path00 = home_path + "cmem/"
path01 = path00 + "data/01_raw/"
path01_1=path00 + "data/01.1_raw/"
path02 = path00 + "data/02_r_input/"
path0200 = path00 + "data/02.2_data_r_input_kf/"
path04 = path00 + "r_output/04_r_output_raw_data/"
path04_1=path00 + "r_output/04_1_rOuputFeatured/"
path0400  =path00 + "r_output/0400_r_kl_output_raw_data/"
path0400_1=path00 + "r_output/0400_1_rOuputFeatured/"
path04_2=path00+"02_raw_component/"
path0400_2=path00+"02_raw_component/"
# path04_2="/home/kanli/seventh/02_raw_component/"
path05 = path00 + "r_output/05_r_output_raw_pkl/"
path0500 = path00 + "r_output/0500_r_output_raw_pkl/"
path06 = path00 + "r_output/06_r_output_raw_pkl/"
path0600 = path00 + "r_output/0600_r_output_raw_pkl/"
path060000 = path00 + "output/06_r_output_raw_csv/"

path060000_fractional_shares = path00 + "output/0600_r_output_with_features_csv_fractional_shares_clipped/"
path0600_1 = path00+'output/0600_1_r_output_raw_csv/'
path0600_1_22 = path00+'output/0600_1_22_r_output_raw_csv/'

path0700 = path00+"output/0700_universal_fractional_shares_clipped/"
path0701 = path00+"output/0701_one_file_fractional_shares_clipped/"
path0702 = path00+"output/0702_single_fractional_shares_clipped/" 
# path0700 = path00+"output/0700_universal/"
# path0701 = path00+"output/0701_one_file/"
# path0702 = path00+"output/0702_single/" 

# NOTE path060000_fractional_shares should be the latest output file 20th Nov, but only 100 stocks are computed


'''used as the input of the get_results 7=>8'''

''''
still confused about what is the difference between path0600_1 and path0600_1_22
path060000: read pkl data from path06 and save into csv

'''
import numpy as np
import pandas as pd
from os import listdir;
import time
from os.path import isfile, join;

import os
os.sys.path.append("/home/kanli/cmem/src/")
os.sys.path.append("/Users/kang/CMEM/src/")
os.sys.path.append("/homes/80/kang/cmem/src/")



def tryMkdir(path):
    try: listdir(path)
    except:import os;os.mkdir(path)
    return 0


readFromPath = lambda data_path: sorted([f for f in listdir(data_path) if isfile(join(data_path, f)) and f != '.DS_Store'])
# path01Files, path01_1Files, path02Files, path04Files, path0500Files, path0600Files =\
#     map(readFromPath, [path01, path01_1, path02, path04, path0500, path0600])


