
path11 = '/home/kanli/cmem/data/01.1_raw_fraction/'
path12 = '/home/kanli/cmem/data/02.2_data_r_input_kf/'
# path11 = '/home/kanli/cmem/data/01.1_raw/'
# path12 = '/home/kanli/cmem/data/02.2_data_r_input_kf/'
# path11 = '/Users/kang/CMEM/data/01.1_raw/'
# path12 = '/Users/kang/CMEM/data/02.2_data_r_input_kf/'
import pandas as pd
import numpy as np
from os import listdir;from os.path import isfile,join
files = lambda path: sorted([f for f in listdir(path) if isfile(join(path,f)) and f != '.DS_Store'])
files11 = files(path11)

for i in range(len(files11)):
    name = files11[i][:-4]
    df = pd.read_pickle(path11+files11[i])
    resampled_df = df[['date','timeHMs','qty_notional']]
    # resampled_df = df[['date','timeHMs','qty']]

    # Step 2: Pivot the DataFrame to get the desired format
    pivot_df = resampled_df.pivot_table(index=resampled_df.index.time, columns=resampled_df.index.date, values='qty')
    # Step 3: Create a new DataFrame with formatted column names
    new_columns = [col.strftime('%Y-%m-%d') for col in pivot_df.columns]
    pivot_df.columns = new_columns

    # Step 4: Add the 'AM' or 'PM' label to the index
    pivot_df.index = [f'{t.strftime("%I:%M %p")}' for t in pivot_df.index]

    # Step 5: Save the DataFrame to a CSV file
    pivot_df.to_csv(path12 + name +".csv")

    # Optionally, display the DataFrame
    # print(pivot_df)

    # '/Users/kang/CMEM/data_for_r.csv'
