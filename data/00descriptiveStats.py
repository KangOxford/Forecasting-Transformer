path = "/home/kanli/cmem/data/01.1_raw/"
# /home/kanli/seventh/02_raw_component/A.pkl
path = "/home/kanli/seventh/02_raw_component/"
path = "/home/kanli/seventh/01_raw/"
import pandas as pd
import numpy as np
from os import listdir;from os.path import isfile,join
files = sorted([f for f in listdir(path) if isfile(join(path,f)) and f != '.DS_Store'])

lst = []
for i in range(len(files)):

    file = path + files[i]
    df = pd.read_pickle(file)
    df
    df['sumOfBuyAndSell'] = df['volBuyQty']+df['volSellQty']

    # g = df.groupby("timeHMs")[['qty']]
    # lst1 = []
    # for index,item in g:
    #     lst1.append(item)
    # pd.concat(lst1,axis=0)
    dff = df[['timeHMs','date','qty']].reset_index(drop=True).pivot(index='timeHMs',columns='date')
    dff.index


    try:
        assert (dff.index == [ 930.0,  945.0, 1000.0, 1015.0, 1030.0, 1045.0, 1100.0, 1115.0,
                  1130.0, 1145.0, 1200.0, 1215.0, 1230.0, 1245.0, 1300.0, 1315.0,
                  1330.0, 1345.0, 1400.0, 1415.0, 1430.0, 1445.0, 1500.0, 1515.0,
                  1530.0, 1545.0]).all()
        f = dff.fillna(method="ffill")
        b = dff.fillna(method="bfill")
        m = (f + b) / 2

        m_normalized = m.div(m.mean())
        # df_normalized = dff.div(dff.mean())
        lst.append(m_normalized)
    except:
        print(f"{files[i]} not found")
        continue

len(lst)

df1 = pd.concat(lst,axis=1)

# Convert the index
df1.index = df1.index.map(lambda x: f"{int(x)//100:02d}:{int(x)%100:02d}")
df1


df_normalized = df1.div(df1.mean())
df_normalized

df_rescaled = df1.div(df1.iloc[0])
df_rescaled


df1 = m_normalized
df2 = m_normalized.iloc[:-1,:]
# df1 = df_rescaled

df1 = df_normalized.iloc[:-1,:]

# %%%%%%%%
m = df1.mean(axis=1)
s = df1.std(axis=1)

import matplotlib.pyplot as plt

# Plotting the mean
plt.plot(m, label='Mean')

# Plotting one standard deviation above and below the mean
plt.fill_between(m.index, m-s, m+s, color='blue', alpha=0.1, label='One Std Deviation')
plt.grid(True, alpha=0.3)
plt.xticks(rotation=45)  # Rotating the x-axis labels
plt.xlabel("Start Time of Each Bin")
plt.ylabel("Normalized Trading Volume")
plt.legend()  # Displaying the legend
plt.tight_layout()
# Save the figure with the generated filename
from datetime import timedelta, datetime
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
filename = f"plot_{timestamp}.pdf"
path00 = "/home/kanli/cmem/"
plt.savefig(path00+filename, dpi=1200, bbox_inches='tight', format='pdf')
plt.show()
# %%%%%%%%



#
#
# # '''
# df3 = df1
# df3.index = df3.index.astype(int).astype(str)
# m = df3.mean(axis=1) # by date
# s = df3.std(axis=1) # by date
# df3.mean(axis=1).mean() # all mean
# # df3.to_csv(path00 + "07_r2df_universal_day_483_"+"lasso"+"_.csv", mode = 'w')
# # start plotting
# a = (m-s).values
# b = m.values
# c = (m+s).values
# mean = b.mean()
# import matplotlib.pyplot as plt
# import matplotlib.dates as mdates
# from datetime import timedelta, datetime
# font = 20# Font size variable
# plt.figure(figsize=(16, 12))# Plotting
# # plt.figure(figsize=(12, 8))
# x_axis = m.index
# # dates = m.index
# # x_axis = pd.to_datetime(dates, format='%Y%m%d')
# # plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d/%Y'))
# # plot first group with shadow
# plot_label = 'Mean of Volume'
# plt.plot(x_axis, b, label=plot_label, color='blue')
# plt.fill_between(x_axis, a, c, color='blue', alpha=0.1)
# plt.axhline(mean, color='red', linestyle='-', label='Mean across all dates')
# plt.text(x_axis[-1], mean + 0.01, f"{mean:,.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)
# # plt.text(x_axis[-1]+timedelta(days=5), mean + 0.01, f"{mean:,.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)
# # Adjusting font sizes with the font variable
# plt.xlabel("Date", fontsize=font*1.2)
# ylabel = "Volume Value"
# plt.ylabel(ylabel, fontsize=font*1.2)
# plt.xticks(fontsize=font*1.2)
# plt.yticks(fontsize=font*1.2)
# plt.legend(fontsize=font*1.2)
# plt.grid(True)
# # Save the figure with the generated filename
# timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
# filename = f"plot_{timestamp}.pdf"
# # plt.savefig(path00+filename, dpi=1200, bbox_inches='tight', format='pdf')
# plt.show()
# # '''
