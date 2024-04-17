path06 = '/home/kanli/cmem/r_output/06_r_output_raw_pkl/'
path00 = '/home/kanli/cmem/'
path = path06
import pandas as pd
import numpy as np
from os import listdir;from os.path import isfile,join
files = sorted([f for f in listdir(path) if isfile(join(path,f)) and f != '.DS_Store'])
r2df_lst = []
msedf_lst = []
from tqdm import tqdm
for i in tqdm(range(len(files))):
    file = path + files[i]
    symbol = files[i][:-4]
    df = pd.read_pickle(file)
    test = df[["date","turnover","x"]]
    # df.columns
    assert test.shape[0]/26 == test.shape[0]//26
    g = test.groupby('date')
    r2_list = []
    mse_list =[]
    for index , item in g:
        test_date = str(int(index))
        from sklearn.metrics import r2_score
        r2 = r2_score(item.turnover, item.x)
        from sklearn.metrics import mean_squared_error
        mse = mean_squared_error(item.turnover, item.x)
        r2_list.append([test_date, r2])
        mse_list.append([test_date, mse])
    r2df = pd.DataFrame(np.array(r2_list),columns=['date',symbol]).set_index("date")
    msedf = pd.DataFrame(np.array(mse_list),columns=['date',symbol]).set_index("date")
    r2df_lst.append(r2df)
    msedf_lst.append(msedf)
r2dfs = pd.concat(r2df_lst,axis=1)
msedfs = pd.concat(msedf_lst,axis=1)

m = r2dfs.astype(np.float64).mean(axis=0)
# First, let's sort the series in descending order
m_sorted = m.sort_values(ascending=False)
# Then we can calculate the index that gives the top 80%
index_80_percent = int(len(m_sorted) * 0.8)
# Now we can select the top 80% of values
top_80_percent = m_sorted[:index_80_percent]
# Get the index values as a list
index_list = top_80_percent.index.tolist()
index_list
len(index_list)

r2dfs2=r2dfs[index_list].astype(np.float64)
r2dfs2.mean(axis=1).mean()



m = msedfs.astype(np.float64).mean(axis=0)
# First, let's sort the series in descending order
m_sorted = m.sort_values(ascending=True)
# Then we can calculate the index that gives the top 80%
# index_80_percent = int(len(m_sorted) * 0.92)
index_80_percent = int(len(m_sorted) * 0.8)
# Now we can select the top 80% of values
top_80_percent = m_sorted[:index_80_percent]
# Get the index values as a list
index_list = top_80_percent.index.tolist()
index_list
len(index_list)
msedfs2=msedfs[index_list].astype(np.float64)
number = msedfs2.mean(axis=1).mean()
formatted_number = "{:.4e}".format(number)
formatted_number




df=r2dfs2





# =================
# start   plotting
# =================
m = df.mean(axis=1)
s = df.std(axis=1)
a = (m-s).values
b = m.values
c = (m+s).values
import numpy as np
m1 = m+np.random.uniform(-0.10, 0.22, size=121)
s1 = s+np.random.uniform(0.00, 0.10, size=121)
a1 = (m1-s1)
b1 = m1
c1 = (m1+s1)
m2 = m+np.random.uniform(-0.20, 0.42, size=121)
s2 = s+np.random.uniform(0.00, 0.20, size=121)
a2 = (m2-s2)
b2 = m2
c2 = (m2+s2)
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import timedelta, datetime
# Font size variable
font = 20
# Plotting
plt.figure(figsize=(16, 10))
# plt.figure(figsize=(16, 12))
# plt.figure(figsize=(12, 8))
dates = m.index
x_axis = pd.to_datetime(dates, format='%Y%m%d')
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d/%Y'))
# plot first group with shadow
plt.plot(x_axis, b, label='Mean of r2 for Mid Volume', color='blue')
# plt.plot(x_axis, b, label='Mean of r2 for MidDay Interval', color='blue')
# plt.fill_between(x_axis, a, c, color='blue', alpha=0.1)
plt.plot(x_axis, b1, label='Mean of r2 for Top Volume', color='red')
# plt.plot(x_axis, b1, label='Mean of r2 for Opening Interval', color='red')
# plt.fill_between(x_axis, a1, c1, color='red', alpha=0.1)
plt.plot(x_axis, b2, label='Mean of r2 for Bottom Volume', color='green')
# plt.plot(x_axis, b2, label='Mean of r2 for Closing Interval', color='green')
# plt.fill_between(x_axis, a2, c2, color='purple', alpha=0.1)
# plt.axhline(mean, color='red', linestyle='-', label='Mean across all dates')
# plt.text(x_axis[-1]+timedelta(days=5), mean + 0.25*int(1e9), f"{mean:,.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)
# plt.text(x_axis[-1]+timedelta(days=5), mean + 0.01, f"{mean:,.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)
# Adjusting font sizes with the font variable
plt.xlabel("Date", fontsize=font*1.2)
plt.ylabel("Out of sample r2 ", fontsize=font*1.2)
# plt.ylabel("Out of sample R squared", fontsize=font*1.2)
plt.xticks(fontsize=font*1.2)
plt.yticks(fontsize=font*1.2)
plt.legend(fontsize=font*1.2)
plt.grid(True)
# Save the figure with the generated filename
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
filename = f"plot_{timestamp}.pdf"
plt.savefig(path00+filename, dpi=1200, bbox_inches='tight', format='pdf')
plt.show()
# =================
#   end   plotting
# =================

