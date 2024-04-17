import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/codes/")
import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/codes/")

from utils import *
import pandas as pd


def plot_df(df):
    m = df.mean(axis=1)
    s = df.std(axis=1)
    '''
    start plotting
    '''

    a = (m-s).values
    b = m.values
    c = (m+s).values
    mean = b.mean()


    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    from datetime import timedelta, datetime

    # Font size variable
    font = 20

    # Plotting
    plt.figure(figsize=(16, 12))
    # plt.figure(figsize=(12, 8))

    dates = m.index.astype(int)
    x_axis = pd.to_datetime(dates, format='%Y%m%d')
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%m/%d/%Y'))
    plt.xticks(rotation=30)  # Rotate x-axis labels by 45 degrees

    # plot first group with shadow
    plt.plot(x_axis, b, label='Mean of R Squared', color='blue')
    plt.fill_between(x_axis, a, c, color='blue', alpha=0.1)
    plt.axhline(mean, color='red', linestyle='-', label='Mean across all dates')
    plt.text(x_axis[-1]+timedelta(days=5), mean + 0.01, f"{mean:.2f}", verticalalignment='bottom', horizontalalignment='right', color='red', fontsize=font*1.2)

    # Adjusting font sizes with the font variable
    plt.xlabel("Date", fontsize=font*1.2)
    plt.ylabel("Out of sample R squared", fontsize=font*1.2)
    plt.xticks(fontsize=font*1.2)
    plt.yticks(fontsize=font*1.2)
    plt.legend(fontsize=font*1.2)

    plt.grid(True)

    # Save the figure with the generated filename
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"plot_{timestamp}.png"
    plt.savefig(path00+filename, dpi=300, format='png')
    # filename = f"plot_{timestamp}.pdf"
    # plt.savefig(path00+filename, dpi=1200, bbox_inches='tight', format='pdf')
    print(path00+filename)
    plt.show()



'''
end   plotting
'''


if __name__=="__main__":
    # df = pd.read_csv("/homes/80/kang/cmem/08_r2df_universal_day_483_XGB_0.4997.csv",index_col=0)
    df = pd.read_csv("/homes/80/kang/cmem/08_r2df_universal_day_483_XGB_0.4946.csv",index_col=0)
    plot_df(df)
    
    
    