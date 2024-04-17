from datetime import datetime, timedelta
from datetime import timedelta, date

def generate_option_expire_date(year):
    lst = []
    for month in range(1,13):
        exp_date = datetime(year, month, 15)
        wkday = exp_date.weekday()

        if wkday == 4:
            exp_date = exp_date  
        elif wkday > 4: 
            exp_date = exp_date + timedelta(days=(11 - wkday))
        else:
            exp_date = exp_date + timedelta(days=(21 - wkday))

        # print(exp_date.strftime('%Y%m%d'))
        lst.append(exp_date.strftime('%Y%m%d'))
    return lst

def generate_black_friday_date(year):
    # Finding the fourth Thursday of November
    thanksgiving = date(year, 11, 1)
    while thanksgiving.weekday() != 3: # Thursday
        thanksgiving += timedelta(days=1)
    thanksgiving += timedelta(weeks=3)
    # Black Friday is the day after Thanksgiving
    black_friday_date = thanksgiving + timedelta(days=1)
    return [thanksgiving.strftime("%Y%m%d"),black_friday_date.strftime("%Y%m%d")]

def generate_end_of_months_date(year):
    lst = []
    for month in range(1, 13):
        if month == 12:
            next_month_first_date = date(year + 1, 1, 1)
        else:
            next_month_first_date = date(year, month + 1, 1)
        last_date_of_month = next_month_first_date - timedelta(days=1)
        # If the last day of the month is a weekend, go back to the nearest weekday
        while last_date_of_month.weekday() >= 5:
            last_date_of_month -= timedelta(days=1)
        lst.append(last_date_of_month.strftime('%Y%m%d'))
    return lst

def generate_russell_rebalance_date(year):
    if year==2017:
        dates = ['20170512', '20170609', '20170612', '20170616', '20170623']

def generate_triple_witching_date(year):
    '''
    Third Friday of March
    Third Friday of June
    Third Friday of September
    Third Friday of December
    '''
    lst = []
    for month in [3, 6, 9, 12]:
        third_friday = datetime(year, month, 1)
        # Find the first Friday of the month
        while third_friday.weekday() != 4:
            third_friday += timedelta(days=1)
        # Add two weeks to find the third Friday
        third_friday += timedelta(weeks=2)
        lst.append(third_friday.strftime('%Y%m%d'))
    return lst

def generate_unusual_date(year):
    option_expire_dates = generate_option_expire_date(year)
    black_friday_dates = generate_black_friday_date(year)
    end_of_months_dates = generate_end_of_months_date(year)
    triple_witching_dates = generate_triple_witching_date(year)
    d = sorted(list({
        *option_expire_dates,
        *black_friday_dates,
        *end_of_months_dates,
        *triple_witching_dates
        }))
    d_int = [int(date) for date in d]
    return d_int

if __name__=="__main__":
    year = 2017
    d = generate_unusual_date(year)
    print(d)
  
  