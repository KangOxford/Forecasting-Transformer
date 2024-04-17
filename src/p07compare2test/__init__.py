def select_quantile(r2df,quantile):
    '''
    r2df rows are date, cols are stock
    r2df.mean(axis=0) get the mean of each stock,
    '''
    mean_values = r2df.mean(axis=0)
    threshold = mean_values.quantile(quantile)
    selected_r2df = r2df.loc[:, mean_values >= threshold]
    print(quantile, selected_r2df.shape[1]/r2df.shape[1])
    return selected_r2df

# select_quantile(r2df,0.20).mean(axis=1).mean()
