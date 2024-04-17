import time
import torch
import pandas as pd
import numpy as np
array1 = np.concatenate([np.arange(1, 10, 0.01), np.arange(10, 50, 0.1)])
array2 = np.arange(1, 0.001, -0.001)

combined_array = np.array(list(zip(array1, array2))).flatten()

def check_GPU_memory():
    import GPUtil
    # Get the list of GPU devices
    devices = GPUtil.getGPUs()
    # Loop through devices and print their memory usage
    for device in devices:
        print(f"Device: {device.id}, Free Memory: {device.memoryFree}MB, Used Memory: {device.memoryUsed}MB")


# used for alphas
def regularity_ols(X_train, y_train, X_test, config):
    regulator,num = config["regulator"],config["num"]
    if regulator == "CMEM":
        y_pred = X_test['log_x'].to_numpy().flatten()
        # y_pred = X_test['x'].to_numpy().flatten()
        return y_pred
    elif regulator == "OLS":
        # print("OLS")
        import statsmodels.api as sm
        def ols_with_summary(X, y):
            X = sm.add_constant(X, has_constant='add')
            results = sm.OLS(y, X).fit()
            return results

        model = ols_with_summary(X_train, y_train)
        X = sm.add_constant(X_test, has_constant='add')
        y_pred = model.predict(X).values
        # assert type(y_pred) == np.float64
        return y_pred
    elif regulator in ["Lasso", "Ridge"]:
        import warnings
        from sklearn.exceptions import DataConversionWarning

        # Suppress DataConversionWarning globally
        warnings.filterwarnings("ignore", category=DataConversionWarning)

        # Now you can run your code that generates the warning
        # model.fit(X, y)
        # print("LASSO / RIDGE")
        def find_best_regularity_alpha(X_train, y_train):
            if regulator == "Lasso":
                from sklearn.linear_model import LassoCV
                model = LassoCV(random_state=0, max_iter=10000000)
            if regulator == "Ridge":
                from sklearn.linear_model import RidgeCV
                model = RidgeCV(alphas=combined_array)
            model.fit(X_train, y_train)
            return model.alpha_

        best_regularity_alpha = find_best_regularity_alpha(X_train, y_train)
        # print(best_regularity_alpha) #$
        if regulator == "Lasso":
            from sklearn.linear_model import Lasso
            reg = Lasso(alpha=best_regularity_alpha, max_iter=10000000, tol=1e-2)
        if regulator == "Ridge":
            from sklearn.linear_model import Ridge
            reg = Ridge(alpha=best_regularity_alpha, max_iter=10000000, tol=1e-2)
        reg.fit(X_train, y_train)
        
        def feature_importance():
            print(regulator)
            # Get feature names if you have them
            # feature_names = ['Feature1', 'Feature2', ...] or
            feature_names = X_train.columns if isinstance(X_train, pd.DataFrame) else [f'Feature{i}' for i in range(X_train.shape[1])]

            # Get feature importance (absolute value of coefficients for Lasso/Ridge)
            feature_importance = np.abs(reg.coef_)

            # Create a DataFrame for easy visualization
            importance_df = pd.DataFrame({
                'Feature': feature_names,
                'Importance': feature_importance[0]
            })

            # Sort the DataFrame by importance
            importance_df = importance_df.sort_values(by='Importance', ascending=False)

            # Display the feature importance
            print(importance_df)

        # X = pd.DataFrame(X_test).T
        # y_pred = reg.predict(X)
        y_pred = reg.predict(X_test)
        y_pred = y_pred.flatten()
        # print(y_pred.shape)
        # breakpoint()
        return y_pred
    elif regulator == "XGB":
        import xgboost as xgb
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_lambda=0.1)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_lambda=1.0)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_lambda=10)
        
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_alpha=0.1)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_alpha=1.0)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_alpha=10)
        
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160)
        # model = xgb.XGBRegressor(max_depth=6, learning_rate=0.1, n_estimators=160)
        # model = xgb.XGBRegressor(max_depth=7, learning_rate=0.1, n_estimators=160)
        
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=200)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=300)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=400)
        
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.001, n_estimators=160)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.01, n_estimators=160)
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=1.0, n_estimators=160)
        
        
        # model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160,reg_lambda=0.1,reg_alpha=0.1)
        model = xgb.XGBRegressor(max_depth=5, learning_rate=0.1, n_estimators=160)
        

        # print(model.get_params())
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        y_pred = y_pred.flatten()
        return y_pred



        # X_train, y_train, X_test = X_train.to_numpy(), y_train.to_numpy(), X_test.to_numpy()
        # print(X_train.shape,y_train.shape)
        # X_test_new = np.concatenate([X_train[X_test.shape[0]:,:],X_test])
        # '''#TODO slice the last X_test.shape[0] y_pred'''
        # X_train_scaled, y_train_scaled, X_test_scaled, scaler_X, scaler_y = normalize_data(X_train, y_train, X_test_new)
        # device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
        # # Normalize the data
        # # Convert to PyTorch tensors
        # X_train_tensor, y_train_tensor, X_test_tensor = to_torch_tensors(X_train_scaled, y_train_scaled, X_test_scaled, device)
        # num_stock=1;num_feature=52
        # # num_stock=483;num_feature=52 
        # '''CAUTION remeber to pass right value'''
        # X_train_tensor=X_train_tensor.reshape(num_stock,-1,num_feature).unsqueeze(1)
        # y_train_tensor=y_train_tensor.reshape(num_stock,-1,1)
        # X_test_tensor = X_test_tensor.reshape(num_stock,-1,num_feature).unsqueeze(1)
        # # Initialize the model
        # stock_prediction_model = NNPredictionModel(learning_rate=0.0002, epochs=1200, batch_size=483)
        # # Convert the model's parameters to Double
        # stock_prediction_model.model.double().to(device)
        # # Train and predict
        # stock_prediction_model.train(X_train_tensor, y_train_tensor)
        # y_pred_normalized = stock_prediction_model.predict(X_test_tensor)
        # y_pred = y_pred_normalized[:,-26:,:]
        # y_pred_flatten = y_pred.reshape(-1,1)
        # y_pred_flatten = denormalize_predictions(y_pred_flatten.numpy(), scaler_y)
        # '''caution how y_pred is flattened deserves attention!!!'''
        # return y_pred_flatten
    else:
        raise NotImplementedError

def normalize_data(X, y):
    from sklearn.preprocessing import MinMaxScaler
    scaler_X = MinMaxScaler()
    scaler_y = MinMaxScaler()
    X_scaled = scaler_X.fit_transform(X)
    y_scaled = scaler_y.fit_transform(y.reshape(-1, 1))
    return X_scaled, y_scaled, scaler_X, scaler_y


# 130 5
# 1300 50


def regularity_nn(X_train, y_train, X_test,y_test, config):
    pass

def model_nn(X_train, y_train, X_test, y_test, config):
    '''take the first 9 days of X_train as X_train_new
    y_train take the last 1 day of y_train as y_train_new
    one day include 26bins(26rows) of data
    the nn file is also in need of modification to forecast 
    from 1,1,train_days*bin_size,52 X 1, train_days*bin_size,1
    to   1,1,1274,52 X 1,   26,1   
    '''
    regulator = config["regulator"]
    num = config["num"]
    bin_size = config["bin_size"]
    train_days = config["train_days"]
    
    assert regulator == "Inception"
    import torch.nn as nn
    from codes.nn import NNPredictionModel

    def to_torch_tensors(X_train, y_train, device):
        X_train_tensor = torch.tensor(X_train, dtype=torch.float64).to(device)
        y_train_tensor = torch.tensor(y_train, dtype=torch.float64).to(device)
        return X_train_tensor, y_train_tensor
    def denormalize_predictions(y_pred_normalized, scaler_y):
        y_pred_normalized = y_pred_normalized.reshape(-1, 1)
        y_pred = scaler_y.inverse_transform(y_pred_normalized)
        y_pred = y_pred.reshape(-1)
        return y_pred
    
    
    
    def slice_and_stack(X_scaled, y_scaled, num_stock,i):
        '''
        X_scaled is stacked by: 
            26 bins of stock1 of day1
            26 bins of stock2 of day1
            ...
            26 bins of stockn of day1
            
            26 bins of stock1 of day2
            26 bins of stock2 of day2
            ...
            26 bins of stockn of day2

            26 bins of stock1 of daym
            26 bins of stock2 of daym
            ...
            26 bins of stockn of daym
            
        for example, 
        if num_stock == 1:
        X_scaled is stacked by:  
            26 bins of stock1 of day1
            26 bins of stock1 of day2
            ...
            26 bins of stock1 of daym
            X_scaled[i:1300+i, :] means take 1300/26=50 days of bins out.
            if i == 1, then we expect to take
                25 bins of stock1 of day1, start from index 1 (included)
                26 bins of stock1 of day2
                ...
                26 bins of stock1 of day50
                1 bins of stock1 of day50, end to index 1 (not included)
                at this point, the X_test is expected to be one element difference from X_train:
                25 bins of stock1 of day1, start from index 2(1+1, the first 1 is the value of i, the second 1 is fixed) (included)
                26 bins of stock1 of day2
                ...
                26 bins of stock1 of day50
                1 bins of stock1 of day50, end to index 2(1+1) (not included)
                
            
             
        if num_stock == 2:
        X_scaled is stacked by:  
            26 bins of stock1 of day1
            26 bins of stock2 of day1
            26 bins of stock1 of day2
            26 bins of stock2 of day2
            ...
            26 bins of stock1 of daym
            26 bins of stock2 of daym
            if i == 1, then we expect to take
                25 bins of stock1 of day1, start from index 1 (included)
                25 bins of stock2 of day1, start from index 1 (included)
                26 bins of stock1 of day1
                26 bins of stock2 of day1
                ...
                26 bins of stock1 of day50
                26 bins of stock2 of day50
                1 bins of stock1 of day50, end to index 1 (not included)
                1 bins of stock2 of day50, end to index 1 (not included)
                at this point, the X_test is expected to be one element difference from X_train:
                25 bins of stock1 of day1, start from index 2 (included)
                25 bins of stock2 of day1, start from index 2 (included)
                26 bins of stock1 of day1
                26 bins of stock2 of day1
                ...
                26 bins of stock1 of day50
                26 bins of stock2 of day50
                1 bins of stock1 of day50, end to index 2 (not included)
                1 bins of stock2 of day50, end to index 2 (not included)
        ''' 
        if num_stock ==1 :
            X_train_window=X_scaled[i:train_days*bin_size+i, :]
            y_train_window=y_scaled[i:train_days*bin_size+i,:]
            X_test_window=X_scaled[i+1:train_days*bin_size+i+1, :]
            return X_train_window, y_train_window, X_test_window
        else:
            '''It include the situation of num_stock to be 1'''
            num_bins_per_day = bin_size  # Number of bins for each stock each day
            num_days = train_days  # Number of days you want to consider
            # Number of bins for 'num_stock' stocks for 'num_days' days
            total_bins = num_bins_per_day * num_stock * num_days
            
            # Initialize the arrays to hold the training and test data
            X_train_window = np.zeros((total_bins, X_scaled.shape[1]))
            y_train_window = np.zeros((total_bins, y_scaled.shape[1]))
            X_test_window = np.zeros((total_bins, X_scaled.shape[1]))
            
            # Loop to populate the training and test data
            for j in range(num_days):  # For each of the 'num_days' days
                for k in range(num_stock):  # For each stock
                    start_idx = j * (num_bins_per_day * num_stock) + k * num_bins_per_day + i
                    end_idx = start_idx + num_bins_per_day
                    
                    # Corresponding indices in the output arrays
                    out_start_idx = j * (num_bins_per_day * num_stock) + k * num_bins_per_day
                    out_end_idx = out_start_idx + num_bins_per_day
                    
                    # Slice and copy data for training window
                    X_train_window[out_start_idx:out_end_idx, :] = X_scaled[start_idx:end_idx, :]
                    y_train_window[out_start_idx:out_end_idx, :] = y_scaled[start_idx:end_idx, :]
                    
                    # Slice and copy data for test window
                    X_test_window[out_start_idx:out_end_idx, :] = X_scaled[start_idx + 1:end_idx + 1, :]
            
            return X_train_window, y_train_window, X_test_window
        
    def slice_and_stack_batch(X_scaled, y_scaled, num_stock, idx):
        '''idx is not used here'''
        X_train_window, y_train_window, _ = slice_and_stack(X_scaled, y_scaled, num_stock, 0)
        '''idx is used here'''
        _, _, X_test_window = slice_and_stack(X_scaled, y_scaled, num_stock, idx)
        
        num_bins_per_day = bin_size  # Number of bins for each stock each day
        num_days = train_days  # Number of days you want to consider
        # Number of bins for 'num_stock' stocks for 'num_days' days
        total_bins = num_bins_per_day * num_stock * num_days
        '''
        >>> Only one model trained and the X_train is sliding window 
        26 bins of stock1 of day1
        26 bins of stock2 of day1
        26 bins of stock1 of day2
        26 bins of stock2 of day2
        ...
        26 bins of stock1 of day50
        26 bins of stock2 of day50
        
        >>> For the X_test, the sliding window is perfomed as follows
        25 bins of stock1 of day1, start from index 1 (included)
        25 bins of stock2 of day1, start from index 1 (included)
        26 bins of stock1 of day1
        26 bins of stock2 of day1
        ...
        26 bins of stock1 of day50
        26 bins of stock2 of day50
        1 bins of stock1 of day50, end to index 1 (not included)
        1 bins of stock2 of day50, end to index 1 (not included)
        '''
        def reshape_X_2Dinto3D_V2(X,num_days_):
            num_feature = X.shape[1]
            X_3D = np.zeros((num_stock, bin_size * num_days_, num_feature))
            for i in range(num_days_):
                for j in range(num_stock):
                    sliced = X[i*bin_size*num_stock+j*bin_size:i*bin_size*num_stock+(j+1)*bin_size,:]
                    # Insert sliced data into the right position in the big_array
                    X_3D[j, i * bin_size:(i + 1) * bin_size, :] = sliced #(num_stock, bin_size*num_days, num_features)
            return X_3D
        X_train_reshaped = reshape_X_2Dinto3D_V2(X_train_window,num_days_=num_days)
        y_train_reshaped = reshape_X_2Dinto3D_V2(y_train_window,num_days_=num_days)
        def reshape_X_test2Dinto3D_V2(X,idx):
            if idx ==0:
                return reshape_X_2Dinto3D_V2(X,num_days_=num_days)
            else:
                first_index = (bin_size-idx)*num_stock
                end_index = -1*idx*num_stock
                X1 = X[:first_index,:]
                X2 = X[first_index:idx,end_index:]
                X3 = X[end_index:,:]
                X1_3D=reshape_X_2Dinto3D_V2(X1,num_days_=1) #（483,25,52）
                X2_3D=reshape_X_2Dinto3D_V2(X2,num_days_=num_days-2) #（483,1274,52）
                X3_3D=reshape_X_2Dinto3D_V2(X3,num_days_=1) #（483,1,52）
                X_3D = np.concatenate((X1_3D, X2_3D, X3_3D), axis=1) # stack by second dimension:（483,1300,52）
                return X_3D
        X_test_reshaped  = reshape_X_test2Dinto3D_V2(X_test_window,idx) # TODO reamain to be tested
        return X_train_reshaped, y_train_reshaped, X_test_reshaped
    
    # def slice_and_stack_single_distribution(X_scaled, y_scaled, num_stock, i):
        


    def train_and_predict_without_sliding_window(X_scaled, y_scaled, num_stock, num_feature, device):
        '''
        only train one model from 0 to bin_size*train_days
        then predict from bin_size*train_days to bin_size*train_days+bin_size
        '''
        # Get the training data using the slice_and_stack function
        X_train_window, y_train_window, _ = slice_and_stack_batch(X_scaled, y_scaled, num_stock, 0)  # Assuming i=0 as it's a single window
        # X_train_window, y_train_window, _ = slice_and_stack(X_scaled, y_scaled, num_stock, 0)  # Assuming i=0 as it's a single window

        # Convert to Torch tensors and Reshape
        X_train_tensor_window, y_train_tensor_window = to_torch_tensors(X_train_window, y_train_window, device)
        # X_train_tensor_window = X_train_tensor_window.reshape(1, -1, num_feature).unsqueeze(1)
        # y_train_tensor_window = y_train_tensor_window.reshape(1, -1, 1)

        # Train the model with the training data
        stock_prediction_model = NNPredictionModel(num, learning_rate=0.002, epochs=1000, batch_size=483)  # Adjust hyperparameters as needed
        stock_prediction_model.model = nn.DataParallel(stock_prediction_model.model.double())  # wrap your model in DataParallel
        stock_prediction_model.model.to(device)  # send it to the device

        # Train the model
        start = time.time()
        stock_prediction_model.train(X_train_tensor_window, y_train_tensor_window)
        print(f"Training time taken: ", time.time()-start)        
            
        # List to hold the last element of each prediction
        last_elements = []
        
        # Iterate over the bins, making a prediction on each iteration
        for i in range(26):
            # Obtain the test data for the current bin using the slice_and_stack function
            _, _, X_test_window = slice_and_stack(X_scaled, y_scaled, num_stock, i)
            
            # Convert the test data to Torch tensor and Reshape
            X_test_tensor_window = torch.tensor(X_test_window, dtype=torch.float64).to(device).reshape(num_stock, -1, num_feature).unsqueeze(1)
            
            # Make a prediction using the trained model
            y_pred_normalized = stock_prediction_model.predict(X_test_tensor_window)
            
            # Extract the last element of the prediction and append it to the list
            last_element = y_pred_normalized[0, -1, 0].item()
            last_elements.append(last_element)
        
        # Convert the list of last elements to a numpy array and return it
        return np.array(last_elements).reshape(-1, 1)



    # Ensure inputs are NumPy arrays
    # X_train, y_train, X_test, y_test = np.array(X_train), np.array(y_train), np.array(X_test), np.array(y_test)
    
    # # Concatenate training and test data for scaling
    # X = np.concatenate([X_train, X_test])
    # y = np.concatenate([y_train, y_test])
    X = pd.concat([X_train, X_test])
    y = pd.concat([y_train, y_test])
    
    # Normalize data and get scalers
    X,y = np.array(X),np.array(y)
    X_scaled, y_scaled, scaler_X, scaler_y = normalize_data(X, y)
    
    # Device configuration
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    
    # Constants for reshaping and sliding window
    num_stock = num
    num_feature = 52
    
    
    # check_GPU_memory()
    # Train model and predict with sliding window
    # last_preds = train_and_predict_with_sliding_window(X_scaled, y_scaled, num_stock, num_feature, device)
    last_preds = train_and_predict_without_sliding_window(X_scaled, y_scaled, num_stock, num_feature, device)
    
    # Denormalize predictions
    last_preds_denorm = denormalize_predictions(last_preds, scaler_y)
    
    return last_preds_denorm


