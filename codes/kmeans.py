import numpy as np
import pandas as pd
from tqdm import tqdm
import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/codes/")
import os;os.sys.path.append(os.path.expanduser('~') + "/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/")
import os;os.sys.path.append("/homes/80/kang/cmem/codes/")
from utils import *
from model import *
import multiprocessing
import time

path0600_1Files = readFromPath(path0600_1)
print(len(path0600_1Files))


def get_features(new_dflst_lst,x_list,type="volume"):
    if type =="volume":
        return get_volume_features(new_dflst_lst,x_list)
    elif type =="features":
        return get_features_features(new_dflst_lst,x_list)
    else:
        raise NotImplementedError

def get_volume_features(new_dflst_lst,x_list):
    feature_list = []
    for index, item in enumerate(new_dflst_lst):
        pass
        item.date = item.date.astype(np.int32)
        item = item.set_index('date')
        value = item.turnover
        value.name = path0600_1Files[index][:-4]
        feature_list.append(value)
    features = pd.concat(feature_list,axis = 1)
    return features

def get_features_features(new_dflst_lst,x_list):
    nfeatures = []
    for col in x_list:
        feature_list = []
        for index, item in enumerate(new_dflst_lst):
            pass
            item.columns
            item.date = item.date.astype(np.int32)
            item = item.set_index('date')
            value = item.loc[:,col]
            # assert item.shape[1] == 108
            value.name = path0600_1Files[index][:-4]
            feature_list.append(value)
        features = pd.concat(feature_list,axis = 1)
        nfeatures.append(features)
    nfeatures = np.stack(nfeatures)
    len(nfeatures.shape)
    nfeatures.shape
    return nfeatures

def get_corr_matrix(train_start_Index, train_end_Index, features):
    if len(features.shape) ==2:
        print(f"shape of features: {features.shape}")
        f = features.iloc[train_start_Index:train_end_Index,:]
        fv=f.values
        corr_matrix = np.corrcoef(fv, rowvar=False)
        # Print the shape of the correlation matrix
        print("Shape of correlation matrix:", corr_matrix.shape)
        return corr_matrix
    elif len(features.shape) ==3:
        print(f"shape of features: {features.shape}")
        nfeatures = features
        f = np.array([nfeatures[i,train_start_Index:train_end_Index,:] for i in range(nfeatures.shape[0])])
        f.shape
        ncorr_matrix = np.array([np.corrcoef(fv, rowvar=False) for fv in f])
        ncorr_matrix.shape
        corr_matrix = np.mean(ncorr_matrix,axis=0)
        # Print the shape of the correlation matrix
        # print("Shape of correlation matrix:", corr_matrix.shape)
        return corr_matrix
    else:
        raise NotImplementedError
def get_labels_byPCA(corr_matrix,ratio_cumsum,n_components,n_clusters):
    from sklearn.decomposition import PCA
    pca = PCA()
    # Fit PCA on the correlation matrix
    # print(corr_matrix)
    pca.fit(corr_matrix)
    # Obtain the principal
    ratio=pca.explained_variance_ratio_
    # pca=PCA(n_components=40)
    # pca=PCA(n_components=100)
    # pca=PCA(n_components=np.argmax(ratio.cumsum() >= 0.9999))
    # pca=PCA(n_components=np.argmax(ratio.cumsum() >= 0.99))
    if ratio_cumsum == 1.00:
        pca = PCA(n_components=n_components)
        print("n_components 100")
    else:
        pca = PCA(n_components=np.argmax(ratio.cumsum() >= ratio_cumsum))
        print(f"n_components {np.argmax(ratio.cumsum() >= ratio_cumsum)}")
    pca.fit(corr_matrix)
    scores_pca = pca.transform(corr_matrix)

    from sklearn.cluster import KMeans
    kmeans_pca = KMeans(n_clusters=n_clusters, init="k-means++",random_state=42)
    kmeans_pca.fit(scores_pca)
    assert kmeans_pca.labels_.shape == (n_components,)
    labels = kmeans_pca.labels_
    return labels

# # n_clusters = 2
# # n_clusters = 5
# n_clusters = 10
# # n_clusters = 20
# # n_clusters = 50


# ratio_cumsum = 0.80
# # ratio_cumsum = 0.99
# # ratio_cumsum = 0.9999
# # ratio_cumsum = 1.00
# features = get_features(new_dflst_lst,type="volume")
# # features = get_features(new_dflst_lst,type="features")
# features.shape





# groupped_dfs = [new_dflst_lst[i] for i in list_]
# gs = [dflst.iterrows() for dflst in groupped_dfs]
# dff = []
# for i in range(one_stock_shape):
# # for i in tqdm(range(one_stock_shape)):
#     for g in gs:
#         elem = next(g)[1].T
#         dff.append(elem)
# df = pd.concat(dff, axis=1).T
# df.reset_index(inplace=True, drop=True)
# # print(df.shape)

# num = len(groupped_dfs)