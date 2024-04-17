import torch
from torch import nn
from informer2020.models.model import Informer


model = Informer(
    enc_in=52,  # Number of features
    dec_in=1,   # Number of target features
    c_out=1,    # Output size
    seq_len=260, # Sequence length
    label_len=26, # Label length
    out_len=26,  # Output length
    # Other parameters
)

