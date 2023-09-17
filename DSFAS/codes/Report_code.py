# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.15.1
#   kernelspec:
#     display_name: tillenv
#     language: python
#     name: python3
# ---

# +
import numpy as np 
import pandas as pd

df_first = pd.read_csv("G:\My Drive\PhD\Projects\DSFAS\Landsat_data\dry\df_first_dry.csv", index_col= 0)
df_second = pd.read_csv("G:\My Drive\PhD\Projects\DSFAS\Landsat_data\dry\df_second_dry.csv", index_col= 0)
df_second

