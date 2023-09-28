# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.15.2
#   kernelspec:
#     display_name: tillmap
#     language: python
#     name: python3
# ---

# +
import pandas as pd

# Read data
path_to_data = ("/Users/aminnorouzi/Library/CloudStorage/"
                "OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/Projects/"
                "Tillage_Mapping/Data/")
df_2122 = pd.read_csv(
    path_to_data + 'field_level_data/field_level_main_glcm_seasonBased_joined_2122.csv',
    index_col=0)
df_2223 = pd.read_csv(
    path_to_data + 'field_level_data/field_level_main_glcm_seasonBased_joined_2223.csv',
    index_col=0)

# -

df_2122.shape, df_2223.shape

df_2223


