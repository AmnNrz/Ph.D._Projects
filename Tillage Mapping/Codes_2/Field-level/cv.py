# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
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
cdl_2122 = pd.read_csv(path_to_data + 'cdl_data/cdl_2122.csv')


df = pd.concat([df_2122, df_2223])
cdl = cdl_2122[['pointID', 'most_frequent_class']].copy()
cdl.rename(columns={'most_frequent_class': 'PriorCropT'}, inplace=True)

replacement_dict = {24: 'grain', 23: 'grain', 51: 'legume',
                    51: 'legume', 31: 'canola', 53: 'legume',
                    21: 'grain', 51: 'legume', 52: 'legume',
                    28: 'grain'}

cdl['PriorCropT'] = cdl['PriorCropT'].replace(
    replacement_dict)
cdl = cdl.loc[cdl['PriorCropT'].isin(
    ['grain', 'legume', 'canola'])]
cdl['PriorCropT'].value_counts()
cdl

df_2122.drop(columns='PriorCropT', inplace=True)
df_2122 = pd.merge(df_2122, cdl, on='pointID')

# Define the new order of columns
cols = list(df_2122.columns)
cols.remove('PriorCropT')
cols.insert(3, 'PriorCropT')

# Reindex the DataFrame with the new column order
df_2122 = df_2122[cols]

df_2122['PriorCropT'].isna().value_counts()

df = pd.concat([df_2122, df_2223])

df

# -



cdl


