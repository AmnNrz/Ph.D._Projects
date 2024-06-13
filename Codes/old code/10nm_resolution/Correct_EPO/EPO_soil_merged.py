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
#     display_name: tillenv
#     language: python
#     name: python3
# ---

import pandas as pd
import numpy as np

# +
path_to_data = ("/Users/aminnorouzi/Library/CloudStorage/"
                "OneDrive-WashingtonStateUniversity(email.wsu.edu)"
                "/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_res_individual/")
# path_to_data = "/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/Projects/Spectroscopy paper/EPO/"

soil_df = pd.read_csv(path_to_data + "Soil.csv")
soil_df = soil_df.drop(columns=['Unnamed: 0', 'EPO_Reflect'])
soil_df.rename({'Soil': 'Type_Name'}, axis=1, inplace=True)
colnames = ['Wvl', 'Sample', 'Scan', 'Type_Name', 'Reflect', 'RWC']
soil_df = soil_df[colnames]
soil_df



# +
#== Apply EPO to all crops ==#
D = pd.DataFrame([])
X_wet = pd.DataFrame([])
res_df_WET = pd.DataFrame([])
for r in soil_df['Type_Name'].unique():
    print(r)
    #== Apply EPO for all Crops ==#
    res_df = soil_df[soil_df['Type_Name'] == f'{r}'].copy()
    res_df_wet = res_df.loc[~(res_df['RWC'] == min(res_df['RWC']))]
    res_df_wet = res_df_wet.sort_values(by='RWC')
    res_df_WET = pd.concat([res_df_WET, res_df_wet])
    res_df_WET = res_df_WET.reset_index(drop=True)

    x_wd = res_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')
    # reordered_cols = ['rwc=1.12', 'rwc=1', 'rwc=0.86', 'rwc=0.73',  'rwc=0.65', 'rwc=0.39','rwc=0.16']
    # x_wd = x_wd[reordered_cols]
    print(x_wd.shape, "x_wd.shape")

    #== Apply EPO ==#
    X = x_wd/100

    print(min(X.columns), "min(X.columns)")
    x_wet = X.drop(min(X.columns), axis=1)
    X_wet = pd.concat([X_wet, x_wet], axis=1)
    d = x_wet.sub(X[min(X.columns)], axis='index')
    d = -d

    # Add Type_Name row
    Type_Name = pd.DataFrame({'Type_Name': [r] * d.shape[1]}).T
    Type_Name.columns = d.columns
    d = pd.concat([d, Type_Name])

    # Add Sample row
    Sample = pd.DataFrame(
        {'Sample': [res_df['Sample'].iloc[0]] * d.shape[1]}).T
    Sample.columns = d.columns
    d = pd.concat([d, Sample])

    # Add Scan row
    Scan = pd.DataFrame(
        {'Scan': res_df['Scan'].unique()[::-1]}).T.drop(columns=0)
    Scan.columns = d.columns
    d = pd.concat([d, Scan])

    D = pd.concat([D, d], axis=1)
DD = D.apply(pd.to_numeric, errors='coerce')
# Perform a singular value decompostion on D(D.T)
U, S, V = np.linalg.svd(np.array(DD.iloc[:-3].T @ DD.iloc[:-3]))
print(U.shape, 'U.shape')
print(S.shape, 'S.shape')
print(V.shape, 'V.shape')

Vs = V[:, 0:2]
Q = Vs@Vs.T
print(Q.shape, 'Q.shape')

P = np.ones([Q.shape[0], Q.shape[0]]) - Q
P.shape

X_raw = X_wet
X_transformed = X_raw @ P

X_transformed.columns = X_wet.columns
X_transformed

# # # add driest column
# # X_transformed[min(X.columns)] = X[min(X.columns)]

# X_transformed.sort_index(axis=1, inplace=True)

# Create a copy of X_wd to avoid modifying the original DataFrame
X_transformed_copy = X_transformed.copy()

# Reset the index to make 'Wvl' a column again
X_transformed_copy.reset_index(inplace=True)

df_transformed = pd.melt(X_transformed_copy, id_vars=[
                         'Wvl'], var_name='RWC', value_name='Reflect')
df_transformed

df_transformed['Sample'] = res_df_WET['Sample']
df_transformed['Type_Name'] = res_df_WET['Type_Name']
df_transformed['Scan'] = res_df_WET['Scan']

desired_column_order = ['Wvl', 'Scan', 'RWC', 'Reflect', 'Type_Name', 'Sample']
df_transformed = df_transformed[desired_column_order]
df_transformed.rename(columns={'Type_Name': 'Crop'}, inplace=True)
df_transformed

# -

df_transformed.to_csv(
    path_to_data + 'Soil_Transformed__Dmerged.csv', index=False)

