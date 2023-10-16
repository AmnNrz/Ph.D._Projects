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

# +
import pandas as pd

import numpy as np

# +
path_to_data = ("/Users/aminnorouzi/Library/CloudStorage/"
                "OneDrive-WashingtonStateUniversity(email.wsu.edu)"
                "/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_res_individual/")
# path_to_data = "/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/Projects/Spectroscopy paper/EPO/"

residue_df = pd.read_csv(path_to_data + "Residue.csv")
residue_df = residue_df.drop(columns=['Unnamed: 0', 'EPO_Reflect'])
residue_df.rename({'Soil': 'Type_Name'}, axis=1, inplace=True)
residue_df['Sample'] = residue_df['Sample'].map({'Crop Residue':'Residue'})
colnames = ['Wvl', 'Sample', 'Scan', 'Type_Name', 'Reflect', 'RWC']
residue_df = residue_df[colnames]
residue_df



#== Apply EPO to all crops ==#
Residue_transformed = pd.DataFrame([])
for r in residue_df['Type_Name'].unique():
    print(r)
    #== Apply EPO for all Crops ==#
    res_df = residue_df[residue_df['Type_Name'] == f'{r}'].copy()

    X_wd = res_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')
    # reordered_cols = ['rwc=1.12', 'rwc=1', 'rwc=0.86', 'rwc=0.73',  'rwc=0.65', 'rwc=0.39','rwc=0.16']
    # X_wd = X_wd[reordered_cols]
    print(X_wd.shape, "X_wd.shape")

    #== Apply EPO ==#
    X = X_wd/100

    print(min(X.columns), "min(X.columns)")
    X_wet = X.drop(min(X.columns), axis=1)
    X_wet
    D = X_wet.sub(X[min(X.columns)], axis = 'index')
    D = -D
    # print(D.shape)
    # Perform a singular value decompostion on D(D.T)
    U, S, V = np.linalg.svd(np.array(D.T @ D))
    print(U.shape, 'U.shape')
    print(S.shape, 'S.shape')
    print(V.shape, 'V.shape')

    Vs = V[:, 0:2]
    Q = Vs@Vs.T
    print(Q.shape, 'Q.shape')

    P = np.ones([Q.shape[0],Q.shape[0]]) - Q
    P


    X_raw = X_wet
    X_transformed = X_raw @ P

    X_transformed.columns = X_wet.columns
    X_transformed

    # # add driest column
    # X_transformed[min(X.columns)] = X[min(X.columns)]
    
    X_transformed.sort_index(axis=1, inplace=True)

    # Create a copy of X_wd to avoid modifying the original DataFrame
    X_transformed_copy = X_transformed.copy()

    # Reset the index to make 'Wvl' a column again
    X_transformed_copy.reset_index(inplace=True)

    df_original = pd.melt(X_transformed_copy, id_vars=['Wvl'], var_name='RWC', value_name='Reflect')

    # Sort the DataFrame by the 'Wvl' column to get the original order
    df_original.sort_values(by='Wvl', inplace=True)

    # Optionally, reset the index if you want a continuous integer index
    df_original.reset_index(drop=True, inplace=True)

    # # Display the resulting DataFrame (df_original)
    Crop_df = residue_df[residue_df['Type_Name'] == f'{r}'].copy()
    Crop_df['Scan_RWC'] = Crop_df['Scan'].astype(str) + '_' + Crop_df['RWC'].astype(str)
    scan_list = list(Crop_df['Scan_RWC'].unique())

    # Create a mapping dictionary from RWC to Scan strings
    mapping_dict = {float(scan.split('_')[1]): scan.split('_')[0] for scan in scan_list}

    # Create the 'Scan' column using the map function with the mapping dictionary
    df_original['Scan'] = df_original['RWC'].map(mapping_dict)
    df_original

    target_position = 1

    # Insert the column "Scan" at position 1
    df_original.insert(target_position, 'Scan', df_original.pop('Scan'))

    # Create Crop column
    df_original['Crop'] = r

    # Create Sampe column
    df_original['Sample'] = 'Residue'

    # Append to the Residue_transformed
    Residue_transformed = pd.concat([Residue_transformed, df_original])
# Display the resulting DataFrame (df_original)
Residue_transformed

Residue_transformed


Residue_transformed.to_csv(path_to_data + "Residue_Transformed.csv", index=False)
