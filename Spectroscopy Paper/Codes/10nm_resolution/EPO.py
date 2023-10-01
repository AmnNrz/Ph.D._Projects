# ---
# jupyter:
#   jupytext:
#     cell_metadata_filter: -all
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.15.2
# ---

import pandas as pd
import numpy as np

def epo(df):
    '''
    Inputs: Original spectra dataframe
    outputs: Spectra after EPO dataframe

    '''



    X_wd = df.pivot(
        index=['Wvl'], columns='Scan', values='Reflect')

    #== Apply EPO ==#
    X = X_wd/100

    print(min(X.columns), "min(X.columns)")
    X_wet = X.drop(min(X.columns), axis=1)
    X_wet
    D = X_wet.sub(X[min(X.columns)], axis='index')
    D = -D
    # print(D.shape)
    # Perform a singular value decompostion on D(D.T)
    U, S, V = np.linalg.svd(np.array(D.T @ D))
    print(U.shape, 'U.shape')
    print(S.shape, 'S.shape')
    print(V.shape, 'V.shape')

    Vs = V[:, 0:4]
    Q = Vs@Vs.T
    print(Q.shape, 'Q.shape')

    P = np.ones([Q.shape[0], Q.shape[0]]) - Q
    P

    X_raw = X_wet
    X_transformed = X_raw @ P

    X_transformed.columns = X_wet.columns
    X_transformed

    # add driest column
    X_transformed[min(X.columns)] = X[min(X.columns)]
    X_transformed.sort_index(axis=1, inplace=True)

    # Create a copy of X_wd to avoid modifying the original DataFrame
    X_transformed_copy = X_transformed.copy()

    # Reset the index to make 'Wvl' a column again
    X_transformed_copy.reset_index(inplace=True)

    df_transformed = pd.melt(X_transformed_copy, id_vars=[
        'Wvl'], var_name='Scan', value_name='Reflect')

    # Sort the DataFrame by the 'Wvl' column to get the original order
    df_transformed.sort_values(by='Wvl', inplace=True)

    # Optionally, reset the index if you want a continuous integer index
    df_transformed.reset_index(drop=True, inplace=True)

    # # # Display the resulting DataFrame (df_transformed)
    # Crop_df = residue_df[residue_df['Type_Name'] == f'{r}'].copy()
    # Crop_df['Scan_RWC'] = Crop_df['Scan'].astype(
    #     str) + '_' + Crop_df['RWC'].astype(str)
    # scan_list = list(Crop_df['Scan_RWC'].unique())

    # # Create a mapping dictionary from RWC to Scan strings
    # mapping_dict = {float(scan.split('_')[1]): scan.split('_')[
    #     0] for scan in scan_list}

    # # Create the 'Scan' column using the map function with the mapping dictionary
    # df_transformed['Scan'] = df_transformed['RWC'].map(mapping_dict)
    # df_transformed

    target_position = 1
    # Insert the column "Scan" at position 1
    df_transformed.insert(target_position, 'Scan', df_transformed.pop('Scan'))

    # Create Sampe column
    df_transformed['Sample'] = 'Residue'
    df_transformed['RWC'] = df_transformed['Scan']
    return df_transformed
