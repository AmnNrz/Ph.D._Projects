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

import pandas as pd
import numpy as np

# +
residue_df = pd.read_csv(
    "/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/Residue_08_18.csv")

colnames = ['Wvl', 'Sample', 'Scan', 'Type_Name', 'Reflect', 'RWC']
residue_df.columns = colnames
residue_df["Type_Name"].unique()
# residue_df = pd.read_csv(
    # "/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main/Residue_08_18.csv")
# soil_df = pd.read_csv(r"G:\My Drive\PhD\Projects\Spectroscopy paper\Spectrometry-main\Soil_08_18.csv")
# soil_df = pd.read_csv(
    # "/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main/Soil_08_18.csv")


# +
Wheat_Duet = residue_df[residue_df['Type_Name'] == 'Weathered Canola'].copy()

Wheat_Duet


# +
#== Plot the Raw reflectance vs Wvl for Wheat Duet ==#  

Wheat_Duet = residue_df[residue_df['Type_Name'] == 'Weathered Canola'].copy()
Wheat_Duet['RWC'].unique()

df = Wheat_Duet

import matplotlib.pyplot as plt
import pandas as pd

# Assuming your dataframe is stored in a variable called 'df'

# List of unique RWC levels
rwc_levels = df['RWC'].unique()

# Set up the figure and axes
fig, ax = plt.subplots()

# Iterate over each RWC level and plot Reflect vs Wvl
for rwc_level in rwc_levels:
    # Filter the dataframe for the current RWC level
    rwc_df = df[df['RWC'] == rwc_level]
    
    # Plot Reflect vs Wvl for the current RWC level
    ax.plot(rwc_df['Wvl'], rwc_df['Reflect'], label=f'RWC {rwc_level}')
    
# Set labels and title
ax.set_xlabel('Wvl')
ax.set_ylabel('Reflect')
ax.set_title('Reflect vs Wvl of Wheat Duet for Different RWC Levels (Before EPO)')

# Add a legend
ax.legend()

# Display the plot
plt.show()

# -

df.to_csv('G:\My Drive\PhD\Projects\Spectroscopy paper\EPO\wheatDuet_sperctra_original.csv')

for crp in residue_df['Type_Name'].unique():
    print(crp)
    #== Apply EPO for one Crop ==#
    res_df = residue_df[residue_df['Type_Name'] == crp].copy()
    res_df
    #== Prepare X ==#
    X_wd = res_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')
    X_wd
    X = X_wd/100
    print(min(X.columns))

# +
#==    Apply EPO on Raw    ==#
X = X_wd/100

X_wet = X.drop(min(X.columns), axis=1)
X_wet
# D = X_wet.sub(X[min(X.columns)], axis = 'index')
# D = -D
# print(D.shape)
# Perform a singular value decompostion on D(D.T)
U, S, V = np.linalg.svd(np.array(X_wet.T@X_wet))
print(U.shape, 'U.shape')
print(S.shape, 'S.shape')
print(V.shape, 'V.shape')

Vs = V[:, 0:4]
Q = Vs@Vs.T
print(Q.shape, 'Q.shape')

P = np.ones([Q.shape[0],Q.shape[0]]) - Q
P


X_raw = X_wet
X_transformed = X_raw @ P

X_transformed.columns = X_wet.columns
X_transformed

# +
#==   Apply EPO on D   ==#
#== Apply EPO for one Crop ==#
res_df = residue_df[residue_df['Type_Name'] == 'Wheat Duet'].copy()
# res_df.set_index(['Wvl', 'RWC'], inplace=True)
res_df
#== Prepare X ==#
X_wd = res_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')
X_wd
X = X_wd/100

X_wet = X.drop(min(X.columns), axis=1)
X_wet
D = X_wet.sub(X[min(X.columns)], axis = 'index')
D = -D
print(D.shape)
# Perform a singular value decompostion on D(D.T)
U, S, V = np.linalg.svd(np.array(D.T@D))
print(U.shape, 'U.shape')
print(S.shape, 'S.shape')
print(V.shape, 'V.shape')
print(f"{V.shape = }")
Vs = V[:, 0:4]
Q = Vs@Vs.T
print(Q.shape, 'Q.shape')

P = np.ones([Q.shape[0],Q.shape[0]]) - Q
P


X_raw = X_wet
X_transformed = X_raw @ P

X_transformed.columns = X_wet.columns
X_transformed

# +
import matplotlib.pyplot as plt
import pandas as pd

df = X_raw

# List of unique RWC levels
rwc_levels = list(df.columns)

# Set up the figure and axes
fig, ax = plt.subplots()

# Iterate over each RWC level and plot Reflect vs Wvl
for rwc_level in rwc_levels:
    # # Filter the dataframe for the current RWC level
    # rwc_df = df[df['RWC'] == rwc_level]
    
    # Plot Reflect vs Wvl for the current RWC level
    ax.plot(np.array(X_transformed.index), df[rwc_level], label=f'RWC {rwc_level}')
    
# Set labels and title
ax.set_xlabel('Wvl')
ax.set_ylabel('Reflect')
ax.set_title('Reflect vs Wvl for Different RWC Levels (Before EPO)')

# Add a legend
ax.legend()

# Display the plot
plt.show()


# +
import matplotlib.pyplot as plt
import pandas as pd

df = X_transformed

# List of unique RWC levels
rwc_levels = list(df.columns)

# Set up the figure and axes
fig, ax = plt.subplots()

# Iterate over each RWC level and plot Reflect vs Wvl
for rwc_level in rwc_levels:
    # # Filter the dataframe for the current RWC level
    # rwc_df = df[df['RWC'] == rwc_level]
    
    # Plot Reflect vs Wvl for the current RWC level
    ax.plot(np.array(X_transformed.index), df[rwc_level], label=f'RWC {rwc_level}')
    
# Set labels and title
ax.set_xlabel('Wvl')
ax.set_ylabel('Reflect')
ax.set_title('Reflect vs Wvl of Wheat Duet for Different RWC Levels (After EPO)')

# Add a legend
ax.legend()

# Display the plot
plt.show()


# +
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
Wheat_Duet_df = residue_df[residue_df['Type_Name'] == 'Wheat Duet'].copy()
Wheat_Duet_df['Scan_RWC'] = Wheat_Duet_df['Scan'].astype(str) + '_' + Wheat_Duet_df['RWC'].astype(str)
scan_list = list(Wheat_Duet_df['Scan_RWC'].unique())

# Create a mapping dictionary from RWC to Scan strings
mapping_dict = {float(scan.split('_')[1]): scan.split('_')[0] for scan in scan_list}

# Create the 'Scan' column using the map function with the mapping dictionary
df_original['Scan'] = df_original['RWC'].map(mapping_dict)
df_original
# -

df_original.to_csv('G:\My Drive\PhD\Projects\Spectroscopy paper\EPO\WheatDuet_spectra_EpoAppliedOnRaw.csv')

residue_df

#== Filter spectra at > 1500 nm wavelength ==#
residue_df = residue_df.loc[residue_df['Wvl']>= 1500]

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

    Vs = V[:, 0:4]
    Q = Vs@Vs.T
    print(Q.shape, 'Q.shape')

    P = np.ones([Q.shape[0],Q.shape[0]]) - Q
    P


    X_raw = X_wet
    X_transformed = X_raw @ P

    X_transformed.columns = X_wet.columns
    X_transformed

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

Residue_transformed.to_csv("Residue_Transformed.csv", index=False)

# +
# Combine the Soil and Residue 

Soil_Transformed = pd.read_csv('G:\My Drive\PhD\Projects\Spectroscopy paper\EPO\Soil_Transformed.csv')
Soil_Transformed

Combined_transformed = pd.concat([Residue_transformed, Soil_Transformed])
Combined_transformed.to_csv('Combined_transformed.csv', index=False)
