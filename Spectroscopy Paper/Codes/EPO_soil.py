# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
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
# path_to_data = "/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/"
path_to_data = "/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/Projects/Spectroscopy paper/EPO/"
soil_df = pd.read_csv(path_to_data + "Soil_08_18.csv")

colnames = ['Wvl', 'Sample', 'Scan', 'Type_Name', 'Reflect', 'RWC']
soil_df.columns = colnames
soil_df
# -


soil_df

# +
#== Plot the Raw reflectance vs Wvl for wheat duet ==#  

Almira_top = soil_df[soil_df['Type_Name'] == 'Almira_top'].copy()
Almira_top['RWC'].unique()

df = Almira_top

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
ax.set_title('Reflect vs Wvl for Different RWC Levels')

# Add a legend
ax.legend()

# Display the plot
plt.show()

# -

Almira_top

Almira_top['RWC'].unique()

# +
#== Prepare X ==#
# scan1: rwc=1, scan2: rwc=1.12, scan3: rwc= 0.86, scan4: rwc= 0.73,
# scan5: rwc=0.65, scan6: rwc=0.39, scan7: rwc=0.16

# Replace Scan column with RWC values
replacement_dict = {'Scan1':'rwc=1', 'Scan2':'rwc=0.84', 'Scan3':'rwc=0.68', 'Scan4':'rwc=0.65',
'Scan5':'rwc=0.53', 'Scan6':'rwc=0.35', 'Scan7':'rwc=0.18', 'Scan8':'rwc=0.11', 'Scan9':'rwc=0.09'}

Almira_top['Scan'] = Almira_top['Scan'].replace(replacement_dict)
Almira_top

X_wd = Almira_top.pivot(index=['Wvl'], columns='Scan', values='Reflect')
reordered_cols = ['rwc=1', 'rwc=0.84', 'rwc=0.68', 'rwc=0.65',
                   'rwc=0.53', 'rwc=0.35','rwc=0.18', 'rwc=0.11', 'rwc=0.09']
X_wd = X_wd[reordered_cols]
X_wd

# +
#== Apply EPO ==#
X = X_wd/100
X_wet = X.iloc[:, :8]

D = X_wet.sub(X.iloc[:, 8], axis = 'index')
D = -D
print(D.shape)
# Perform a singular value decompostion on D(D.T)
U, S, V = np.linalg.svd(np.array(D.T@D))
print(U.shape, 'U.shape')
print(S.shape, 'S.shape')
print(V.shape, 'V.shape')

Vs = V[:, 0:4]
Q = Vs@Vs.T
print(Q.shape, 'Q.shape')

P = np.ones([Q.shape[0],Q.shape[0]]) - Q
P

# +
X_raw = X.iloc[:, :8]
X_transformed = X_raw @ P
X_transformed

X_transformed_cols = ['rwc=1', 'rwc=0.84', 'rwc=0.68', 'rwc=0.65',
                   'rwc=0.53', 'rwc=0.35','rwc=0.18', 'rwc=0.11']
X_transformed.columns = X_transformed_cols
# -

X_raw

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
ax.set_title('Reflect vs Wvl of Almira_top for Different RWC Levels (Before EPO)')

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
ax.set_title('Reflect vs Wvl of Almira_top for Different RWC Levels (After EPO)')

# Add a legend
ax.legend()

# Display the plot
plt.show()

# -

X_transformed

# +
import pandas as pd

import pandas as pd
import pandas as pd

# Assuming X_wd is your pivoted DataFrame
# Create a copy of X_wd to avoid modifying the original DataFrame
X_transformed_copy = X_transformed_copy.copy()

# Reset the index to make 'Wvl' a column again
X_transformed_copy.reset_index(inplace=True)

# Use pd.melt() to unpivot the DataFrame back to its original form
reordered_cols = ['rwc=1', 'rwc=0.84', 'rwc=0.68', 'rwc=0.65',
                   'rwc=0.53', 'rwc=0.35','rwc=0.18', 'rwc=0.11']

df_original = pd.melt(X_transformed_copy, id_vars=['Wvl'], value_vars=reordered_cols, var_name='Scan', value_name='Reflect')

# Sort the DataFrame by the 'Wvl' column to get the original order
df_original.sort_values(by='Wvl', inplace=True)

# Optionally, reset the index if you want a continuous integer index
df_original.reset_index(drop=True, inplace=True)

# Display the resulting DataFrame (df_original)
df_original

# df_original.to_csv(r'G:\My Drive\PhD\Projects\Spectroscopy paper\EPO\soil_transformed.csv')
# -

soil_df['Type_Name'].unique()

s_df = soil_df[soil_df['Type_Name'] == 'Almira_bottom'].copy()
s_df
X_wd = s_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')
X_wd

s_df = soil_df[soil_df['Type_Name'] == 'Bickleton_bottom'].copy()
print(s_df.shape)
s_df.drop_duplicates(subset=['Wvl', 'RWC'], inplace=True)
print(s_df.shape)
X_wd = s_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')

#== Filter spectra at > 1500 nm wavelength ==#
soil_df = soil_df.loc[soil_df['Wvl']>= 1500]

#== Apply EPO to all soils ==#
Soil_transformed = pd.DataFrame([])
for r in soil_df['Type_Name'].unique():
    print(r)
    #== Apply EPO for all Crops ==#
    s_df = soil_df[soil_df['Type_Name'] == f'{r}'].copy()
    s_df.drop_duplicates(subset=['Wvl', 'RWC'], inplace=True)
    X_wd = s_df.pivot(index=['Wvl'], columns='RWC', values='Reflect')
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
    Crop_df = soil_df[soil_df['Type_Name'] == f'{r}'].copy()
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
    df_original['Sample'] = 'Soil'

    # Append to the Residue_transformed
    Soil_transformed = pd.concat([Soil_transformed, df_original])
# Display the resulting DataFrame (df_original)
Soil_transformed

Soil_transformed.to_csv(path_to_data + 'Soil_Transformed.csv', index=False)
