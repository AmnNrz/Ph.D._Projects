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
#     display_name: tillenv
#     language: python
#     name: python3
# ---

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import EPO

# +
# path_to_data = ("/home/amnnrz/OneDrive - a.norouzikandelati/"
#                 "Ph.D/Projects/Spectroscopy_Paper/Data/")

path_to_data = ("/Users/aminnorouzi/Library/CloudStorage/OneDrive"
                "-WashingtonStateUniversity(email.wsu.edu)/Ph.D/Projects/"
                "Spectroscopy_Paper/Data/")

# -

# ### All Residues

# +
# Read residue raw data
residue_df = pd.read_csv(path_to_data + "Updated_data_2_10nm_res/Residue.csv", index_col=0)

rename_dict = {
    'Soil':'Type_Name'
}
residue_df.rename(columns=rename_dict, inplace=True)
residue_df
colnames = ['Wvl', 'Sample', 'Scan', 'Type_Name', 'Reflect', 'RWC', 'EPO_Reflect']
residue_df = residue_df[colnames]

# +
########################################################
###### Merge original reflectance for all crops ########
########################################################
pivoted_df_original = pd.DataFrame([])
for crp in residue_df['Type_Name'].unique():
    sub_df = residue_df.loc[residue_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='Reflect')
    pivoted_df_original = pd.concat([pivoted_df_original, pivoted_sub_df])

pivoted_df_original = pivoted_df_original.groupby('Scan').median()
pivoted_df_original = pivoted_df_original.reset_index()
pivoted_df_original
# Melt the DataFrame
melted_df_original = pd.melt(pivoted_df_original, id_vars=['Scan'], value_vars=pivoted_df_original.columns.drop('Scan'),
                    var_name='Wvl', value_name='Reflect')
melted_df_original

########################################################
####   Merge Haly's EPO reflectance for all crops   ######
########################################################
pivoted_df_Haly_epo = pd.DataFrame([])
for crp in residue_df['Type_Name'].unique():
    sub_df = residue_df.loc[residue_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='EPO_Reflect')
    pivoted_df_Haly_epo = pd.concat([pivoted_df_Haly_epo, pivoted_sub_df])

pivoted_df_Haly_epo = pivoted_df_Haly_epo.groupby('Scan').median()
pivoted_df_Haly_epo = pivoted_df_Haly_epo.reset_index()
pivoted_df_Haly_epo
# Melt the DataFrame
melted_df_Haly_epo = pd.melt(pivoted_df_Haly_epo, id_vars=['Scan'], value_vars=pivoted_df_Haly_epo.columns.drop('Scan'),
                             var_name='Wvl', value_name='EPO_Reflect')
melted_df_Haly_epo

# Apply EPO
df_transformed = EPO.epo(melted_df_original)

##   Save transformed and original dataframes   ##
df_transformed.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/allCrops_EPO.csv', index=False)

melted_df_original.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/residue_original.csv', index=False)

# -


melted_df_original


# +
#== Plot reflectance vs Wvl  ==#  
df = melted_df_original

# List of unique RWC levels
rwc_levels = df['Scan'].unique()

# Set up the figure and axes
fig, ax = plt.subplots()

# Iterate over each RWC level and plot Reflect vs Wvl
for rwc_level in rwc_levels:
    # Filter the dataframe for the current RWC level
    rwc_df = df.loc[df['Scan'] == rwc_level]
    
    # Plot Reflect vs Wvl for the current RWC level
    ax.plot(rwc_df['Wvl'], rwc_df['Reflect'], label=f'{rwc_level}')
    
# Set labels and title
ax.set_xlabel('Wavelength')
ax.set_ylabel('Reflectance')
ax.set_title('All Crops merged (Original)')

# Add a legend
ax.legend()

# Display the plot
plt.show()

# plt.savefig(path)


# -

# ### All Soils
#

# +
soil_df = pd.read_csv(path_to_data + "Updated_data_2_10nm_res/Soil.csv", index_col=0)

rename_dict = {
    'Soil':'Type_Name'
}
soil_df.rename(columns=rename_dict, inplace=True)
soil_df
colnames = ['Wvl', 'Sample', 'Scan', 'Type_Name', 'Reflect', 'RWC', 'EPO_Reflect']
soil_df = soil_df[colnames]

########################################################
###### Merge original reflectance for all Soils ########
########################################################
pivoted_df_original = pd.DataFrame([])
for crp in soil_df['Type_Name'].unique():
    sub_df = soil_df.loc[soil_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='Reflect')
    pivoted_df_original = pd.concat([pivoted_df_original, pivoted_sub_df])

pivoted_df_original = pivoted_df_original.groupby('Scan').median()
pivoted_df_original = pivoted_df_original.reset_index()
pivoted_df_original
# Melt the DataFrame
melted_df_original = pd.melt(pivoted_df_original, id_vars=['Scan'], value_vars=pivoted_df_original.columns.drop('Scan'),
                             var_name='Wvl', value_name='Reflect')
melted_df_original

# Apply EPO
df_transformed = EPO.epo(melted_df_original)

##   Save transformed and original dataframes   ##
df_transformed.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/allSoils_EPO.csv', index=False)

melted_df_original.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/soil_original.csv', index=False)

# -

# ### Fresh Residue

# +
#############################################################
###### Merge original reflectance for fresh residues ########
#############################################################

weathrd_residues = ['Weathered Wheat', 'Weathered Canola']
fresh_df = residue_df.loc[~(residue_df['Type_Name'].isin(weathrd_residues))]

pivoted_df_original = pd.DataFrame([])
for crp in fresh_df['Type_Name'].unique():
    sub_df = fresh_df.loc[fresh_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='Reflect')
    pivoted_df_original = pd.concat([pivoted_df_original, pivoted_sub_df])

pivoted_df_original = pivoted_df_original.groupby('Scan').median()
pivoted_df_original = pivoted_df_original.reset_index()
pivoted_df_original
# Melt the DataFrame
melted_df_original = pd.melt(pivoted_df_original, id_vars=['Scan'], value_vars=pivoted_df_original.columns.drop('Scan'),
                             var_name='Wvl', value_name='Reflect')
melted_df_original

# Apply EPO
df_transformed = EPO.epo(melted_df_original)

##   Save transformed and original dataframe   ##
df_transformed.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/fresh_res_EPO.csv', index=False)

melted_df_original.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/fresh_res_org.csv', index=False)

# -

# ### Weathered Residue

# +
#############################################################
###### Merge original reflectance for weathered residues ########
#############################################################

weathrd_residues = ['Weathered Wheat', 'Weathered Canola']
weathrd_df = residue_df.loc[residue_df['Type_Name'].isin(weathrd_residues)]

pivoted_df_original = pd.DataFrame([])
for crp in weathrd_df['Type_Name'].unique():
    sub_df = weathrd_df.loc[weathrd_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='Reflect')
    pivoted_df_original = pd.concat([pivoted_df_original, pivoted_sub_df])

pivoted_df_original = pivoted_df_original.groupby('Scan').median()
pivoted_df_original = pivoted_df_original.reset_index()
pivoted_df_original
# Melt the DataFrame
melted_df_original = pd.melt(pivoted_df_original, id_vars=['Scan'], value_vars=pivoted_df_original.columns.drop('Scan'),
                             var_name='Wvl', value_name='Reflect')
melted_df_original

# Apply EPO
df_transformed = EPO.epo(melted_df_original)

##   Save transformed and original dataframe   ##
df_transformed.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/weathrd_res_EPO.csv', index=False)

melted_df_original.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/weathrd_res_org.csv', index=False)

# -

# ### Lightest Soil

# +
#############################################################
###### Merge original reflectance for fresh residues ########
#############################################################

Lightest_soils = ['Benwy', 'Shano', 'Lance']

Lightest_soils_df = soil_df.loc[soil_df['Type_Name'].isin(
    Lightest_soils)]

pivoted_df_original = pd.DataFrame([])
for crp in Lightest_soils_df['Type_Name'].unique():
    sub_df = Lightest_soils_df.loc[Lightest_soils_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='Reflect')
    pivoted_df_original = pd.concat([pivoted_df_original, pivoted_sub_df])

pivoted_df_original = pivoted_df_original.groupby('Scan').median()
pivoted_df_original = pivoted_df_original.reset_index()
pivoted_df_original
# Melt the DataFrame
melted_df_original = pd.melt(pivoted_df_original, id_vars=['Scan'], value_vars=pivoted_df_original.columns.drop('Scan'),
                             var_name='Wvl', value_name='Reflect')
melted_df_original

# Apply EPO
df_transformed = EPO.epo(melted_df_original)

##   Save transformed and original dataframe   ##
df_transformed.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/lightest_soil_EPO.csv', index=False)

melted_df_original.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/lightest_soil_org.csv', index=False)

# -

# ### Darkest Soil

# +
#############################################################
###### Merge original reflectance for fresh residues ########
#############################################################

Darkest_soils = ['Bagdad', 'Mondovi 1', 'Athena']

Darkest_soils_df = soil_df.loc[soil_df['Type_Name'].isin(
    Darkest_soils)]

pivoted_df_original = pd.DataFrame([])
for crp in Darkest_soils_df['Type_Name'].unique():
    sub_df = Darkest_soils_df.loc[Darkest_soils_df['Type_Name'] == crp]
    pivoted_sub_df = sub_df.pivot(
        index='Scan', columns='Wvl', values='Reflect')
    pivoted_df_original = pd.concat([pivoted_df_original, pivoted_sub_df])

pivoted_df_original = pivoted_df_original.groupby('Scan').median()
pivoted_df_original = pivoted_df_original.reset_index()
pivoted_df_original
# Melt the DataFrame
melted_df_original = pd.melt(pivoted_df_original, id_vars=['Scan'], value_vars=pivoted_df_original.columns.drop('Scan'),
                             var_name='Wvl', value_name='Reflect')
melted_df_original

# Apply EPO
df_transformed = EPO.epo(melted_df_original)

##   Save transformed and original dataframe   ##
df_transformed.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/Darkest_soil_EPO.csv', index=False)

melted_df_original.to_csv(
    path_to_data + 'Updated_data_2_10nm_res/Darkest_soil_org.csv', index=False)

