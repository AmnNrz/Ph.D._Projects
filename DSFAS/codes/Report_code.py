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

import numpy as np 
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
import seaborn as sns
import geopandas as gpd
import matplotlib.colors as mcolors


# +
# Read data
# path_to_data = ("/Users/aminnorouzi/Library/CloudStorage/"
#                 "GoogleDrive-msaminnorouzi@gmail.com/My Drive/"
#                 "PhD/Projects/DSFAS/Data/")

path_to_data = ("/home/amnnrz/GoogleDrive - "
                "msaminnorouzi/PhD/Projects/DSFAS/Data/")

df = pd.read_csv(path_to_data + "Carbon&satellite_data_joined_v1.csv")

# Convert year to integer
df['YearSample'] = df['YearSample'].astype(int)

# remove old index column
df.drop(columns='index', axis=1, inplace=True)
# -

df

# check 0_6 -- 0_12 samples' year
sampleYear_6_12 = df.loc[df['DepthSampl'] ==
                         '0_6', 'YearSample'].values[0]
print('Two-depth samples are for:', f'{sampleYear_6_12}')


# +
# Get average of total_C over 0-6 and 6-12 inches samples 
dup_df = df.loc[df.SampleID.duplicated(keep=False)]
dup_df

averaged_C = pd.DataFrame([])
averaged_C['SampleID'] = dup_df.SampleID.unique()
for id in dup_df.SampleID.unique():
    averaged_C.loc[averaged_C["SampleID"] == id, "TotalC"] = np.mean(
        dup_df.loc[dup_df["SampleID"] == id, "TotalC"])

averaged_C.head(5)
# -

df = df.loc[~df.SampleID.duplicated()]
df.loc[df.SampleID.isin(averaged_C.SampleID),
        'TotalC'] = averaged_C['TotalC'].values
df.loc[df.SampleID.isin(averaged_C.SampleID), 'TotalC']
df.loc[df['DepthSampl'] == '0_6', 'DepthSampl'] = '0_12'
df

df.columns

# +
# Normalize band values
largeValue_idx = (df.iloc[:, 11:].describe().loc["min"] < -2) | \
    (df.iloc[:, 8:].describe().loc["max"] > 2)
largeValue_cols = largeValue_idx[largeValue_idx].index

scaler = StandardScaler()

# fit the scaler on the selected columns
scaler.fit(df[largeValue_cols].copy())

# transform the selected columns to have zero mean and unit variance
df.loc[:, largeValue_cols] = scaler.transform(df[largeValue_cols].copy())
df.describe()

# -

# Convert Total_C_% to g/cm2
# "total_c_%" /100 * height * A * 2.54 (inch to cm) * BD
df.loc[:, "Total_C (g/cm2)"] = df["TotalC"]/100 * 12 * 2.54 * 1 * df["BD_g_cm3"]
df["Total_C (g/cm2)"].describe()


# +
# DENSITY DISTRIBUTION PLOT FOR ALL YEARS TOGETHER
# Increase the font size of the labels
plt.rcParams.update({'font.size': 12})

# Increase the resolution of the plot
plt.figure(figsize=(12, 8), dpi=300)

# Plot the density distribution of column 'Total_C_g/cm2'
df['Total_C (g/cm2)'].plot(kind='density')

# Set x-axis label
plt.xlabel('Total C (g/cm$^2$)', fontsize=14)

# Mark actual values on the curve
min_value = df['Total_C (g/cm2)'].min()
max_value = df['Total_C (g/cm2)'].max()

# Plotting the actual values on the curve
plt.axvline(x=min_value, color='red', linestyle='--', label='Min')
plt.axvline(x=max_value, color='blue', linestyle='--', label='Max')

# Display legend
plt.legend(fontsize=12)

# Show the plot
plt.show()


# +
# TOTAL C DENSITY DISTRIBUTION PLOTS GROUPED BY YEARS
# Set the style and size of the plot
sns.set(style="whitegrid")
plt.figure(figsize=(10, 6))

# Loop through each year and plot the density distribution
for year in df['YearSample'].unique():
    subset = df[df['YearSample'] == year]
    sns.kdeplot(subset['Total_C (g/cm2)'], label=f'Year {year}', fill=True)

# Add labels and title
plt.xlabel('Total_C (g/cm2)')
plt.ylabel('Density')
plt.title('Density Distribution of Total C Grouped by Year')
plt.legend()

# Show the plot
plt.show()
# -


# # Create map of data

# +
# Load dry_irrigated dataframe 
allSamples_df = pd.read_csv(path_to_data + "Carbon&satellite_data_dryIrgted_joined_v1.csv")

# Convert dataframes to GeoDataFrames
dry_df = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.Longitude, df.Latitude))
allSamples_df = gpd.GeoDataFrame(allSamples_df, geometry=gpd.points_from_xy(allSamples_df.Longitude, allSamples_df.Latitude))

# Remove reduntant columns
allSamples_df = allSamples_df.loc[:, 'TotalC':].copy()
dry_df = dry_df.loc[:, 'TotalC':].copy()

dry_df.reset_index(drop = True, inplace=True)
allSamples_df.reset_index(drop = True, inplace=True)

# merge two dataframes
allSamples_df.sort_values(by='DepthSampl', inplace=True)
unique_locations_df = allSamples_df.loc[~(allSamples_df['geometry'].duplicated(keep='last'))].copy()
irrigated_df = unique_locations_df.loc[~(unique_locations_df['geometry'].isin(dry_df['geometry']))].copy()

# add irrigation column
irrigated_df['Irrigation'] = 'Irrigated'
dry_df['Irrigation'] = 'Dryland'

df = pd.concat([dry_df, irrigated_df])
# df
# -

colors_from_viridis[0]

# +
# Load U.S. states shapefiles (You can download from U.S. Census Bureau or other sources)
path_to_shpfiles = "/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/Projects/DSFAS/Data/GIS_Data/"

us_states = gpd.read_file(path_to_shpfiles + "cb_2022_us_state_500k/cb_2022_us_state_500k.shp")
us_counties = gpd.read_file(path_to_shpfiles + "cb_2022_us_county_500k/cb_2022_us_county_500k.shp")

# Filter for just Washington state
wa_state = us_states[us_states['NAME'] == 'Washington'].copy()
wa_counties = us_counties[us_counties['STATE_NAME'] == 'Washington']
wa_counties

# extract two colors from the 'viridis' colormap
color_map_values = [0, 0.5]  # Start and end of the colormap
colors_from_viridis = plt.cm.viridis(color_map_values)

# Convert to hexadecimal
colors_hex = [mcolors.to_hex(c) for c in colors_from_viridis]


# Plot Washington state
# Create a color map dictionary
color_map_dict = {'Dryland': colors_hex[0], 'Irrigated': colors_hex[1]}

# Map the colors to the DataFrame
df['color'] = df['Irrigation'].map(color_map_dict)

ax = wa_state.boundary.plot(figsize=(40, 20), linewidth=2)
wa_counties.boundary.plot(ax=ax, linewidth=1, edgecolor="black")
wa_counties.apply(lambda x: ax.annotate(text=x.NAME, xy=x.geometry.centroid.coords[0], ha='center', fontsize=16, color='black'), axis=1)

# Plot the points with the specified colors
for color in color_map_dict.values():
    subset = df[df['color'] == color]
    subset.plot(ax=ax, marker='o', color=color, markersize=300, label=subset['Irrigation'].unique()[0])

# Add a legend
handles, labels = ax.get_legend_handles_labels()
ax.legend(handles, labels, title='Irrigation Status')

# Add title and axis labels
plt.title("Washington State with County Boundaries and Points", fontsize=22)
plt.xlabel("Longitude", fontsize=14)
plt.ylabel("Latitude", fontsize=14)

# Show the plot
plt.figure(dpi=300)
plt.show()


# -

colors_hex[0], colors_hex[1]

# +
import pandas as pd
import numpy as np
import seaborn as sns

# Load your data into a Pandas DataFrame
df = df[selected_cols].copy()

# Drop columns with just one value
# df.drop(columns= df.nunique()[df.nunique() == 1].index[0], inplace=True )

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Create a new column in the DataFrame to indicate whether each row is in the top, middle, or bottom tercile
df['tercile'] = pd.cut(df[y_var], bins=[df[y_var].min(
), bottom_tercile, top_tercile, df[y_var].max()], labels=['bottom', 'middle', 'top'])

# Loop through each x-variable and create a density distribution plot for the top, middle, and bottom terciles
for x_var in df.columns.drop([y_var, 'tercile']):
    g = sns.FacetGrid(df[df['tercile'] != 'middle'], hue='tercile', height=4, aspect=1.2)
    g.map(sns.kdeplot, x_var, shade=True)
    g.add_legend()

# -

df1.loc[df['tercile'] == 'top']['Total_C_g/cm2'].describe()

# +
Y = df1['Total_C_g/cm2']
X = df1[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']]
# X = df_second[['B12_second']]

X = sm.add_constant(X)

model = sm.OLS(Y, X).fit()
print(model.summary())

# +
import seaborn as sns

# calculate the correlation matrix
corr_matrix = df[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']].corr()


# plot the correlation matrix as a heatmap
plt.figure(figsize=(12, 10))
sns.heatmap(corr_matrix, cmap='coolwarm', annot=True)

# show the plot
plt.show()

# -

selected_cols = ['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second', 'Total_C_g/cm2']

df = df1[selected_cols]
df.reset_index(inplace=True)

# +
import pandas as pd
import numpy as np
import seaborn as sns

# Load your data into a Pandas DataFrame
df = df1[selected_cols]

# # Drop columns with just one value
# df.drop(columns= df.nunique()[df.nunique() == 1].index[0], inplace=True )

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Create a new column in the DataFrame to indicate whether each row is in the top, middle, or bottom tercile
df['tercile'] = pd.cut(df[y_var], bins=[df[y_var].min(
), bottom_tercile, top_tercile, df[y_var].max()], labels=['bottom', 'middle', 'top'])

# Loop through each x-variable and create a density distribution plot for the top, middle, and bottom terciles
for x_var in df.columns.drop([y_var, 'tercile']):
    g = sns.FacetGrid(df[df['tercile'] != 'middle'], hue='tercile', height=4, aspect=1.2)
    g.map(sns.kdeplot, x_var, shade=True)
    g.add_legend()

# +
Y = df_second['Total_C_g/cm2']
X = df_second[['NDVI_second', 'tvi_second',
               'savi_second', 'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
               'MSAVI2_second', 'BI_second', 'BI2_second', 'RI_second',
               'CI_second',
               'B2_second', 'B3_second', 'B4_second', 'B8_second', 'B11_second',
               'B12_second']]

X = sm.add_constant(X)

model = sm.OLS(Y, X).fit()
print(model.summary())

# +
import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix

# Load your data into a Pandas DataFrame
df = df1[selected_cols]

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Subset the DataFrame to include only top and bottom tercile rows
df_terciles = df[(df[y_var] <= bottom_tercile) |
                 (df[y_var] >= top_tercile)].copy()

# Create a new column for the target variable ('high' or 'low') based on tercile membership
df_terciles['target'] = np.where(
    df_terciles[y_var] >= top_tercile, 'high', 'low')

# Select only the X variables of interest
# Replace with the actual X variable names
X_terciles = df_terciles[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']]
y_terciles = df_terciles['target']

# from sklearn.model_selection import train_test_split

# # Split the data into training and testing sets
# X_train, X_test, y_train, y_test = train_test_split(
#     X_terciles, y_terciles, test_size=0.25, random_state=42)

from sklearn.linear_model import LogisticRegression

# Initialize the classifier
classifier = LogisticRegression()

# Perform cross-validation
cv_scores = cross_val_score(classifier, X_terciles, y_terciles, cv=5)

# Print the cross-validation scores
print("Cross-Validation Scores:", cv_scores)
print("Average Cross-Validation Score:", np.mean(cv_scores))

# Train the classifier on the entire data
classifier.fit(X_terciles, y_terciles)

# Make predictions on the testing data
y_pred = classifier.predict(X_terciles)

# Generate a contingency table
contingency_table = pd.crosstab(y_terciles, y_pred, rownames=['Actual'], colnames=['Predicted'])

print(contingency_table)

# +
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import cross_val_score, train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix, accuracy_score

# Load your data into a Pandas DataFrame
df = df1[selected_cols]

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Subset the DataFrame to include only top and bottom tercile rows
df_terciles = df[(df[y_var] <= bottom_tercile) |
                 (df[y_var] >= top_tercile)].copy()

# Create a new column for the target variable ('high' or 'low') based on tercile membership
df_terciles['target'] = np.where(
    df_terciles[y_var] >= top_tercile, 'high', 'low')

# Select only the X variables of interest
# Replace with the actual X variable names
X_terciles = df_terciles[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']]
y_terciles = df_terciles['target']

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(
    X_terciles, y_terciles, test_size=0.25, random_state=42)

# Initialize the classifier
classifier = RandomForestClassifier()

# Perform cross-validation
cv_scores = cross_val_score(classifier, X_train, y_train, cv=3)

# Print the cross-validation scores
print("Cross-Validation Scores:", cv_scores)
print("Average Cross-Validation Score:", np.mean(cv_scores))

# Train the classifier on the training data
classifier.fit(X_train, y_train)

# Make predictions on the testing data
y_pred = classifier.predict(X_test)

# Calculate the accuracy score
test_score = accuracy_score(y_test, y_pred)

# Generate a confusion matrix
conf_matrix = confusion_matrix(y_test, y_pred)

# Create a DataFrame from the confusion matrix
confusion_df = pd.DataFrame(conf_matrix, index=['Actual low', 'Actual high'], columns=['Predicted low', 'Predicted high'])

# Plot the confusion matrix
plt.figure(figsize=(8, 6))
sns.heatmap(confusion_df, annot=True, fmt='d', cmap='Blues')
plt.title("Confusion Matrix (Test Set)")
plt.xlabel("Predicted")
plt.ylabel("Actual")
plt.show()

# Print the test score
print("Test Score:", test_score)

# +
import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix

# Load your data into a Pandas DataFrame
df = df_second[selected_cols]

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Subset the DataFrame to include only top and bottom tercile rows
df_terciles = df[(df[y_var] <= bottom_tercile) |
                 (df[y_var] >= top_tercile)].copy()

# Create a new column for the target variable ('high' or 'low') based on tercile membership
df_terciles['target'] = np.where(
    df_terciles[y_var] >= top_tercile, 'high', 'low')

# Select only the X variables of interest
# Replace with the actual X variable names
X_terciles = df_terciles[['NDVI_second', 'tvi_second',
                          'savi_second', 'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
                          'MSAVI2_second', 'BI_second', 'BI2_second', 'RI_second',
                          'CI_second', 'B1_second', 'B2_second', 'B3_second',
                          'B4_second', 'B8_second', 'B11_second', 'B12_second']]
y_terciles = df_terciles['target']

# from sklearn.model_selection import train_test_split

# # Split the data into training and testing sets
# X_train, X_test, y_train, y_test = train_test_split(
#     X_terciles, y_terciles, test_size=0.25, random_state=42)

from sklearn.linear_model import LogisticRegression

# Initialize the classifier
classifier = LogisticRegression()

# Perform cross-validation
cv_scores = cross_val_score(classifier, X_terciles, y_terciles, cv=5)

# Print the cross-validation scores
print("Cross-Validation Scores:", cv_scores)
print("Average Cross-Validation Score:", np.mean(cv_scores))

# Train the classifier on the entire data
classifier.fit(X_terciles, y_terciles)

# Make predictions on the testing data
y_pred = classifier.predict(X_terciles)

# Generate a contingency table
contingency_table = pd.crosstab(y_terciles, y_pred, rownames=['Actual'], colnames=['Predicted'])

print(contingency_table)

# -

df1 = df.loc[~df.SampleID.duplicated()]
df1.loc[df1.SampleID.isin(averaged_C.SampleID), 'Total_C_g/cm2'] = averaged_C['Total_C_g/cm2'].values
df1.loc[df1.SampleID.isin(averaged_C.SampleID), 'Total_C_g/cm2']

df1.columns

# +
# Normalize band values
from sklearn.preprocessing import StandardScaler
# assuming df is your pandas dataframe
scaler = StandardScaler()

# select the columns you want to normalize
cols_to_normalize = ['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']

# fit the scaler on the selected columns
scaler.fit(df[cols_to_normalize])

# transform the selected columns to have zero mean and unit variance
df[cols_to_normalize] = scaler.transform(df[cols_to_normalize])

# -

df1.iloc[:, 8:]

# +
import matplotlib.pyplot as plt

# Increase the font size of the labels
plt.rcParams.update({'font.size': 12})

# Increase the resolution of the plot
plt.figure(figsize=(12, 8), dpi=300)

# Plot the density distribution of column 'Total_C_g/cm2'
df1['Total_C_g/cm2'].plot(kind='density')

# Set x-axis label
plt.xlabel('Total C (g/cm$^2$)', fontsize=14)

# Mark actual values on the curve
min_value = df1['Total_C_g/cm2'].min()
max_value = df1['Total_C_g/cm2'].max()

# Plotting the actual values on the curve
plt.axvline(x=min_value, color='red', linestyle='--', label='Min')
plt.axvline(x=max_value, color='blue', linestyle='--', label='Max')

# Display legend
plt.legend(fontsize=12)

# Show the plot
plt.show()

# -

df_first['Total_C_g/cm2'].describe()

df1.columns

# +
selected_cols = ['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second', 'Total_C_g/cm2']

df = df1[selected_cols]
df
# -

df.nunique()[df.nunique() == 1].index[0]



# +
import pandas as pd
import numpy as np
import seaborn as sns

# Load your data into a Pandas DataFrame
df = df1[selected_cols].copy()

# Drop columns with just one value
# df.drop(columns= df.nunique()[df.nunique() == 1].index[0], inplace=True )

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Create a new column in the DataFrame to indicate whether each row is in the top, middle, or bottom tercile
df['tercile'] = pd.cut(df[y_var], bins=[df[y_var].min(
), bottom_tercile, top_tercile, df[y_var].max()], labels=['bottom', 'middle', 'top'])

# Loop through each x-variable and create a density distribution plot for the top, middle, and bottom terciles
for x_var in df.columns.drop([y_var, 'tercile']):
    g = sns.FacetGrid(df[df['tercile'] != 'middle'], hue='tercile', height=4, aspect=1.2)
    g.map(sns.kdeplot, x_var, shade=True)
    g.add_legend()

# -

df1.loc[df['tercile'] == 'top']['Total_C_g/cm2'].describe()

# +
Y = df1['Total_C_g/cm2']
X = df1[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']]
# X = df_second[['B12_second']]

X = sm.add_constant(X)

model = sm.OLS(Y, X).fit()
print(model.summary())

# +
import seaborn as sns

# calculate the correlation matrix
corr_matrix = df[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']].corr()


# plot the correlation matrix as a heatmap
plt.figure(figsize=(12, 10))
sns.heatmap(corr_matrix, cmap='coolwarm', annot=True)

# show the plot
plt.show()

# -

selected_cols = ['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second', 'Total_C_g/cm2']

df = df1[selected_cols]
df.reset_index(inplace=True)

# +
import pandas as pd
import numpy as np
import seaborn as sns

# Load your data into a Pandas DataFrame
df = df1[selected_cols]

# # Drop columns with just one value
# df.drop(columns= df.nunique()[df.nunique() == 1].index[0], inplace=True )

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Create a new column in the DataFrame to indicate whether each row is in the top, middle, or bottom tercile
df['tercile'] = pd.cut(df[y_var], bins=[df[y_var].min(
), bottom_tercile, top_tercile, df[y_var].max()], labels=['bottom', 'middle', 'top'])

# Loop through each x-variable and create a density distribution plot for the top, middle, and bottom terciles
for x_var in df.columns.drop([y_var, 'tercile']):
    g = sns.FacetGrid(df[df['tercile'] != 'middle'], hue='tercile', height=4, aspect=1.2)
    g.map(sns.kdeplot, x_var, shade=True)
    g.add_legend()

# +
Y = df_second['Total_C_g/cm2']
X = df_second[['NDVI_second', 'tvi_second',
               'savi_second', 'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
               'MSAVI2_second', 'BI_second', 'BI2_second', 'RI_second',
               'CI_second',
               'B2_second', 'B3_second', 'B4_second', 'B8_second', 'B11_second',
               'B12_second']]

X = sm.add_constant(X)

model = sm.OLS(Y, X).fit()
print(model.summary())

# +
import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix

# Load your data into a Pandas DataFrame
df = df1[selected_cols]

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Subset the DataFrame to include only top and bottom tercile rows
df_terciles = df[(df[y_var] <= bottom_tercile) |
                 (df[y_var] >= top_tercile)].copy()

# Create a new column for the target variable ('high' or 'low') based on tercile membership
df_terciles['target'] = np.where(
    df_terciles[y_var] >= top_tercile, 'high', 'low')

# Select only the X variables of interest
# Replace with the actual X variable names
X_terciles = df_terciles[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']]
y_terciles = df_terciles['target']

# from sklearn.model_selection import train_test_split

# # Split the data into training and testing sets
# X_train, X_test, y_train, y_test = train_test_split(
#     X_terciles, y_terciles, test_size=0.25, random_state=42)

from sklearn.linear_model import LogisticRegression

# Initialize the classifier
classifier = LogisticRegression()

# Perform cross-validation
cv_scores = cross_val_score(classifier, X_terciles, y_terciles, cv=5)

# Print the cross-validation scores
print("Cross-Validation Scores:", cv_scores)
print("Average Cross-Validation Score:", np.mean(cv_scores))

# Train the classifier on the entire data
classifier.fit(X_terciles, y_terciles)

# Make predictions on the testing data
y_pred = classifier.predict(X_terciles)

# Generate a contingency table
contingency_table = pd.crosstab(y_terciles, y_pred, rownames=['Actual'], colnames=['Predicted'])

print(contingency_table)

# +
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import cross_val_score, train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix, accuracy_score

# Load your data into a Pandas DataFrame
df = df1[selected_cols]

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Subset the DataFrame to include only top and bottom tercile rows
df_terciles = df[(df[y_var] <= bottom_tercile) |
                 (df[y_var] >= top_tercile)].copy()

# Create a new column for the target variable ('high' or 'low') based on tercile membership
df_terciles['target'] = np.where(
    df_terciles[y_var] >= top_tercile, 'high', 'low')

# Select only the X variables of interest
# Replace with the actual X variable names
X_terciles = df_terciles[['NDVI_first', 'tvi_first',
       'savi_first', 'MSI_first', 'GNDVI_first', 'GRVI_first', 'LSWI_first',
       'MSAVI2_first', 'WDVI_first', 'BI_first', 'BI2_first', 'RI_first',
       'CI_first', 'B1_first', 'B2_first', 'B3_first', 'B4_first', 'B8_first',
       'B11_first', 'B12_first', 'NDVI_second', 'tvi_second', 'savi_second',
       'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
       'MSAVI2_second', 'WDVI_second', 'BI_second', 'BI2_second', 'RI_second',
       'CI_second', 'B1_second', 'B2_second', 'B3_second', 'B4_second',
       'B8_second', 'B11_second', 'B12_second']]
y_terciles = df_terciles['target']

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(
    X_terciles, y_terciles, test_size=0.25, random_state=42)

# Initialize the classifier
classifier = RandomForestClassifier()

# Perform cross-validation
cv_scores = cross_val_score(classifier, X_train, y_train, cv=3)

# Print the cross-validation scores
print("Cross-Validation Scores:", cv_scores)
print("Average Cross-Validation Score:", np.mean(cv_scores))

# Train the classifier on the training data
classifier.fit(X_train, y_train)

# Make predictions on the testing data
y_pred = classifier.predict(X_test)

# Calculate the accuracy score
test_score = accuracy_score(y_test, y_pred)

# Generate a confusion matrix
conf_matrix = confusion_matrix(y_test, y_pred)

# Create a DataFrame from the confusion matrix
confusion_df = pd.DataFrame(conf_matrix, index=['Actual low', 'Actual high'], columns=['Predicted low', 'Predicted high'])

# Plot the confusion matrix
plt.figure(figsize=(8, 6))
sns.heatmap(confusion_df, annot=True, fmt='d', cmap='Blues')
plt.title("Confusion Matrix (Test Set)")
plt.xlabel("Predicted")
plt.ylabel("Actual")
plt.show()

# Print the test score
print("Test Score:", test_score)

# +
import pandas as pd
import numpy as np
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix

# Load your data into a Pandas DataFrame
df = df_second[selected_cols]

# Set the name of your y-variable
y_var = 'Total_C_g/cm2'

# Set the terciles to use for separating the data
bottom_tercile = np.percentile(df[y_var], 33.33)
top_tercile = np.percentile(df[y_var], 66.66)

# Subset the DataFrame to include only top and bottom tercile rows
df_terciles = df[(df[y_var] <= bottom_tercile) |
                 (df[y_var] >= top_tercile)].copy()

# Create a new column for the target variable ('high' or 'low') based on tercile membership
df_terciles['target'] = np.where(
    df_terciles[y_var] >= top_tercile, 'high', 'low')

# Select only the X variables of interest
# Replace with the actual X variable names
X_terciles = df_terciles[['NDVI_second', 'tvi_second',
                          'savi_second', 'MSI_second', 'GNDVI_second', 'GRVI_second', 'LSWI_second',
                          'MSAVI2_second', 'BI_second', 'BI2_second', 'RI_second',
                          'CI_second', 'B1_second', 'B2_second', 'B3_second',
                          'B4_second', 'B8_second', 'B11_second', 'B12_second']]
y_terciles = df_terciles['target']

# from sklearn.model_selection import train_test_split

# # Split the data into training and testing sets
# X_train, X_test, y_train, y_test = train_test_split(
#     X_terciles, y_terciles, test_size=0.25, random_state=42)

from sklearn.linear_model import LogisticRegression

# Initialize the classifier
classifier = LogisticRegression()

# Perform cross-validation
cv_scores = cross_val_score(classifier, X_terciles, y_terciles, cv=5)

# Print the cross-validation scores
print("Cross-Validation Scores:", cv_scores)
print("Average Cross-Validation Score:", np.mean(cv_scores))

# Train the classifier on the entire data
classifier.fit(X_terciles, y_terciles)

# Make predictions on the testing data
y_pred = classifier.predict(X_terciles)

# Generate a contingency table
contingency_table = pd.crosstab(y_terciles, y_pred, rownames=['Actual'], colnames=['Predicted'])

print(contingency_table)

