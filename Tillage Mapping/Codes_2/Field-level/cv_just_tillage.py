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
#     display_name: tillmap
#     language: python
#     name: python3
# ---

# +
import pandas as pd

# Read data
# path_to_data = ("/Users/aminnorouzi/Library/CloudStorage/"
#                 "OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/Projects/"
#                 "Tillage_Mapping/Data/")

path_to_data = ("/home/amnnrz/OneDrive - "
                "a.norouzikandelati/Ph.D/Projects/Tillage_Mapping/Data/")

df_2122 = pd.read_csv(
    path_to_data + 'field_level_data/field_level_main_glcm_seasonBased_joined_2122.csv',
    index_col=0)
df_2223 = pd.read_csv(
    path_to_data + 'field_level_data/field_level_main_glcm_seasonBased_joined_2223.csv',
    index_col=0)
cdl_2122 = pd.read_csv(path_to_data + 'cdl_data/cdl_2122.csv')


# Rename df_2223 PriorCropT labels
replacement_dict = {
    'Grain':'grain', 
    'Canola':'canola', 
    'Legume':'legume'
}

df_2223['PriorCropT'] = df_2223['PriorCropT'].replace(
    replacement_dict
)

df_2223 = df_2223.loc[
    df_2223['PriorCropT'].isin(['grain', 'canola', 'legume'])
]

df = pd.concat([df_2122, df_2223])
cdl = cdl_2122[['pointID', 'most_frequent_class']].copy()
cdl.rename(columns={'most_frequent_class': 'PriorCropT'}, inplace=True)


# Rename cdl labels
replacement_dict = {24: 'grain', 23: 'grain', 51: 'legume',
                    51: 'legume', 31: 'canola', 53: 'legume',
                    21: 'grain', 51: 'legume', 52: 'legume',
                    28: 'grain'}

cdl['PriorCropT'] = cdl['PriorCropT'].replace(
    replacement_dict)

cdl = cdl.loc[cdl['PriorCropT'].isin(
                    ['grain', 'legume', 'canola'])]

df_2122.drop(columns='PriorCropT', inplace=True)
df_2122 = pd.merge(df_2122, cdl, on='pointID')

# Define the new order of columns
cols = list(df_2122.columns)
cols
cols.remove('PriorCropT')
cols.insert(3, 'PriorCropT')

# Reindex the DataFrame with the new column order
df_2122 = df_2122[cols]

df_2122['PriorCropT'].isna().value_counts()

df1 = pd.concat([df_2122, df_2223])

df1

# -
df_2223['PriorCropT'].value_counts()

#

y_train.value_counts(
)

calculate_custom_weights(y_train, a = 2)

# +
from sklearn.base import BaseEstimator, ClassifierMixin
from sklearn.ensemble import RandomForestClassifier

# Custom weight formula function
def calculate_custom_weights(y, a):
    unique_classes, class_counts = np.unique(y, return_counts=True)
    weight_dict = {}
    sum_weight = np.sum((1 / class_counts) ** a)
    for cls, cnt in zip(unique_classes, class_counts):
        weight_dict[cls] = (1 / cnt) ** a / sum_weight
    return weight_dict

class CustomWeightedRF(BaseEstimator, ClassifierMixin):
    def __init__(self, n_estimators=100, max_depth=None, a=1, **kwargs):
        self.n_estimators = n_estimators
        self.max_depth = max_depth
        self.a = a
        self.rf = RandomForestClassifier(n_estimators=self.n_estimators,
                                         max_depth=self.max_depth, **kwargs)
    
    def fit(self, X, y, **kwargs):
        target_weights_dict = calculate_custom_weights(y, self.a)
        target_weights = np.array([target_weights_dict[sample] for sample in y])

        # Rest of the weight calculation can stay same
        feature_cols = ['PriorCropT']
        feature_weights = np.zeros(X.shape[0])
        for col in feature_cols:
            feature_weights_dict = calculate_custom_weights(X[col].values, self.a)
            feature_weights += X[col].map(feature_weights_dict).values

        sample_weights = target_weights * feature_weights
        
        # Now fit the RandomForestClassifier with the computed weights
        self.rf.fit(X, y, sample_weight=sample_weights)
        return self
    
    def predict(self, X, **kwargs):
        return self.rf.predict(X)
    
    def predict_proba(self, X, **kwargs):
        return self.rf.predict_proba(X)
    
    @property
    def feature_importances_(self):
        return self.rf.feature_importances_


# -

df_encoded['Tillage'].value_counts()

df1['PriorCropT'].value_counts()

df1['Tillage']

df_ordered['Tillage']

# +
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV, cross_val_score, train_test_split
from sklearn.metrics import confusion_matrix, accuracy_score
import seaborn as sns
import matplotlib.pyplot as plt
# from imblearn.over_sampling import RandomOverSampler
from collections import Counter
from sklearn.preprocessing import LabelEncoder

# Load your dataframe with categorical features
df = df1

# Perform one-hot encoding for "Residue Cover" features
df_encoded = pd.get_dummies(df, columns=['ResidueCov'])

# Encode "PriorCropT"
encode_dict = {
    'grain': 1,
    'legume': 2,
    'canola': 3
}
df_encoded['PriorCropT'] = df_encoded['PriorCropT'].replace(encode_dict)
df_encoded

# Place the one-hot encoded columns on the left side of the dataframe
ordered_columns = list(df_encoded.columns.difference(df.columns)) + \
[col for col in df.columns if col not in ['ResidueCov']]
df_ordered = df_encoded[ordered_columns]

# Remove NA from Tillage
df_ordered = df_ordered.dropna(subset="Tillage")

le = LabelEncoder()
df_ordered['Tillage'] = le.fit_transform(df_ordered['Tillage'])

# Split features and target variable
X = df_ordered.iloc[:, np.concatenate(
    [np.arange(0, 3), np.arange(6,7), np.arange(9, df_ordered.shape[1])]
    )]
    
y = df_ordered['Tillage']

# Impute missing values with the median
X = X.fillna(X.median())



# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

param_grid = {
    'n_estimators': [20, 30, 40, 50, 100, 300],
    'max_depth': [5, 10, 20, 30, 45],
    # 'a' : [0.5, 10, 100]
    'a': [0.1]
}

# Perform cross-validation for 20 times and calculate accuracies
mean_accuracies = []
best_model = None
best_val_accuracy = 0
feature_counter = Counter()  # Counter to keep track of feature occurrences

# Initialize a list to store mean test scores for each hyperparameter combination
mean_test_scores = []

for _ in range(1):
    
    grid_search = GridSearchCV(
        CustomWeightedRF(), param_grid, cv=5, return_train_score=False)
    grid_search.fit(X_train, y_train)

    # Store mean test scores in the list
    mean_test_scores.append(grid_search.cv_results_['mean_test_score'])

    # Get the best model and its predictions
    current_model = grid_search.best_estimator_
    y_pred = current_model.predict(X_test)  # Use the test data for prediction

    # Calculate the accuracy for the current run
    val_accuracy = accuracy_score(y_test, y_pred)
    print(_, ":", "Validation Accuracy is ", val_accuracy)
    mean_accuracies.append(val_accuracy)

    # Update the best model if the current model has a higher validation accuracy
    if val_accuracy > best_val_accuracy:
        best_model = current_model
        best_val_accuracy = val_accuracy

    # Update the feature counter with the top 20 important features of the current model
    top_20_indices = current_model.feature_importances_.argsort()[::-1][:50]
    top_20_features = X.columns[top_20_indices]
    feature_counter.update(top_20_features)

# Calculate mean accuracy across the 20 runs
mean_accuracy = sum(mean_accuracies) / len(mean_accuracies)

# Print accuracies for all cross-validations
print("Accuracies for all cross-validations:")
for i, accuracy in enumerate(mean_accuracies, 1):
    print(f"Cross-Validation {i}: {accuracy:.4f}")

# Print mean accuracy
print(f"Mean Accuracy: {mean_accuracy:.4f}")

# Create a confusion matrix using predictions from the best model
y_pred_best = best_model.predict(X_test)
cm = confusion_matrix(y_test, y_pred_best)

# Plot the confusion matrix
labels = ['ConventionalTill', 'MinimumTill', 'NoTill-DirectSeed']
# labels = ['MinimumTill', 'NoTill-DirectSeed']
plt.figure(figsize=(8, 6))
plt.imshow(cm, interpolation='nearest', cmap=plt.cm.Blues)
plt.title('Confusion Matrix')
plt.colorbar()

tick_marks = np.arange(len(labels))
plt.xticks(tick_marks, labels, rotation=45)
plt.yticks(tick_marks, labels)

plt.ylabel('True label')
plt.xlabel('Predicted label')

# Displaying the values in the cells
for i in range(cm.shape[0]):
    for j in range(cm.shape[1]):
        plt.text(j, i, format(cm[i, j], 'd'),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > cm.max() / 2 else "black")

plt.tight_layout()
plt.show()


# Print the features that appeared most frequently in the top 20 important features
most_common_features = feature_counter.most_common()
print("Features that appeared most frequently in the top 20 important features:")
for feature, count in most_common_features:
    print(f"{feature}: {count} times")

# Plot the 20 most important features over all runs
top_20_features = [feature[0] for feature in most_common_features[:20]]
top_20_importances = [feature[1] for feature in most_common_features[:20]]

plt.figure(figsize=(10, 8))
plt.barh(top_20_features, top_20_importances)
plt.xlabel('Importance')
plt.ylabel('Features')
plt.title('Top 20 Most Important Features')
plt.show()

# After printing important features, plot the boxplot for validation accuracies
plt.figure(figsize=(10, 8))
plt.boxplot(mean_test_scores, vert=False)
plt.xlabel('Mean Cross-Validated Accuracy')
plt.ylabel('Hyperparameter Combination')
plt.title('Boxplot of Validation Accuracies for each Hyperparameter Combination')
plt.show()
