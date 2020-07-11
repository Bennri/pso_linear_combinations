import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder
import json

url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
names = ['sepal-length', 'sepal-width', 'petal-length', 'petal-width', 'class']
dataset = pd.read_csv(url, names=names)

two_classes = dataset[dataset['class'] == 'Iris-setosa'].append(dataset[dataset['class'] == 'Iris-virginica'])

short_dataset = two_classes.loc[:, ['sepal-length', 'sepal-width', 'class']]

short_labelencoder = LabelEncoder()
short_labelencoder = short_labelencoder.fit(short_dataset['class'])
short_labelencoder.inverse_transform([0, 1])

label_dict = {0: 'Iris-setosa', 1: 'Iris-virginica'}

# numeric_shorten_labels
short_dataset.loc[short_dataset['class'] == 'Iris-setosa', 'class'] = 0
short_dataset.loc[short_dataset['class'] == 'Iris-virginica', 'class'] = 1

# no train / test split since the data set is only used to evaluate the PSO algorithm
short_dataset.to_csv('data_2_class_Iris-setosa_Iris-virginica.csv', sep=',', index=False)