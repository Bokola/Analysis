# -*- coding: utf-8 -*-
"""Copy of Copy of CatboostModelling with GDrive Data.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1E3Gk2JQlWFoB6BrFlmLMRbeaT85YHD4J
"""

#!pip install catboost

#Import required libraries and modules
import pandas as pd
import numpy as np
from catboost import CatBoostClassifier as catclass
#from google.colab import files
import io



from sklearn.model_selection import train_test_split
from catboost import CatBoostClassifier, Pool, cv
#from google.colab import drive

#drive.mount("/content/gdrive", force_remount=True)

import os
#colab_path=[x[0]  for x in os.walk('/content/gdrive') if "Capstone Project" in x[0]];colab_path
colab_path=[x[0]  for x in os.walk('C:/Users/bokola/Analysis') if "capstone" in x[0]];colab_path
os.chdir(colab_path[3])

train_values = pd.read_csv("train_values.csv")
test_values = pd.read_csv("test_values.csv")
train_labels = pd.read_csv("train_labels.csv")

test_values.head(n=2)

train_labels.head(n=2)

train_values.head(n=2)

#Make rows Strings
train_values["row_id"]=train_values["row_id"].astype("object")
train_labels["row_id"]=train_labels["row_id"].astype("object")
test_values["row_id"]=test_values["row_id"].astype("object")

train_values["co_applicant"]=train_values["co_applicant"].astype("int64")
test_values["co_applicant"]=test_values["co_applicant"].astype("int64")

#train_values["minority_population"]=train_values["minority_population_pct"]*train_values["population"]/100
#test_values["minority_population"]=test_values["minority_population_pct"]*test_values["population"]/100

#train_values["tract_to_msa_md_income"]=train_values["tract_to_msa_md_income_pct"]*train_values["ffiecmedian_family_income"]/100
#test_values["tract_to_msa_md_income"]=test_values["tract_to_msa_md_income_pct"]*test_values["ffiecmedian_family_income"]/100

#Select Numeric Columns
numcols = ['population', 'minority_population_pct',
       'ffiecmedian_family_income', 'tract_to_msa_md_income_pct',
       'number_of_owner-occupied_units', 'number_of_1_to_4_family_units']

numcols1 = ['loan_amount','applicant_income']

for col in numcols1:
  bins_train  = [train_values[col].min(), train_values[col].quantile(.50),train_values[col].quantile(.75), train_values[col].max()]
  bins_test  = [test_values[col].min(), test_values[col].quantile(.50),test_values[col].quantile(.75), test_values[col].max()]

  train_values[col] = pd.cut(train_values[col],bins_train, labels =["1","2","3"]).astype("category")

  test_values[col] = pd.cut(test_values[col],bins_test, labels =["1","2","3"]).astype("category")

test_values.head()

train_values.head(n= 2)

for col in numcols1:
  train_values[col] = pd.to_numeric(train_values[col], errors='coerce')
  test_values[col] =  pd.to_numeric(test_values[col], errors='coerce')

#Replace Missing Values for numeric columns with mean of each
for numcol in numcols:
    train_values[numcol] = train_values[numcol].fillna(-999)

    test_values[numcol] = test_values[numcol].fillna(-999)



for numcol in numcols1:
    train_values[numcol] = train_values[numcol].fillna(-1)

    test_values[numcol] = test_values[numcol].fillna(-1)

    #train_values[numcol] = train_values[numcol].cat.add_categories("-1").fillna("-1")
    #test_values[numcol] = test_values[numcol].cat.add_categories("-1").fillna("-1")



train_values.isnull().sum()[0:10]

train_values.head(n=2)

test_values.head(n=2)

test_values_clean = test_values
train_values_clean =train_values

all_train = pd.merge(train_values_clean,train_labels, on="row_id", how="inner")

y_labs = train_labels["accepted"]


train_values_clean_1 = train_values_clean.loc[:, ~train_values_clean.columns.isin(['row_id'])]
test_values_clean_1 = test_values_clean.loc[:, ~test_values_clean.columns.isin(['row_id'])]

#Names of categorical columns in a list for looping on two datasets
catcols = ['loan_type', 'property_type', 'loan_purpose', 'occupancy','preapproval', 'msa_md', 'state_code', 'county_code',
       'applicant_ethnicity', 'applicant_race', 'lender','applicant_sex', 'co_applicant', 'loan_amount', 'applicant_income']

category_index =  [train_values_clean_1.columns.get_loc(c) for c in catcols if c!= "accepted"]; category_index

xtrain,xtest,ytrain,ytest = train_test_split(train_values_clean_1,y_labs,test_size=0.22)

model_cat = CatBoostClassifier(iterations= 1000, depth=8, learning_rate=0.4,eval_metric='Accuracy',use_best_model=True, random_seed=1920)

model_cat.fit(xtrain,ytrain,cat_features=category_index,eval_set=(xtest,ytest))

pred = model_cat.predict(test_values_clean_1)

import sklearn.metrics as metmod

def score_model(probs, threshold):
    return np.array([1 if x > threshold else 0 for x in probs[:,1]])

def key_metrics(labels, probs, threshold):
    scores = score_model(probs, threshold)
    metrics = metmod.precision_recall_fscore_support(labels, scores)
    conf = metmod.confusion_matrix(labels, scores)
    print('                 Confusion matrix')
    print('                 Score positive    Score negative')
    print('Actual positive    {}'.format(conf[0,0]) + '             {}'.format(conf[0,1]))
    print('Actual negative     {}'.format(conf[1,0]) + '             {}'.format(conf[1,1]))
    print('')
    print('Accuracy        {}'.format(round(metmod.accuracy_score(labels, scores), 2)))
    print('AUC              {}'.format(round(metmod.roc_auc_score(labels, probs[:,1]), 2)))
    print('Macro precision {}'.format(round(float((float(metrics[0][0]) + float(metrics[0][1]))/2.0), 2)))
    print('Macro recall    {}'.format(round(float((float(metrics[1][0]) + float(metrics[1][1]))/2.0), 2)))
    print(' ')
    print('           Positive      Negative')
    print('Num case   {}'.format(round(metrics[3][0], 2)) + '        {}'.format(round(metrics[3][1], 2)))
    print('Precision  {}'.format(round(metrics[0][0], 2)) + '         {}'.format(round(metrics[0][1], 2)))
    print('Recall      {}'.format(round(metrics[1][0],2)) + '        {}'.format(round(metrics[1][1], 2)))
    print('F1         {}'.format(round(metrics[2][0], 2)) + '         {}' .format(round(metrics[2][1],2)))

probabilities = model_cat.predict_proba(xtest)
key_metrics(ytest, probabilities, 0.5)
a = key_metrics(ytest, probabilities, 0.5)
a
pred_df  = pd.DataFrame({'row_id':test_values.row_id ,'accepted': pred})
pred_df['accepted'] = pred_df.accepted.astype('int64')


pred_df.to_csv('predicted.csv', index=False)
#files.download('predicted.csv')
