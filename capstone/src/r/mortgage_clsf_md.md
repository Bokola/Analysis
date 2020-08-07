**Executive Summary**
=====================

The objective is Predicting Mortgage Approvals From Government Data,
taking into consideration the **demographics, location, property type,
lender**, and other factors to predict whether a mortgage application
was accepted or denied. It is a classification problem - A classifier is
a machine learning model that separates the **label** into categories or
**classes**. In other words, classification models are **supervised**
machine learning models which predict a categorical label. We apply
relevant skills to classify the label **`accepted`** - whether a
mortgage application was accepted or declined using data obtained across
the United States.

We used a CatBoostclassifier model with 1000 iterations with 8
cross-validations with an accuracy of 0.72.

**Methodology**
===============

The section outlines data manipulation procedures, visualizations and
model selection.

**Description of Data**
-----------------------

Data consisted of 23 variables (21 possible features and 1 binary
label(`accepted`) and 1 arbitrary identifier(`row_id`). Both training
and test sets spanned 500000 records

There are a number of variables in the train and test sets with missing
values: applicant\_income - 39948, population - 22465,
minority\_population\_pct - 22466, ffiecmedian\_family\_income - 22440,
tract\_to\_msa\_md\_income\_pct - 22514,
number\_of\_owner.occupied\_units - 22565,
number\_of\_1\_to\_4\_family\_units - 22530. The missing entries were
recoded to -999 for numeric variables.

**Data Recoding and Visualization**
-----------------------------------

Variable recoding and visualization are key steps that ensure every
variable is of the desired class/type. Visualizaation is key to
understand the distribution and separation of values by the two levels
of a binary label.

### **Data Recoding**

Our data has a number of categorical featuers (`msa_md`,
`state_code`,`county_code`,`lender`,`loan_type`,`property_type`,
`loan_purpose`,
`occupancy`,`preapproval`,`applicant_ethnicity`,`applicant_race`,
`applicant_sex`) but which are read in as numeric. We begin by recoding
such variables into the desired character class and explicitly supplying
the categories in both train and test sets. We then visualize the
separation of values between different levels of the label -
`accepted(0,1)`.

### **Data Visualization**

1.  **Class separation by numeric variables**

We explore the distinction of the distribution of values between the two
levels of the label through boxplots.

![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-1.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-2.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-3.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-4.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-5.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-6.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-7.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-numerics-8.png)

From the plots, three variables - loan\_amount,
minority\_population\_pct and tract\_to\_msa\_md\_income\_pct show clear
separation of values between the two levels. Next we explore this
separation by categorical variables.

1.  **Class separation by categorical variables**

We explore the distinction of the distribution of values between the two
levels of the label through bar plots.

![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-1.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-2.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-3.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-4.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-5.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-6.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-7.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-8.png)![](mortgage_clsf_md_files/figure-markdown_strict/Data%20viz-9.png)

We observe the following:

1.  Some features such as property\_type, loan\_purpose,
    applicant\_ethnicity, applicant\_race and co\_applicant have an
    almost significant different distribution of categories between the
    label categories.

2.  Others features such as preapproval, applicant\_sex, occupancy show
    small differences, but these differences are unlikely to be
    significant.

Next we do feature engineering in readiness for modelling.

**Feature Engineering**
-----------------------

Data preparation is an important step. We recoded loan\_amount and
applicant\_income to categorical strings which help in distinguishing
levels within the dataset and imputed missing numeric variables with
means.

Feature engineering and modelling were done in python

**Model Building**
------------------

Modelling was done with python, selecting the CatBoostclassifier library
for the classification. As earlier indicated we settled on 8
cross-validations with 1000 iterations to minimize bias and variance of
the model. The classification model was defined as:
`model_cat = CatBoostClassifier(iterations=1000, depth=8, learning_rate=0.4, eval_metric='Accuracy', use_best_model=True, random_seed=2019)`
and fitted as:
`model_cat.fit(xtrain, ytrain, cat_features = category_index, eval_set = (xtest, ytest))`
with best test of .72 and best iteration at 159

**Results and Conclusions**
===========================

The model was able to correctly classify 72% of the accepted values, but
there were discripancies with the supplied test values indicating that
our model was overfitting. However, it offers a good starting point to
build on and improve.

                 Confusion matrix
                 Score positive    Score negative

Actual positive 36750 18402 Actual negative 12255 42593

Accuracy 0.72 AUC 0.8 Macro precision 0.72 Macro recall 0.72

           Positive      Negative

Num case 55152 54848 Precision 0.75 0.7 Recall 0.67 0.78 F1 0.71 0.74
