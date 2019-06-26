# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pandas as pd
df1 = pd.read_csv(r'C:\Users\bokola\Analysis\csvdiff_1\examples\majestic_million.csv')
df2 = pd.read_csv(r'C:\Users\bokola\Analysis\csvdiff_1\examples\majestic_million_diff.csv')
output = pd.merge(df1, df2, how="inner", on="Domain") #column_name should be common in both dataframe
#how represents type of intersection. In your case it will be inner(INNER JOIN)
output['count'] = output.groupby('Domain')['Domain'].transform('size') #pandas query
final_output = output.drop_duplicates() #It will remove duplicate rows 
