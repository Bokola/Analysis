# -*- coding: utf-8 -*-
"""
Created on Tue Jan  8 10:01:49 2019

@author: BOkola
"""

## ** Numpy**
import numpy as np
ar1 =  np.array([0,1,2]) # 1 dim array
ar2 = np.array([[0,1,2], [3,4,5]]) # 2 dim array
ar2.shape # shape of array
ar2.ndim #dim
ar3 = np.arange(10); ar3 
ar4 = np.arange(3, 10, 3); ar4 # start, end (exclusive), step size
ar5 = np.linspace(0,5,3); ar5 # args - start element,end element, number of elements
ar7 = np.ones((2,3,2)); ar7 # Produces 2x3x2 array of 1's.
ar8 = np.zeros((4,2)); ar8 # 4x2 array of zeros
ar9 = np.eye((3)); ar9 # identity matrix
ar10 = np.diag((1,2,3,4)); ar10 # diagonal matrix
np.random.seed(99) # set seed
ar11 = np.random.rand(10); ar11 # draw a random sample
ar12 = np.empty((3,2)); ar12 # create unutilized array
ar13 = np.tile(np.array([[1,2],[3,4]]),4); ar13 # repeated elements of array
## Numpy data types
ar = np.array([2, -1, 5], dtype = 'float'); ar.dtype; ar
## coercion
ar_c = np.array([1, 3, 6.9]); ar_c.dtype
ar_c.astype(int); ar_c.dtype
## Numpy indexing and slicing
ar = np.arange(5); print(ar); ar[0], ar[1], ar[-1] # print entire array, element 0, element 1, last element
# arrays can be reversed using the '::-1' idiom
ar = np.arange(5); ar[::-1]
# multi-dimensional arrays can be indexed using tuples of integers
ar = np.array([[2,3,4],[9,8,7],[11,12,13]]); ar[1,1]
# we can also modify
ar; ar[1,1] = 5
ar[2] # retrieve row 2
ar[:,1] # retrieve col 1

# Array slicing -- ar[startIndex: endIndex(exclusive): stepValue]
ar = np.arange(6); ar[1:5:2]
ar[1:6:2] # going above last index to include it
ar[:4] # obtain the first n-elements using ar[:n]
ar[4:] # start at element 4 til end
ar[::3] # slice array with 'stepValue =3'
a = np.array([[0,1,2,3,4,5],[10,11,12,13,14,15],[20,21,22,23,24,25],[30,31,32,33,34,35],[40,41,42,43,44,45],[50,51,52,53,54,55]]);a
a[0,3:5]
a[4:,4:]
## Complex indexing
ar = np.arange(15)
ar2 = np.arange(0, -10, -1)[::-1] #reverse ordering
ar[:10] = ar2; ar # slice first 10 elements of ar, replace with ar2
## Copies and views
ar1 = np.arange(12)
ar2 = ar1[::2] # a view of ar1
ar2[1] = -1; ar1 # modifying a view modifies original array
# we use `np.copy` to force Numpy to copy an array
ar = np.arange(8)
arc = ar[:3].copy(); arc
arc[0] = -1; arc
ar # original array unaltered
## Operations
ar = np.arange(1000)
%timeit ar**3
np.prod(np.arange(1,5)) # element-wise products, same to np.sum(<for sums>)
##Statistical operators
## Array shape manipulation
ar = np.array([np.arange(1,6), np.arange(10,15)]); ar
ar.ravel() # flaten a multi-d array
ar.T.ravel() # transpose and ravel
## Reshaping
ar = np.arange(15).reshape(3,5); ar
## Resizing -- works if no additional references to array
ar = np.arange(1,8)
ar1 = ar
ar.resize((8,)) # fails, way around is .resize() function
np.resize(ar,(8,))
## Adding additional dim with np.newaxis()
ar = np.array([14, 15, 16]); ar.shape
ar = ar[:, np.newaxis]; ar.shape
# array sorting along an axis
ar = np.array([[3,2],[10, -1]])
ar.sort(axis = 1); ar

## Data Structures in Pandas - - Series, dataframe, panel
## Series<ndarray, python dictionary, scalar value>
import pandas as pd
# ser = pd.Series(data, index = idx)

#1. numpy.ndarray
import numpy as np
np.random.seed(99)
ser = pd.Series(np.random.rand(7)); ser
# a sewries of 1st 5 months
import calendar as cal
monthNames = [cal.month_name[i] for i in np.arange(1,6)]
months = pd.Series(np.arange(1,6), index = monthNames); months
# using python dictionary
currDict={'US' : 'dollar', 'UK' : 'pound','Germany': 'euro', 'Mexico':'peso','Nigeria':'naira','China':'yuan', 'Japan':'yen'}
currSeries = pd.Series(currDict); currSeries
# assigning indixes manually
stockPrices = {'GOOG' : 180.97,'FB':62.57,'TWTR': 64.50, 'AMZN':358.69,'AAPL':500.6}
stockPriceSeries = pd.Series(stockPrices, index = ['GOOG', 'FB', 'YHOO', 'TWTR', 'AMZN', 'AAPL'], name = 'stockPrices'); stockPriceSeries
# using scalar values -- an index must be provided, the value will be repeated for as many index values as possible
dogSeries=pd.Series('chihuahua', index=['breed','countryOfOrigin','name', 'gender']); dogSeries
# not providing an index returns a scalar value
dogSeries = pd.Series('pekingese'); dogSeries

## operations on Series
# vals are set and assigned using the index label in a dictionary - like manner...
currDict["China"]
stockPriceSeries["goog"] = 1200.0 # add a label
stockPriceSeries.drop(labels = "goog") # drop a labels
## Slicing 
# behaves as Numpy array:
stockPriceSeries[stockPriceSeries > 1000]
# other operations 
# element-wise operations can be done on series
ser; ser[:-2]
ser**ser

## DataFrames
# creation 
#1. using dictionaries  of Series
                                    'Market Cap(B)','Beta'])}
