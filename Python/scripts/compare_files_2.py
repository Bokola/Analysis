# -*- coding: utf-8 -*-
"""
Created on Mon Jan 14 14:33:23 2019

@author: BOkola
"""
import csv
import glob
import os
import itertools

def get_data(fname):
    """
    Load a .csv file
    Returns a dict of {'exchange':float(price)}
    """
    with open(fname, 'rt') as inf:
        items = (row.split() for row in csv.reader(inf))
        return {item[0]:float(item[1]) for item in items}

def do_compare(a_name, a_data, b_name, b_data):
    """
    Compare two data files of {'key': float(value)}

    Returns a list of
      - the name of the first file
      - the name of the second file
      - the number of keys in A which are not in B
      - the number of keys in B which are not in A
      - the number of values in A less than the corresponding value in B
      - the number of values in A equal to the corresponding value in B
      - the number of values in A greater than the corresponding value in B
    """
    a_keys = set(a_data.iterkeys())
    b_keys = set(b_data.iterkeys())

    unique_to_a = len(a_keys - b_keys)
    unique_to_b = len(b_keys - a_keys)

    lt,eq,gt = 0,0,0
    pairs = ((a_data[key], b_data[key]) for key in a_keys & b_keys)
    for ai,bi in pairs:
        if ai < bi:
            lt +=1 
        elif ai == bi:
            eq += 1
        else:
            gt += 1

    return [a_name, b_name, unique_to_a, unique_to_b, lt, eq, gt]

def main():
    os.chdir(r'F:\data\Neonatal' )

    # load data from csv files
    data = {}
    for fname in glob.glob("*.csv"):
        data[fname] = get_data(fname)

    # do comparison
    files = data.keys()
    files.sort()
    with open('summary.csv', 'wb') as outf:
        outcsv = csv.writer(outf)
        outcsv.writerow(["File A", "File B", "Unique to A", "Unique to B", "A<B", "A==B", "A>B"])
        for a,b in itertools.combinations(files, 2):
            outcsv.writerow(do_compare(a, data[a], b, data[b]))

if __name__=="__main__":
    main()

?open
#If your files are small, you could do something basic like this
data = dict()
for fname in os.listdir(csvDir):
    with open(fname, 'rb') as fin:
        data[fname] = dict((key, value) for key, value in fin.readlines())
# All the data is now loaded into your data dictionary
# data -> {'file1.csv': {1201007: 0.006, 1201032: 0.0119, 1201040: 0.0106}, 'file2.csv': ...}
Now everything is readily accessible for you to compare keys and their resultant values in your data dictionary. 
Otherwise, if you have much larger datasets to work with that might not be loadable in memory you might want to consider just working with 2 files at a time, with one being stored in memory. You can create a list of filename combinations with itertools.combinations which is you called like combinations(filenames, 2) would yield you a 2 filename pair out of unique combinations you can use.
From there you can still optimize further but that should get you going
You still need to think about what to do with duplicate records - EG, what if file1 has 1234567,0.1 twice, and so does file2? And what if file1 has 3 of them, and file2 has 5 - and vice-versa?
http://en.literateprograms.org/Merge_sort_%28Python%29
http://stromberg.dnsalias.org/~strombrg/sort-comparison/
http://en.wikipedia.org/wiki/Merge_sort
import os 
?os.listdir

