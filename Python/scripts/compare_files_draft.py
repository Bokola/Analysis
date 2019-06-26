# -*- coding: utf-8 -*-
"""
Created on Tue Jan 15 08:46:07 2019

@author: BOkola
"""

"""
importing data and merging data
"""
import glob
import pandas as pd
import csv
import glob
import os
import itertools
path =r'F:\data\Neonatal' # use your path
allFiles = glob.glob(path + "/*.csv")

list_ = []

for file_ in allFiles:
    df = pd.read_csv(file_,index_col=None, header=0)
    list_.append(df)

frame = pd.concat(list_, axis = 0, ignore_index = True); frame.head()
frame.info()
frame.describe()
frame.columns

"""
 all files together

"""

import os
from itertools import combinations

path = r'F:\data\Neonatal'
root, _, rel_filenames = next(os.walk(path))
full_filenames = [os.path.join(root, f) for f in rel_filenames]

"""

compare files

"""

for (file1, file2) in combinations(full_filenames, 2):
    with open(file1) as f1, open(file2) as f2:
         fileone = f1.readlines()
         filetwo = f2.readlines()
"""
    with open('difference.csv', 'w') as outFile:
      for line in filetwo:
        if line not in fileone:
            outFile.write(line)
 """
with open(file1) as t1:
    old_csv = t1.readlines()
with open(file2) as t2:
    new_csv = t2.readlines()     
with open('update.csv', 'w') as out_file:
    line_in_new = 0
    line_in_old = 0
while line_in_new < len(new_csv) and line_in_old < len(old_csv):
        if old_csv[line_in_old] != new_csv[line_in_new]:
            out_file.write(new_csv[line_in_new])
        else:
            line_in_old += 1
        line_in_new += 1       
       
import sys
import argparse
import csv


def get_dataset(f):
    return set(map(tuple, csv.reader(f)))


def main(f1, f2, outfile, sorting_column):
    set1 = get_dataset(f1)
    set2 = get_dataset(f2)
    different = set1 ^ set2

    output = csv.writer(outfile)

    for row in sorted(different, key=lambda x: x[sorting_column], reverse=True):
        output.writerow(row)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('infile', nargs=2, type=argparse.FileType('r'))
    parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'), default=sys.stdout)
    parser.add_argument('-sc', '--sorting-column', nargs='?', type=int, default=0)

    args = parser.parse_args()

    main(*args.infile, args.outfile, args.sorting_column)
