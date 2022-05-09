import pandas as pd
import json
import numpy as np
import matplotlib.pyplot as plt
import os

# use shift+alt+e to run a line
# get logged user
print(os.getlogin())
# get path to materials
path = os.path.join('C:\\Users',os.getlogin(),'Downloads',
                    'pydata-book-2nd-edition')
print(path)
data_dir = os.path.join(path, 'datasets', 'bitly_usagov', 'example.txt')
data_dir
# read lines
open(data_dir).readlines()
# using json
records = [json.loads(line) for line in open(data_dir)]
# counting time zones in record
time_zones = [rec['tz'] for rec in records]
# not all records have a time zone, use if condition
time_zones = [rec['tz'] for rec in records if 'tz' in rec]
time_zones[:10]
# getting counts - harder way


def get_counts(sequence):
    counts = {}
    for x in sequence:
        if x in counts:
            counts[x] += 1
        else:
            counts[x] = 1
    return counts


counts = get_counts(time_zones)
# some acrobatics for top 1o time zones and their counts


def top_counts(count_dict, n = 10):
    value_key_pairs = [(count, tz) for tz, count in count_dict.items()]
    value_key_pairs.sort()
    return value_key_pairs[-n:]


top_counts(counts)
# counting timezones with pandas
from pandas import DataFrame, Series
frame = DataFrame(records)
tz_counts = frame['tz'].value_counts()
# we may want to plot the conts with a plotting library
tz_counts[:10].plot(kind='barh', rot=0)
