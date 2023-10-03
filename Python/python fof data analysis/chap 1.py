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

"""
Content of "log.txt":
10.1.2.1 - car [01/Mar/2022:13:05:05 +0900] "GET /python HTTP/1.0" 200 2222
10.1.1.9 - bike [01/Mar/2022:13:05:10 +0900] "GET /python HTTP/1.0" 200 2222

Expected output:
01/Mar/2022:13:05:05 +0900
01/Mar/2022:13:05:10 +0900
"""


def parse1() :
    for line in open("log.txt") :
        print(line.split("[")[1].split("]")[0])


def parse2() :
    for line in open("log.txt", "r") :
        print(line.split()[3].strip("[]"))


def parse3() :
    for line in open("log.txt", "r") :
        print(" ".join(line.split("[" or "]")[3 :5]))


def parse4() :
    for line in open("log.txt", "rw") :
        print(" ".join(line.split()[3 :5]).strip("[]"))


def parse5() :
    for line in open("log.txt") :
        print(re.split("\[|\]", line)[1])


def sqsum4():
  	return sum(x**2 for x in nums if x > 0)