#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul  8 15:44:28 2021

@author: claytonfields
"""

import pandas as pd
import os
import re
# os.chdir('CS538/Assignments')

read_filepath = '/Users/claytonfields/Research/R/cbsa-asthma-raw.txt'
write_filepath = '/Users/claytonfields/Research/R/cbsa-asthma.csv'
file = open(write_filepath,'w+')
# header = 'name, beerId, brewerId, ABV, style, appearance, aroma, palate, taste, overall, time, profileName\n'
# file.write(header)

# rev_file.write('text\n')
# num=0
with open(read_filepath) as fp:
   # print(num) 
   line = fp.readline()
   
   while line:
       # num+=1
       # print(num)
       new=''
       for i in range(3):
           new = new+ line.replace('\n','|')
           
           print(new)
           line = fp.readline()
        
       new = new[:-1] 
       new = new + '\n'
       file.write(new)
       print(new)
       # line = line.replace('\n',',')
           
       # print(type(new))
       # print(line)
       line = fp.readline()
 
fp.close()
file.close()
# df = pd.read_csv('/Users/claytonfields/Research/R/cbsa-asthma.csv',sep='|',header=None)
# df[0] = df[0].str.replace(r",.*","")
# df.rename(columns={0:'City',1:'n',2:'Value'},inplace=True)
# df.to_csv('/Users/claytonfields/Research/R/cbsa-asthma.csv', index=False)
