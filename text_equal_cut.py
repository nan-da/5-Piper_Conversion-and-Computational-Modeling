#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 28 19:18:05 2017

@author: dt
"""
import glob
import textwrap

files = glob.glob("/Users/dt/Documents/UChicago/Literature/5/English/*")
text_list = []
# iterate over the list getting each file 
for file in files:
   # open the file and then call .read() to get the text 
   with open(file) as f:
      text = f.read()
      text_list.append(text)
      
text_all = ' '.join(text_list).split()
n = int(len(text_all)/2)
print("text all")

for i, x in enumerate(range(0, len(text_all), n)):
    new = " ".join(text_all[x:x+n])
    print(i)
    open("/Users/dt/Documents/UChicago/Literature/5/English_5_cut/{}.txt".format(i), mode='w').write(new)

    
    #for i, new in enumerate(textwrap.wrap(text_all, len(text_all)/10)):
#    print(i)
#    open("/Users/dt/Documents/UChicago/Literature/5/English_10_cut/{}.txt".format(i), mode='w').write(new)
