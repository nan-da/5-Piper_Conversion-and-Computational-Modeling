#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# http://snowball.tartarus.org/otherapps/schinke/intro.html
# /Users/dt/Documents/UChicago/Literature/5/Schinke_Latin_Stemming.py
"""
Created on Sun Aug 13 22:23:33 2017

@author: dt
"""

convert_1 = [[ 'j', 'i'], ['v', 'u']]
stop_case = ['atque', 'quoque', 'neque', 'itaque', 'absque', 'apsque', 'abusque', 'adaeque', 'adusque', 'denique', 
            'deque', 'susque', 'oblique', 'peraeque', 'plenisque', 'quandoque', 'quisque', 'quaeque',
            'cuiusque', 'cuique', 'quemque', 'quamque', 'quaque', 'quique', 'quorumque', 'quarumque',
            'quibusque', 'quosque', 'quasque', 'quotusquisque', 'quousque', 'ubique', 'undique', 'usque',
            'uterque', 'utique', 'utroque', 'utribique', 'torque', 'coque', 'concoque', 'contorque',
            'detorque', 'decoque', 'excoque', 'extorque', 'obtorque', 'optorque', 'retorque', 'recoque',
            'attorque', 'incoque', 'intorque', 'praetorque']

figure_6a = ['ibus', 'ius', 'ae', 'am', 'as', 'em', 'es', 'ia', 'is', 'nt',
             'os', 'ud', 'um', 'us', 'a', 'e', 'i', 'o', 'u']

figure_6b = ['iuntur','beris', 'erunt', 'untur', 'iunt', 'mini', 'ntur', 
             'stis', 'bor', 'ero', 'mur', 'mus', 'ris', 'sti', 'tis', 
             'tur', 'unt', 'bo', 'ns', 'nt', 'ri', 'm', 'r', 's', 't']

convert_2 = [ [['iuntur', 'erunt', 'untur', 'iunt', 'unt'], 'i'],
              [['beris', 'bor', 'bo'], 'bi'],
              [['ero'], 'eri'] ]

def schinke_latin_stemming(string):
    res = {}
    for convert_pair in convert_1:
        string = string.replace(convert_pair[0], convert_pair[1])

    if string[-3:] == 'que':
        if string in stop_case:
            res['noun'] = string
            res['verb'] = string
            return res
        else:
            string = string[:-3]

    noun = string
    for suffix in figure_6a:
        if string.endswith(suffix):
            noun = string.rstrip(suffix)
            break
    if len(noun)>=2:
        res['noun'] = noun
        
    for pair in convert_2:
        for suffix in pair[0]:
            if string.endswith(suffix):
                string = pair[1].join(string.rsplit(suffix, 1))
                break
                    
    for suffix in figure_6b:
        if string.endswith(suffix):
            string = string.rstrip(suffix)
            break
        
    if len(string)>=2:
        res['verb'] = string
    return res
    
print(schinke_latin_stemming("ducibus"))