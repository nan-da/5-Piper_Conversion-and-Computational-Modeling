#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# http://snowball.tartarus.org/otherapps/schinke/intro.html
# /Users/dt/Documents/UChicago/Literature/5/Schinke_Latin_Stemming.r
#Created on Sun Aug 13 22:23:33 2017

#@author: dt
library("stringr")

convert_1 = list(c( 'j', 'i'), c('v', 'u'))
stop_case = c('atque', 'quoque', 'neque', 'itaque', 'absque', 'apsque', 'abusque', 'adaeque', 'adusque', 'denique', 
              'deque', 'susque', 'oblique', 'peraeque', 'plenisque', 'quandoque', 'quisque', 'quaeque',
              'cuiusque', 'cuique', 'quemque', 'quamque', 'quaque', 'quique', 'quorumque', 'quarumque',
              'quibusque', 'quosque', 'quasque', 'quotusquisque', 'quousque', 'ubique', 'undique', 'usque',
              'uterque', 'utique', 'utroque', 'utribique', 'torque', 'coque', 'concoque', 'contorque',
              'detorque', 'decoque', 'excoque', 'extorque', 'obtorque', 'optorque', 'retorque', 'recoque',
              'attorque', 'incoque', 'intorque', 'praetorque')

figure_6a = c('ibus', 'ius', 'ae', 'am', 'as', 'em', 'es', 'ia', 'is', 'nt',
              'os', 'ud', 'um', 'us', 'a', 'e', 'i', 'o', 'u')

figure_6b = c('iuntur','beris', 'erunt', 'untur', 'iunt', 'mini', 'ntur', 
              'stis', 'bor', 'ero', 'mur', 'mus', 'ris', 'sti', 'tis', 
              'tur', 'unt', 'bo', 'ns', 'nt', 'ri', 'm', 'r', 's', 't')

convert_2 = c( c(c('iuntur', 'erunt', 'untur', 'iunt', 'unt'), 'i'),
               c(c('beris', 'bor', 'bo'), 'bi'),
               c(c('ero'), 'eri') )

schinke_latin_stemming <- function(string, noun_or_verb_form='noun'){
  for (convert_pair in convert_1){
    string = gsub(convert_pair[1], convert_pair[2],string)
  }
  
  if(str_sub(string, -3) == 'que'){
    if( string %in% stop_case)
      return( string )
    else
      string = str_sub(string, 1, -4)
  }
  
  if(noun_or_verb_form == 'noun'){
    for (suffix in figure_6a){
      noun = sub(paste(suffix,'$',sep = ""),'',string)
      if(noun!=string)
        break
    }
    if (str_length(noun)>=2)
      return( noun )
  }
  else if(noun_or_verb_form == 'verb'){
    for (pair in convert_2)
      for (suffix in pair[1])
          string = sub(paste(suffix,'$',sep = ""),pair[2],string)
    
    for (suffix in figure_6b){
      verb = sub(paste(suffix,'$',sep = ""),'',string)
      if(verb!=string)
        break
    }
    if (str_length( verb )>=2)
      return( verb )
  }
}


#print(schinke_latin_stemming("sed tamen domine tibi excellentissimo optimo conditori et rectori universitatis deo nostro gratias etiamsi me puerum tantum esse voluisses eram enim etiam tunc vivebam atque sentiebam meamque incolumitatem vestigium secretissimae unitatis ex qua eram curae habebam custodiebam interiore sensu integritatem sensuum meorum inque ipsis parvis parvarumque rerum cogitationibus veritate delectabar falli nolebam memoria vigebam locutione instruebar amicitia mulcebar fugiebam dolorem abiectionem ignorantiam quid in tali animante non mirabile atque laudabile at ista omnia dei mei dona sunt non mihi ego dedi haec et bona sunt et haec omnia ego bonus ergo est qui fecit me et ipse est bonum meum et illi exulto bonis omnibus quibus etiam puer eram hoc enim peccabam quod non in ipso sed in creaturis eius me atque ceteris voluptates sublimitates veritates quaerebam atque ita inruebam in dolores confusiones errores gratias tibi dulcedo mea et honor meus et fiducia mea deus meus gratias tibi de donis tuis sed tu mihi ea serva ita enim servabis me et augebuntur et perficientur quae dedisti mihi et ero ipse tecum quia et ut sim tu dedisti mihi "))
schinke_latin_stemming_passage <- function(passage){
  word_list = strsplit(passage, " ")
  new_passage = ""
  for (paragraph in word_list)
    for(word in paragraph){
      if(word=="")
        next
      new_passage = paste(new_passage, schinke_latin_stemming(word))
    }
  return(new_passage)
}

#this sums the distances for each row and then averages them
# for in-half1, inhalf2, crosshalf
in_cross_half <- function(freq.dist, half_cut_point){
  a=0 # in half
  c=0 # cross half
  n = nrow(freq.dist)
  for (i in 1:half_cut_point){
    for (j in 1:i)
      a=a+freq.dist[i,j]
    for (j in (half_cut_point+1):n)
      c=c+freq.dist[i,j]
  }
  
  # a=a/40
  c=c/half_cut_point^2
  b=0
  for (i in (half_cut_point+1):n)
    for (j in (half_cut_point+1):i)
      b=b+freq.dist[i,j]
  # b=b/40
  contraction = (a-b)/( half_cut_point*(half_cut_point-1)/2 )
  print(c("In-half: ", contraction, "cross-half:", c))
  return( c(contraction, c) )
}