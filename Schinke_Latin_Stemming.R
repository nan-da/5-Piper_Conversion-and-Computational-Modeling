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

convert_2 = list( i=c('iuntur', 'erunt', 'untur', 'iunt', 'unt'),
                  bi=c('beris', 'bor', 'bo'),
                  eri=c('ero') )

schinke_latin_stemming <- function(string, noun_or_verb_form='verb'){
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
    for (key in names(convert_2)){
      value = convert_2[[key]]
      for (suffix in value)
          string = sub(paste(suffix,'$',sep = ""),key,string)
    }
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
  a_scale=c()
  c_scale=c()
  n = nrow(freq.dist)
  for (i in 1:half_cut_point){
    for (j in 1:i){
      a=a+freq.dist[i,j]
      a_scale = c(a_scale, freq.dist[i,j])
    }
    for (j in (half_cut_point+1):n){
      c=c+freq.dist[i,j]
      c_scale = c(c_scale, freq.dist[i,j])
    }
  }
  a = a/( half_cut_point*(half_cut_point-1)/2 )
  # a=a/40

  b=0
  for (i in (half_cut_point+1):n){
    for (j in (half_cut_point+1):i){
      b=b+freq.dist[i,j]
      a_scale=c(a_scale,freq.dist[i,j])
    }
  }
  # b=b/40
  b = b/( (n-half_cut_point)*(n-half_cut_point-1)/2 )
  contraction = a-b
  c=c/(half_cut_point*(n-half_cut_point))
  x=scale(a_scale)
  y=scale(c_scale)
  
  # print(c("Sample Number In-half:", half_cut_point*(half_cut_point-1)/2,(n-half_cut_point)*(n-half_cut_point-1)/2, "Cross-half:",half_cut_point*(n-half_cut_point)))
  # print(c('z-score:',(contraction-attr(x,"scaled:center"))/attr(x,"scaled:scale"), (c-attr(y,"scaled:center"))/attr(y,"scaled:scale")))
  print(c("In-half:", contraction, "Cross-half:", c))
  return( c(contraction, c) )
}


plot_cmds <- function(conversion.scaled, k){
  dist.matrix<-dist(conversion.scaled, method = "euclidean")
  fit <- cmdscale(dist.matrix, eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2]
  png(filename=sprintf("plot/MDS_%s.png", k),width = 720, height = 480)
  plot(x, y, type="n", xlab="Coordinate 1", ylab="Coordinate 2", main="")
  text(x, y, labels = 1:length(x), cex=1.1)
  dev.off()
}

plot_pca <- function(conversion.scaled, k){
  autoplot(prcomp(conversion.scaled),loadings = F,label = TRUE, shape = FALSE, label.size = 4)
  ggsave(filename=sprintf("plot/PCA_%s.png", k),width = 7.2, height = 4.8)
  # dev.off()
}
