library("cluster")
library("tm")
# library("proxy")
library("SnowballC")
library("lsa")
library("stats")

#table<-array(0, c(153,3)) # second digit = # of works in corpus, third # = number of columns in table
final.df<-NULL
# for (k in 3:152) {
for (k in 1:1) {
 # dir <- paste("/Users/andrewpiper/Sites/Topologies - Tests/Semantics of Life/Conversionality Test/Conversionality Test - Binary/Binary Test 1/binarytest1_new/DataNovelGerman20/", as.character(k), sep="")
 dir <- paste("/Users/dt/Documents/UChicago/Literature/5/", sep="")
#load corpus
 corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="German"))
 #run transformations
 corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
 corpus1 <- tm_map(corpus1, content_transformer(tolower))
 corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
 corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
 #remove problems
 problems<-c("apparat","datumsangaben","seite","page","erl??uterungen", "kommentar")
 corpus1 <- tm_map(corpus1, removeWords, problems)
 #get scaling value
 corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
 corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
 scaling1<-rowSums(corpus1.matrix)
 #remove stopwords
 corpus1 <- tm_map(corpus1, removeWords, stopwords("German"))
  #Latin stopwords
  #words<-c("ab", "ac", "ad ", "adhic ", "aliqui ", "aliquis ", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero")
  #corpus1 <- tm_map(corpus1, removeWords, words)
 #stem
 corpus1 <- tm_map(corpus1, stemDocument, language = "german")
 #remake DTM
 corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
 #remove sparse terms
 corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .4)
 corpus1.sparse.matrix<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
 #scale
 conversion.scaled<-corpus1.sparse.matrix
 conversion.scaled[,1:ncol(corpus1.sparse.matrix)]<- corpus1.sparse.matrix[,1:ncol(corpus1.sparse.matrix)]/scaling1
 
 #option: only keep top X MFW
#  keep<-1000
#  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#  conversion.scaled<-corpus1.matrix
#  conversion.scaled[,1:ncol(corpus1.matrix)]<- corpus1.matrix[,1:ncol(corpus1.matrix)]/scaling1
#  tdm<-t(conversion.scaled)
#  mfw<-rowSums(tdm)
#  mfw.sort<-as.data.frame(sort(mfw, TRUE)[1:keep])
#  mfw.dict<-row.names(mfw.sort)
#  conversion.scaled<-conversion.scaled[,mfw.dict]
 
#this remakes the dtm in proper order
nword=dim(conversion.scaled)[2]
freq.dat<-array(0,c(20,nword))

	freq.dat[1,]=conversion.scaled[1,]
	freq.dat[2,]=conversion.scaled[12,]
	for (j in 3:9){
		freq.dat[j,]=conversion.scaled[11+j,]
	}
	for (j in 10:19){
		freq.dat[j,]=conversion.scaled[j-8,]
	}
	freq.dat[20,]=conversion.scaled[13,]
#Euclidean distance
conversion.dist<-dist(freq.dat, method = "Euclidean")
freq.dist<-as.matrix(conversion.dist)
#Cosine distance
# cosine.dist<-simil(freq.dat, method = "cosine")
# freq.dist<-as.matrix(cosine.dist, stringsAsFactors=F)
# freq.dist<-pr_simil2dist(freq.dist)
# freq.dist[is.na(freq.dist)] <- 0

#this sums the distances for each row and then averages them
# for in-half1, inhalf2, crosshalf
a=0 #in half
c=0 # cross half
for (i in 1:10){
	for (j in 1:i)
		a=a+freq.dist[i,j]
	for (j in 11:20)
		c=c+freq.dist[i,j]
}

a=a/40
c=c/100
b=0
for (i in 11:20)
   for (j in 11:i)
		b=b+freq.dist[i,j]
b=b/40
contraction<-a-b
#table[k,]=c(a,b,c)
#}
#table[1,]=c(a,b,c)
#write.csv(table, file="binarytest_novel_German_mfw_500.csv")
# 
# 
#alternative calculation of in and between group variance
group1<-vector()
for (m in 1:9) {
  for (n in m:9)
    group1<-append(group1, freq.dist[m,n+1])
}
group2<-vector()
#alternative calculation of in and between group variance
for (m in 11:19) {
  for (n in m:19)
    group2<-append(group2, freq.dist[m,n+1])
}
# group3<-vector()
# for (m in 1:10) {
#   for (n in 1:10)
#     group3<-append(group3, freq.dist[m,n+10])
# }
#ratio of variance model
group.diff<-var.test(group1, group2)
var.ratio<-group.diff$statistic[[1]]
var.p<-group.diff$p.value[[1]]
#comparing in-group similarities
sim.test<-t.test(group1, group2)
sim.test.p<-sim.test$p.value
#sim.diff<-(sim.test$estimate[[2]]-sim.test$estimate[[1]])/min(sim.test$estimate[[1]], sim.test$estimate[[2]])
sim.diff<-(sim.test$estimate[[2]]-sim.test$estimate[[1]])/sim.test$estimate[[1]]
#silhouette model
#tk<-pam(freq.dist,2,diss=TRUE)$silinfo$avg.width
#tk<-pam(freq.dist,2,diss=TRUE)
#regression model
reg.df<-NULL
for (p in 1:10){
  remove<-which(freq.dist[p,] == 0)
  test<-freq.dist[p, -remove]
  value.var<-var(test[1:9])
  temp.df<-data.frame(p, value.var)
  reg.df<-rbind(reg.df, temp.df)
}
reg2.df<-NULL
for (p in 11:20){
  remove<-which(freq.dist[p,] == 0)
  test<-freq.dist[p, -remove]
  value.var<-var(test[11:19])
  temp.df<-data.frame(p, value.var)
  reg2.df<-rbind(reg2.df, temp.df)
}
reg.final<-rbind(reg.df, reg2.df)
inter.lm<-lm(reg.final$value.var ~ reg.final$p, data=reg.final)
slope<-inter.lm$coefficients[[2]]
p.value<-pf(summary(inter.lm)$fstatistic[1],summary(inter.lm)$fstatistic[2],summary(inter.lm)$fstatistic[3],lower.tail=FALSE)[[1]]
r.squared<-summary(inter.lm)$adj.r.squared
#summary(inter.lm)
#plot(reg.final$value.var)
#lines(reg.final$p, predict(inter.lm), col="black")
word.count<-sum(scaling1)
temp.df<-data.frame(k, var.ratio, var.p, word.count, contraction, slope, p.value, r.squared, sim.diff, sim.test.p)
#group3.var<-var(group3)
final.df<-rbind(final.df, temp.df)
}
setwd("~/Sites/Topologies - Tests/Conversion")
write.csv(final.df, file="Conversion_Sparse_German_All_2.csv")



