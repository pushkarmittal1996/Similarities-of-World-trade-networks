#Graph maker
Adjacency<-read.csv("Adjacency.csv", header = T, sep=",")
library(igraph)
val<-as.matrix.data.frame(Adjacency,rownames.force = TRUE)
val2<-val[,-1]
rownames(val2) <- val[,1]
rownames(val2)=substring(rownames(val2),2)
colnames(val2)=substring(colnames(val2),2)
g=graph.adjacency(val2, mode="undirected",weighted=TRUE,diag=FALSE)