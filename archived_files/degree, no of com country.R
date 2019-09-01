#Adjacency<-read.csv("Adjacency.csv", header = T, sep=",")
#library(igraph)
#val<-as.matrix.data.frame(Adjacency,rownames.force = TRUE)
#val2<-val[,-1]
#rownames(val2) <- val[,1]
#colnames(val2)=substring(colnames(val2),2)
#rownames(val2)=substring(rownames(val2),2)
#g=graph.adjacency(val2, mode="undirected",weighted=TRUE,diag=FALSE)
x=0.250
g <- delete.edges(g, which(E(g)$weight < x))
#data<-read.csv("data.csv", header = T, sep=",")
countries<-as.character(unique(data$location_name_short_en))
cl <- rainbow(235)
output <- matrix(ncol=3, nrow=235)
t=0
for(row in countries){
  temp<-data[which(data$location_name_short_en==row), ]
  temp<-aggregate(temp$export_value, by=list(hs_product_code=temp$hs_product_code), FUN=sum)
  temp$hs_product_code<-as.character(temp$hs_product_code) #convert from original format to charecter
  temp$hs_product_code<-as.numeric(temp$hs_product_code) #to remove the leading zeros in the names
  temp$hs_product_code<-as.character(temp$hs_product_code) #convert from original format to charecter finally
  subgraph_temp <- induced_subgraph(g, intersect(temp[temp$x>0,]$hs_product_code,V(g)$name))
  output[t,1] <- row
  output[t,2] <- components(subgraph_temp)$no
  output[t,3] <- mean(degree(subgraph_temp))
  print(row)
  t=t+1
}

output <- matrix(ncol=3, nrow=99)
t=0.0005
i=1
while(t<=0.05){
  subgraph_temp <- delete.edges(g, which(E(g)$weight > t))
  output[i,1] <- components(subgraph_temp)$no
  output[i,2] <- mean(degree(subgraph_temp))
  output[i,3] <- max(components(subgraph_temp)$csize)
  t=t+0.0005
  i=i+1
}