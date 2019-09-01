data<-read.csv("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\data.csv", header = T, sep=",")
data<-aggregate(data$export_value, by=list(hs_product_code=data$hs_product_code,location_name_short_en=data$location_name_short_en) , FUN=sum)
data<-data[which(data$x>0), ]
data<-data[which(data$x>quantile(data$x,probs = 0.20)), ]
Adjacency<-read.csv("output.csv", header = T, sep=",")
rownames(Adjacency)<-Adjacency[,1]
Adjacency$X<-NULLdata$
diag(Adjacency)<-0
library(Matrix)
#Adjacency <- forceSymmetric(Adjacency)
Adjacency[lower.tri(Adjacency)]<-Adjacency[upper.tri(Adjacency)]
Adjacency<-as.matrix(Adjacency)
library(vegan)
require(vegan)
pcomain.result <- wcmdscale(Adjacency, eig = TRUE, add="lingoes", k = 2)
library(cluster) 
library(corrplot)
library(factoextra)
y=hcut(pcomain.result$points, k = 10, hc_method = "complete")
all_countries=unique(data$location_name_short_en)
for(k in 1:10){
  countries=y$labels[y$cluster==k]
  if(length(countries)>11)
    next
  temp1 <- matrix(ncol=1, nrow=1000)
  temp2 <- matrix(ncol=1, nrow=length(unique(data$hs_product_code)))
  rownames(temp2)<-unique(data$hs_product_code)
  temp2[is.na(temp2)]=0
  for(i in 1:1000){
    indexes=round(runif(length(countries))*235)
    com=unique(data[which(data$location_name_short_en==all_countries[indexes[1]]), ]$hs_product_code)
    for(j in 2:length(indexes)){
      com=intersect(com,unique(data[which(data$location_name_short_en==all_countries[indexes[j]]), ]$hs_product_code))
      temp1[i]=length(com)
    }
    for(commo in com){
      temp2[commo,]=temp2[commo,]+1
    }
    print(i)
  }
  assign(paste("cluster_size_",as.character(k),sep = ""), temp1)
  assign(paste("cluster_comm_",as.character(k),sep = ""), temp2)
}
somePDFPath = "C:\\Users\\PUSHKAR\\Desktop\\Clusters_size_MC.pdf"
pdf(file=somePDFPath,width=8,height=6,paper='special')
for(k in 1:10){
  if(length(countries)>11)
    next
  hist(get(paste("cluster_size_",as.character(k),sep = "")),breaks = 235, main = paste("Histogram of 1000 Simulations of Cluster",as.character(k)),xlab = "Number of common commodities")
  abline(v = length(y$cluster==k), col = "blue", lwd = 2)
}
dev.off()
for(k in 1:10){
  countries=y$labels[y$cluster==k]
  cdata=data[which(data$country==countries[1]),]
  com_agg=aggregate(cdata$x, by=list(product=cdata$product) , FUN=sum)
  coun_agg=aggregate(cdata$x, by=list(partner=cdata$partner) , FUN=sum)
  commod=com_agg[order(com_agg$x,decreasing = TRUE),]$product[1:20]
  counod=coun_agg[order(coun_agg$x,decreasing = TRUE),]$partner[1:20]
  if(length(countries)>11)
    next
  for(i in 2:length(countries)){
    cdata=data[which(data$country==countries[i]),]
    com_agg=aggregate(cdata$x, by=list(product=cdata$product) , FUN=sum)
    coun_agg=aggregate(cdata$x, by=list(partner=cdata$partner) , FUN=sum)
    commod=intersect(commod,com_agg[order(com_agg$x,decreasing = TRUE),]$product[1:20])
    counod=intersect(counod,coun_agg[order(coun_agg$x,decreasing = TRUE),]$partner[1:20])
  }
  print(paste("Cluster ",k))
  print(commod)
  print(counod)
}

Gabon=data[which(data$location_name_short_en=='Gabon'),]
Denmark=data[which(data$location_name_short_en=='Denmark'),]
