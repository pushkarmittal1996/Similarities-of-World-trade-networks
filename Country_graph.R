#Adjacency<-read.csv("Adjacency.csv", header = T, sep=",")
#library(igraph)
#val<-as.matrix.data.frame(Adjacency,rownames.force = TRUE)
#val2<-val[,-1]
#rownames(val2) <- val[,1]
#colnames(val2)=substring(colnames(val2),2)
#rownames(val2)=substring(rownames(val2),2)
#g=graph.adjacency(val2, mode="undirected",weighted=TRUE,diag=FALSE)
x=0.5
g <- delete.edges(g, which(E(g)$weight < x))
data<-read.csv("data.csv", header = T, sep=",")
countries<-as.character(unique(data$location_name_short_en))
cl <- rainbow(235)
t=0
for(row in countries){
  temp<-data[which(data$location_name_short_en==row), ]
  temp<-aggregate(temp$export_value, by=list(hs_product_code=temp$hs_product_code), FUN=sum)
  temp$hs_product_code<-as.character(temp$hs_product_code) #convert from original format to charecter
  temp$hs_product_code<-as.numeric(temp$hs_product_code) #to remove the leading zeros in the names
  temp$hs_product_code<-as.character(temp$hs_product_code) #convert from original format to charecter finally
  iterations = 20
  d=10^(ceiling(log10(max(temp$x)))/iterations)
  i=d
  output <- matrix(ncol=3, nrow=20)
  for(it in 1:iterations)
  {output
    subgraph_temp <- induced_subgraph(g, intersect(temp[temp$x>i,]$hs_product_code,V(g)$name))
    output[it,1] <- it
    output[it,2] <- components(subgraph_temp)$no
    output[it,3] <- max(components(subgraph_temp)$csize)
    i=i*d
  }
  output[output=="-Inf"]<-0
  print(row)
  t=t+1
  if(t==1){
    x=output[,1]
    y=output[,2]
    #y=temp$v5
    smoothingSpline = smooth.spline(x, y, spar=0.35)
    plot(smoothingSpline,type = 'n', xlab = "Threshold Interval", ylab = "no of components",col = cl[t], ylim=c(1,250), xlim=c(1,20))
  }
  else{
    x=output[,1]
    y=output[,2]
    #y=temp$v5
    smoothingSpline = smooth.spline(x, y, spar=0.35)
    lines(smoothingSpline, col = cl[t])
  }
}