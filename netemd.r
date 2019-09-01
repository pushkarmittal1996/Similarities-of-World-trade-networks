setwd("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code")
Adjacency<-read.csv("Adjacency.csv", header = T, sep=",")
val<-as.matrix.data.frame(Adjacency,rownames.force = TRUE)
val2<-val[,-1]
rownames(val2) <- val[,1]
rownames(val2)=substring(rownames(val2),2)
colnames(val2)=substring(colnames(val2),2)
g=graph.adjacency(val2, mode="undirected",weighted=TRUE,diag=FALSE)
x=0.5
g <- delete.edges(g, which(E(g)$weight < x))
data<-read.csv("data.csv", header = T, sep=",")
data<-aggregate(data$export_value, by=list(hs_product_code=data$hs_product_code,location_name_short_en=data$location_name_short_en) , FUN=sum)
data<-data[which(data$x>0), ]
data<-data[which(data$x>quantile(data$x,probs = 0.20)), ]
countries=unique(data$location_name_short_en)
data$hs_product_code<-as.character(data$hs_product_code) #convert from original format to charecter
data$hs_product_code<-as.numeric(data$hs_product_code) #to remove the leading zeros in the names
data$hs_product_code<-as.character(data$hs_product_code)
table<-read.csv("color_scheme.csv", header = T, sep=",") #Importing the color scheme
V(g)$color=as.character(table$color[match(V(g)$name,table$ï..commoditycode_2)]) #defining the colors of each node in graph
V(g)$commodity=as.character(table$Name[match(V(g)$name,table$ï..commoditycode_2)])
V(g)$label=V(g)$commodity
output <- matrix(ncol=length(countries), nrow=length(countries))
rn=0
cn=0
print(Sys.time())
for(row in countries){
  print(row)
  country<-data[which(data$location_name_short_en==row), ]
  subgraph_country <- induced_subgraph(g, intersect(country$hs_product_code,V(g)$name))
  props_a=orca2(graph = subgraph_country,subgraph_size = 'five')
  assign(row, props_a)
}
# for(row in countries){walkt
#   cn=0
#   for(column in countries){
#     netemd_value =wdist_netdistpack(countsa=get(row),countsb=get(column),smoothing_window_width=1)#this function requries netdist pacakge from github
#     output[rn,cn] <- netemd_value
#     write.csv(output, file = "output.csv")
#     print(Sys.time())
#     print(netemd_value)
#     cn=cn+1
#     print(Sys.time())
#   }
#   rn=rn+1
#   print(Sys.time())
# }
for(i in seq(1, length(countries), 1)){
  for(j in seq(i+1, length(countries), 1)){
    print(Sys.time())
    netemd_value =wdist_netdistpack(countsa=get(as.character(countries[i])),countsb=get(as.character(countries[j])),smoothing_window_width=1)#this function requries netdist pacakge from github
    output[i,j] <- netemd_value
    write.csv(output, file = "output.csv")
    print(Sys.time())
    print(netemd_value)
    print(Sys.time())
  }
}
print(paste('start',Sys.time(),sep = ' '))
country<-data[which(data$location_name_short_en=='United Kingdom'), ]
print(paste('Data sliced for country 1',Sys.time(),sep = ' '))
subgraph_country <- induced_subgraph(g, intersect(country$hs_product_code,V(g)$name))
print(paste('Graph made for country 1',Sys.time(),sep = ' '))
props_a=orca2(graph = subgraph_country,subgraph_size = 'five')
print(paste('ORCA function Executed for country 1',Sys.time(),sep = ' '))
country<-data[which(data$location_name_short_en=='United States of America'), ]
print(paste('Data sliced for country 2',Sys.time(),sep = ' '))
subgraph_country <- induced_subgraph(g, intersect(country$hs_product_code,V(g)$name))
print(paste('Graph made for country 2',Sys.time(),sep = ' '))
props_b=orca2(graph = subgraph_country,subgraph_size = 'five')
print(paste('ORCA function Executed for country 2',Sys.time(),sep = ' '))
netemd_value =wdist_netdistpack(countsa=props_a,countsb=props_b,smoothing_window_width=1)#this function requries netdist pacakge from github
print(paste('NETEMD executed',Sys.time(),netemd_value,sep = ' '))
i=1
while(i<=235){
  val2[i,i]<-0
  i=i+1
}
for(i in seq(1, 235, 1)){
  for(j in seq(1, i, 1)){
    val2[i,j]<-val2[j,i]
  }
}