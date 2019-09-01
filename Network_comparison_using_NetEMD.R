orca2=function(graph,subgraph_size=c('four','five','three')[1]){
  if(!is.simple(graph)) warning('Graph is not simple')
  #If graph does not have edges
  if(ecount(graph)<1){ 
    if(subgraph_size=='four' || subgraph_size=='three') return(matrix(nrow=vcount(graph),ncol=15,0))
    if(subgraph_size=='five') return(matrix(nrow=vcount(graph),ncol=73,0))
  }
  #If graph has edges
  V(graph)$name=paste(1:vcount(graph)) #required for orca #possible BUG in V()
  e_list=matrix(ncol=2,nrow=ecount(graph),as.integer(get.edgelist(graph)))
  if(subgraph_size[1]=='three')
    orbitcounts<-trianglecount4(e_list) #modified orca version that is in leofthr package
  else {
    if(subgraph_size[1]=='four')
      orbitcounts<-count4(e_list)
    if(subgraph_size[1]=='five')
      orbitcounts<-count5(e_list)
  }
  return(orbitcounts)
  #tri=sum(orbitcounts[,4])/3
}
#countsa matrix with numerical entries, not necesrily integers.
#smoothing_window_width 0 for continuous entries, 1 for integers.
wdist_netdistpack=function(countsa,countsb,smoothing_window_width){
  #print('using new netemd') #netdistpackage
  dhists1=netdist::graph_features_to_histograms(countsa)
  dhists2=netdist::graph_features_to_histograms(countsb)
  val=netdist::net_emd(dhists1, dhists2, method = 'optimise',smoothing_window_width = smoothing_window_width)
  return(val)
}
###############////////////////////////////////////////////////////

require(netdist)
require(igraph)
require(orca)
data<-read.csv("data.csv", header = T, sep=",")
data<-aggregate(data$export_value, by=list(product_code=data$hs_product_code,location_name=data$location_name_short_en) , FUN=sum)
#Delteing trades with 0 value
data<-data[which(data$x>0), ]
#Removing smaller trades lower than 20th quantile
data<-data[which(data$x>quantile(data$x,probs = 0.20)), ] #Change the Quantile here
Adjacency<-read.csv("Adjacency.csv", header = T, sep=",")
val<-as.matrix.data.frame(Adjacency,rownames.force = TRUE)
val2<-val[,-1]
rownames(val2) <- val[,1]
rownames(val2)=substring(rownames(val2),2)
colnames(val2)=substring(colnames(val2),2)
g=graph.adjacency(val2, mode="undirected",weighted=TRUE,diag=FALSE)
table<-read.csv("color_scheme.csv", header = T, sep=",") #Importing the color scheme
V(g)$color=as.character(table$color[match(V(g)$name,table$ï..commoditycode_2)]) #defining the colors of each node in graph
V(g)$commodity=as.character(table$Name[match(V(g)$name,table$ï..commoditycode_2)])
V(g)$label=V(g)$commodity
#Removing edges with low relationship (prximity)
x=0.5
g <- delete.edges(g, which(E(g)$weight < x))
countries=unique(data$location_name)
data$hs_product_code<-as.character(data$product_code) #convert from original format to charecter
data$hs_product_code<-as.numeric(data$product_code) #to remove the leading zeros in the names
data$hs_product_code<-as.character(data$product_code)
output <- matrix(ncol=length(countries), nrow=length(countries))
rn=0
cn=0
print(Sys.time())
#Extracting features from the Graphs
for(row in countries){
  print(row)
  country<-data[which(data$location_name==row), ]
  subgraph_country <- induced_subgraph(g, intersect(country$product_code,V(g)$name))
  props_a=orca2(graph = subgraph_country,subgraph_size = 'five')
  assign(row, props_a)
}
#Oh(n^2) NetEMD comparisons of each country with every other country and saving the progress
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
