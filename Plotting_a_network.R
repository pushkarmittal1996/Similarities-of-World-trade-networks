#set the working directory first
folder<-choose.dir(default = "C:\\Users", caption = "Select Working directory")
setwd(folder)
library(dplyr)
library(tidyr)
library(ggplot2)
library(visNetwork)
library(Matrix)
library(vegan)
library(igraph)
library("googledrive")
data<-read.csv("data.csv", header = T, sep=",")
data<-aggregate(data$export_value, by=list(product_code=data$hs_product_code,location_name=data$location_name_short_en) , FUN=sum)
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

#making a product space graph
igraph_maker <- function(g,data,country="United Kingdom",proximity_threshold=0.5, trade_threshold=0.2)
{
  graph <- delete.edges(g, which(E(g)$weight < proximity_threshold))
  data=data[which(data$x>quantile(data$x,trade_threshold)),]
  data=data[which(data$location_name==country),]
  graph <- induced_subgraph(graph, intersect(data$product_code,V(g)$name)) #choosing the nodes present in country
  V(graph)$size=as.numeric(ceiling((log10(data$x[match(V(graph)$name,data$product_code)]))-6)*3) #setting the size of nodes
  #plotting the graph
  drive_download(
    "https://drive.google.com/open?id=1OLXc-mD3eMUXibu6vWGbZT_KBuNh_dYJ",
    path = "vis_options_custom.R",
    overwrite = TRUE
  )
  tkplot(graph,vertex.label=NA,scale=3)
  source('vis_options_custom.R')
  names(vertex_attr(graph))[which(names(vertex_attr(graph)) == "label")] <- "title"
  visIgraph(graph, idToLabel = F, layout = "layout_nicely") %>% visOptions_custom(highlightNearest = TRUE, selectedBy = "group")
  return(graph)
}
