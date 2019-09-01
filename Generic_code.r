folder<-choose.dir(caption = "Select Working directory")
setwd(folder)
data<-read.csv("United Kingdom2016Export.csv", header = T, sep=",")
library(igraph)
uk_export_2016<-data[which(data$export_value>0), ]
library(rgl)
product<-read.csv("hs92_proximities.csv", header = T, sep=",")
uk_export_2016<-aggregate(uk_export_2016$export_value, by=list(hs_product_code=uk_export_2016$hs_product_code), FUN=sum)
uk_export_2016$hs_product_code<-as.character(uk_export_2016$hs_product_code) #convert from original format to charecter
uk_export_2016$hs_product_code<-as.numeric(uk_export_2016$hs_product_code) #to remove the leading zeros in the names
uk_export_2016$hs_product_code<-as.character(uk_export_2016$hs_product_code) #convert from original format to charecter finally
product=product[product[,3]>0.5,] #Using only those edges with higher proximity than 0.35
product$proximity<-product$proximity*10
g=graph.data.frame(product) #graph of edges
table<-read.csv("color_scheme.csv", header = T, sep=",") #Importing the color scheme
V(g)$color=as.character(table$color[match(V(g)$name,table$ï..commoditycode_2)]) #defining the colors of each node in graph
g<-as.undirected(g) #making the graph undirected
#plot(g,vertex.label=NA)
uk_export_2016<-uk_export_2016[uk_export_2016$x>10000000,]
uk_export_graph <- induced_subgraph(g, intersect(uk_export_2016$hs_product_code,V(g)$name)) #choosing the nodes present in uk_export_graph
V(uk_export_graph)$size=as.numeric(ceiling(log10(uk_export_2016$x[match(V(uk_export_graph)$name,uk_export_2016$hs_product_code)]))*2) #setting the sizes
plot(uk_export_graph,vertex.label=NA,scale=3)