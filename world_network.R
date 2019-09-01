#data<-read.csv("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\data.csv", header = T, sep=",")
#Plastics<-data[which(data$hs_product_code==8471), ]
#Plastics<-aggregate(Plastics$export_value, by=list(location_name_short_en=Plastics$location_name_short_en,location_code=Plastics$location_code,partner_name_short_en=Plastics$partner_name_short_en,partner_code=Plastics$partner_code) , FUN=sum)
library(raster)
library(igraph)
library(maptools)
data<-data[which(data$x>0), ]
quan6<-data[which(data$x>quantile(data$x,probs = 0.50)), ]
crude<-data[which(data$com=='Petroleum oils, crude'),]
world_capital<-read.csv("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\world_capital.csv", header = T, sep=",")
world_borders <- readShapePoly("TM_WORLD_BORDERS-0.3\\TM_WORLD_BORDERS-0.3.shp")
rownames(world_capital)<-world_capital$country.etc
world_capital$x<-NULL
world_capital$country.etc<-NULL
map_world=graph.empty(n=0, directed=TRUE)
map_world=add_vertices(map_world,nv=nrow(world_capital),attr = list(name=world_capital$country.etc))
V(map_world)$name=as.character(rownames(world_capital))
V(map_world)$label=V(map_world)$name
lo <- layout.norm(as.matrix(world_capital[,3:4]))
plot(map_world, layout=lo, add = TRUE, label=NA)
mapping<-read.csv("mapping.csv", header = T, sep=",")
for (i in 1:nrow(mapping))
{
  worldreplace(data,mapping$Atlas[i],mapping$Map[i])
}
world_capital<-read.csv("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\world_capital.csv", header = T, sep=",")
for(k in 1:10){
  countries=y$labels[y$cluster==k]
  if(length(countries)>11)
    next
  output <- matrix(ncol=3, nrow=length(countries)*5)
  for(i in 1:(length(countries))){
    cdata=data[which(data$country==countries[i]),]
    coun_agg=aggregate(cdata$x, by=list(partner=cdata$partner, country=cdata$country) , FUN=sum)
    start=1+((i-1)*5)
    end=5*i
    output[start:end,1]=countries[i]
    output[start:end,2]=as.character(coun_agg[order(coun_agg$x,decreasing = TRUE),]$partner[1:5])
    output[start:end,3]=coun_agg[order(coun_agg$x,decreasing = TRUE),]$x[1:5]
  }
  g <- graph.data.frame(output, directed=T, vertices=world_capital)
  E(g)$width=(log10(as.numeric(output[,3]))-min(log10(as.numeric(output[,3])))+1)
  lo <- as.matrix(world_capital[,2:3])
  map("world")
  plot(g, layout=lo, add = TRUE, rescale = FALSE, vertex.label.font=ifelse(degree(g,mode="out") > 0, 2, 1),vertex.label = ifelse(degree(g,mode="out") > 0, V(g)$name, NA), vertex.label.cex=ifelse(degree(g,mode="out") > 0, 1, 0.7), vertex.label.color=ifelse(degree(g,mode="out") > 0, 'blue', 'red'), edge.color='yellow',main=paste("Cluster ",k," countries and their top 5 partners"))
}
library(maps)
rownames(crude)= 1:length(crude$source)
rown=setdiff(crude$source,world_capital$name)
crude=droplevels(crude)
for( i in 1 : length(rown)){
  crude=crude[which(crude$source!=rown[i]),]
}
rown=setdiff(crude$dest,world_capital$name)
for( i in 1 : length(rown)){
  crude=crude[which(crude$dest!=rown[i]),]
}
crude=crude[order(crude$x,decreasing = TRUE),][1:100,]
g <- graph.data.frame(crude, directed=T, vertices=world_capital)
E(g)$width=(log10(crude$x)-min(log10(crude$x))+1)
lo <- as.matrix(world_capital[,2:3])
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90))
plot(g, layout=lo, add = TRUE, rescale = FALSE, edge.color='maroon',main=paste("Cluster ",k," countries and their top 5 partners"),vertex.label = NA)
plot(g, layout=lo, add = TRUE, rescale = FALSE, vertex.label.font=ifelse(degree(g,mode="out") > 0, 2, 1),vertex.label = ifelse(degree(g,mode="out") > 5, V(g)$name, NA), vertex.label.cex=ifelse(degree(g,mode="out") > 0, 1, 0.7), vertex.label.color=ifelse(degree(g,mode="out") > 0, 'blue', 'red'), edge.color='yellow',main=paste("Cluster ",k," countries and their top 5 partners"))
plot(g, layout=lo, add = TRUE, rescale = FALSE, vertex.label.font=ifelse(degree(g,mode="out") > 0, 2, 1),vertex.label = ifelse(degree(g) > 5, V(g)$name, NA), vertex.label.cex=ifelse(degree(g,mode="out") > 0, 1, 0.7), vertex.label.color=ifelse(degree(g,mode="out") > 0, 'blue', 'red'), edge.color='yellow',main=paste("Cluster ",k," countries and their top 5 partners"))
