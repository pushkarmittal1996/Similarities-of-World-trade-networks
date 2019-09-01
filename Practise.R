#data<-read.csv("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\data.csv", header = T, sep=",")
data<-aggregate(data$export_value, by=list(hs_product_code=data$hs_product_code,location_name_short_en=data$location_name_short_en) , FUN=sum)
temp<-aggregate(temp$x, by=list(hs_product_code=temp$hs_product_code,location_name_short_en=temp$location_name_short_en) , FUN=quantile(probs = 0.05))
temp<-temp[which(temp$x>0), ]
countryquan05<-aggregate(temp$x, by=list(location_name_short_en=temp$location_name_short_en,hs_product_code=temp$hs_product_code) , FUN=function(x,location_name_short_en)
  {
    if(x>quan05[which(quan05$location_name_short_en==location_name_short_en),]$x){ return(x) }
    else
      return(0)
  })
countryquan05<-aggregate(temp$x, by=list(location_name_short_en=temp$location_name_short_en) , FUN=function(x){quantile(x,probs = 0.05)})
sm.density.compare(temp$x, temp$location_name_short_en, xlab="Sum of export value")
sm.density.compare(log10(temp$x), temp$location_name_short_en, xlab="Sum of export value")
iterations =32
variables = 3
x=0
d=0.025
output <- matrix(ncol=variables, nrow=iterations)
for(i in 1:iterations){
  x=x+d
  temp <- delete.edges(g, which(E(g)$weight < x))
  output[i,2] <- components(temp)$no
  output[i,3] <- max(components(temp)$csize)
  output[i,1] <- x
}

x=output[,1]
y1=output[,2]
y2=output[,3]
smoothingSpline1 = smooth.spline(x, y1, spar=0.35)
plot(x,y1,xlab = "Proximity", ylab = "No of components")
lines(smoothingSpline1)
smoothingSpline2 = smooth.spline(x, y2, spar=0.35)
plot(x,y2,xlab = "Proximity", ylab = "Size of biggest Component")
lines(smoothingSpline2)

t=1
for(row in countries){
  xyz<-temp[which(temp$location_name_short_en==row), ]
  quan5=quantile(xyz$x,probs = 0.05)
  quan10=quantile(xyz$x,probs = 0.1)
  xyz5<-xyz[which(xyz$x>quan5), ]
  xyz10<-xyz[which(xyz$x>quan10), ]
  subgraph_temp <- induced_subgraph(g, intersect(xyz5$hs_product_code,V(g)$name))
  output[t,1] <- row
  output[t,2] <- components(subgraph_temp)$no
  output[t,3] <- mean(degree(subgraph_temp))
  output[t,4] <- max(components(subgraph_temp)$csize)
  subgraph_temp <- induced_subgraph(g, intersect(xyz10$hs_product_code,V(g)$name))
  output[t,5] <- components(subgraph_temp)$no
  output[t,6] <- mean(degree(subgraph_temp))
  output[t,7] <- max(components(subgraph_temp)$csize)
  t=t+1
  print(row)
}
t=1
for(row in countries){
  xyz<-temp[which(temp$location_name_short_en==row), ]
  quan5=quantile(xyz$x,probs = 0.05)
  quan10=quantile(xyz$x,probs = 0.1)
  output_val[t,1] <- nrow(xyz[which(xyz$x>quan5), ])
  output_val[t,2] <- nrow(xyz[which(xyz$x>quan10), ])
  output_val[t,3] <- nrow(xyz)
  t=t+1
}
nocomp<-matrix(ncol=20, nrow=235)
size_temp<-matrix(ncol=20, nrow=235)
avg_degree_temp<-matrix(ncol=20, nrow=235)

xyz<-data[which(data$x>0), ]
for(i in 1:20){
  quan=quantile(data$x,probs = (i*0.05))
  xyz<-data[which(data$x>quan), ]
  for(row in countries)
  {
    t=1
    xyz<-xyz[which(xyz$location_name_short_en==row), ]
    subgraph_temp <- induced_subgraph(g, intersect(xyz$hs_product_code,V(g)$name))
    nocomp[t,i]<-1240-vcount(subgraph_temp)+components(subgraph_temp)$no
    avg_degree_temp[t,i]<-sum(degree(subgraph_temp))/1240
    t=t+1
  }
}
boxplot(nocomp)
sm.density.compare(flag$x, flag$location_name_short_en, xlab="Sum of export value")
sm.density.compare(log10(flag$x), flag$location_name_short_en, xlab="log of Sum of export value")
world_capital[world_capital$country.etc=="British Virgin Islands"]<-"British Indian Ocean Territory"
setdiff(world_capital$country.etc,Plastics$location_name_short_en)
for(row in nrow(world_capital)){
  add_vertices(map,nv=1,name=world_capital[row,]$country.etc)
}