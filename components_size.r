#Calculating the global max
global_max=max(data$export_value)
for(row in countries){
  temp<-data[which(data$location_name_short_en==row), ]
  temp<-aggregate(temp$export_value, by=list(hs_product_code=temp$hs_product_code), FUN=sum)
  if(global_max<max(temp$x)){
    global_max=max(temp$x)
  }
}
t=0
for(row in countries){
  temp<-data[which(data$location_name_short_en==row), ]
  temp<-aggregate(temp$export_value, by=list(hs_product_code=temp$hs_product_code), FUN=sum)
  temp$hs_product_code<-as.character(temp$hs_product_code) #convert from original format to charecter
  temp$hs_product_code<-as.numeric(temp$hs_product_code) #to remove the leading zeros in the names
  temp$hs_product_code<-as.character(temp$hs_product_code) #convert from original format to charecter finally
  temp<-temp[which(temp$x>0), ]
  t=t+1
  if(t==1){
    #plot(density(temp$x),type = 'n', xlab = "Export destribution", ylab = "Density",col = cl[t], xlim = c(1,global_max), ylim=c(0,1))
    plot(density(as.numeric(temp$x)), xlab = "Export destribution", ylab = "Density",col = cl[t], xlim = c(1,global_max))
  }
  else{
    lines(density(as.numeric(temp$x)),col = cl[t])
  }
}
  
  #name<-paste(x1,"Export.csv", sep="")
  #write.csv(country_data, file = name)
  #x1=x1+d1
#} 
  g2<-(unique(product$commoditycode_1))