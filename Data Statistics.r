setwd("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\CSV")
#Adjacency<-read.csv("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\Adjacency.csv", header = T, sep=",")
#val<-as.matrix.data.frame(Adjacency,rownames.force = TRUE)
#val2<-val[,-1]
#rownames(val2) <- val[,1]
#rownames(val2)=substring(rownames(val2),2)
#colnames(val2)=substring(colnames(val2),2)
#g=graph.adjacency(val2, mode="undirected",weighted=TRUE,diag=FALSE)
#x=0.5
#g <- delete.edges(g, which(E(g)$weight < x))
df_no_trns<-matrix(ncol=22, nrow=235)
df_no_com<-matrix(ncol=22, nrow=235)
#df_avg_com<-matrix(ncol=22, nrow=235)
df_avg_country<-matrix(ncol=22, nrow=235)
df_avg_deg<-matrix(ncol=22, nrow=235)
for(i in 1995:2016){
  print(paste("File",as.character(i),"started"))
  main_data<-read.csv(paste("country_partner_hsproduct4digit_year_",as.character(i),".csv",sep = ""), header = T, sep=",")
  print(paste("File",as.character(i),"read completed"))
  nc=length(data.frame(table(main_data$location_code))[,2])
  df_no_trns[1:nc,i-1994]<-data.frame(table(main_data$location_code))[,2]
  data<-aggregate(main_data$export_value, by=list(hs_product_code=main_data$hs_product_code,location_name_short_en=main_data$location_name_short_en) , FUN=sum)
  data<-data[which(data$x>0), ]
  df_no_com[1:nc,i-1994]<-data.frame(table(data$location_name_short_en))[,2]
  df_avg_country[1:length(aggregate(data$x, by=list(location_name_short_en=data$location_name_short_en) , FUN=mean)$x),i-1994]<-aggregate(data$x, by=list(location_name_short_en=data$location_name_short_en) , FUN=mean)$x
  data<-data[which(data$x>quantile(data$x,probs = 0.20)), ]
  data$hs_product_code<-as.character(data$hs_product_code) #convert from original format to charecter
  data$hs_product_code<-as.numeric(data$hs_product_code) #to remove the leading zeros in the names
  data$hs_product_code<-as.character(data$hs_product_code)
  data<-aggregate(main_data$export_value, by=list(location_name_short_en=main_data$location_name_short_en,partner_name_short_en=main_data$partner_name_short_en) , FUN=sum)
  data<-data[which(data$x>0), ]
  df_avg_deg[1:nc,i-1994]<-data.frame(table(data$location_name_short_en))[,2]
  print(paste("File",as.character(i),"process completed"))
}
df_no_trns<-matrix(ncol=22, nrow=250)
df_no_com<-matrix(ncol=22, nrow=250)
#df_avg_com<-matrix(ncol=22, nrow=235)
df_avg_country<-matrix(ncol=22, nrow=250)
df_avg_deg<-matrix(ncol=22, nrow=250)
write.csv(df_no_trns, file = "df_no_trns1.csv")
write.csv(df_no_com, file = "df_no_com1.csv")
write.csv(df_avg_country, file = "df_avg_country1.csv")
write.csv(df_avg_deg, file = "df_avg_deg1.csv")

boxplot()