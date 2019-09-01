file<-choose.files(default = "", caption = "Select files")
folder<-choose.dir(default = "", caption = "Select directory")
setwd(folder)
data<-read.csv(file, header = T, sep=",")
countries<-as.character(unique(data$location_name_short_en))
for(row in countries){
  temp<-data[which(data$export_value>0 & data$location_name_short_en==row), ]
  name<-paste(row,data$year[1],"Export.csv", sep="")
  write.csv(temp, file = name)
  print(row)
}