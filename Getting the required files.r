folder<-choose.dir(default = "C:\\Users", caption = "Select Working directory")
setwd(folder)
#Get the following packages
install.packages("googledrive")
install.packages('devtools')
install.packages('orca')
install.packages('igraph')
install.packages('rgl')
install.packages('Matrix')
install.packages('vegan')
install.packages('visNetwork')
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
devtools::install_github('alan-turing-institute/network-comparison')
#Adjacency matrix of product space
drive_download(
  "https://drive.google.com/open?id=1dGztZkHDb-xwI6dzqCZM14CLy4rN-23z",
  path = "Adjacency.csv"
)
#HS color encoding
drive_download(
  "https://drive.google.com/open?id=1aeQsGAiFDe-wV9HUWdyNxtGatu2kPRnA",
  path = "color_scheme.csv"
)
#Transactions Data for 2016
drive_download(
  "https://drive.google.com/open?id=1W-3VtajCd8QM_f50IWxUqDBrSzrPLTm2",
  path = "data.csv"
)
#Output of NetEMD on 2016 data
drive_download(
  "https://drive.google.com/file/d/1-d-qfQCzqhOMuXbz2k7PrUTtIybWTzr3",
  path = "output.csv"
)
#Kmeans++ code
drive_download(
  "https://drive.google.com/open?id=1cm0P4fi4eruJpzQvtW2w9SQYKqGLrx6c",
  path = "kmeanspp.r"
)
