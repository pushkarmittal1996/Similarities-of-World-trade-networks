setwd("C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code")
Adjacency<-read.csv("output.csv", header = T, sep=",")
rownames(Adjacency)<-Adjacency[,1]
Adjacency$X<-NULL
diag(Adjacency)<-0
library(Matrix)
Adjacency <- forceSymmetric(Adjacency)
Adjacency[lower.tri(Adjacency)]<-Adjacency[upper.tri(Adjacency)]
Adjacency<-as.matrix(Adjacency)
library(vegan)
require(vegan)
pco.result <- wcmdscale(Adjacency, eig = TRUE, add="lingoes", k = 75)
plot(pco.result$points, type = "p", asp = 1/1)
library(ape)
pco=pcoa(dist(Adjacency), correction="none", rn=NULL)
library(magrittr)
names(clusterCut)[clusterCut==1]
clusterCut <- cutree(hclust(dist(Adjacency), method = 'complete'), 20)
data<-as.matrix(pco.result$points)
library(cluster) 
library(corrplot)
library(factoextra)
fviz_nbclust(data, kmeans, method='silhouette')
fviz_nbclust(data, kmeans, method='gap_stat')
silhouette_score <- function(k){
  km <- kmeans(data, centers = k, nstart=25, iter.max = 25)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}
wss_score <- function(k){
  km <- kmeans(data, centers = k, nstart=25, iter.max = 25)
  ss <- WSS(km$cluster, dist(data))
  mean(ss[, 3])
}
silhouette_score <- function(k){
  km <- kmeans(data, centers = k, nstart=25, iter.max = 25)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}
wss_score <- function(k){
  km <- kmeans(data, centers = k, nstart=25, iter.max = 25)
  ss <- WSS(km$cluster, dist(data))
  mean(ss[, 3])
}
k <- 2:100
avg_sil <- sapply(k, silhouette_score)
avg_wss<-sapply(k, wss_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
plot(k, type='b', avg_wss, xlab='Number of clusters', ylab='wss Scores', frame=FALSE)

CHCriterion <- function( data, kmax, clustermethod, ...  )
{
  if( !clustermethod %in% c( "kmeanspp", "hclust" ) )
    stop( "method must be one of 'kmeanspp' or 'hclust'" )
  
  # total sum squared error (independent with the number of cluster k)
  tss <- Distance( cluster = data )
  
  # initialize a numeric vector storing the score
  wss <- numeric(kmax)
  sil <- numeric(kmax)
  
  # k starts from 2, cluster 1 is meaningless
  if( clustermethod == "kmeanspp" )
  {
    for( k in 2:kmax )
    {
      results <- Kmeanspp( data, k, ... )
      wss[k]  <- results$tot.withinss
      sil[k]  <- silhouette(results$cluster, dist(data))
    }		
  }else # "hclust"
  {
    for( k in 2:kmax )
    {
      #clustering <- hclust(as.dist(data,diag = TRUE, upper = TRUE), method= 'complete',...)
      #sil[k]<- silhouette(cutree(clustering, k = k) ,as.dist(data))
      #rownames(sil_cl) <- rownames(tmp)
      clustering <- hclust( dist(Adjacency), method = 'complete', ... )
      groups <- cutree( clustering, k )
      wss[k] <- WSS( data = data, groups =  groups )
      sil[k]  <- mean(silhouette(groups, dist(Adjacency))[, 3])
    }
  }		
  
  # between sum of square
  bss <- tss - wss[-1]
  
  # cluster count start from 2! 
  numerator <- bss / ( 1:(kmax-1) )
  denominator <- wss[-1] / ( nrow(data) - 2:kmax )
  
  criteria <- data.frame( k = 2:kmax,
                          CHIndex = numerator / denominator,
                          wss = wss[-1], sil=sil[-1])
  
  # convert to long format for plotting 
  criteria_long <- gather( criteria, "index", "value", -1 )
  
  plot <- ggplot( criteria_long, aes( k, value, color = index ) ) + 
    geom_line() + geom_point( aes( shape = index ), size = 3 ) +
    facet_wrap( ~ index, scale = "free_y" ) + 
    guides( color = FALSE, shape = FALSE )
  
  return( list( data = criteria, 
                plot = plot ) )
}
