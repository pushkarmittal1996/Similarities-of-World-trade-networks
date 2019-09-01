rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
dat <- rescale(dat)
library(dplyr)
library(tidyr)
library(ggplot2)

## method 1. WSS :compute the total within sum square error, this measures how close
#  are the points in a cluster to each other 

# [Distance] : calculates the sum squared distance of a given cluster of points,
#              note that "sum squared distance" is used here for measuring variance 
Distance <- function(cluster)
{
  # the center of the cluster, mean of all the points
  center <- colMeans(cluster)
  
  # calculate the summed squared error between every point and 
  # the center of that cluster 
  distance <- apply( cluster, 1, function(row)
  {
    sum( ( row - center )^2 )
  }) %>% sum()
  
  return(distance)
}

# calculate the within sum squared error manually for hierarchical clustering 
# [WSS] : pass in the dataset, and the resulting groups(cluster)
WSS <- function( data, groups )
{
  k <- max(groups)
  
  # loop through each groups (clusters) and obtain its 
  # within sum squared error 
  total <- lapply( 1:k, function(k)
  {
    # extract the data point within the cluster
    cluster <- subset( data, groups == k )
    
    distance <- Distance(cluster)
    return(distance)
  }) %>% unlist()
  
  return( sum(total) )
}
CHCriterion <- function( data, kmax, clustermethod,linkmethod, ...  )
{
  if( !clustermethod %in% c( "kmeanspp", "hclust","pam" ) )
    stop( "method must be one of 'kmeanspp' or 'hclust' or 'pam'" )
  
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
      sil[k]  <- mean(silhouette(results$cluster, dist(data))[,3])
    }		
  } else if ( clustermethod == "pam" )
  {
    for( k in 2:kmax )
    {
      results <- pam( data, k)
      wss[k]  <- WSS( data = data, groups =  results$cluster )
      sil[k] <- mean(silhouette(results$cluster, dist(data))[,3])
      #sil[k]  <- mean(silhouette_val[, 3])
    }		
  }
  else # "hclust"
  {
    for( k in 2:kmax )
    {
      #clustering <- hclust(as.dist(data,diag = TRUE, upper = TRUE), method= 'complete',...)
      #sil[k]<- silhouette(cutree(clustering, k = k) ,as.dist(data))
      #rownames(sil_cl) <- rownames(tmp)
      clustering <- hclust( dist(data), method = linkmethod, ... )
      groups <- cutree( clustering, k )
      wss[k] <- WSS( data = data, groups =  groups )
      sil[k]  <- mean(silhouette(groups, dist(data))[, 3])
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
  print(paste(clustermethod,linkmethod))
  plot <- ggplot( criteria_long, aes( k, value, color = index)) + 
    geom_line() + geom_point( aes( shape = index ), size = 3 ) +
    facet_wrap( ~ index, scale = "free_y" ) + 
    guides( color = FALSE, shape = FALSE ) #+labs(title=paste(clustermethod,linkmethod), x ="Number of clusters", y = "Score")
  
  return( list( data = criteria, 
                plot = plot ) )
}
somePDFPath = "C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\some.pdf"
pdf(file=somePDFPath,width=8,height=4,paper='special')
criteria <- CHCriterion(data = pco.result$points, kmax = 50, clustermethod = "hclust", linkmethod = 'single')
criteria$plot
criteria <- CHCriterion(data = pco.result$points, kmax = 50, clustermethod = "hclust", linkmethod = 'complete')
criteria$plot
criteria <- CHCriterion(data = pco.result$points, kmax = 50, clustermethod = "hclust", linkmethod = 'average')
criteria$plot
criteria <- CHCriterion(data = pco.result$points, kmax = 50, clustermethod = "kmeanspp", linkmethod = 'average')
criteria$plot
criteria <- CHCriterion(data = pco.result$points, kmax = 50, clustermethod = "pam", linkmethod = 'average')
criteria$plot
somePDFPath = "C:\\Users\\PUSHKAR\\Desktop\\Sem2\\New folder\\Code\\2D.pdf"
pdf(file=somePDFPath,width=8,height=4,paper='special')
for (i in seq(2,51))   
{   
  plot(fviz_cluster(Kmeanspp( pco.result$points, i), pco.result$points, frame = FALSE, geom = "point",main = paste("Kmeans, number of clusters =",i)))
  plot(fviz_cluster(pam( pco.result$points, i), data, frame = FALSE, geom = "point",main = paste("Kmedioids, number of clusters =",i)))
  #criteria <- CHCriterion(data = Adjacency, kmax = 20, clustermethod = "hclust", linkmethod = 'single')
  plot(fviz_cluster(hcut(pco.result$points, k = i, hc_method = "complete"), data, frame = FALSE, geom = "point",main = paste("Complete link, number of clusters =",i)))
  #criteria <- CHCriterion(data = Adjacency, kmax = i, clustermethod = "hclust", linkmethod = 'complete')
  plot(fviz_cluster(hcut(pco.result$points, k = i, hc_method = "single"), data, frame = FALSE, geom = "point",main = paste("Single link, number of clusters =",i)))
  plot(fviz_cluster(hcut(pco.result$points, k = i, hc_method = "average"), data, frame = FALSE, geom = "point",main =  paste("Average link, number of clusters =",i)))
} 
dev.off() 
k=9
data<-read.csv(paste("netemd_data ",as.character(k)," .csv",sep=""), header = T, sep=",")
rownames(data)<-data[,1]
data$X<-NULL
diag(data)<-0
#data <- forceSymmetric(data)
data[lower.tri(data)]<-data[upper.tri(data)]
data<-as.matrix(data)
pco.result <- wcmdscale(data, eig = TRUE, add="lingoes", k = 2)
i=12
plot(fviz_cluster(Kmeanspp( pco.result$points, i), pco.result$points, frame = FALSE, geom = "point",main = paste("Kmeans, number of clusters =",i)))
i=7
plot(fviz_cluster(pam( pco.result$points, i), data, frame = FALSE, geom = "point",main = paste("Cluster",k,", Kmedioids, number of clusters =",i)))
i=9
#criteria <- CHCriterion(data = Adjacency, kmax = 20, clustermethod = "hclust", linkmethod = 'single')
plot(fviz_cluster(hcut(pco.result$points, k = i, hc_method = "complete"), data, frame = FALSE, geom = "point",main = paste("Cluster",k,", Complete link, number of clusters =",i)))
#criteria <- CHCriterion(data = Adjacency, kmax = i, clustermethod = "hclust", linkmethod = 'complete')
i=7
plot(fviz_cluster(hcut(pco.result$points, k = i, hc_method = "single"), data, frame = FALSE, geom = "point",main = paste("Cluster",k,", Single link, number of clusters =",i)))
i=8
plot(fviz_cluster(hcut(pco.result$points, k = i, hc_method = "average"), data, frame = FALSE, geom = "point",main =  paste("Cluster",k,", Average link, number of clusters =",i)))
dev.off()