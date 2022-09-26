#means_calculate_result
#' Title
#'
#' @param data matrix
#' @param centers Number of clusters
#' @param row Number of drawing columns line
#' @param col Number of drawing columns
#' @param center_col Color of center line
#' @param las Whether to rotate the abscissa, 1 means no rotation and 2 means rotation.
#'
#' @return data_new2
#' @export kmeans_plot4
#'
#' @examples
#' runif(10)
kmeans_plot4 <- function(data,centers,row,col,center_col = 'black',las = 2) {
  data_scale <- round(t(apply(data, 1, scale)),2)
  colnames(data_scale) <- colnames(data)
  set.seed(400)
  # Data transformation -----------------------------------------------------
  cl <- kmeans(data_scale,centers = centers)
  table(cl$cluster)
  data_new <- data.frame('Cluster'=cl$cluster,data_scale)
  data_new <- data.frame('meta'=rownames(data_new),data_new)
  data_new$Cluster = paste0("cluster",data_new$Cluster)
  colnames(data_new)[3:length(colnames(data_new))]<- colnames(data)
  data_new2 <- data_new
  data_new = reshape2::melt(data_new)

  # plot1 -------------------------------------------------------------------


  cluster <- unique(data_new$Cluster)
  cluster <- cluster[order(cluster)]
  par(mfrow = c(row,col))
  for (k in cluster) {


    data_new$Cluster2 <-rep(1:length(data),#
                            each = length(data_new$variable)/length(data))#
    meta_num <- data_new2[data_new2$Cluster==k,]$meta
    aggregate(data_new[data_new$Cluster==k,]$value,
              by=list(type=data_new[data_new$Cluster==k,]$Cluster2),mean)
    cluster_mean <- data.frame(aggregate(data_new[data_new$Cluster==k,]$value,
                                         by=list(type=data_new[data_new$Cluster==k,]$Cluster2),mean))
    plot(cluster_mean$type,
         cluster_mean$x,
         axes = FALSE,
         xlab="",
         ylab="Standardised value",
         ylim = c(-3,3),
         col= center_col,
         pch=16,cex=0
    )
    lines(cluster_mean$type,
          cluster_mean$x,
          col= center_col,
          lwd=3)
    #points(as.numeric(data_new[data_new$Cluster==k & data_new$gene=='m61',]$Cluster2),
    #data_new[data_new$Cluster==k & data_new$gene=='m61',]$value,
    #col='black',pch=16,cex = 2)
    text.x<-colnames(data)
    axis(1, 1:length(colnames(data)), text.x,las= las)
    axis(2)
    #axis(3,labels = FALSE,tick = FALSE)
    #axis(4,labels = FALSE,tick = FALSE)
    #box()
    point_and_line_col <- rainbow(length(cluster))
    for (i in meta_num) {

      lines(as.numeric(data_new[data_new$Cluster==k & data_new$meta==i,]$Cluster2),
            data_new[data_new$Cluster==k & data_new$meta==i,]$value,
            col= point_and_line_col[grep(k,cluster,value=F)])
    }


    # #point_and_line_col <- rainbow(length(cluster))
    # points(cluster_mean$type,
    #        cluster_mean$x,
    #        col='black',
    #        pch=16,cex=2)
    lines(cluster_mean$type,
          cluster_mean$x,
          col='black',
          lwd=3)
    title(main = paste0(k,': ',length(meta_num)))
    point_and_line_col <- rainbow(length(cluster))


    title(main = paste0(k,': ',length(meta_num)))
  }

  return(data_new2)
}
