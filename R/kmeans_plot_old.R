#' Generate a time series of fractional Brownian motion.
#'
#' This function generatea a time series of one dimension fractional Brownian motion.
#' adapted from http://www.mathworks.com.au/matlabcentral/fileexchange/38935-fractional-brownian-motion-generator .
#'
#' @details Big data trend analysis
#' @param data Expression matrix
#' @param group Sample group
#' @param cluster_num Number of clusters
#' @param row plot number of row
#' @param col plot number of col
#' @export kmeans_plot_old
#' @import readxl
#' @import ggplot2
#' @import reshape2
#' @import ggthemes
#' @import RColorBrewer
#' @import vegan
#' @import permute
#' @import lattice
#' @importFrom grDevices rainbow
#' @importFrom graphics axis box lines par points title
#' @importFrom stats aggregate kmeans
#' @importFrom utils write.csv
#' @name kmeans_plot_old
#' @examples
#' runif(10)

library(readxl)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(vegan)
library(permute)
library(lattice)
kmeans_plot_old <- function(data, group, cluster_num,row,col) {

  all_sample_mean <- list()
  all_sample_col_name <- list()
  for (n in round(1:length(unique(group$Group)))) {
    sample_mean <- data[, which(group$Group == unique(group$Group)[n])]
    sample_mean <- apply(sample_mean, 1, mean)
    sample_mean <- data.frame(col_sample = sample_mean)
    colnames(sample_mean) <- unique(group$Group)[n]
    all_sample_mean[n] <- sample_mean
    all_sample_col_name[n] <- unique(group$Group)[n]
    all_sample_mean <- data.frame(all_sample_mean)
    colnames(all_sample_mean) <- all_sample_col_name
    rownames(all_sample_mean) <- rownames(data)
  }
  kmeans_data_scale <- vegan::decostand(
    all_sample_mean,
    method = "standardize",
    MARGIN = 1,
    logbase = 2,
    na.rm = FALSE
  )
  rownames(kmeans_data_scale) <- rownames(data)
  set.seed(400)
  cl <- kmeans(kmeans_data_scale, centers = cluster_num)
  table(cl$cluster)
  data_new <- data.frame('Cluster' = cl$cluster, kmeans_data_scale)
  # result <- data.frame('Cluster'=cl$cluster,kmeans_data)
  # write.csv(result,'result.csv')
  data_new <- data.frame('meta' = rownames(data_new), data_new)
  data_new$Cluster = paste0("cluster", data_new$Cluster)
  colnames(data_new)[3:length(colnames(data_new))] <-
    colnames(all_sample_mean)
  data_new2 <- data_new
  data_new = melt(data_new)
  cluster <- unique(data_new$Cluster)
  cluster <- cluster[order(cluster)]

  par(mfrow = c(row,col))
  for (k in cluster) {


    data_new$Cluster2 <-rep(1:length(unique(group$Group)),
                            each = length(data_new$variable)/length(unique(group$Group)))
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
         col='red',
         pch=16,cex=2)
    lines(cluster_mean$type,
          cluster_mean$x,
          col='red',
          lwd=3)
    #points(as.numeric(data_new[data_new$Cluster==k & data_new$gene=='m61',]$Cluster2),
    #data_new[data_new$Cluster==k & data_new$gene=='m61',]$value,
    #col='black',pch=16,cex = 2)
    text.x<-colnames(all_sample_mean)
    axis(1, 1:length(colnames(all_sample_mean)), text.x,,las="2")
    axis(2)
    axis(3,labels = FALSE,tick = FALSE)
    axis(4,labels = FALSE,tick = FALSE)
    box()
    for (i in meta_num) {

      lines(as.numeric(data_new[data_new$Cluster==k & data_new$meta==i,]$Cluster2),
            data_new[data_new$Cluster==k & data_new$meta==i,]$value,
            col='grey')
    }


    point_and_line_col <- rainbow(length(cluster))
    points(cluster_mean$type,
           cluster_mean$x,
           col=point_and_line_col[grep(k,cluster,value=F)],
           pch=16,cex=2)
    lines(cluster_mean$type,
          cluster_mean$x,
          col=point_and_line_col[grep(k,cluster,value=F)],
          lwd=3)
    title(main = paste0(k,': ',length(meta_num)))
    point_and_line_col <- rainbow(length(cluster))


    title(main = paste0(k,': ',length(meta_num)))
  }

}
