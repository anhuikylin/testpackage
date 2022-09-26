#' Title
#'
#' @param data View examples
#' @param group View examples
#' @param pca_digits decimal digits
#' @description pca plot
#' @return pca_data
#' @export PCA
#' @author Fei Liang
#' @title pca analysis plot
#' @import plyr
#' @import ggplot2
#' @importFrom stats prcomp
#' @examples
#' data <- data.frame(matrix(sample(1:100000, 150000, replace = TRUE),ncol=15))
#' colnames(data)=paste0(rep(LETTERS[1:5],each=3),rep(1:3,5))
#' rownames(data)=paste0('gene',1:10000)
#' data <- data.frame(data)
#' group <- data.frame('Group'=c(rep(LETTERS[1:5],each=3)),'samplenames'=colnames(data))
#' PCA(data,group,pca_digits = 2)
PCA <- function(data,group,pca_digits = 2) {
  yanse <- c('#00f000','#0000f0','#b30000','#f0f000',
             '#00f0f0','#a000f0','#f0a000','#7e3343',
             '#00f0a0','#fb8072','#80b1d3','#fdb462',
             '#b3de69','#fccde5','#bc80bd','#ccebc5',
             '#ffed6f','#64b267','#47a7bd','#f36621',
             '#31629d','#9fde00','#ffbe2a','#ec008c','#ff7404')
  data <- t(data)
  pca_data <- prcomp(data, scale = TRUE)
  x_per <- round(summary(pca_data)$importance[2, 1]*100, pca_digits)
  y_per <- round(summary(pca_data)$importance[2, 2]*100, pca_digits)
  df_sample <- data.frame(samplenames=rownames(pca_data$x), pca_data$x)
  df_sample <- plyr::join(df_sample,group, by = "samplenames")
  PC1 <- NA
  PC2 <- NA
  Group <- NA
  ggplot2::ggplot(df_sample, aes(x = PC1, y = PC2)) +
    geom_point(aes(colour=Group),size=3,shape=16) +
    theme_bw()+
    xlab(paste("PC1","(", x_per,"%)",sep=" ")) +
    ylab(paste("PC2","(", y_per,"%)",sep=" ")) +
    theme(legend.background = element_rect(colour="black", size=0.5))+
    scale_colour_manual(values = yanse[1:length(unique(group$Group))])
}
