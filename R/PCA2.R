#' Title
#' @author Fei Liang
#' @description pca plot
#' @import plyr
#' @import car
#' @importFrom stats prcomp
#' @param data Sample matrix
#' @param group Sample group
#' @param col dot and ellipse color.
#' @param size point size
#' @param points_type points_type
#' @param line_type ellipse line_type
#' @param title title
#' @param xlim_min Axis margin
#' @param xlim_max Axis margin
#' @param ylim_min Axis margin
#' @param ylim_max Axis margin
#' @param legend Logic value, whether to display legend or not.
#' @param legend_group The name of the group in the legend
#' @param edge_number_of_ellipse Number of confidence interval edges
#'
#' @return pca_data
#' @export PCA2
#'
#' @examples
#' pca_test_data <- pca_test_data
#' pca_test_group <- pca_test_group
#' testpackage::PCA2(data = pca_test_data,
#'  group = pca_test_group)
#' pca_test_data <- pca_test_data[, c(1:8, 17:24)]
#' pca_test_group <- pca_test_group[c(1:8, 17:24), ]
#' testpackage::PCA2(data = pca_test_data,
#' group = pca_test_group,
#' col = c("#0000f0", "#b30000"),
#' legend_group = c("B", "C"),
#' edge_number_of_ellipse = 4,
#' size = 2,
#' xlim_min = 2,
#' ylim_min = 1.3,
#' ylim_max = 0.7)
#' testpackage::PCA2(data = pca_test_data,
#' group = pca_test_group,
#' col = c("#0000f0", "#b30000"),
#' legend_group = c("B", "C"),
#' edge_number_of_ellipse = 3,
#' size = 2,
#' xlim_min = 2,
#' ylim_min = 1.3,
#' ylim_max = 0.7)

PCA2 <- function(data, group, col = c('#00f000', '#0000f0', '#b30000'),
                 size = 3,points_type = 15:17,
                 line_type = 1, title = 'Principal Component Analysis',
                 xlim_min = 1.5, xlim_max = 2,
                 ylim_min = 1.1, ylim_max = 1.5,
                 legend = TRUE,
                 legend_group = c("A", "B", "C "),
                 edge_number_of_ellipse = 10) {
  data <- t(data)#转置数据
  colnames(group)[1] <- "samplenames"
  colnames(group)[1] <- "Group"
  #主成分计算
  pca_data <- prcomp(data, scale = TRUE)
  #查看合适主成分个数
  #screeplot_result <- screeplot(pca_data, type = "lines")
  summary(pca_data)
  #查看行名，确认是否为样品的名称
  rownames(pca_data$x)
  #提取PC1的百分比
  x_per <- round(summary(pca_data)$importance[2, 1] * 100, 2)
  #提取PC2的百分比
  y_per <- round(summary(pca_data)$importance[2, 2] * 100, 2)
  #按照样品名称添加组并且合并
  df_sample <-
    data.frame(samplenames = rownames(pca_data$x), pca_data$x)
  colnames(group)[1] <- 'samplenames'
  df_sample <- join(df_sample, group, by = "samplenames")
  #数据尾部添加变量Group，只截取部分数据展示
  df_sample
  df_sample2 <- data.frame(df_sample[, 2:3])
  df_sample2$Group <- df_sample$Group
  # 1 -----------------------------------------------------------------------

  par(mfrow = c(1, 1))
  with(
    df_sample,
    dataEllipse(
      x = PC1,
      y = PC2,
      groups = factor(Group),
      group.labels = NA,
      pch = points_type,
      lty = line_type,#椭圆线的类型
      lwd=1,#椭圆线的粗细
      cex = size,
      segments = edge_number_of_ellipse,
      xlim = c(
        min(df_sample2$PC1) - abs(min(df_sample2$PC1) * xlim_min),
        max(df_sample2$PC1) + abs(max(df_sample2$PC1) *
                                    xlim_max)
      ),
      center.cex=0,
      #center.pch="+",
      #group.labels=c("setosa", "versicolor", "virginica "),
      ylim = c(
        min(df_sample2$PC2) - abs(min(df_sample2$PC2) * ylim_min),
        max(df_sample2$PC2) + abs(max(df_sample2$PC2) *
                                    ylim_max)
      ),
      level = .95,
      fill = TRUE,
      fill.alpha = 0.3,
      xlab = paste("PC1", "(", x_per, "%)", sep =
                     " "),
      ylab = paste("PC2", "(", y_per, "%)", sep =
                     " "),
      col = col,
      grid = TRUE
    )
  )
  if (legend == TRUE) {
    legend(
      "topright",
      legend = factor(legend_group),
      col = col,
      lty = line_type,
      pch = points_type,
      bg = NA
    )
  }

  title(title)
  return(pca_data)
  #return(screeplot_result)
}
