#' Title
#'
#' @param data Multiomics data
#' @param group Sample grouping data
#' @description means calculate
#' @author Fei Liang
#' @title means calculate
#' @return all_sample_mean
#' @export means_calculate

means_calculate <- function(data,group) {
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
  return(all_sample_mean)
}
