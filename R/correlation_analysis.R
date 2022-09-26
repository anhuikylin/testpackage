
#' Title
#'
#' @param data A matrix for analyzing correlation.
#' @param break_length Number of segments divided into legend
#' @param length_lim Legend range
#' @param number_digits Reserved decimal places
#' @description Correlation analysis plot
#' @return result
#' @export correlation_analysis
#' @import corrplot
#' @importFrom grDevices colorRampPalette
#' @author Fei Liang
#' @title Correlation analysis plot
#'
#' @examples
#' correlation_analysis(mtcars)
correlation_analysis <- function(data,break_length = 5,length_lim = c(0,1),number_digits = 3) {
  result <- corrplot(abs(stats::cor(data)), type = 'lower',
           method = 'color',
           #order = 'AOE',
           diag = FALSE,
           tl.pos = 'ld',#字体位置
           cl.pos = 'r',#图例位置
           tl.col = 'black',#轴字体颜色
           cl.length = break_length, #数字大小
           addCoef.col = 'white',#数字颜色
           col.lim = length_lim,#范围必须覆盖矩阵
           col = colorRampPalette(c("#d53e4f",
                                    '#fdb366',
                                    "#6cc4a4",
                                    '#d6de96',
                                    "#d53e4f"))(100),
           outline = "white",
           number.digits = number_digits
  )
  return(result)
}
