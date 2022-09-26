#' Title
#' @author Fei Liang
#' @description  Numeric or character positioning.
#'
#' @param data A numerical matrix or table
#' @param str The character or number you want to search.
#'
#' @return result
#' @export search2022
#' @examples
#' testpackage::search2022(mtcars,27.3)
#' testpackage::search2022(mtcars,0)
search2022 <- function(data, str) {
  a <- which(data == str)
  row <- a %% dim(data)[1]
  col <- ceiling(a / dim(data)[1])
  result <- data.frame('row' = row,
                       'col' = col)
  return(result)
}
