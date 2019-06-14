#' An Outlier removal Function
#'
#' @title This package enables you to remove the outliers in your dataset in one go. If you find more outliers then repeat the provess to obtain cleaned dataset.
#' @param outlier Only the range of values are to be inputed
#' @keywords outliers, oultlier, removal
#' @export
#' @examples outlierFUN(mtcars$disp)
#' outlierFUN()
outlierFUN <- function(x, na.rm =TRUE, ...)
{
  qnt <- quantile(x, probs = c(.25,.75), na.rm = na.rm,...)
  H <- 1*IQR(x,na.rm = na.rm)

  y <- x
  y[ x < (qnt[1] - H)] <- NA
  y[ x > (qnt[2] + H)] <- NA
  x <- y
}
