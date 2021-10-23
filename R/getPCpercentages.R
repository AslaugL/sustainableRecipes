#' @title getPCpercentages
#'
#' @description Takes a prcomp object and calculates the percentages each PC contributes.
#'
#' @param prcomp_object A prcomp object.
#'
#' @return A dataframe with the % contributed by each principal component.
#'
#' @export

getPCpercentages <- function(prcomp_object){
  percentage <- round(((prcomp_object$sdev^2) / (sum(prcomp_object$sdev^2)))*100,2)
  percentage <- paste0(colnames(prcomp_object$x), " (", as.character(percentage), "%)")
}
