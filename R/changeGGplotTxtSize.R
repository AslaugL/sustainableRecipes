#' @title changeGGplotTxtSize
#'
#' @description Change the text size of the axis title and text of a ggplot2 plot.
#'
#' @param plot A ggplot.
#' @param txt_size New text size to be used in plot, default is 12.
#'
#' @return A ggplot with resized text.
#'
#' @export

changeGGplotTxtSize <- function(plot, txt_size = 12){
  plot + theme(axis.title = element_text(size = txt_size),
               axis.text = element_text(size = txt_size),
               legend.title = element_text(size = txt_size),
               legend.text = element_text(size = txt_size))
}
