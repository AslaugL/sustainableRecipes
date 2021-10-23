#' @title addLinesToPlot
#' @description Adds a gray dashed line to the plot at the intercepts
#' @param plot The plot to add the lines to.
#' @param yintercept The y axis intercept for the horizontal line.
#' @param xintercept The x axis intercept for the vertical line.
#' @return The plot with dashed gray lines as the x and y intercepts.
#'
#' @export

addLinesToPlot <- function(plot, yintercept = 0, xintercept = 0){

  plot <- plot +
    geom_vline(xintercept = xintercept, color = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = yintercept, color = 'gray', linetype = 'dashed')

}
