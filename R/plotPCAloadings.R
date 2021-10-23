#' @title plotPCAloadings
#'
#' @description Creates a PCA loadings plot using ggplot.
#'
#' @param df A dataframe with the principal components to be plotted in the first two columns.
#' @param cutoff Numerical value used to label all features with a loading >cutoff or <-cutoff. Default NULL.
#'
#' @return A PCA loadings plot.
#'
#' @export

plotPCAloadings <- function(df, cutoff = NULL){

  #Get the names of the principal component columns to be used as x and y axis
  pc_x <- names(df[1])
  pc_y <- names(df[2])

  #Plot the loadings colored by feature_annotation or not
  if('feature_anno' %in% names(df)){
    #Create a color manual for the feature annotations
    #Temporary color manual with all colors
    temp <- colorManuals(df = df, group = FALSE, feature_anno = TRUE)
    #Only select those feature_annotations that are in df
    color_manual <- list(
      'feature_anno' = temp$feature_anno[unique(as.character(df$feature_anno))])

    temp <- ggplot(df, aes(x = !!ensym(pc_x), y = !!ensym(pc_y), color = feature_anno)) +
      scale_color_manual(values = color_manual$feature_anno)
  } else{
    temp <- ggplot(df, aes(x = !!ensym(pc_x), y = !!ensym(pc_y)))
  }

  #Make the rest of the plot
  plot <- temp +
    geom_point(alpha = 0.8, size = 2) +

  #Set names of axis ad fix color legend so it doesn't show letters
    labs(
      color = '',
      x = paste0('Loadings ', pc_x),
      y = paste0('Loadings ', pc_y)
    ) +
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

  #Add labels/names to the features above/below the threshold
  if(is.numeric(cutoff)){
    plot <- plot +
      geom_label_repel(data = df %>% filter(df[1] > cutoff | df[2] > cutoff | df[1] < -cutoff | df[2] < -cutoff),
                       aes(label = feature), #Only show labels for the features above/below the threshold
                       size = 2.5, box.padding = unit(0.2, "lines"), show.legend = FALSE) #Change size of labels and not show legend for this layer

  }else if(is.null(cutoff)){
    plot
  } else {
    stop('Cutoff must be either a numeric or NULL.')
  }

  plot %>% addLinesToPlot(.)

}
