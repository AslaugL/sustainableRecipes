#' @title plotPCAscores
#'
#' @description Creates a PCA score plot using ggplot.
#'
#' @param df Dataframe with the principal components to be plotted in the first two columns, and a group column to color by.
#' @param prc A vector with the percentage the principal components contribute, to be used for x and y axis titles.
#' @param interesting_samples Character vector of samples to label.
#'
#' @return A ggplot PCA score plot.
#'
#' @export

plotPCAscores <- function(df, prc, interesting_samples = NULL){

  #Get the names of the principal component columns to be used as x and y axis
  pc_x <- names(df[1])
  pc_y <- names(df[2])

  #Color manual to color group by
  color_manual <- colorManuals(df = df, group = TRUE, feature_anno = FALSE)

  #Plot PCA colored by annotation column
  plot <- ggplot(df, aes(x = !!ensym(pc_x), y = !!ensym(pc_y), color = group)) +
    geom_point() +

    #Set color by annotation
    scale_color_manual(values = color_manual$sample_group,
                       guide = guide_legend(order = 1)) + #Set as first legend

  #Add title, correct labels
    labs(color = 'Group',
         x = prc[1],
         y = prc[2])

  #Add a dashed line at x = 0, y = 0 to show center of plot
  plot %>% addLinesToPlot()
  
  print(head(df %>% arrange(PC1)))
  print(tail(df%>% arrange(PC1)))
  print(head(df %>% arrange(PC2)))
  print(tail(df%>% arrange(PC2)))
  
  #Add labels for interesting samples or not
  if(is.null(interesting_samples)){
    
    plot
    
  } 
  #If a character vector og sample_id's is supplies, label the samples
  else if (is.character(interesting_samples)){
    
    plot <- plot +
      geom_label_repel(data = df %>% filter(sample_id %in% interesting_samples),
                                            aes(label = sample_id), #Only show labels for the features above/below the threshold
                                            size = 2.5, box.padding = unit(0.2, "lines"), show.legend = FALSE) #Change size of labels and not show legend for this layer)
    
  } else {
    stop("interesting_samples must be a character vector of sample_id's in the dataframe, or NULL.")
  }

}
