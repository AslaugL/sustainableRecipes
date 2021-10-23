#' @title plotViolinBox
#'
#' @description Create a half violin/half boxplot with individual data points.
#'
#' @param df Dataframe with a 'feature' column with feature names, a 'sample_id' column with sample ID's, a 'value' column with feature values for the samples, and a 'group' column splitting the samples into two groups (cases and controls).
#' @param r_statix An optional r statix t-test/wilcoxon test result table, that add a string of stats to the title of the plot.
#'
#' @return A half violin/half boxplot with individual data points.
#'
#' @export

plotViolinBox <- function(df, r_statix = NULL) {

  if(!is.null(r_statix)){
    data <- inner_join(df, extractStats(r_statix, filterp = 'no'))
  }else{data <- df}

  #Color manual to color group by
  color_manual <- colorManuals(df = df, group = TRUE, feature_anno = FALSE)

  plot <- ggplot(data, aes(x = group, y = value, color = group)) +
    scale_color_manual(values = color_manual$sample_group) +

    #Add mock linetypes for the legend
    geom_line(aes(linetype = 'dashed')) +
    geom_line(aes(linetype = 'solid')) +

    #Build the jitterplot or dotplot, half violin and half boxplot
    geom_half_violin(side = 'l') +
    geom_half_boxplot(side = 'r', outlier.size = 0) +
    geom_quasirandom(width = 0.2, alpha = 0.5) +

    #Add the stippled line for the mean
    stat_summary(fun = mean, geom = 'errorbar',
                 aes(ymax = ..y.., ymin = ..y..),
                 linetype = 'dashed',
                 width = 0.38, position = position_nudge(x = 0.185)) + #Add dashed line to indicate mean

    #Make it nicer
    labs(
      x = 'Group',
      y = 'Value',
      color = 'Group'
    ) +

    scale_linetype_manual(name = 'Boxplot summary',
                          values = c('solid', 'dashed'),
                          labels = c('Median', 'Mean')) +

    #Set legend to the right
    theme(legend.position = 'right')

  if(!is.null(r_statix)){
    plot <- plot + facet_wrap(~string, scales = 'free_y')
  }else{plot}
}
