#' myCorrelations but with changed textsize.
#' @title myCorrelations_textsize
#'
#' @description Creates the upper part of ggpairs with correlation scores by group from corr.test, and groups can by customly colored.
#'
#' @param data Uses the data loaded from ggpairs.
#' @param mapping Uses the mapping from ggpairs.
#' @param ... Uses arguments from ggpairs.
#'
#' @return The upper segment of ggpairs with correlation scores from corr.test, colored with custom colors.
#'
#' @export

myCorrelations_textsize <- function(data,mapping,...){
  
  #Get the data to use for correlation analysis
  data2 = data
  data2$x = as.numeric(data[,as_label(mapping$x)])
  data2$y = as.numeric(data[,as_label(mapping$y)])
  data2$group = data[,as_label(mapping$colour)]
  
  #Split into separate datasets based on group and run corr.test each + all groups combined
  to_correlate <- data2 %>%
    select(group, x, y) %>%
    #Split into individual df's and remove group column as it's numeric
    split(f = as.factor(.$group)) %>%
    lapply(., select, ... = -group)
  #Add the df with all groups to the list  
  to_correlate$`Overall Corr` <- data2 %>%
    select(x,y)
  
  #Run correlation analysis on each list element, and turn into a single df before plotting
  correlations <- lapply(to_correlate, corr.test, method = 'spearman', adjust = 'BH') %>%
    #Get the estimate, p.value and create a column with symbols that show stat.sig. og p value
    lapply(., collectCorr) %>%
    #Bind together
    bind_rows(., .id = 'group')
  
  ggplot(data = correlations, aes(x = 1, y = factor(group, levels = c('Overall Corr', 'US', 'UK', 'Norway')), color = group)) +
    geom_text(aes(label=paste0(group, ": ", estimate, pvalue_star)), size = 3.5)
  
}
