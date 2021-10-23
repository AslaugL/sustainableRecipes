#' @title createPCA
#'
#' @description Creates a PCA loadings plot using ggplot.
#'
#' @param df A tidy dataframe with at least sample_id, group, feature and value columns.
#' @param principal_components Principal components to be used as x and y axis, default is 1 and 2.
#' @param plots Which plots should be made? 'scores', 'loadings' or 'both'.
#' @param cutoff Numerical value used to label all features with a loading >cutoff or <-cutoff. Default NULL.
#' @param interesting_samples Character vector of samples to label in the score plot.
#'
#' @return A PCA loadings plot.
#'
#' @export

createPCA <- function(df, principal_components = c(1,2), plots = 'scores', interesting_samples = NULL, cutoff = NULL) {

  #Do the PCA analysis
  prcomp_data <- df %>% PCAprep(.) %>% prcomp(., scale. = FALSE)

  #Get metadata for coloring
  meta <- list()
    #Samples
    meta$samples <- df %>% select(c(sample_id, group)) %>% unique()
    #Features
    if('feature_anno' %in% names(df)){
      meta$features <- df %>% select(c(feature, feature_anno)) %>% unique()
    }else{
      meta$features <- df %>% select(feature) %>% unique()
    }

  ## Get the data needed to plot in one dataframe ##

  #Principal components scores
  plot_samples <- as_tibble(prcomp_data$x) %>% #The principal components
    dplyr::select(principal_components) %>% #Filter out the principal components of interest

    #Add subject metadata
    mutate(sample_id = meta$samples$sample_id) %>%
    inner_join(., meta$samples) #This could be a provided df to include other metadata, such as age group or gender?

  #Get percentage the different PC's contribute
  percentages <- getPCpercentages(prcomp_data)

  #Features contributing the most to the PC plotPCAloadings <- function(df, feature_annotations = 'no', addLabels = FALSE, cutoff)
  loadings <- getPCAloadings(prcomp_data, principal_components = principal_components) %>%
    #Add annotatons to loadings
    inner_join(., meta$features)

  #Which plot to plot?
  if(plots == 'scores'){
    plot <- plotPCAscores(plot_samples, prc = percentages[principal_components], interesting_samples = interesting_samples)
  } else if (plots == 'loadings') {
    plot <- plotPCAloadings(loadings, cutoff = cutoff)
  } else if (plots == 'both') { #Stopped working for some reason
    plot <- plot_grid(plotPCAscores(plot_subjects, prc = percentages[principal_components]),
                      plotPCAloadings(loadings, cutoff = cutoff),
                      labels = 'AUTO',
                      ncol = 2)
  } else {
    stop("Please define which plot you want. 'scores', 'loadings' or 'both'")
  }

  plot %>% changeGGplotTxtSize(.)

}
