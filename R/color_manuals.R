#' @title groupColors
#' @description Create a color manual for the group factor to be used in plots.
#' @param df The dataframe with data to be plotted.
#' @return  A list with a named vector with colors for the variables in the 'group' column.
#' @export

groupColors <- function(df){

  data <- df %>% mutate_at('group', ~as.factor(.))

  group_colors <- list(
    'sample_group' = setNames(c('#44AA99', '#AA4499', '#A87742'),
                              unique(data$group)))
}

#' @title featureAnnotationColors
#' @description Create a color manual for the feature annotation factor to be used in plots.
#' @param df The dataframe with data to be plotted.
#' @return  A list with a named vector with colors for the variables in the 'feature_anno' column.
#' @export

featureAnnotationColors <- function(df){

  if('feature_anno' %in% names(df)){
  data <-df %>%
    mutate_at('feature_anno', ~as.factor(.))

  #Does feature_anno provide new feature annotations? Create a color manual for each individual feature annotation category
  feature_anno_colors <- list(
    'feature_anno' = setNames(c('#DDDDDD', '#1F9E89', '#D8456C', '#D8456C', '#000000', '#E69FD0', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#AA4499'),
                            c('Below threshold', 'Underrepresented', 'Overrepresented', 'Above threshold', (unique(data$feature_anno) %>% str_remove(., '\\bBelow threshold\\b|\\bOverrepresented\\b|\\bUnderrepresented\\b|\\bAbove threshold\\b')))))
  }else{
    feature_anno_colors <- list(
      'feature_anno' = setNames(c('#DDDDDD', '#1F9E89', '#D8456C', '#D8456C'),
                                c('Below threshold', 'Underrepresented', 'Overrepresented', 'Above threshold')))
}

  }

#' @title colorManuals
#' @description Create a color manual to be used in various plots.
#' @param df The dataframe with data to be plotted.
#' @param group Should a color manual be made for the 'group' column? Default TRUE.
#' @param feature_anno Should a color manual be made for the 'feature_anno' column? Default TRUE.
#' @return A list with one or two named vectors with colors for the variables in the 'group' and/or feature_anno' columns.
#' @export

colorManuals <- function(df, group = TRUE, feature_anno = TRUE){
#Color manuals

if(isTRUE(group) & isFALSE(feature_anno)){
  color_manuals <- groupColors(df = df)
}
  else if(isFALSE(group) & isTRUE(feature_anno)){
    color_manuals <- featureAnnotationColors(df = df)
  }
  else if(isTRUE(group) & isTRUE(feature_anno)){

  color_manuals <- bind_rows(groupColors(df = df), featureAnnotationColors(df = df))
  }

}

