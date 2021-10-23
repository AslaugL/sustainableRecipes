#' @title PCAprep
#'
#' @description Turns a tidy dataframe into a wide dataframe that can be run through prcomp or ropls, keeping group affiliation of the samples.
#'
#' @param df A tidy dataframe with sample_id, group, feature and value columns.
#'
#' @return A wide dataframe with subject_id and group affiliation as rownames
#'
#' @export

PCAprep <- function(df){

  #Data to analyse
  prepped <- df %>%
    select(sample_id, group, feature, value) %>% #Get necessary columns
    unite('sample_group', c(sample_id, group), sep = '_') %>% #Create new subject ID with health status to use to separate samples later

    #Turn wide
    pivot_wider(., #Turn wide
                names_from = 'feature',
                values_from = 'value') %>%
    column_to_rownames('sample_group') #Turn subject ID to rownames for opls

}
