#' @title getPCAloadings
#'
#' @description Get the loading scores for each feature in a prcomp object from different principal components.
#'
#' @param prcomp_object A prcomp object.
#' @param principal_components The principal components to extract the loadings from.
#'
#' @return A dataframe with the loadings of each feature from the principal components chosen.
#'
#' @export

getPCAloadings <- function(prcomp_object, principal_components) {

  #Get the feature names
  features <- as_tibble(rownames(prcomp_object$rotation)) %>%
    rename(feature = value) %>% #Call the column with names 'feature'
    rownames_to_column(., 'number')

  #Get the loadings and plot
  loadings <- as_tibble(prcomp_object$rotation, rownames = NA) %>% #The loadings, keep rownames
    dplyr::select(principal_components) %>% #Filter out the principal components of interest
    rownames_to_column('number') %>% #rownames are not kept when doing this, workaround by adding the features df
    inner_join(., features) %>%
    select(-number) #Remove unnecessary column

}
