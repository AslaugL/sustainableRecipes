#' Does recipe fulfill whole grain criteria of the Keyhole certification?
#' @title hasWholeGrains
#'
#' @description Checks if grain-based carbohydrates in a recipe fullfills criteria of Keyhole to be whole grains.
#'
#' @param df A dataframe with recipes, with recipe names in a sample_id column, and ingredient names in rows in an Ingredient column.
#'
#' @return The dataframe with a new column "whole_grain_requirement", with "yes" if ingredients of recipe fulfill Keyhole whole grain criteria, "no" if not.
#'
#' @export
hasWholeGrains <- function(df) {
  
  wholegrains <- df %>%
    
    group_by(sample_id) %>%
    #Create a whole grain column to write 'yes' or 'no' in depending on if ingredients fulfull criteria
    mutate(whole_grain_requirement = case_when(
      
      #If recipe contains grain-products, is it whole grains? If so it fullfills requirement, if they are white it doesn't. Recipes without grains fullfill this requirement by default
      str_detect(Ingredients, 'pasta|rice|bread|tortilla|wheat flour|puff pastry') & str_detect(Ingredients, 'whole grain|coarse|brown') ~ 'yes',
      str_detect(Ingredients, 'pasta|rice|bread|tortilla|wheat flour|puff pastry') & !str_detect(Ingredients, 'whole grain|coarse|brown') ~ 'no',
      
      TRUE ~ 'yes'
    )) %>%
    #Keep only relevant columns
    select(sample_id, whole_grain_requirement) %>% unique() %>%
    #If any 'no' is found in whole_grain_requirement column, keep it and remove yes
    mutate(whole_grain_requirement = case_when(
      any(whole_grain_requirement == 'no') ~ 'no',
      TRUE ~ 'yes'
    )) %>% unique() %>% ungroup()
  
}
