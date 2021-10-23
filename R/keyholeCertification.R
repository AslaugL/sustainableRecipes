#' Does a recipe fullfill the Keyhole certification for its recipetype?
#' @title keyholeCertification
#'
#' @description Checks if recipe fulfills criteria for Keyhole certification.
#'
#' @param df A dataframe with columns sample_id (recipe name), recipe_type, keyhole_amounts_pct, Sugar, Salt, SatFa, whole_grain_requirement
#'
#' @return The dataframe with a new column "keyhole_certified" with "Keyhole" or "No keyhole".
#'
#' @export

keyholeCertification <- function(df){

keyhole_certifications <- df %>%
  
  #Do the recipe fulfill keyhole criteria?
  mutate(keyhole_certified = case_when(
    
    #Different requirements based on type of dish
    recipe_type == 'soups_and_stews' & keyhole_amounts_pct >= 35 & Sugar <= 3 & Salt <= 0.8 & SatFa <= 1.5 & whole_grain_requirement == 'yes' ~ 'Keyhole',
    recipe_type == 'pizza_and_pies' & keyhole_amounts_pct >= 28 & Sugar <= 3 & Salt <= 1 & SatFa <= 2 & whole_grain_requirement == 'yes' ~ 'Keyhole',
    recipe_type == 'wraps' & keyhole_amounts_pct >= 25 & Sugar <= 3 & Salt <= 0.9 & SatFa <= 2 & whole_grain_requirement == 'yes' ~ 'Keyhole',
    recipe_type == 'ready_meals_27' & keyhole_amounts_pct >= 50 & Sugar <= 3 & Salt <= 0.8 & SatFa <= 1.5 & whole_grain_requirement == 'yes' ~ 'Keyhole',
    
    TRUE ~ 'No Keyhole'
  )) %>%
  select(sample_id, keyhole_certified)

}