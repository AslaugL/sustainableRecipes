#' @title calculateNutritionScore_keyhole
#'
#' @description Score a recipe using the Keyhole system.
#'
#' @param df A long format dataframe with an Ingredients column, a Foodgroup column with food groups named as they are in analyses.R script, a sample_id column for recipe ID, and a feature column with nutrient names in line with Matvaretabellen, with their values in a value column.
#'
#' @return A dataframe with the recipe ID and a column noting if it is keyhole certified or not.
#'
#' @export

calculateNutritionScore_keyhole <- function(df){
  
#Get all the data necessary for calculations 
data <- list(
  
  #Find the total of all relevant nutrients in the recipe
  'total_nutrients' = df %>% filter(feature %in% c('Sugar', 'SatFa', 'Salt', 'Dietary fibre')) %>%
    group_by(sample_id, feature) %>%
    summarise(temp = sum(value, na.rm = TRUE)) %>% ungroup() %>% rename(value = temp) %>%
    
    #turn wide
    pivot_wider(.,
                names_from = feature,
                values_from = value),
  
  #Find if the recipe fullfills the Keyhole whole grains criteria
  'wholegrains' = df %>% hasWholeGrains(),
  
  #Calculate pct fruit, veg, legumes and nuts 
  'pct' = df %>% pctOfFruitVegLegumesNutsOils(.) %>% rename(value = pct) %>%
    
    #Turn wide
    pivot_wider(.,
                names_from = feature,
                values_from = value),
  
  #Find the type of recipe
  'type_of_recipe' = df %>% select(sample_id) %>% unique() %>% findTypeOfRecipe() 
  
) %>%
  #Merge together
  Reduce(function(x, y, ...) merge(x, y, all = TRUE, ...),
         .)

  #Calculate keyhole
  final <- keyholeCertification(data)
  
  final
  
}

