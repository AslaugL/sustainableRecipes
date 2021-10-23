#' @title calculateNutritionScore_nnr
#'
#' @description Score a recipe on how well it adheres to Nordic Nutritional Recommendations macronutrient recommendations.
#'
#' @param df A dataframe with a sample_id column for recipe ID, and macronutrient names in line with Matvaretabellen.
#'
#' @return A dataframe with the recipe ID and its corresponding NNR score.
#'
#' @export

calculateNutritionScore_nnr <- function(df){
  
  #first calculate energy percent
  energy_percent <- calculateEnergypercentDensity(df)
  
  #NNR recommendations
  nnr_score <- energy_percent %>%
    
    #Add 1 for each fulfilled reccomendation
    mutate(nnr_recommendation = case_when(
      feature == 'Carbo' & (energy_percent >= 45 & energy_percent <= 60) ~ 1,
      feature == 'Sugar' & energy_percent < 10 ~ 1,
      feature == 'Dietary fibre' & densityMJ >3 ~ 1,
      feature == 'Protein' & (energy_percent >=10 & energy_percent <=20) ~ 1,
      feature == 'Fat' & (energy_percent >= 25 & energy_percent <= 40) ~ 1,
      feature == 'SatFa' & energy_percent <10 ~ 1,
      TRUE ~ 0
    )) %>%
    
    #Sum scores together
    select(sample_id, nnr_recommendation) %>%
    group_by(sample_id) %>%
    summarise(nnr_score = sum(nnr_recommendation)) %>%
    ungroup()
  
}