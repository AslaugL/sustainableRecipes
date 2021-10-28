#' @title calculateNutritionScore_nnr
#'
#' @description Score a recipe on how well it adheres to Nordic Nutritional Recommendations macronutrient recommendations.
#'
#' @param df A dataframe with a sample_id column for recipe ID, and macronutrient names in line with Matvaretabellen.
#' @param raw_scores Should raw scores be included in the output? TRUE/FALSE.
#'
#' @return A dataframe with the recipe ID and its corresponding NNR score. If raw_scores is TRUE, a list with the total scores in one dataframe and the raw scores in another.
#'
#' @export

calculateNutritionScore_nnr <- function(df, raw_scores = FALSE){
  
  #first calculate energy percent
  energy_percent <- calculateEnergypercentDensity(df)
  
  #NNR recommendations
  nnr_score_raw <- energy_percent %>%
    
    #Add 1 for each fulfilled reccomendation
    mutate(nnr_recommendation = case_when(
      feature == 'Carbo' & (energy_percent >= 45 & energy_percent <= 60) ~ 1,
      feature == 'Sugar' & energy_percent < 10 ~ 1,
      feature == 'Dietary fibre' & densityMJ >3 ~ 1,
      feature == 'Protein' & (energy_percent >=10 & energy_percent <=20) ~ 1,
      feature == 'Fat' & (energy_percent >= 25 & energy_percent <= 40) ~ 1,
      feature == 'SatFa' & energy_percent <10 ~ 1,
      TRUE ~ 0
    ))
    
    #Sum scores together
   nnr_score <- nnr_score_raw %>% select(sample_id, nnr_recommendation) %>%
    group_by(sample_id) %>%
    summarise(nnr_score = sum(nnr_recommendation)) %>%
    ungroup()
   
   #Include raw scores in output or not
   if(isTRUE(raw_scores)) {
     
     output = list(
       'total' = nnr_score,
       'raw' = nnr_score_raw
     )
     
     output
     
   } else {
     
     nnr_score
     
   }
  
}