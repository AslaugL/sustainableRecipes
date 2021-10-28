#' @title calculateNutritionScore_who
#'
#' @description Score a recipe on how well it adheres to WHO macronutrient recommendations.
#'
#' @param df A dataframe with a sample_id column for recipe ID, and macronutrient names in line with Matvaretabellen.
#' @param raw_scores Should raw scores be included in the output? TRUE/FALSE.
#'
#' @return A dataframe with the recipe ID and its corresponding WHO score. If raw_scores is TRUE, a list with the total scores in one dataframe and the raw scores in another.
#'
#' @export
calculateNutritionScore_who <- function(df, raw_scores = FALSE){
  
  #first calculate energy percent
  energy_percent <- calculateEnergypercentDensity(df)
  
  #WHO recommendations
  who_score_raw <- energy_percent %>%
    
    #Add 1 for each fulfilled recomendation
    mutate(who_recommendation = case_when(
      feature == 'Carbo' & (energy_percent >= 55 & energy_percent <=75) ~ 1,
      feature == 'Sugar' & energy_percent < 10 ~ 1,
      feature == 'Dietary fibre' & densityMJ >3 ~ 1,
      feature == 'Protein' & (energy_percent >=10 & energy_percent <=15) ~ 1, #Is it really 15? Other countries uses 20
      feature == 'Fat' & (energy_percent >= 15 & energy_percent <= 30) ~ 1,
      feature == 'SatFa' & energy_percent <10 ~ 1,
      TRUE ~ 0
    ))
    
    #Sum scores together
    who_score <- who_score_raw %>% select(sample_id, who_recommendation) %>%
    group_by(sample_id) %>%
    summarise(who_score = sum(who_recommendation)) %>%
    ungroup()
    
    #Include raw scores in output or not
    if(isTRUE(raw_scores)) {
      
      output = list(
        'total' = who_score,
        'raw' = who_score_raw
      )
      
      output
      
    } else {
      
      who_score
      
    }
  
}