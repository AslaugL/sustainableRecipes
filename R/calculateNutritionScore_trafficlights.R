#' @title calculateNutritionScore_trafficlights
#'
#' @description Score a recipe unsing the UK Traffic Light System.
#'
#' @param df A dataframe with a sample_id column for recipe ID, a salt column with salt in gram/100g recipe, and nutrient columns with names in line with Matvaretabellen.
#' @param inverted Should the returned score be inverted or not? Either 'yes' or 'no', default 'yes'.
#' @param raw_scores Include raw scores in the ouput. TRUE/FALSE, default FALSE.
#'
#' @return A dataframe with the recipe ID and its total traffic light score. If raw_score = TRUE, a list with the total score dataframe in addition to a raw_score dataframe.
#'
#' @export
calculateNutritionScore_trafficlights <- function(df, inverted = 'yes', raw_scores = FALSE){
  #Traffic light recommendations
  #Turn df long
  temp <- df %>%
    pivot_longer(
      cols = -c(sample_id),
      names_to = 'feature',
      values_to = 'value'
    )
  
  #Inverted or not?
  if(inverted == 'yes'){
    
    traffic_lights_raw <- temp %>%
      
      #Score values
      mutate(inverted_traffic_light_rating = case_when(
        
        #Low/green values, score 3
        feature == 'Fat' & value <= 3 ~ 3,
        feature == 'SatFa' & value <= 1.5 ~ 3,
        feature == 'Sugar' & value <= 5 ~ 3,
        feature == 'Salt' & value <= 0.3 ~ 3,
        
        #Medium/amber values score 2
        feature == 'Fat' & (value >3 & value <= 17.5) ~ 2,
        feature == 'SatFa' & (value >1.5 & value <= 5) ~ 2,
        feature == 'Sugar' & (value >5 & value <= 22.5) ~ 2,
        feature == 'Salt' & (value >0.3 & value <= 1.5) ~ 2,
        
        #High/red values score 1
        feature == 'Fat' & value > 17.5 ~ 1,
        feature == 'SatFa' & value >5 ~ 1,
        feature == 'Sugar' & value > 22.5 ~ 1,
        feature == 'Salt' & value >1.5 ~ 1
      ))
    
    #Sum scores together
    traffic_lights <- traffic_lights_raw %>%
      select(sample_id, inverted_traffic_light_rating) %>%
      group_by(sample_id) %>%
      summarise(inverted_traffic_score = sum(inverted_traffic_light_rating, na.rm = TRUE)) %>%
      ungroup()
    
    
  } else if (inverted == 'no'){
    
    traffic_lights_raw <- temp %>%
      #Score values
      mutate(traffic_light_rating = case_when(
        
        #Low/green values, score 1
        feature == 'Fat' & value <= 3 ~ 1,
        feature == 'SatFa' & value <= 1.5 ~ 1,
        feature == 'Sugar' & value <= 5 ~ 1,
        feature == 'Salt' & value <= 0.3 ~ 1,
        
        #Medium/amber values score 2
        feature == 'Fat' & (value >3 & value <= 17.5) ~ 2,
        feature == 'SatFa' & (value >1.5 & value <= 5) ~ 2,
        feature == 'Sugar' & (value >5 & value <= 22.5) ~ 2,
        feature == 'Salt' & (value >0.3 & value <= 1.5) ~ 2,
        
        #High/red values score 3
        feature == 'Fat' & value > 17.5 ~ 3,
        feature == 'SatFa' & value >5 ~ 3,
        feature == 'Sugar' & value > 22.5 ~ 3,
        feature == 'Salt' & value >1.5 ~ 3
      ))
    
    #Sum scores together
    traffic_lights <- traffic_lights_raw %>%
      select(sample_id, traffic_light_rating) %>%
      group_by(sample_id) %>%
      summarise(traffic_score = sum(traffic_light_rating, na.rm = TRUE)) %>%
      ungroup()
    
  }
  
  #Include raw scores or not
  if(isTRUE(raw_scores)) {
    
    output <- list(
      'total_scores' = traffic_lights,
      'raw_scores' = traffic_lights_raw %>% drop_na(ends_with('rating'))
    )
    
    output
    
  } else {
    
    traffic_lights
    
  }
  
  
  
}