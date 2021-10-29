#' @title calculateNutritionScore_nutriscore
#'
#' @description Score a recipe using the Nutriscore system.
#'
#' @param df A long format dataframe with an Ingredients column, a Foodgroup column with food groups named as they are in analyses.R script, a sample_id column for recipe ID, and a feature column with nutrient names in line with Matvaretabellen (except kJ which is 'Kilojoules'), with their values in a value column.
#' @param inverted Should the returned score be inverted or not? Either 'yes' or 'no', default 'yes'.
#' @param raw_scores Should the raw scores for each qualifying/disqualifying element be included in the results? TRUE/FALSE.
#'
#' @return A dataframe with the recipe ID and its nutriscore with corresponding letter. If raw_scores is true, a list with two dataframes, one with the Nutriscore and one with the raw scores used for calculating it.
#'
#' @export
calculateNutritionScore_nutriscore <- function(df, inverted = 'yes', raw_scores = FALSE){
  
  #Find the total of all relevant nutrients in the recipe
  total_nutrients <- df %>% filter(feature %in% c('Kilojoules', 'Sugar', 'SatFa', 'Sodium', 'Dietary fibre', 'Protein')) %>%
    group_by(sample_id, feature) %>%
    summarise(temp = sum(value, na.rm = TRUE)) %>% ungroup() %>% rename(value = temp)
  
  #Calculate pct fruit, veg, legumes, nuts and oils
  pct <- df %>% pctOfFruitVegLegumesNutsOils(.) %>% rename(value = pct)
  
  #Join them together and calculate nutriscore
  nutriscore_raw <- bind_rows(total_nutrients, pct) %>%
    
    #Calculate nutriscore
    mutate(nutriscore_raw = case_when(
      
      #Energy
      feature == 'Kilojoules' & value <= 335 ~ 0,
      feature == 'Kilojoules' & (value >335 & value <= 670) ~ 1,
      feature == 'Kilojoules' & (value >670 & value <= 1005) ~ 2,
      feature == 'Kilojoules' & (value >1005 & value <= 1340) ~ 3,
      feature == 'Kilojoules' & (value >1340 & value <= 1675) ~ 4,
      feature == 'Kilojoules' & (value >1675 & value <= 2010) ~ 5,
      feature == 'Kilojoules' & (value >2010 & value <= 2345) ~ 6,
      feature == 'Kilojoules' & (value >2345 & value <= 2680) ~ 7,
      feature == 'Kilojoules' & (value >2680 & value <= 3015) ~ 8,
      feature == 'Kilojoules' & (value >3015 & value <= 3350) ~ 9,
      feature == 'Kilojoules' & value >3350 ~ 10,
      
      #(Added) Sugars
      feature == 'Sugar' & value <= 4.5 ~ 0,
      feature == 'Sugar' & (value >4.5 & value <= 9) ~ 1,
      feature == 'Sugar' & (value >9 & value <= 13.5) ~ 2,
      feature == 'Sugar' & (value >13.5 & value <= 18) ~ 3,
      feature == 'Sugar' & (value >18 & value <= 22.5) ~ 4,
      feature == 'Sugar' & (value >22.5 & value <= 27) ~ 5,
      feature == 'Sugar' & (value >27 & value <= 31) ~ 6,
      feature == 'Sugar' & (value >31 & value <= 36) ~ 7,
      feature == 'Sugar' & (value >36 & value <= 40) ~ 8,
      feature == 'Sugar' & (value >40 & value <= 45) ~ 9,
      feature == 'Sugar' & value >45 ~ 10,
      
      #Saturated Fat
      feature == 'SatFa' & value <= 1 ~ 0,
      feature == 'SatFa' & (value >1 & value <= 2) ~ 1,
      feature == 'SatFa' & (value >2 & value <= 3) ~ 2,
      feature == 'SatFa' & (value >3 & value <= 4) ~ 3,
      feature == 'SatFa' & (value >4 & value <= 5) ~ 4,
      feature == 'SatFa' & (value >5 & value <= 6) ~ 5,
      feature == 'SatFa' & (value >6 & value <= 7) ~ 6,
      feature == 'SatFa' & (value >7 & value <= 8) ~ 7,
      feature == 'SatFa' & (value >8 & value <= 9) ~ 8,
      feature == 'SatFa' & (value >9 & value <= 10) ~ 9,
      feature == 'SatFa' & value >10 ~ 10,
      
      #Sodium
      feature == 'Sodium' & value <= 90 ~ 0,
      feature == 'Sodium' & (value >90 & value <= 180) ~ 1,
      feature == 'Sodium' & (value >180 & value <= 270) ~ 2,
      feature == 'Sodium' & (value >270 & value <= 360) ~ 3,
      feature == 'Sodium' & (value >360 & value <= 450) ~ 4,
      feature == 'Sodium' & (value >450 & value <= 540) ~ 5,
      feature == 'Sodium' & (value >540 & value <= 630) ~ 6,
      feature == 'Sodium' & (value >630 & value <= 720) ~ 7,
      feature == 'Sodium' & (value >720 & value <= 810) ~ 8,
      feature == 'Sodium' & (value >810 & value <= 900) ~ 9,
      feature == 'Sodium' & value >900 ~ 10,
      
      #Fruits and veggies an legumes and nuts and (certain) oils
      feature == 'nutriscore_amounts_pct' & value <= 40 ~ 0,
      feature == 'nutriscore_amounts_pct' & (value > 40 & value <= 60) ~ 1,
      feature == 'nutriscore_amounts_pct' & (value > 60 & value <= 80) ~ 2,
      feature == 'nutriscore_amounts_pct' & value > 80 ~ 5,
      
      #Fibre
      feature == 'Dietary fibre' & value <= 0.9 ~ 0,
      feature == 'Dietary fibre' & (value >0.9 & value <=1.9) ~ 1,
      feature == 'Dietary fibre' & (value >1.9 & value <=2.8) ~ 2,
      feature == 'Dietary fibre' & (value >2.8 & value <=3.7) ~ 3,
      feature == 'Dietary fibre' & (value >3.7 & value <=4.7) ~ 4,
      feature == 'Dietary fibre' & value >4.7 ~ 5,
      
      #Protein
      feature == 'Protein' & value <= 1.6 ~ 0,
      feature == 'Protein' & (value >1.6 & value <=3.2) ~ 1,
      feature == 'Protein' & (value >3.2 & value <=4.8) ~ 2,
      feature == 'Protein' & (value >4.8 & value <=6.4) ~ 3,
      feature == 'Protein' & (value >6.4 & value <=8.0) ~ 4,
      feature == 'Protein' & value >8.0 ~ 5,
    )) %>% drop_na(nutriscore_raw) #Drop features not needed for calculations
  
  #temporary categories used to calculate final score
  categories <- nutriscore_raw %>%
    mutate(category = case_when(
      feature %in% c('Kilojoules', 'Sugar', 'SatFa', 'Sodium') ~ 'N',
      feature %in% c('nutriscore_amounts_pct', 'Dietary fibre', 'Protein') ~ 'P'
    )) %>% drop_na(category) %>%
    #get scores for the categories
    group_by(sample_id, category) %>%
    summarise(nutriscore_raw = sum(nutriscore_raw)) %>%
    ungroup() %>%
    #Turn into wide format
    pivot_wider(
      names_from = 'category',
      values_from = 'nutriscore_raw'
    )
  
  categories_2 <- nutriscore_raw %>%
    mutate(category = case_when(
      feature == 'nutriscore_amounts_pct' ~ 'FruitVegLegumesNutsOils',
      feature == 'Dietary fibre' ~ 'Fibre',
      feature == 'Protein' ~ 'Protein'
    )) %>% drop_na(category) %>%
    #get scores for the categories
    group_by(sample_id, category) %>%
    summarise(nutriscore_raw = sum(nutriscore_raw)) %>%
    ungroup() %>%
    #Turn into wide format
    pivot_wider(
      names_from = 'category',
      values_from = 'nutriscore_raw'
    )
  
  #Calculate nutriscore 
  nutriscore <- full_join(categories, categories_2) %>%
    mutate(nutriscore = case_when(
      N < 11 ~ N-P,
      N >= 11 & FruitVegLegumesNutsOils >= 5 ~ N-P,
      N >= 11 & FruitVegLegumesNutsOils < 5 ~ N-(FruitVegLegumesNutsOils+Fibre)
    ),
    nutriscore_letter = case_when(
      nutriscore <= -1 ~ 'A',
      nutriscore >= 0 & nutriscore <=2 ~ 'B',
      nutriscore >= 3 & nutriscore <=10 ~ 'C',
      nutriscore >= 11 & nutriscore <=18 ~ 'D',
      nutriscore >= 19 ~ 'E',
    )) %>%
    #Turn letters into factor
    mutate(nutriscore_letter = as.factor(nutriscore_letter))
  
  #Invert score or not?
  if(inverted == 'yes'){
    
    nutriscore <- nutriscore %>%
      mutate(nutriscore = nutriscore * -1) %>%
      #Rename to show it's the inverted score
      rename(inverted_nutriscore = nutriscore)
    
  } else if (inverted == 'no'){
    
    nutriscore <- nutriscore
    
  }
  
  nutriscore <- nutriscore %>% select(-c(N, P, Fibre, FruitVegLegumesNutsOils, Protein))
  
  #Include raw scores in the output or not
  if(isTRUE(raw_scores)){
    
    output <- list(
      'nutriscore' = nutriscore,
      'raw_scores' = nutriscore_raw)
      
    output
    }else{
      
      nutriscore
      
    }
    
  
  
}