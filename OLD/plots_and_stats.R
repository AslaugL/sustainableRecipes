#Baklava (recipe with most sugar) is a dessert

library(tidyverse)
library(stringi)
library(ggbeeswarm)
library(ggcorrplot)
library(ggridges)
library(thesisToolsOmics)
library(readxl)
library(DT)
library(psych)
library(ComplexHeatmap)
library(rstatix)
library(GGally)
library(kableExtra)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data')
theme_set(theme_bw())

#Functions----
calculateEnergypercentDensity <- function(df){
  
  df %>%
    
    #Turn tidy for calculations
    pivot_longer(
      cols = -c(sample_id, group, kcal),
      names_to = 'feature',
      values_to = 'grams') %>%
    #Rename kcal column
    rename(total_kcal = kcal) %>%
    #Create a MJ column for density pr MJ calculations
    mutate(
      MJ = total_kcal*0.004184,
      
      #Calculate how many kcals from each macronutrient, and the density of fibre pr megajoule energy
      #Carbohydrates, sugar and protein is about 4kcal/g, fat 9kcal/g
      kcal = case_when(
        str_detect(feature, 'carbohydrate|sugar|protein') ~ grams*4,
        str_detect(feature, 'fat') ~ grams*9,
        str_detect(feature, 'fibre') ~ grams*2),
      densityMJ = grams/MJ,
      energy_percent = case_when(
        !str_detect(feature, 'salt') ~ kcal/total_kcal*100)
    )
  
}
calculateNutritionScore_who <- function(df){
  
  #first calculate energy percent
  energy_percent <- calculateEnergypercentDensity(df)
  
  #WHO recommendations
  who_score <- energy_percent %>%
    
    #Add 1 for each fulfilled recomendation
    mutate(who_recommendation = case_when(
      feature == 'carbohydrates' & (energy_percent >= 55 & energy_percent <=75) ~ 1,
      feature == 'added_sugar' & energy_percent < 10 ~ 1,
      feature == 'fibre' & densityMJ >3 ~ 1,
      feature == 'proteins' & (energy_percent >=10 & energy_percent <=15) ~ 1, #Is it really 15? Other countries uses 20
      feature == 'fat' & (energy_percent >= 15 & energy_percent <= 30) ~ 1,
      feature == 'saturated_fat' & energy_percent <10 ~ 1,
      TRUE ~ 0
    )) %>%
    
    #Sum scores together
    select(sample_id, group, who_recommendation) %>%
    group_by(sample_id, group) %>%
    summarise(who_score = sum(who_recommendation)) %>%
    ungroup()
  
}
calculateNutritionScore_nnr <- function(df){
  
  #first calculate energy percent
  energy_percent <- calculateEnergypercentDensity(df)
  
  #NNR recommendations
  nnr_score <- energy_percent %>%
    
    #Add 1 for each fulfilled reccomendation
    mutate(nnr_recommendation = case_when(
      feature == 'carbohydrates' & (energy_percent >= 45 & energy_percent <= 60) ~ 1,
      feature == 'added_sugar' & energy_percent < 10 ~ 1,
      feature == 'fibre' & densityMJ >3 ~ 1,
      feature == 'proteins' & (energy_percent >=10 & energy_percent <=20) ~ 1,
      feature == 'fat' & (energy_percent >= 25 & energy_percent <= 40) ~ 1,
      feature == 'saturated_fat' & energy_percent <10 ~ 1,
      TRUE ~ 0
    )) %>%
    
    #Sum scores together
    select(sample_id, group, nnr_recommendation) %>%
    group_by(sample_id, group) %>%
    summarise(nnr_score = sum(nnr_recommendation)) %>%
    ungroup()
  
}
calculateNutritionScore_trafficlights <- function(df, inverted = 'yes'){
#Takes a wide dataframe and calculate the fsa trafficligght score of the recipe
#Inverted can be either 'yes' for an inverted score (1 for high/red, 2 for medium/amber, 3 for low/green)
#or no following the normal scoring system. Inverted is standard
  
  #Traffic light recommendations
  temp <- calculateEnergypercentDensity(df)
  
  #Inverted or not?
  if(inverted == 'yes'){
    
    traffic_lights <- temp %>%
      
      #Score values
      mutate(inverted_traffic_light_rating = case_when(
        
        #Low/green values, score 3
        feature == 'fat' & grams <= 3 ~ 3,
        feature == 'saturated_fat' & grams <= 1.5 ~ 3,
        feature == 'added_sugar' & grams <= 5 ~ 3,
        feature == 'salt' & grams <= 0.3 ~ 3,
        
        #Medium/amber values score 2
        feature == 'fat' & (grams >3 & grams <= 17.5) ~ 2,
        feature == 'saturated_fat' & (grams >1.5 & grams <= 5) ~ 2,
        feature == 'added_sugar' & (grams >5 & grams <= 22.5) ~ 2,
        feature == 'salt' & (grams >0.3 & grams <= 1.5) ~ 2,
        
        #High/red values score 1
        feature == 'fat' & grams > 17.5 ~ 1,
        feature == 'saturated_fat' & grams >5 ~ 1,
        feature == 'added_sugar' & grams > 22.5 ~ 1,
        feature == 'salt' & grams >1.5 ~ 1
      ))
    
    #Sum scores together
    traffic_lights <- traffic_lights %>%
      select(sample_id, group, inverted_traffic_light_rating) %>%
      group_by(sample_id, group) %>%
      summarise(inverted_traffic_score = sum(inverted_traffic_light_rating, na.rm = TRUE)) %>%
      ungroup()
    
    
  } else if (inverted == 'no'){
    
    traffic_lights <- temp %>%
      #Score values
      mutate(traffic_light_rating = case_when(
        
        #Low/green values, score 1
        feature == 'fat' & grams <= 3 ~ 1,
        feature == 'saturated_fat' & grams <= 1.5 ~ 1,
        feature == 'added_sugar' & grams <= 5 ~ 1,
        feature == 'salt' & grams <= 0.3 ~ 1,
        
        #Medium/amber values score 2
        feature == 'fat' & (grams >3 & grams <= 17.5) ~ 2,
        feature == 'saturated_fat' & (grams >1.5 & grams <= 5) ~ 2,
        feature == 'added_sugar' & (grams >5 & grams <= 22.5) ~ 2,
        feature == 'salt' & (grams >0.3 & grams <= 1.5) ~ 2,
        
        #High/red values score 3
        feature == 'fat' & grams > 17.5 ~ 3,
        feature == 'saturated_fat' & grams >5 ~ 3,
        feature == 'added_sugar' & grams > 22.5 ~ 3,
        feature == 'salt' & grams >1.5 ~ 3
      ))
    
    #Sum scores together
    traffic_lights <- traffic_lights %>%
      select(sample_id, group, traffic_light_rating) %>%
      group_by(sample_id, group) %>%
      summarise(traffic_score = sum(traffic_light_rating, na.rm = TRUE)) %>%
      ungroup()
    
  }
  
  traffic_lights
    
}
calculateNutritionScore_nutriscore <- function(df, inverted = 'yes'){
#Takes a tidy df and calcualtes nutriscore
  
  #First turn kcal into kJ, and salt (g) into sodium (mg)
  nutriscore_raw <- df %>%
    #rename
    mutate(feature = feature %>%
             str_replace('kcal/100g', 'kJ/100g') %>%
             str_replace('Salt/100g', 'sodium/100g')) %>%
    #calculate new values
    mutate(value = case_when(
      feature == 'kJ/100g' ~ value*4.184,
      feature == 'sodium/100g' ~ value/2.4*1000,
      TRUE ~ value
    )) %>%
    
    #Calculate nutriscore
    mutate(nutriscore_raw = case_when(
      
      #Energy
      feature == 'kJ/100g' & value <= 335 ~ 0,
      feature == 'kJ/100g' & (value >335 & value <= 670) ~ 1,
      feature == 'kJ/100g' & (value >670 & value <= 1005) ~ 2,
      feature == 'kJ/100g' & (value >1005 & value <= 1340) ~ 3,
      feature == 'kJ/100g' & (value >1340 & value <= 1675) ~ 4,
      feature == 'kJ/100g' & (value >1675 & value <= 2010) ~ 5,
      feature == 'kJ/100g' & (value >2010 & value <= 2345) ~ 6,
      feature == 'kJ/100g' & (value >2345 & value <= 2680) ~ 7,
      feature == 'kJ/100g' & (value >2680 & value <= 3015) ~ 8,
      feature == 'kJ/100g' & (value >3015 & value <= 3350) ~ 9,
      feature == 'kJ/100g' & value >3350 ~ 10,
      
      #(Added) Sugars
      feature == 'Added sugar/100g' & value <= 4.5 ~ 0,
      feature == 'Added sugar/100g' & (value >4.5 & value <= 9) ~ 1,
      feature == 'Added sugar/100g' & (value >9 & value <= 13.5) ~ 2,
      feature == 'Added sugar/100g' & (value >13.5 & value <= 18) ~ 3,
      feature == 'Added sugar/100g' & (value >18 & value <= 22.5) ~ 4,
      feature == 'Added sugar/100g' & (value >22.5 & value <= 27) ~ 5,
      feature == 'Added sugar/100g' & (value >27 & value <= 31) ~ 6,
      feature == 'Added sugar/100g' & (value >31 & value <= 36) ~ 7,
      feature == 'Added sugar/100g' & (value >36 & value <= 40) ~ 8,
      feature == 'Added sugar/100g' & (value >40 & value <= 45) ~ 9,
      feature == 'Added sugar/100g' & value >45 ~ 10,
      
      #Saturated fat
      feature == 'Saturated fat/100g' & value <= 1 ~ 0,
      feature == 'Saturated fat/100g' & (value >1 & value <= 2) ~ 1,
      feature == 'Saturated fat/100g' & (value >2 & value <= 3) ~ 2,
      feature == 'Saturated fat/100g' & (value >3 & value <= 4) ~ 3,
      feature == 'Saturated fat/100g' & (value >4 & value <= 5) ~ 4,
      feature == 'Saturated fat/100g' & (value >5 & value <= 6) ~ 5,
      feature == 'Saturated fat/100g' & (value >6 & value <= 7) ~ 6,
      feature == 'Saturated fat/100g' & (value >7 & value <= 8) ~ 7,
      feature == 'Saturated fat/100g' & (value >8 & value <= 9) ~ 8,
      feature == 'Saturated fat/100g' & (value >9 & value <= 10) ~ 9,
      feature == 'Saturated fat/100g' & value >10 ~ 10,
      
      #Sodium
      feature == 'sodium/100g' & value <= 90 ~ 0,
      feature == 'sodium/100g' & (value >90 & value <= 180) ~ 1,
      feature == 'sodium/100g' & (value >180 & value <= 270) ~ 2,
      feature == 'sodium/100g' & (value >270 & value <= 360) ~ 3,
      feature == 'sodium/100g' & (value >360 & value <= 450) ~ 4,
      feature == 'sodium/100g' & (value >450 & value <= 540) ~ 5,
      feature == 'sodium/100g' & (value >540 & value <= 630) ~ 6,
      feature == 'sodium/100g' & (value >630 & value <= 720) ~ 7,
      feature == 'sodium/100g' & (value >720 & value <= 810) ~ 8,
      feature == 'sodium/100g' & (value >810 & value <= 900) ~ 9,
      feature == 'sodium/100g' & value >900 ~ 10,
      
      #Fruits and veggies an legumes and nuts and (certain) oils
      feature == 'pct_FruitVegLegumesNutsOils' & value <= 40 ~ 0,
      feature == 'pct_FruitVegLegumesNutsOils' & (value > 40 & value <= 60) ~ 1,
      feature == 'pct_FruitVegLegumesNutsOils' & (value > 60 & value <= 80) ~ 2,
      feature == 'pct_FruitVegLegumesNutsOils' & value > 80 ~ 5,
      
      #Fibre
      feature == 'Fibre/100g' & value <= 0.9 ~ 0,
      feature == 'Fibre/100g' & (value >0.9 & value <=1.9) ~ 1,
      feature == 'Fibre/100g' & (value >1.9 & value <=2.8) ~ 2,
      feature == 'Fibre/100g' & (value >2.8 & value <=3.7) ~ 3,
      feature == 'Fibre/100g' & (value >3.7 & value <=4.7) ~ 4,
      feature == 'Fibre/100g' & value >4.7 ~ 5,
      
      #Protein
      feature == 'Protein/100g' & value <= 1.6 ~ 0,
      feature == 'Protein/100g' & (value >1.6 & value <=3.2) ~ 1,
      feature == 'Protein/100g' & (value >3.2 & value <=4.8) ~ 2,
      feature == 'Protein/100g' & (value >4.8 & value <=6.4) ~ 3,
      feature == 'Protein/100g' & (value >6.4 & value <=8.0) ~ 4,
      feature == 'Protein/100g' & value >8.0 ~ 5,
    )) %>% drop_na(nutriscore_raw) #Drop features not needed for calculations
    
    #temporary categories used to calculate final score
    categories <- nutriscore_raw %>%
      mutate(category = case_when(
        feature %in% c('kJ/100g', 'Added sugar/100g', 'Saturated fat/100g', 'sodium/100g') ~ 'N',
        feature %in% c('pct_FruitVegLegumesNutsOils', 'Fibre/100g', 'Protein/100g') ~ 'P'
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
        feature == 'pct_FruitVegLegumesNutsOils' ~ 'FruitVegLegumesNutsOils',
        feature == 'Fibre/100g' ~ 'Fibre',
        feature == 'Protein/100g' ~ 'Protein'
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
    
    nutriscore 
  
}

plotCorrelations <- function(corr_data){
  #Corr_data is the result from corr.test
  
  plot <- ggcorrplot(corr_data$r,
                     #Cluster, easier to compare plots if they're not
                     #hc.order = TRUE,
                     #Use an outline for each square, and only show the lower squares
                     outline.col = "white", type = "lower",
                     
                     #Change colors and add labels
                     ggtheme = ggplot2::theme_bw, method = 'circle',
                     colors = c("#6D9EC1", "white", "#E46726"),
                     #lab = TRUE,
                     p.mat = corr_data$p
  )
  plot
}
cleanUpFeatures <- function(df){
#Cleans up the names of the feature column for plots/tables
  df %>% mutate(feature = case_when(
    str_detect(feature, 'sugar|Protein|Fat|Carbohydrates|fat') ~ str_replace(feature, '/100g', ' E%'),
    str_detect(feature, 'Salt|Fibre') ~ str_replace(feature, '/', ' g/'),
    str_detect(feature, 'nnr') ~ 'NNR score',
    str_detect(feature, 'who') ~ 'WHO score',
    feature == 'inverted_nutriscore' ~ 'Inverted Nutriscore',
    feature == 'nutriscore' ~ 'Nutriscore',
    feature == 'inverted_traffic_score' ~ 'Inverted Traffic Light score',
    feature == 'traffic score' ~ 'Traffic Light score',
    TRUE ~ feature
  ))
    
}

#Various dataframes kept when cleaning the datasets----
various <- list(
  'Norway' = readRDS('various_NOR.Rds'),
  'UK' = readRDS('various_UK.Rds'),
  'SHARP' = readRDS('various_sustainability.Rds')
)
#Missing ingredients
various$missing_amounts_ingredients <- list(
  'Norway' = bind_rows(various$Norway$no_amounts, various$Norway$not_found_in_ref) %>% unique(),
  'UK' = bind_rows(various$UK$no_amounts, various$UK$not_found_in_ref) %>% unique()
) %>% bind_rows(., .id = 'Country') %>%
  select(`Selected Meals`, Ingredients, Source, Country) %>%
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>% #Turn lowercase for easier changing of values
  
  #Standardize names of Ingredients with no amounts
  mutate(Ingredients2 = case_when(
    #Seasoning and conditments
    str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'oil') ~ 'salt and pepper and oil',
    str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'lemon juice') ~ 'salt and pepper and lemon juice',
    str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') ~ 'salt and pepper',
    str_detect(Ingredients, 'salt') ~ 'salt',
    str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'black|white|peppercorn|red|ground|whole') ~ 'pepper', #Black standard pepper, and a few red/white peppers
    str_detect(Ingredients, 'cayenne pepper') ~ 'cayenne pepper',
    str_detect(Ingredients, 'pepper') ~ 'pepper',
    str_detect(Ingredients, 'thyme') ~ 'thyme',
    str_detect(Ingredients, 'mint') ~ 'mint',
    str_detect(Ingredients, 'oregano') ~ 'oregano',
    str_detect(Ingredients, 'lemon juice') ~ 'lemon juice',
    str_detect(Ingredients, 'white wine|red wine|balsamic') & str_detect(Ingredients, 'vinegar') ~ 'vinegar',
    str_detect(Ingredients, 'mustard') ~ 'mustard',
    str_detect(Ingredients, 'chili sauce') ~ 'chili sauce',
    str_detect(Ingredients, 'chives') ~ 'chives',
    str_detect(Ingredients, 'coriander') ~ 'coriander',
    str_detect(Ingredients, 'parsley') ~ 'parsley',
    str_detect(Ingredients, 'cumin') ~ 'cumin',
    str_detect(Ingredients, 'ginger') ~ 'ginger',
    #Grains and grain based produvts
    str_detect(Ingredients, 'wheat flour') ~ 'wheat flour',
    str_detect(Ingredients, 'maisstivelse') ~ 'corn starch',
    str_detect(Ingredients, 'rice') ~ 'rice',
    #Vegetables
    str_detect(Ingredients, 'paprika') ~ 'sweet peppers',
    str_detect(Ingredients, 'leeks') ~ 'leeks',
    str_detect(Ingredients, 'salad') ~ 'salad',
    #Fats
    str_detect(Ingredients, 'olive oil') ~ 'olive oil',
    str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'oil') ~ 'peanut oil',
    str_detect(Ingredients, 'oil') & !str_detect(Ingredients, 'olive|peanut|sesame') ~ 'vegetable oil',
    str_detect(Ingredients, 'butter') ~ 'butter',
    str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'fat') ~ 'duck fat',
    #Dairy
    str_detect(Ingredients, 'parmesan') ~ 'parmesan cheese',
    #Meat
    str_detect(Ingredients, 'sausage') ~ 'sausage',
    #Div
    str_detect(Ingredients, 'stock') ~ 'stock/broth',
    str_detect(Ingredients, 'sugar') ~ 'sugar',
    str_detect(Ingredients, 'egg') ~ 'egg',
    str_detect(Ingredients, 'honey') ~ 'honey',
    
    TRUE ~ Ingredients
  )) %>%
  
  #Add foodgroups
  mutate(Foodgroups = case_when(
    
    #Seasoning and conditments
    Ingredients2 %in% c('bay leaf', 'pepper', 'cayenne pepper', 'oregano', 'basil',
                       'thyme', 'rosemary', 'coriander', 'cinnamon', 'onion powder',
                       'italian seasoning', 'white sauce', 'cardamom', 'garam masala',
                       'chili powder', 'paprika powder', 'cumin', 'caraway seed', 
                       'fenugreek seed', 'fenugreek', 'estragon', 'ginger', 'turmeric',
                       'saffron', 'cloves', 'capers', 'mint', 'old el paso burrito spice mix',
                       'fennel seed', 'allspice', 'stock/broth', 'chinese five spice',
                       'chervil', 'broth cube', 'tabasco', 'dip mix', 'cilantro',
                       'juniper berry', 'taco seasoning mix', 'cress', 'tandoori spice',
                       'dill', 'cloves', 'chives', 'curry paste', 'nutmeg', 'sage',
                       'steak seasoning', 'chili flake', 'adobo seasoning', 'sazon seasoning',
                       'mint sauce', 'salvie', 'pav bhaji masala', 'mustard', 'white miso paste',
                       'liquid smoke flavoring', 'chili paste', 'god fiskekraft',
                       'bolognese, base', 'lemongrass', 'wasabi', 'chili sauce', 'vinegar',
                       'soy sauce', 'shake mixed spice', 'salt and pepper', 'salt',
                       'salt and pepper and lemon juice', 'salt and pepper and oil',
                       'parsley', 'ginger', 'herbs', 'horseradish sauce') ~ 'Seasoning, sauces and condiments',
    #Alcohol
    Ingredients2 %in% c('cognac', 'white wine') ~ 'Alcoholic beverages',
    
    #Vegetables and fruit
    Ingredients2 %in% c('grape juice', 'lemon juice') ~ 'Fruit and vegetable juices and nectars (including concentrates)',
    Ingredients2 %in% c('wok-vegetables', 'sweet peppers', 'sauerkraut', 'red cabbage',
                        'onion', 'garlic', 'leeks', 'salad') ~ 'Vegetables and vegetable products',
    
    #Meat (reindeer might be classed as lamb? Similar raising in Norway..)
    Ingredients2 %in% c('sausage') ~ 'Meat and meat products',
    
    #Grains and grain-based products
    Ingredients2 %in% c('wheat flour', 'rice', 
                       'corn starch') ~ 'Grains and grain-based products',
    
    #Fat
    Ingredients2 %in% c('duck fat', 'butter', 'olive oil', 'peanut oil',
                        'sesame oil', 'vegetable oil') ~ 'Animal and vegetable fats and oils and primary derivatives thereof',
    
    #Composite dishes
    Ingredients2 %in% c('puff pastry') ~ 'Composite ingredients',
    
    #Dairy
    Ingredients2 %in% c('plain yoghurt', 'parmesan cheese', 'goat cheese') ~ 'Milk and dairy products',
    
    #Sweeteners
    Ingredients2 %in% c('honey', 'sugar') ~ 'Sugar and similar, confectionery and water-based sweet desserts',
    
    #Egg
    Ingredients2 %in% c('egg') ~ 'Eggs and egg products',
    
    #Water
    Ingredients2 %in% c('water') ~ 'Water and water-based beverages'

  ))

    #Counts
    various$missing_amounts_ingredients_counts <- list()
    #Per ingredients
    various$missing_amounts_ingredients_counts$Ingredients <- various$missing_amounts_ingredients %>%
      #Count
      group_by(Ingredients2, Country) %>%
      summarise(value = n()) %>%
      ungroup() %>%
      #Rename
      rename(feature = Ingredients2)
    #Per foodgroup
    various$missing_amounts_ingredients_counts$Foodgroups <- various$missing_amounts_ingredients %>%
      #Count
      group_by(Foodgroups, Country) %>%
      summarise(value = n()) %>%
      ungroup() %>%
      #Rename
      rename(feature = Foodgroups)
    #Both in one
    various$missing_amounts_ingredients_counts <- bind_rows(various$missing_amounts_ingredients_counts, .id = 'Type')
    
  #Save
  #Ingredients with no amounts
  saveRDS(various$missing_amounts_ingredients, 'noamounts_ingredients.Rds')
  saveRDS(various$missing_amounts_ingredients_counts, 'noamounts_ingredients_counts.Rds')
  #Ingredients with no sustainability data in SHARP
  saveRDS(various$SHARP$not_in_ref, 'notinSHARP_ingredients.Rds')
  saveRDS(various$SHARP$not_in_ref_counts, 'notinSHARP_counts.Rds')

#Data----
data <- readRDS('data_to_analyze.Rds') %>%
  #Remove special characters and use lower case letters for recipe names
  mutate(
    `Selected Meals` = `Selected Meals` %>%
      str_replace('Biff with BearnÃ© saus', 'Biff with Bearné saus') %>%
      str_replace('PinnekjÃ¸tt', 'Pinnekjøtt') %>%
      str_replace('sauted reindeer', 'sautéed reindeer') %>%
      str_replace_all("'|\\(|\\)", "") %>%
      str_replace_all('-', ' ') %>%
      stri_trans_tolower() %>%
      str_squish() %>%
      str_trim() %>%
      str_replace('pregnant joolsâ€™s pasta crunchy chicory and watercress salad', 'pregnant joolss pasta crunchy chicory and watercress salad'),
    L1 = str_replace(L1, 'Composite dishes', 'Composite ingredients'))
  
  #Numbers/percentage of missing ingredients from SHARP for each recipes
  various$missing_ingredients <- data %>%
    group_by(`Selected Meals`, Country) %>%
    #Total number of ingredients, and number of missing values
    dplyr::summarise(n_ingredients = n(),
                     NA_ingredients = sum(is.na(Land_use))) %>%
    #Percentage complete
    mutate(pct_complete = 100 - (NA_ingredients/n_ingredients*100)) %>%
    #Rename columns
    rename(sample_id = `Selected Meals`,
           group = Country)

#Get kcal, macronutrients etc, rename columns in datasets to the same names
various$macros <- list(
  'Norway' = read_csv('./oppskrifter/Data_Norway_fullweight.csv') %>%
    rename(`Added sugar` = AddedSugar,
           `Saturated fat` = Saturated,
           #Turn Na (mg) to the equivalent salt (g) value
           Salt = Na) %>% mutate(Salt = Salt*2.5/1000),
  'UK' = read_csv('./oppskrifter/Data_UK_fullweight.csv') %>%
    rename(kcal = Calories,
           `Added sugar` = Sugar,
           Carbohydrates = Carbohydrate,
           `Saturated fat` = `Sat Fat`,
           Weight = `Recipe Size`),
  'US' = read_csv('./oppskrifter/Data_US_fullweight.csv') %>%
    rename(`Added sugar` = Sugar,
           `Selected Meals` = Name,
           Fibre = Fiber,
           kcal = Calories,
           Weight = Size,
           `Saturated fat` = `Saturated Fat`,
           #Turn sodium (g) to the equivalent salt (g) value
           Salt = Sodium) %>% mutate(Salt = Salt*2.5) %>%
    #Multiply with serving size
    mutate(Fat = Fat*Servings,
           Fibre = Fibre*Servings,
           Salt = Salt*Servings,
           Carbohydrates = Carbohydrates*Servings,
           Protein = Protein*Servings,
           `Added sugar` = `Added sugar`*Servings,
           kcal = kcal*Servings,
           `Saturated fat` = `Saturated fat`*Servings)
    
) %>% bind_rows(.id = 'Country') %>%

  #Select columns with data, ignore saturated fat for the time being, not all recipes in the fullweight df's has it
  select(`Selected Meals`, kcal, Protein, Carbohydrates, Fibre, `Added sugar`, Fat, `Saturated fat`, `Salt`, Weight, Country) %>%

  #Turn tidy and get macros pr 100g
  pivot_longer(
    cols = -c(`Selected Meals`, Weight, Country),
    names_to = 'feature',
    values_to = 'value_fullweight'
  ) %>%
  mutate(value = value_fullweight/Weight*100) %>%
  select(-value_fullweight) %>%
  #Rename variables with /100g suffix
  mutate(feature = paste0(feature, '/100g')) %>%
  #Fix up the recipe names, some are different form the other datasets
  mutate(
    `Selected Meals` = `Selected Meals` %>%
      str_replace('Biff with BearnÃ© saus', 'Biff with Bearné saus') %>%
      str_replace('PinnekjÃ¸tt', 'Pinnekjøtt') %>%
      str_replace('sauted reindeer', 'sautéed reindeer') %>%
      str_replace('Carbonnade à la flamande', 'carbonnade ã la flamande') %>%
      str_replace('Broad beans on toast', 'fava beans on toast') %>%
      str_replace_all("'|’|\\(|\\)", "") %>%
      str_replace_all('-', ' ') %>%
      stri_trans_tolower() %>%
      str_replace('chinese pepper steak', 'chinese pepper steak 2') %>%
      str_squish() %>%
      str_trim()
) %>%
  #Only get recipes used for analysis
  inner_join(data %>%
               select(`Selected Meals`, Country) %>% unique()) %>%
  select(-Weight) #Not needed anymore

#Calculate % fruit and veg in each recipe for Nutriscore evaluation
#Starchy vegetables are not included in nutriscore vegetable category, so "Starchy roots or tubers and products thereof, sugar plants" are not included
#Dried fruits/veg, purees and ketchup is multiplied by 2 as per Nutriscore guideline
various$fruit_veg_legumes_nuts_oils <- data %>%
  select(`Selected Meals`, Country, Ingredients, L1, Amounts_kg) %>%
  #Find all fruit and veg ingredients, including juices (and excluding coconut milk) as per guidelines 
  #In addition legumes, nuts and olive/rapeseed/walnut oil
  mutate(L1 = case_when(
    (L1 %in% c('Vegetables and vegetable products', 'Fruit and fruit products',
              'Fruit and vegetable juices and nectars (including concentrates)',
              'Legumes, nuts, oilseeds and spices') &
      !str_detect(Ingredients, 'mushroom|coconut milk|sesame seed|pine')) |
      str_detect(Ingredients, 'olive oil|rapeseed oil|walnut oil') ~ 'fruit_veg_legumes_nuts_oils',
    TRUE ~ L1),
    #Multiply dried fruit/veg and purees/ketchup by 2
    Amounts_kg = case_when(
      str_detect(Ingredients, 'raisin|prune|apricots dried|tomato puree canned|tomato ketchup') ~ Amounts_kg*2,
      TRUE ~ Amounts_kg)
    ) %>%
  
  #Calculate the weight of fruit and vegs and total weight of the recipes
  group_by(`Selected Meals`, L1, Country) %>%
  summarise(weight_L1 = sum(Amounts_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(`Selected Meals`) %>%
  mutate(total_weight = sum(weight_L1, na.rm = TRUE)) %>%
  ungroup() %>%
  #Calculate percentage fruit veg
  filter(L1 == 'fruit_veg_legumes_nuts_oils') %>%
  mutate(`pct_FruitVegLegumesNutsOils` = weight_L1/total_weight*100) %>%
  #select needed columns
  select(`Selected Meals`, Country, pct_FruitVegLegumesNutsOils) %>%
  #Add zero values to recipes with no fruit/veg (tomato based sauces or other fruit/veg conditments are not counted
  full_join(data %>% select (c(`Selected Meals`, Country)) %>% unique()) %>%
  replace_na(list(pct_FruitVegLegumesNutsOils = 0)) %>%
  #Rename fr plotting
  rename(sample_id = `Selected Meals`,
         value = pct_FruitVegLegumesNutsOils,
         group = Country) %>%
  #Add a feature column explaining the value column
  mutate(feature = 'pct_FruitVegLegumesNutsOils')

#Turn tidy
tidy <- data %>%
  #Turn selected meals to lowercase
  mutate(`Selected Meals` = `Selected Meals` %>%
           stri_trans_tolower() %>%
           str_replace_all("'|\\(|\\)", "") %>%
           str_replace_all('-', ' ')) %>%
  #Only keep features with values/100g
  select(-c(meal_CO2, meal_land_use, meal_kg, CO2_footprint, Land_use, L1, Amounts_kg)) %>%
  #Turn into long format
  pivot_longer(
    cols = -c(`Selected Meals`, Ingredients, Country),
    names_to = 'feature',
    values_to = 'value'
  ) %>%
  #Join to macros
  full_join(various$macros) %>%
  #Figure out why there are duplicates
  group_by(`Selected Meals`, feature, Country) %>%
  summarise(value = mean(value)) %>%
  ungroup()  %>%
  #Rename for plotting functions
  rename(group = Country,
         sample_id = `Selected Meals`) %>%
  mutate(feature = feature %>%
           str_replace("meal_CO2_100", "kg CO[2] eq/100g") %>%
           str_replace("meal_CO2", "kg CO[2] eq") %>%
           str_replace("meal_land_use_100", "m^2 land use/100g") %>%
           str_replace("meal_land_use", "m^2 land use")
  ) %>%
  #Add pct_fruit_veg
  full_join(various$fruit_veg_legumes_nuts_oils) %>%
  
  #Reorder feature column
  mutate_at('feature', ~as.factor(.)) %>%
  mutate(feature = fct_relevel(feature, levels = c("kcal/100g", "Fat/100g", "Saturated fat/100g", "Protein/100g", 
                               "Carbohydrates/100g", "Fibre/100g", "Added sugar/100g", "Salt/100g")))

#Save
saveRDS(tidy, 'tidy_with_macros.Rds')

#Healthiness scores---- Redo kcal column based on the amount of macronutrients so it adds up?
#Empty list to fill
various$healthiness <- list()

  #Wide dataframe to calculate percentages for who, nnr and traffic lights
  temp <- tidy %>%
    pivot_wider(names_from = feature,
                values_from = value) %>%
    rename(carbohydrates = `Carbohydrates/100g`,
           fat = `Fat/100g`,
           saturated_fat = `Saturated fat/100g`,
           fibre = `Fibre/100g`,
           protein = `Protein/100g`,
          added_sugar = `Added sugar/100g`,
          kcal = `kcal/100g`,
          salt = `Salt/100g`) %>%
    select(-c(`kg CO[2] eq/100g`, `m^2 land use/100g`))

  #Healthiness scores
  various$healthiness$who <- calculateNutritionScore_who(temp)
  various$healthiness$nnr <- calculateNutritionScore_nnr(temp)
  various$healthiness$fsa_trafficlights <- calculateNutritionScore_trafficlights(temp)
  various$healthiness$nutriscore <- calculateNutritionScore_nutriscore(tidy) %>%
    #Columns for correlation analysis
    select(sample_id, inverted_nutriscore) #Omit nutriscore letters as all other scores are numerical
  various$nutriscore_letters <- calculateNutritionScore_nutriscore(tidy) %>%
    select(sample_id, nutriscore_letter)
  
  #Add all to one df, except the letters
  various$healthiness <- bind_rows(various$healthiness) %>%
    #Fill inn missing country for some recipes
    group_by(sample_id) %>%
    fill(group) %>%
    ungroup() %>%
    #Turn tidy
    pivot_longer(
      cols = -c(sample_id, group),
      names_to = 'feature',
      values_to = 'value'
    ) %>% drop_na() #Drop NAs produced when binding the indivudal dataframes together
  
  #Add to tidy dataframe
  tidy <- full_join(tidy, various$healthiness)
  
  
#Plots----
#Select colors for the different countries for plots not pre-coded
various$country_colors <- groupColors(tidy)

#empty list to fill with plots
plots <- list(
  'violinBoxPlots' = list(),
  'PCA' = list(),
  'correlations' = list()
)

  #Violin Boxplots----
  #Nutrients
  plots$violinBoxPlots$nutrients <- plotViolinBox(
    #Select only nutrients
    tidy %>% filter(
    !feature %in% c('kg CO[2] eq/100g', 'm^2 land use/100g',
                     'nnr_score', 'who_score', 'inverted_traffic_score',
                     'inverted_nutriscore')) %>%
      #Make units clearer
      cleanUpFeatures() %>%
      
      #Change the order of the plots if using facet wrap
      mutate_at('feature', ~as.factor(.)) %>%
      mutate(feature = fct_relevel(feature, levels = c("kcal/100g", "pct_FruitVegLegumesNutsOils", "Salt g/100g",
                                                       "Carbohydrates E%", "Added sugar E%", "Fibre g/100g",
                                                       "Fat E%", "Saturated fat E%", "Protein E%")))
  ) +
    facet_wrap(~feature, nrow = 4, scales = 'free') +
    labs(color = 'Country',
         x = 'Country')

  plots$violinBoxPlots$nutrients
  
  #Healthiness score
  plots$violinBoxPlots$healthiness_scores <- plotViolinBox(various$healthiness %>%
                                                             cleanUpFeatures()) +
    facet_wrap(~feature, nrow = 3, scales = 'free') +
    labs(color = 'Country',
         x = 'Country')
  
  plots$violinBoxPlots$healthiness_scores

  #Plot Sustainability markers
  #CO2 and landuse
  plots$violinBoxPlots$sustainability <- plotViolinBox(
    #Select only nutrients
    tidy %>% filter(
      feature %in% c('kg CO[2] eq/100g', 'm^2 land use/100g'))
    ) +
    facet_wrap(~feature, nrow = 1, scales = 'free') +
    labs(color = 'Country',
         x = 'Country')
  
  plots$violinBoxPlots$sustainability
  
  #Per foodgroup
  temp <- data %>%
    rename(sample_id = `Selected Meals`,
           group = Country,
           Foodgroup = L1) %>%
    #calculate CO2 and lanuse pr foodgroup pr 100g of a recipe
    group_by(sample_id, group, Foodgroup) %>%
      #For whole recipe
      summarise(`kg CO[2] eq` = sum(CO2_footprint, na.rm = TRUE),
                `m^2 land use` = sum(Land_use, na.rm = TRUE),
                weight = sum(meal_kg)) %>%
      ungroup() %>%
      #Per 100g
      mutate(`kg CO[2] eq/100g` = `kg CO[2] eq`/weight/10,
             `m^2 land use/100g` = `m^2 land use`/weight/10) %>%
      #Remove unnecessary columns
      select(-c(`m^2 land use`, `kg CO[2] eq`, weight)) %>%
    #Turn tidy for plot
    pivot_longer(cols = c(`kg CO[2] eq/100g`, `m^2 land use/100g`),
                 names_to = 'feature',
                 values_to = 'value') %>%
    #Remove foodgroups with no values, such as water, rename some categories
    filter(!Foodgroup %in% c('Water and water-based beverages')) %>%
    mutate(Foodgroup = case_when(
      Foodgroup == 'Animal and vegetable fats and oils and primary derivatives thereof' ~ 'Fats',
      Foodgroup == 'Fish, seafood, amphibians, reptiles and invertebrates' ~ 'Seafood',
      Foodgroup == 'Fruit and vegetable juices and nectars (including concentrates)' ~ 'Fruit and vegetable juices',
      Foodgroup == 'Starchy roots or tubers and products thereof, sugar plants' ~ 'Starchy roots or tubers',
      Foodgroup == 'Sugar and similar, confectionery and water-based sweet desserts' ~ 'Sugar and similar',
      Foodgroup == 'Products for non-standard diets, food imitates and food supplements' ~ 'Food imitates',
      TRUE ~ Foodgroup
    ))
  
  #CO2 from animal sources
  plotViolinBox(temp %>%
                  filter(feature == 'kg CO[2] eq/100g') %>%
                  filter(Foodgroup %in% c('Seafood', 'Meat and meat products',
                                          'Milk and dairy products', 'Eggs and egg products'))) +
    facet_wrap(~Foodgroup, nrow = 1) +
    labs(color = 'Country',
         y = bquote('kg '~CO[2]~' eq/100g'),
         x = 'Country')
  
  #CO2 from plant based sources
  plotViolinBox(temp %>%
                  filter(feature == 'kg CO[2] eq/100g') %>%
                  filter(Foodgroup %in% c('Fruit and fruit products', 'Fruit and vegetable juices',
                                          'Sugar and similar', 'Vegetables and vegetable products',
                                          'Legumes, nuts, oilseeds and spices', 'Grains and grain-based products',
                                          'Starchy roots or tubers'))) +
    facet_wrap(~Foodgroup, nrow = 2) +
    labs(color = 'Country',
         y = bquote('kg '~CO[2]~' eq/100g'),
         x = 'Country')
  
  #CO2 from Various sources
  plotViolinBox(temp %>%
                  filter(feature == 'kg CO[2] eq/100g') %>%
                  filter(!Foodgroup %in% c('Fruit and fruit products', 'Fruit and vegetable juices',
                                          'Sugar and similar', 'Vegetables and vegetable products',
                                          'Legumes, nuts, oilseeds and spices', 'Grains and grain-based products',
                                          'Starchy roots or tubers', 'Seafood', 'Meat and meat products',
                                          'Milk and dairy products', 'Eggs and egg products'))) +
    facet_wrap(~Foodgroup, nrow = 2) +
    labs(color = 'Country',
         y = bquote('kg '~CO[2]~' eq/100g'),
         x = 'Country')
  
  #Landuse from animal sources
  plotViolinBox(temp %>%
                  filter(feature == 'm^2 land use/100g') %>%
                  filter(Foodgroup %in% c('Seafood', 'Meat and meat products',
                                          'Milk and dairy products', 'Eggs and egg products'))) +
    facet_wrap(~Foodgroup, nrow = 1) +
    labs(color = 'Country',
         y = bquote(~m^2~' /100g'),
         x = 'Country')
  
  #Land use from plant based sources
  plotViolinBox(temp %>%
                  filter(feature == 'm^2 land use/100g') %>%
                  filter(Foodgroup %in% c('Fruit and fruit products', 'Fruit and vegetable juices',
                                          'Sugar and similar', 'Vegetables and vegetable products',
                                          'Legumes, nuts, oilseeds and spices', 'Grains and grain-based products',
                                          'Starchy roots or tubers'))) +
    facet_wrap(~Foodgroup, nrow = 2) +
    labs(color = 'Country',
         y = bquote(~m^2~' /100g'),
         x = 'Country')
  
  #Land use from Various sources
  plotViolinBox(temp %>%
                  filter(feature == 'm^2 land use/100g') %>%
                  filter(!Foodgroup %in% c('Fruit and fruit products', 'Fruit and vegetable juices',
                                           'Sugar and similar', 'Vegetables and vegetable products',
                                           'Legumes, nuts, oilseeds and spices', 'Grains and grain-based products',
                                           'Starchy roots or tubers', 'Seafood', 'Meat and meat products',
                                           'Milk and dairy products', 'Eggs and egg products'))) +
    facet_wrap(~Foodgroup, nrow = 2) +
    labs(color = 'Country',
         y = bquote(~m^2~' /100g'),
         x = 'Country')
  
  
  #Correlation scattermarices----
  #Fix the colors on the correlation scores in the upper segment, slight alteration of code from Isaac Zhao
  myCorrelations <- function(data,mapping,...){
    data2 = data
    data2$x = as.numeric(data[,as_label(mapping$x)])
    data2$y = as.numeric(data[,as_label(mapping$y)])
    data2$group = data[,as_label(mapping$colour)]
    
    correlation_df = data2 %>% 
      bind_rows(data2 %>% mutate(group="Overall Corr")) %>%
      group_by(group) %>% 
      filter(sum(!is.na(x),na.rm=T)>1) %>%
      filter(sum(!is.na(y),na.rm=T)>1) %>%
      summarize(estimate = round(as.numeric(corr.test(data2$x,data2$y,method="spearman")$r), digits = 3),
                pvalue = corr.test(x,y,method="spearman")$p.adj,
                pvalue_star = as.character(symnum(pvalue, corr = FALSE, na = FALSE, 
                                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                                  symbols = c("***", "**", "*", "'", " "))))%>%
      group_by() %>%
      mutate(group = factor(group, levels=c(as.character(unique(sort(data[,as_label(mapping$colour)]))), "Overall Corr")))
    
    ggplot(data=correlation_df, aes(x=1,y=group,color=group))+
      geom_text(aes(label=paste0(group,": ",estimate,pvalue_star)))
  }
  
  #Healthiness scores
  temp <- various$healthiness %>%
    cleanUpFeatures() %>%
    pivot_wider(
      names_from = 'feature',
      values_from = 'value'
    ) %>%
    #Add nutriscore letters
    full_join(various$nutriscore_letters)
  
  plots$correlations$healthiness <- ggpairs(temp %>% select(-c(sample_id)),
            mapping = ggplot2::aes(color=group, alpha = 0.6),
            columns = 2:6,
            upper = list(continuous = myCorrelations), #list(continuous = wrap("cor", method = "spearman")),
            lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
    scale_color_manual(values = various$country_colors$sample_group) +
    scale_fill_manual(values = various$country_colors$sample_group)
  
  #Healthiness vs sustainability
  temp <- tidy %>%
    #Select healthiness and sustainabilitymarkers
    filter(feature %in% c('who_score', 'nnr_score', 'inverted_traffic_score', 'inverted_nutriscore',
                          'kg CO[2] eq/100g', 'm^2 land use/100g')) %>%
    #Clean up names and turn wide
    cleanUpFeatures() %>%
    pivot_wider(
      names_from = 'feature',
      values_from = 'value'
    )
   
    #Plot 
    ggpairs(temp %>% select(-c(sample_id)),
            mapping = ggplot2::aes(color=group, alpha = 0.6),
            columns = 2:7,
            upper = list(continuous = myCorrelations), #list(continuous = wrap("cor", method = "spearman")),
            lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
      scale_color_manual(values = various$country_colors$sample_group) +
      scale_fill_manual(values = various$country_colors$sample_group)
    
  #Barplots----
  #Counts of different nutriscores
  plots$nutriscore_letters <- various$nutriscore_letters %>%
    full_join(tidy %>% select(sample_id, group)) %>%
    unique() %>%
    #Count the occurences of each letter
    group_by(group, nutriscore_letter) %>%
    summarise(Count = n()) %>% ungroup() %>%
    #Plot
    ggplot(aes(x = Count, y = nutriscore_letter)) +
    geom_bar(position = 'dodge', stat = 'identity', aes(fill = group)) +
    geom_text(aes(x = Count, y = nutriscore_letter, label = Count, group = group),
              position = position_dodge(width = 0.9), hjust = -0.2) +
    scale_fill_manual(values = various$country_colors$sample_group) +
    scale_x_continuous(limits = c(0, 40)) +
    #Change axis labels
    labs(
      y = 'Nutriscore',
      x = 'Counts',
      fill = 'Country'
    )
  
  plots$nutriscore_letters
  
  #PCA----
  PCA_score <- createPCA(tidy %>% cleanUpFeatures())
  PCA_loadings <- createPCA(tidy %>% cleanUpFeatures(), plots = 'loadings') +# %>% filter(!feature == 'kcal/100g'), plots = 'loadings') +
    geom_label_repel(aes(label = feature))

  PCA_loadings
  PCA_score

#Correlations----
#Dataframe for correlation analysis for all data and for each separate country
various$corr_data <- list(
  
  'all' = tidy %>%
    mutate(feature = as.character(feature)) %>%
    mutate(feature = case_when(
      str_detect(feature, '/100g') ~ str_replace(feature, '/100g', ''),
      TRUE ~ feature
    )) %>%
    rename(temp = sample_id) %>%
    unite(col = 'sample_id', c(temp, group)) %>%
    pivot_wider(names_from = feature,
                values_from = value) %>%
    column_to_rownames('sample_id'),
  
  'Norway' = tidy %>%
    mutate(feature = as.character(feature)) %>%
    mutate(feature = case_when(
      str_detect(feature, '/100g') ~ str_replace(feature, '/100g', ''),
      TRUE ~ feature
    )) %>%
    filter(group == 'Norway') %>%
    rename(temp = sample_id) %>%
    unite(col = 'sample_id', c(temp, group)) %>%
    pivot_wider(names_from = feature,
                values_from = value) %>%
    column_to_rownames('sample_id'),
  
  'UK' = tidy %>%
    mutate(feature = as.character(feature)) %>%
    mutate(feature = case_when(
      str_detect(feature, '/100g') ~ str_replace(feature, '/100g', ''),
      TRUE ~ feature
    )) %>%
    filter(group == 'UK') %>%
    rename(temp = sample_id) %>%
    unite(col = 'sample_id', c(temp, group)) %>%
    pivot_wider(names_from = feature,
                values_from = value) %>%
    column_to_rownames('sample_id'),
  
  'US' = tidy %>%
    mutate(feature = as.character(feature)) %>%
    mutate(feature = case_when(
      str_detect(feature, '/100g') ~ str_replace(feature, '/100g', ''),
      TRUE ~ feature
    )) %>%
    filter(group == 'US') %>%
    rename(temp = sample_id) %>%
    unite(col = 'sample_id', c(temp, group)) %>%
    pivot_wider(names_from = feature,
                values_from = value) %>%
    column_to_rownames('sample_id')
)

correlations <- lapply(corr_data, corr.test, method = 'spearman')
  

ggcorrplot(correlations$all$r,
           #Cluster
           hc.order = TRUE,
           #Use an outline for each square, and only show the lower squares
           outline.col = "white", type = "lower",

           #Change colors and add labels
           ggtheme = ggplot2::theme_bw, method = 'circle',
           colors = c("#6D9EC1", "white", "#E46726"),
           #lab = TRUE,
           p.mat = correlations$all$p)



corr_plots <- lapply(correlations, plotCorrelations)

#Kruskal wallis & dunn test----
stats <- list()

stats$kruskal_wallis <- tidy %>%
  group_by(feature) %>%
  kruskal_test(value ~ group)

stats$kruskal_effectsize <- tidy %>%
  group_by(feature) %>%
  kruskal_effsize(value ~ group)

stats$dunn_test <- tidy %>%
  group_by(feature) %>%
  dunn_test(value ~ group)


#Tables----
#Empty list to fill
tables <- list()

  #Ingredients with no amounts in the recipes or not found in SHARP-ID----
  #By foodgroup
  #Format the data
  tables$noamounts <- various$missing_amounts_ingredients_counts %>%
    filter(Type == 'Foodgroups') %>%
    select(-Type) %>%
    #turn wide
    pivot_wider(names_from = Country, values_from = value) %>%
    #Add zeros for NA
    replace_na(list(Norway = 0, UK = 0, US = 0)) %>%
    #Rename column
    rename(Foodgroup = feature)

  #Table
  tables$noamounts %>%
    kbl(caption = 'Ingredients with no amounts in the recipes') %>%
    kable_classic(full_width = F)

  #Top five ingredients pr country
  #Format data
  tables$noamounts_top10 <- various$missing_amounts_ingredients_counts %>%
    filter(Type == 'Ingredients') %>%
    select(-Type) %>%
    #Order by counts and select top 5 for each country
    group_by(Country) %>%
    arrange(desc(value), .by_group = TRUE) %>%
    slice_head(., n = 10) %>%
    ungroup() %>%
    #Rename column
    rename(Ingredients = feature,
           Count = value)

  #Table
  tables$noamounts_top10 %>%
    select(-Country) %>% #Not needed when using pack rows with caption
    kbl(caption = 'Top 10 ingredients with no amounts in the recipes') %>%
    kable_classic(full_width = F) %>%
    #Group
    pack_rows("Norway", 1, 10) %>%
    pack_rows("UK", 11, 20) 
  
  #Missing values in SHARP by foodgroup
  #By foodgroup
  #Format the data
  tables$missing <- various$SHARP$not_in_ref_counts %>%
    filter(Type == 'Foodgroups') %>%
    select(-Type) %>%
    #turn wide
    pivot_wider(names_from = Country, values_from = value) %>%
    #Add zeros for NA
    replace_na(list(Norway = 0, UK = 0, US = 0)) %>%
    #Rename column
    rename(Foodgroup = feature)

    #Table
    tables$missing %>%
      kbl(caption = 'Ingredients with no value in SHARP-ID') %>%
      kable_classic(full_width = F)
    
  #Top ten ingredients pr country
  #Format data
  tables$missing_top10 <- various$SHARP$not_in_ref_counts %>%
    filter(Type == 'Ingredients') %>%
    select(-Type) %>%
    #Order by counts and select top 5 for each country
    group_by(Country) %>%
    arrange(desc(value), .by_group = TRUE) %>%
    slice_head(., n = 10) %>%
    ungroup() %>%
    #Rename column
    rename(Ingredients = feature,
           Count = value)

    #Table
    tables$missing_top10 %>%
      select(-Country) %>% #Not needed when using pack rows with caption
      kbl(caption = 'Top 10 ingredients not in SHARP-ID by country') %>%
      kable_classic(full_width = F) %>%
      #Group
      pack_rows("Norway", 1, 10) %>%
      pack_rows("UK", 11, 20) %>%
      pack_rows("US", 21, 30)

    
  #Stats----
  #Kruskal wallis, with effectsize
  tables$stats_kruskal_wallis <- stats$kruskal_wallis %>%
      #Join dfs
      full_join(stats$kruskal_effectsize, by = 'feature') %>%
      #Cleanup feature names and column names
      mutate(feature = case_when(
        str_detect(feature, 'sugar|Protein|Fat|Carbohydrates|fat') ~ str_replace(feature, '/100g', ' E%'),
        str_detect(feature, 'Salt|Fibre') ~ str_replace(feature, '/', ' g/'),
        TRUE ~ feature
      )) %>%
      rename(Feature = feature,
             n = n.x,
             `Effect size` = effsize,
             Magnitude = magnitude,
             `p value` = p) %>%
      select(Feature, `p value`, `Effect size`, Magnitude, n, df)
      
    #Table
    tables$stats_kruskal_wallis %>%
      kbl(caption = 'Kruskal wallis results with effect size ') %>%
      kable_classic(full_width = F)
    
  #Dunn test, only keep statistically different features
  tables$stats_dunn_test <- stats$dunn_test %>%
    #Cleanup feature names and rename columns
    select(feature, group1, group2, n1, n2, p.adj) %>%
    cleanUpFeatures() %>%
    rename(Feature = feature,
            `Adjusted p.value` = p.adj) %>%
    #Only keep stat. sig. features
    filter(`Adjusted p.value` < 0.05) %>%
    #Arrange in ascending order
    arrange(., `Adjusted p.value`) %>%
    #Format adjusted p value to scientific notation
    mutate_at('Adjusted p.value', ~format(., format = "e", digits = 3))
    
    #Table
    tables$stats_dunn_test %>%
      kbl(caption = 'Statistically significant Dunn test results') %>%
      kable_classic(full_width = F)
    