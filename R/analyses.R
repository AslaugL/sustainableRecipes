library(tidyverse)
library(rstatix)
library(stringi)
library(thesisToolsOmics)
library(kableExtra)
library(psych)
library(GGally)

#Working directory
setwd('./Data')

#GGplot theme
theme_set(theme_bw())

#Read data
data_recipes <- readRDS('./output/recipes_data_ready_for_analysis.Rds') #Nutrient content/sustainability indicators pr 100g recipe
data_ingredients <- readRDS('./output/ingredients_data_ready_for_analysis.Rds') #Nutrient content/sustainability indicators for every ingredient pr 100g recipe
#Turn tidy
tidy_recipes <- data_recipes %>%
  pivot_longer(.,
               cols = -c(sample_id, group),
               names_to = 'feature',
               values_to = 'value')

tidy_ingredients <- data_ingredients %>%
  pivot_longer(.,
               cols = -c(Ingredients, sample_id, group, Source, Foodgroup, Amounts_kg),
               names_to = 'feature',
               values_to = 'value') %>%
  #Rename some foodgroups
  mutate(Foodgroup = Foodgroup %>%
           str_replace('Fish, seafood, amphibians, reptiles and invertebrates', 'Seafood') %>%
           str_replace('Legumes, nuts, oilseeds and spices', 'Legumes, nuts, seeds') %>%
           str_replace('Meat and meat products', 'Meat and\nmeat products') %>%
           str_replace('Milk and dairy products', 'Dairy') %>%
           str_replace('Animal and vegetable fats and oils and primary derivatives thereof', 'Fats and oils') %>%
           str_replace('Eggs and egg products', 'Eggs') %>%
           str_replace('Starchy roots or tubers and products thereof, sugar plants', 'Roots and tubers') %>%
           str_replace('Products for non-standard diets, food imitates and food supplements', 'Food imitates') %>%
           str_replace('Grains and grain-based products', 'Grains and grain\nbased products') %>%
           str_replace('Sugar and similar, confectionery and water-based sweet desserts', 'Sugar and\n confectionary') %>%
           str_replace('Fruit and vegetable juices and nectars \\(including concentrates\\)', 'Fruit/vegetable juice\n and nectar') %>%
           str_replace('Fruit and fruit products', 'Fruit and\nfruit products') %>%
           str_replace('Vegetables and vegetable products', 'Vegetables and\nvegetable products') %>%
           str_replace('Seasoning, sauces and condiments', 'Seasoning, sauces\nand conditments') %>%
           str_replace('Water and water-based beverages', 'Water based\nbeverages')
  ) %>% replace_na(list(Foodgroup = 'Unknown'))

#Various to keep environment clean
various <- list(
  
  'minerals' = c('Calcium', 'Iron', 'Zinc', 'Magnesium', 'Potassium', 'Selenium', 'Iodine', 'Sodium',
                 'Phosphorus', 'Copper'),
  'energy_contributing' = c('Kilojoules', 'Kilocalories', 'Fat', 'SatFa', 'PuFa', 'MuFa', 'Carbo',
                            'Sugar', 'Mono+Di', 'Starch', 'Dietary fibre', 'Alcohol', 'EPA', 'DHA',
                            'DPA', 'Protein'),
  'vitamins' = c('Vitamin A', 'Retinol', 'Beta-carotene', 'Thiamin', 'Riboflavin', 'Niacin', 'Vitamin B6',
                 'Folate', 'Vitamin B12', 'Vitamin C', 'Vitamin D', 'Vitamin E'),
  'sustainability' = c('CO2', 'Landuse'),
  'health' = c('inverted_nutriscore', 'nutriscore_letter', 'inverted_traffic_score', 'who_score', 'nnr_score', 'keyhole_certified')
)
#Select colors for the different countries for plots not pre-coded
various$country_colors <- groupColors(tidy_recipes)

#data completeness
temp <- readRDS('./output/missing_data.Rds') #Read data from cleanup script

  #Fix some column names
  data_completeness <- list(
    'no_amounts' = temp$no_amounts %>% rename(sample_id = `Selected Meals`),
    
    'no_nutrient_info' = temp$no_nutrient_info %>% rename(sample_id = `Selected Meals`,
                                                          value = pct_of_full_recipe) %>%
      #If any recipe has more than one ingredient missing nutrient values, sum the amounts together
      group_by(sample_id) %>% summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
      
      #Add country/group and the recipes with 100% nutrient info
      full_join(., data_recipes %>% select(sample_id, group)) %>%
      replace_na(list(value = 0)), #Reipes added have 0 nutrient information missing
    
    'no_sustainability_indicators' = temp$no_sustainability_indicators %>% rename(sample_id = `Selected Meals`,
                                                                            value = pct_of_full_recipe) %>%
      #If any recipe has more than one ingredient missing sustainability indicators, sum the amounts together
      group_by(sample_id) %>% summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
      #Add country/group and the recipes with 100% sustainability indicators calculates
      full_join(., data_recipes %>% select(sample_id, group)) %>%
      replace_na(list(value = 0)) #Reipes added have 0 sustainability indicators missing
  )

#Calculate health scores----
#Functions
calculateEnergypercentDensity <- function(df){
  
  df %>%
    
    #Select macronutrient columns
    select(sample_id, group, Kilocalories, Kilojoules, Carbo, Sugar, Fat, SatFa, Protein, `Dietary fibre`) %>%
    
    #Turn tidy for calculations
    pivot_longer(
      cols = -c(sample_id, group, Kilocalories, Kilojoules),
      names_to = 'feature',
      values_to = 'grams') %>%
    #Create a MJ column for density pr MJ calculations
    mutate(
      MJ = Kilocalories*0.004184,
      
      #Calculate how many kcals from each macronutrient, and the density of fibre pr megajoule energy
      #Carbohydrates, sugar and protein is about 4kcal/g, fat 9kcal/g
      kcal_macro = case_when(
        str_detect(feature, 'Carbo|Sugar|Protein') ~ grams*4,
        str_detect(feature, 'Fa') ~ grams*9,
        str_detect(feature, 'fibre') ~ grams*2),
      densityMJ = grams/MJ,
      energy_percent = kcal_macro/Kilocalories*100
    ) %>%
    
    #Remove columns
    select(-c(MJ, kcal_macro, grams, Kilojoules, Kilocalories))
  
}
calculateNutritionScore_who <- function(df){
  
  #first calculate energy percent
  energy_percent <- calculateEnergypercentDensity(df)
  
  #WHO recommendations
  who_score <- energy_percent %>%
    
    #Add 1 for each fulfilled recomendation
    mutate(who_recommendation = case_when(
      feature == 'Carbo' & (energy_percent >= 55 & energy_percent <=75) ~ 1,
      feature == 'Sugar' & energy_percent < 10 ~ 1,
      feature == 'Dietary fibre' & densityMJ >3 ~ 1,
      feature == 'Protein' & (energy_percent >=10 & energy_percent <=15) ~ 1, #Is it really 15? Other countries uses 20
      feature == 'Fat' & (energy_percent >= 15 & energy_percent <= 30) ~ 1,
      feature == 'SatFa' & energy_percent <10 ~ 1,
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
      feature == 'Carbo' & (energy_percent >= 45 & energy_percent <= 60) ~ 1,
      feature == 'Sugar' & energy_percent < 10 ~ 1,
      feature == 'Dietary fibre' & densityMJ >3 ~ 1,
      feature == 'Protein' & (energy_percent >=10 & energy_percent <=20) ~ 1,
      feature == 'Fat' & (energy_percent >= 25 & energy_percent <= 40) ~ 1,
      feature == 'SatFa' & energy_percent <10 ~ 1,
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
  #Turn df long
  temp <- df %>%
    pivot_longer(
      cols = -c(sample_id, group),
      names_to = 'feature',
      values_to = 'value'
    )
  
  #Inverted or not?
  if(inverted == 'yes'){
    
    traffic_lights <- temp %>%
      
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
  
  #Turn long
  nutriscore_raw <- df %>%
    pivot_longer(.,
                 cols = -c(sample_id, group),
                 names_to = 'feature',
                 values_to = 'value') %>%
    
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
    group_by(sample_id, group, category) %>%
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
    group_by(sample_id, group, category) %>%
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
  
  nutriscore %>% select(-c(N, P, Fibre, FruitVegLegumesNutsOils, Protein))
  
}
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
    select(sample_id, group, keyhole_certified)
  
}

#Prep----
#Calculate percentage of fruit, vegetables, nuts, legumes and healthy oils to calculate nutriscore and Keyhole
various$KHNS_relevant_amounts <- tidy_ingredients %>%
  
  #Only weight is important, not features
  select(-c(feature, value)) %>% unique() %>%
  
  #Nutriscore and keyhole relevant foodgroups and ingredients
  mutate(
    nutriscore_foods = case_when(
      (Foodgroup %in% c('Vegetables and vegetable products', 'Fruit and fruit products',
                        'Fruit and vegetable juices and nectars \\(including concentrates\\)',
                        'Legumes, nuts, oilseeds and spices') &
         !str_detect(Ingredients, 'mushroom|coconut milk|sesame seed|pine')) |
        str_detect(Ingredients, 'olive oil|rapeseed oil|walnut oil') ~ 'nutriscore_fruit_veg_legumes_nuts_oils'),
    keyhole_foods = case_when(
      Foodgroup %in% c('Vegetables and vegetable products', 'Fruit and fruit products',
                       'Legumes, nuts, oilseeds and spices') & !str_detect(Ingredients, 'peanut') ~ 'keyhole_fruit_veg_legumes')
  ) %>%
  
  #Keep relevant amounts, remove the rest
  mutate(
    nutriscore_amounts_pct = case_when(
      #Dried fruit/veg and ketchup/purees should be doubled
      !is.na(nutriscore_foods) & str_detect(Ingredients, 'paste|ketchup|dried|raisin|prune') ~ Amounts_kg*2,
      !is.na(nutriscore_foods) ~ Amounts_kg),
    keyhole_amounts_pct = case_when(
      !is.na(keyhole_foods) ~ Amounts_kg)
  ) %>%
  select(-c(Amounts_kg, Foodgroup, nutriscore_foods, keyhole_foods)) %>%
  
  #Calculate total amounts for each recipe
  #Turn longer, since amounts are pr 100 g the % of vegetables etc is found by multiplying with 100
  pivot_longer(.,
               cols = c(nutriscore_amounts_pct, keyhole_amounts_pct),
               names_to = 'feature',
               values_to = 'value') %>%
  group_by(sample_id, feature, group) %>%
  summarise(pct = sum(value, na.rm = TRUE)*100) %>%
  ungroup() #%>%

#Which recipes comply with the whole grain requirement for the keyhole certification
various$recipe_keyhole_req <- tidy_ingredients %>%
  
  #Find recipes with whole grain ingredients
  group_by(sample_id) %>%
  mutate(whole_grain_requirement = case_when(
    
    #If recipe contains grain-products, is it whole grain? If so it fullfills requirement, if they are white it doesn't. Recipes without grains fullfill this requirement by default
    str_detect(Ingredients, 'pasta|rice|bread|tortilla|wheat flour|puff pastry') & str_detect(Ingredients, 'whole grain|coarse') ~ 'yes',
    str_detect(Ingredients, 'pasta|rice|bread|tortilla|wheat flour|puff pastry') & !str_detect(Ingredients, 'whole grain|coarse') ~ 'no',
    
    TRUE ~ 'yes'
  )) %>%
  #Keep only relevant columns
  select(sample_id, group, whole_grain_requirement) %>% unique() %>%
  #If any 'no' is found in whole_grain_requirement column, keep it and remove yes
  mutate(whole_grain_requirement = case_when(
    any(whole_grain_requirement == 'no') ~ 'no',
    TRUE ~ 'yes'
  )) %>% unique() %>% ungroup() %>%
  
  #Type of recipe
  mutate(
    recipe_type = case_when(
      str_detect(sample_id, 'soup|stew|\\bpot\\b|casserole') ~ 'soups_and_stews',
      str_detect(sample_id, 'pizza|\\bpie\\b|\\bPies\\b|\\btart\\b') ~ 'pizza_and_pies',
      str_detect(sample_id, 'wraps') ~ 'wraps',
      TRUE ~ 'ready_meals_27' #Use the ready meal with either a carbohydrate or a protein part, description 27 in Forskrift om frivillig merking med Nøkkelhullet
    )
  )


#Health indicator calculations----
temp <- list(
  
  'nutriscore' = data_recipes %>%
    #Add percentage of recipe that is vegetables/fruit/legumes/nuts/healthy oils
    inner_join(various$KHNS_relevant_amounts %>% pivot_wider(names_from = feature, values_from = pct)) %>%
    calculateNutritionScore_nutriscore(),
  
  'multiple_traffic_light' = data_recipes %>%
    calculateNutritionScore_trafficlights(),
  
  'Keyhole' = data_recipes %>%
    #Add percentage of recipe that is vegetables/fruit/legumes/nuts
    inner_join(various$KHNS_relevant_amounts %>% pivot_wider(names_from = feature, values_from = pct)) %>%
    #See if recipes fullfill whole grain criteria
    inner_join(various$recipe_keyhole_req) %>%
    keyholeCertification(),
  
  'who_score' = data_recipes %>%
    calculateNutritionScore_who(),
  
  'nnr_score' = data_recipes %>%
    calculateNutritionScore_nnr()
)

health_indicators <- temp %>% reduce(full_join, by = c('sample_id', 'group')) %>%
  #Turn tidy except for nutriscore letters and keyhole classification
  pivot_longer(
    cols = -c(sample_id, group, nutriscore_letter, keyhole_certified),
    names_to = 'feature',
    values_to = 'value'
  )

#Statistical analyses----
#Turn micronutrients into % of RDI for a woman 18-30 yo
various$RDI <- tibble(
  'feature' = c('Vitamin A', 'Vitamin D', 'Vitamin E',
                'Thiamin', 'Riboflavin', 'Niacin',
                'Vitamin B6', 'Folate', 'Vitamin B12',
                'Vitamin C', 'Calcium', 'Iron',
                'Sodium', 'Potassium', 'Magnesium',
                'Zinc', 'Selenium', 'Copper',
                'Phosphorus', 'Iodine'),
  'rdi' = c(700, 10, 8,
            1.1, 1.3, 15,
            1.2, 400, 2,
            75, 800, 15,
            2300, 3100, 280,
            7, 50, 0.9,
            600, 150)
)

#% of RDI for the various micronutrients that can be found in each recipe
various$with_RDI <- tidy_recipes %>%
  #Join the two df's together and calculate the % of rdi in each recipe
  inner_join(., various$RDI) %>%
  mutate(pct_rdi = round(value/rdi*100, 0)) %>%
  rename(raw_value = value,
         value = pct_rdi) %>%
  select(-c(rdi, raw_value))

#Energy percentage of macronutrients in each recipe
various$with_energy_pct_densityMJ <- data_recipes %>%
  #Energy percent for macros, density pr MJ for fibre
  calculateEnergypercentDensity() %>%
  mutate(value = case_when(
    feature != 'Dietary fibre' ~ energy_percent,
    feature == 'Dietary fibre' ~ densityMJ
  )) %>% select(-c(densityMJ, energy_percent))

#Dataframe with the variables to do stats analyses on
#Macros and micros in E% and % of RDI
run_stats <- bind_rows(various$with_RDI, various$with_energy_pct_densityMJ) %>%
  #Add kilocalories per 100g
  full_join(data_recipes %>% select(sample_id, group, Kilocalories) %>% pivot_longer(., cols = Kilocalories, names_to = 'feature', values_to = 'value')) %>%
  #Add health indicators
  full_join(health_indicators %>% select(-c(nutriscore_letter, keyhole_certified))) %>%
  #Add sustainability indicators
  full_join(tidy_recipes %>% filter(feature %in% c('CO2', 'Landuse')))

#Some descriptive stats----
getDescriptiveStats <- function(df, column){
  #Get descriptive stats of a named column in a df grouped by 'group'
  
  stats <- df %>%
    group_by(group) %>% 
    summarize(min = min(!!sym(column)),
              q1 = quantile(!!sym(column), 0.25),
              median = median(!!sym(column)),
              mean = mean(!!sym(column)),
              q3 = quantile(!!sym(column), 0.75),
              max = max(!!sym(column))) %>% ungroup()
  
}

descriptive_stats <- sapply(as.vector(run_stats$feature %>% unique()),
                            getDescriptiveStats,
                            df = run_stats %>% pivot_wider(names_from = 'feature', values_from = 'value'),
                            simplify = FALSE,USE.NAMES = TRUE) %>%
  bind_rows(., .id = 'feature')

#Descriptive stats of missng data
temp <- data_completeness$no_nutrient_info %>%
  group_by(group) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            q3 = quantile(value, 0.75),
            max = max(value)) %>% ungroup()

temp <- data_completeness$no_sustainability_indicators %>%
  group_by(group) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            q3 = quantile(value, 0.75),
            max = max(value)) %>% ungroup()

#Kruskal Wallis and dunn----
stats <- list(
  
  'kruskal_wallis' = run_stats %>%
    group_by(feature) %>%
    kruskal_test(., value ~ group),
  
  'kruskal_wallis_effectsize' = run_stats %>%
    group_by(feature) %>%
    kruskal_effsize(value ~ group),
  
  'dunn_test' = run_stats %>%
    group_by(feature) %>%
    dunn_test(value ~ group, detailed = TRUE)
  
)

#Correlation analysis----
#Functions
#Helper function tp get results from corr.test
collectCorr <- function(corr_test_results) {
  #Create a df with group, adjusted p.value and symbols for showing the significance of the p value from a corr.test result
  correlations <- tibble(
    'estimate' = round(corr_test_results$r[1,2], 3),
    'pvalue' = corr_test_results$p.adj,
  ) %>%
    mutate(pvalue_star = as.character(symnum(pvalue, corr = FALSE, na = FALSE, 
                                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                             symbols = c("***", "**", "*", "'", " "))))
}
#Fix the colors on the correlation scores in the upper segment, slight alteration of code from Isaac Zhao on statexchange
myCorrelations <- function(data,mapping,...){
  
  #Get the data to use for correlation analysis
  data2 = data
  data2$x = as.numeric(data[,as_label(mapping$x)])
  data2$y = as.numeric(data[,as_label(mapping$y)])
  data2$group = data[,as_label(mapping$colour)]
  
  #Split into separate datasets based on group and run corr.test each + all groups combined
  to_correlate <- data2 %>%
    select(group, x, y) %>%
    #Split into individual df's and remove group column as it's numeric
    split(f = as.factor(.$group)) %>%
    lapply(., select, ... = -group)
  #Add the df with all groups to the list  
  to_correlate$`Overall Corr` <- data2 %>%
    select(x,y)
  
  #Run correlation analysis on each list element, and turn into a single df before plotting
  correlations <- lapply(to_correlate, corr.test, method = 'spearman') %>%
    #Get the estimate, p.value and create a column with symbols that show stat.sig. og p value
    lapply(., collectCorr) %>%
    #Bind together
    bind_rows(., .id = 'group')
  
  ggplot(data = correlations, aes(x = 1, y = factor(group, levels = c('Overall Corr', 'US', 'Norway', 'UK')), color = group)) +
    geom_text(aes(label=paste0(group, ": ", estimate, pvalue_star)))
  
}
#One with altered textsize
myCorrelations_textsize <- function(data,mapping,...){
  
  #Get the data to use for correlation analysis
  data2 = data
  data2$x = as.numeric(data[,as_label(mapping$x)])
  data2$y = as.numeric(data[,as_label(mapping$y)])
  data2$group = data[,as_label(mapping$colour)]
  
  #Split into separate datasets based on group and run corr.test each + all groups combined
  to_correlate <- data2 %>%
    select(group, x, y) %>%
    #Split into individual df's and remove group column as it's numeric
    split(f = as.factor(.$group)) %>%
    lapply(., select, ... = -group)
  #Add the df with all groups to the list  
  to_correlate$`Overall Corr` <- data2 %>%
    select(x,y)
  
  #Run correlation analysis on each list element, and turn into a single df before plotting
  correlations <- lapply(to_correlate, corr.test, method = 'spearman') %>%
    #Get the estimate, p.value and create a column with symbols that show stat.sig. og p value
    lapply(., collectCorr) %>%
    #Bind together
    bind_rows(., .id = 'group')
  
  ggplot(data = correlations, aes(x = 1, y = factor(group, levels = c('Overall Corr', 'US', 'Norway', 'UK')), color = group)) +
    geom_text(aes(label=paste0(group, ": ", estimate, pvalue_star)), size = 3.5)
  
}

#Wide df with the variables
temp <- run_stats %>%
  #Make feature names more presentable
  mutate(feature = feature %>%
           str_replace('inverted_nutriscore', 'Inv. Nutriscore') %>%
           str_replace('inverted_traffic_score', 'Inv. Traffic Light') %>%
           str_replace('who_score', 'WHO Score') %>%
           str_replace('nnr_score', 'NNR Score')
  ) %>%
  pivot_wider(., names_from = 'feature', values_from = 'value')

#Health indicators with sustainability indicators
ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = 29:34,
        upper = list(continuous = myCorrelations),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

#Sustainability vs energy contributing macros
ggpairs(temp %>% select(-sample_id) %>%
          #Rename some column names
          rename(
            `Carbohydrates E%` = Carbo,
            `Sugar E%` = Sugar,
            `Protein E%` = Protein,
            `Saturated Fat E%` = SatFa,
            `Fat E%` = Fat,
            `Dietary fibre g/MJ` = `Dietary fibre`,
            `kcal/100g` = Kilocalories
          ),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = c(22:28, 33,34),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

#Sustainability vs mineral content
ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = c(2,3, 5:7, 9, 10, 12, 13, 21, 33,34),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

#Sustainability vs vitamin content
ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = c(4,8,11,14:20,33,34),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

#Principal Component Analysis----
#Create a feature_anno column to color features by
temp <- run_stats %>%
  mutate(feature_anno = case_when(
    feature %in% various$minerals ~ 'Minerals',
    feature %in% various$vitamins ~ 'Vitamins',
    feature %in% various$energy_contributing ~ 'Macronutrient',
    feature %in% various$sustainability ~ 'Sustainability',
    feature %in% various$health ~ 'Health score'
  ))
PCA_scores <- createPCA(temp)
PCA_loadings <- createPCA(temp, plots = 'loadings') +
  geom_label_repel(aes(label = feature))

#Plots----
#Violin boxplots----
#Data completeness
  #Nutrients
  plotViolinBox(data_completeness$no_nutrient_info) +
    scale_color_manual(values = various$country_colors$sample_group) +
    labs(
      color = 'Country',
      x = 'Country',
      y = 'Percentage of recipe in weight with no nutrient information'
    )
  #Sustainability indicators
  plotViolinBox(data_completeness$no_sustainability_indicators) +
    scale_color_manual(values = various$country_colors$sample_group) +
    labs(
      color = 'Country',
      x = 'Country',
      y = 'Percentage of recipe in weight with no sustainability indicators'
    )

#Sustainability
plotViolinBox(run_stats %>%
                filter(feature %in% various$sustainability) %>%
                mutate(feature = feature %>%
                         str_replace('CO2', 'Kilo CO2 equivalents\nper 100g') %>%
                         str_replace('Landuse', 'm2 per year\nper 100g'))) +
  facet_wrap(~feature, scale = 'free', nrow = 2) +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Value'
  )

#Per foodgroup
temp <- tidy_ingredients %>%
  filter(feature %in% c('CO2', 'Landuse')) %>%
  group_by(sample_id, group, feature, Foodgroup) %>%
  summarise(value = sum(value, na.rm = TRUE))
#CO2
plotViolinBox(temp %>% filter(feature == 'CO2')) +
  facet_wrap(~Foodgroup, scale = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Kilo CO2 equivalents'
  )
#Landuse
plotViolinBox(temp %>% filter(feature == 'Landuse')) +
  facet_wrap(~Foodgroup, scale = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'm2 per year'
  )


#Health scores
plotViolinBox(health_indicators %>%
                filter(feature %in% various$health) %>%
                mutate(feature = feature %>%
                         str_replace('inverted_nutriscore', 'Inverted Nutriscore') %>%
                         str_replace('nnr_score', 'Nordic Nutritional\nRecommendations') %>%
                         str_replace('who_score', 'World Health Organization\nRecommendations') %>%
                         str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model'))) + facet_wrap(~feature, scales = 'free') +
  labs(
    color = 'Country',
    x = 'Country'
  )
#Minerals
plotViolinBox(run_stats %>%
                filter(feature %in% various$minerals)) + facet_wrap(~feature, scales = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Percentage of RDI'
  ) + theme(legend.position="bottom")
#Vitamins
plotViolinBox(run_stats %>%
                filter(feature %in% various$vitamins)) +
  facet_wrap(~feature, scales = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Percentage of RDI'
  ) + theme(legend.position="bottom")
#Energy
plotViolinBox(run_stats %>%
                filter(feature %in% various$energy_contributing) %>%
                mutate(feature = feature %>%
                         str_replace('Carbo', 'Carbohydrates E%') %>%
                         str_replace('Sugar', 'Sugar E%') %>%
                         str_replace('Fat', 'Fat E%') %>%
                         str_replace('SatFa', 'Saturated Fat E%') %>%
                         str_replace('Protein', 'Protein E%') %>%
                         str_replace('Dietary fibre', 'Dietary fibre g/MJ') %>%
                         str_replace('Kilocalories', 'Kilocalories/100g')
                )) +
  facet_wrap(~factor(feature, levels = c('Kilocalories/100g', 'Dietary fibre g/MJ', 'Protein E%',
                                         'Carbohydrates E%', 'Sugar E%', 'Fat E%', 'Saturated Fat E%')) ,scale = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Value'
  )

#Percentage of different foodgroups in the different countries recipes
temp <- tidy_ingredients %>% group_by(group, sample_id, Foodgroup) %>%
  summarise(value = sum(Amounts_kg, na.rm = TRUE))

plotViolinBox(temp) + facet_wrap(~Foodgroup) +
  labs(
    y = 'Percentage of recipe weight',
    x = 'Country'
  )


#Tables----
#Stats----
#Significantly differences from dunn test
temp <- stats$dunn_test %>% filter(p.adj <0.05)

#Format the relevant data
stat_table <- list(
  
  'descriptive_stats' = descriptive_stats %>% filter(feature %in% temp$feature) %>%
    #Create one row for each feature, with the median + interquartile range for each country
    mutate(median_iqr = paste0(round(median, 1), ' (', round(q1, 1), ', ', round(q3, 1), ')')) %>%
    select(feature, group, median_iqr) %>%
    pivot_wider(.,
                names_from = group,
                values_from = median_iqr),
  
  #Get the chi square statistic and p value for each feature
  'kruskal_wallis' = stats$kruskal_wallis %>% filter(feature %in% temp$feature) %>%
    select(feature, statistic, p) %>%
    #Turn p value to scientific notation in text and round chisquare stat
    mutate(p = formatC(p, format = "e", digits = 2),
           statistic = round(statistic, 1)) %>%
    rename(`Chi square` = statistic,
           `p-value` = p),
  
  #Get the pairwise comparison and adjusted p value for each feature
  'dunn' = stats$dunn_test %>% filter(p.adj <0.05) %>%
    mutate(Pairwise = paste0(group1, ' - ', group2)) %>%
    select(feature, Pairwise, p.adj) %>%
    #Turn p value to scientific notation in text
    mutate(p.adj = formatC(p.adj, format = "e", digits = 2)) %>%
    rename(`Adj. p-value` = p.adj) %>%
    #One row for each feature
    group_by(feature) %>%
    summarise(Pairwise = paste0(Pairwise, collapse = '<br><br>'),
              `Adj. p-value` = paste0(`Adj. p-value`, collapse = '<br><br>')) %>% ungroup()
  
) %>%
  
  #Finished table
  reduce(full_join, by = 'feature') %>%
  #Clean up some names
  rename(Feature = feature) %>%
  mutate(Feature = Feature %>%
           str_replace('inverted_nutriscore', 'Inv. Nutriscore') %>%
           str_replace('inverted_traffic_score', 'Inv. Traffic Light') %>%
           str_replace('who_score', 'WHO Score') %>%
           str_replace('nnr_score', 'NNR Score') %>%
           str_replace('Carbo', 'Carbohydrates E%') %>%
           str_replace('Sugar', 'Sugar E%') %>%
           str_replace('Fat', 'Fat E%') %>%
           str_replace('SatFa', 'Saturated Fat E%') %>%
           str_replace('Protein', 'Protein E%') %>%
           str_replace('Dietary fibre', 'Dietary fibre g/MJ') %>%
           str_replace('Kilocalories', 'Kilocalories/100g'))

#KableExtra formatting
formatKableStat <- function(df, features = NULL, caption = NULL){
  
  #Filter out features of interest or show all?
  if(!is.null(features)){
    dt <- df %>% filter(Feature %in% features)
  } else {
    dt <- df
  }
  
  #Add a caption or not
  if(!is.null(caption)){
    kbl(dt, 'html', escape = FALSE) %>%
      kable_classic_2() %>%
      kable_styling(full_width = FALSE) %>%
      add_header_above(c(" " = 1, "Median (IQR)" = 3, "Kruskal-Wallis test" = 2, "Dunn test, BH corrected" = 2))
  } else {
    kbl(dt, 'html', caption = caption, escape = FALSE) %>%
      kable_classic_2() %>%
      kable_styling(full_width = FALSE) %>%
      add_header_above(c(" " = 1, "Median (IQR)" = 3, "Kruskal-Wallis test" = 2, "Dunn test, BH corrected" = 2))
  }
  
}

#All
formatKableStat(stat_table)
#Sustainability indicators
formatKableStat(stat_table, c('CO2', 'Landuse'))
#Health indicators
formatKableStat(stat_table, c('Inv. Nutriscore', 'Inv. Traffic Light', 'WHO Score', 'NNR Score'))
#Energy
formatKableStat(stat_table, c('Carbohydrates E%', 'Sugar E%', 'Fat E%', 'Saturated Fat E%', 'Dietary fibre g/MJ', 'Protein E%'))
#Minerals
formatKableStat(stat_table, various$minerals)
#Vitamins
formatKableStat(stat_table, various$vitamins)

#Others----
standardKbl <- function(df, caption = NULL){
  #df = dataframe to turn into kableExtra table, caption = caption for the table
  
  if(is.null(caption)){
    
    table <- df %>%
      kbl(escape = FALSE) %>%
      kable_classic_2(full_width = FALSE)
    
  } else {
    table <- df %>%
      kbl(caption = caption, escape = FALSE) %>%
      kable_classic_2(full_width = FALSE)
  }
  
  table 
  
}

#Foodgroups in SHARP
temp <- tidy_ingredients %>%
  select(Foodgroup) %>% unique() %>% arrange(., Foodgroup)

standardKbl(temp)

#Guidelines, nutriscore and trafficlights
guidelines_trafficlights <- read_csv2('./health_indicators/guidelines_trafficlights.csv') %>%
  rename(Feature = X1) %>%
  replace(is.na(.), ' ')

#Guidelines
kbl(temp %>% select(Feature, `World Health Organization`, `Nordic Nutritional Recommendation`), escape = FALSE) %>%
  kable_classic_2(full_width = FALSE) %>%
  add_header_above(c(' ' = 1, 'Dietary guidelines' = 2))
#Multiple traffic lights
kbl(temp %>%
      select(Feature, `Green/low`, `Amber/medium`, `Red/high`) %>%
      filter(!Feature %in% c('Carbohydrate', 'Dietary fibre', 'Protein')), escape = FALSE) %>%
  kable_classic_2(full_width = FALSE) %>%
  add_header_above(c(' ' = 1, 'Multiple traffic light model' = 3))
#Nutriscore
nutriscore_points <- read_csv2('./health_indicators/nutriscore_points.csv') %>%
  select(-Points_1) %>%
  replace(is.na(.), ' ')

kbl(temp, escape = TRUE) %>%
  kable_classic_2(full_width = FALSE) %>%
  add_header_above(c(' ' = 1, 'Disqualifying' = 4, 'Qualifying' = 3))

#RDIs
standardKbl(various$RDI %>%
              #Rename columns
              rename(Feature = feature,
                     RDI = rdi) %>%
              #Add units
              mutate(Unit = case_when(
                Feature %in% c('Calcium', 'Iron', 'Zinc', 'Magnesium',
                               'Phosphorus', 'Copper', 'Potassium', 'Thiamin',
                               'Riboflavin', 'Vitamin B6', 'Vitamin C', 'Sodium') ~ 'mg',
                Feature %in% c('Iodine', 'Selenium', 'Vitamin D', 'Folate',
                               'Vitamin B12') ~ 'mcg',
                Feature == 'Vitamin A' ~ 'RAE',
                Feature == 'Vitamin E' ~ 'mcg TE',
                Feature == 'Niacin' ~ 'NE'
              )))

group_colors <- various$country_colors$sample_group
RDI <- various$RDI
minerals <- various$minerals
energy_contributing <- various$energy_contributing
vitamins <- various$vitamins
sustainability <- various$sustainability
health <- various$health

#Save objects to be used in RMarkdown
save(data_ingredients, data_ingredients, tidy_recipes,
     tidy_ingredients, run_stats, stat_table, health_indicators,
     RDI, group_colors, minerals, vitamins, sustainability,
     health, energy_contributing, guidelines_trafficlights,
     nutriscore_points, data_completeness, file = 'results.RData')



