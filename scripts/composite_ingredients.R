library(tidyverse)
library(stringi)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data/oppskrifter')

#Composite ingredient ingredients
Name = c('condensed cream of mushroom soup', 'condensed cream of chicken soup',
         'condensed cream of celery soup', 'refrigerated buttermilk biscuit dough',
         'fish cakes coarse')
Ingredients = c(
#From https://onceamonthmeals.com/blog/recipe-roundups/homemade-cream-of-something-soup/
'2 clove garlic
0.33 cup onion, diced
0.5 cup mushroom
0.25 cup butter
0.25 cup wheat flour
1 cup milk
0.75 cup broth',
                '2 clove garlic
0.33 cup chicken, diced
0.5 cup mushroom
0.25 cup butter
0.25 cup wheat flour
1 cup milk
0.75 cup broth',
                '2 clove garlic
0.33 cup onion, diced
0.5 cup celery stalk
0.25 cup butter
0.25 cup wheat flour
1 cup milk
0.75 cup broth',
                
#From https://www.finecooking.com/recipe/buttermilk-biscuit-dough
                '8 ounce wheat flour
2 tsp baking powder
1 tsp granulated sugar
0.5 tsp baking soda
0.5 tsp table salt
6 tbsp cold unsalted butter
0.75 cup milk',
                
#From https://www.matprat.no/oppskrifter/familien/grove-fiskekaker-med-karristuede-rotgronnsaker/
                '600 g haddock
1 tsp salt
1.5 tbsp potato starch
1 stk egg
1 dl milk
0.25 tsp pepper
2 tbsp leek
2 tbsp butter')

composite_ingredients <- tibble(Name = Name, Ingredients = Ingredients)

#Split amounts and ingredient name into separate columns, turn amounts into grams
#Various objects to keep environment clean
various <- list(
  
  #Units to keep in the recipes
  #Recipes from Kolonialen has translated 'pk/pakke/stykk' to 'hp'
  #What is hp?
  'units' = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf',
              'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice',
              'tassel', 'neve', 'ml', 'bunch', 'pack', 'plate', 'pot', 'drop', 'clove',
              'pound', 'ounce', 'sprig', 'clove') %>%
    #Add whitespace on both sides to only match a unit in a string
    sapply(., function(x) {paste0('\\s', x, '\\s')}))

composite_ingredients <- composite_ingredients %>%
  #Separate ingredients into separate rows
  separate_rows(., Ingredients, sep = '\n') %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Extract the amounts to their own column----
  mutate(Amounts = case_when(
  #Extract amounts with units
  str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', various$units, collapse = '|')) ~
    str_extract(Ingredients, '((\\d+\\.\\d+|\\d+-\\d+|\\d+)\\s\\w+)'),
  #Extract pure numbers
  !str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', various$units, collapse = '|')) &
    str_detect(Ingredients, '^\\d+') ~ str_extract(Ingredients, '^[^\\s]+')),
  
  #Remove this information from Ingredients columns
  Ingredients = case_when(
    str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', various$units, collapse = '|')) ~
      str_remove(Ingredients, '((\\d+\\.\\d+|\\d+-\\d+|\\d+)\\s\\w+)'),
    !str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', various$units, collapse = '|')) & str_detect(Ingredients, '^\\d+') ~
      str_remove_all(Ingredients, '^[^\\s]+'),
    TRUE ~ Ingredients)
) %>%
  
  #Remove whitespace
  mutate(Ingredients = str_trim(Ingredients)) %>%
  
  #Turn volume units to dl or grams----
  #(use 2.45dl for one cup, as 1 cup is 2.5dl in Norway and 2.4 in US), and turn into grams for water and other liquids with 100g/dl
  #Split Amounts into amounts and units
  separate(., Amounts, c('Amounts', 'unit_enhet'), sep = ' ') %>%
  #Turn amounts into numeric
  mutate_at('Amounts', ~as.numeric(.)) %>%
  
  #Turn volume units to dl and ounce/pound to g
  mutate(Amounts = case_when(
    unit_enhet == 'cup' ~ Amounts * 2.45,
    unit_enhet == 'l' ~ Amounts * 10,
    unit_enhet == 'ml' ~ Amounts / 10,
    unit_enhet == 'tbsp' ~ Amounts / 6.67,
    unit_enhet == 'tsp' ~ Amounts / 20,
    unit_enhet == 'krm' ~ Amounts / 100,
    unit_enhet == 'drop' ~ Amounts / 2000, #One drop is 0.05ml 
    unit_enhet == 'pinch' ~ Amounts / (20*16), #A pinch is usually defined as 1/16 of a tsp
    unit_enhet == 'ounce' ~ Amounts * 28.35,
    unit_enhet == 'pound' ~ Amounts * 453.59,
    TRUE ~ Amounts
  )) %>%
  mutate(unit_enhet = case_when(
    unit_enhet %in% c('cup', 'l', 'ml', 'tsp', 'tbsp', 'krm', 'drop', 'pinch') ~ 'dl',
    unit_enhet %in% c('ounce', 'pound') ~ 'g',
    TRUE ~ unit_enhet
  )) %>%
  
  #Turn juice, water, vinegar and other liquids with similar density to water from dl/l to grams as they are all about 100g/dl
  mutate(
    Amounts = case_when(
      (str_detect(Ingredients, 'water|beer|madeira|marsala|cognac|cider|juice|Juice|broth|kraft|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
         unit_enhet == 'dl') &
        !str_detect(Ingredients, 'sugar|cheese|sour|flour') ~ Amounts * 100,
      TRUE ~ Amounts),
    unit_enhet = case_when(
      (str_detect(Ingredients, 'water|beer|madeira|marsala|cognac|cider|juice|Juice|broth|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
         unit_enhet == 'dl') &
        !str_detect(Ingredients, 'sugar|cheese|sour|flour') ~ 'g',
      TRUE ~ unit_enhet)) %>%
  
  #Turn grams into kilos
  mutate(
    Amounts = case_when(
      unit_enhet == 'g' ~ Amounts/1000,
      TRUE ~ Amounts),
    unit_enhet = unit_enhet %>%
      str_replace('\\bg\\b', 'kg'))

#Run through volume to weight database
#Reference volume to weight comparisons for different foods
ref <- readRDS('../porsjoner_vekt_næringsinnhold/food_weight_ref.Rds')

#Helper functions
checkRef <- function(ingredient, reference){
  #Reference is a list of names from a food database
  
  #Fill tibble with info
  results <- tibble(
    Ingredients = character(),
    ref = character(),
    ID = numeric(),
    loop = character()
  )
  
  
  #Look for both search terms in ref in Ingredient
  for(i in 1:nrow(reference)){
    
    
    #Only look for the whole word found in the reference
    if(str_detect(ingredient, regex(paste0('\\b', reference$second_word[i], '\\b|\\b', reference$second_word[i], '\\w+'), ignore_case = TRUE)) &
       str_detect(ingredient, regex(paste0('\\b', reference$first_word[i], '\\b|\\b', reference$first_word[i], '\\w+'), ignore_case = TRUE)) ){
      
      print('first loop')
      results <- results %>%
        add_row(Ingredients = ingredient,
                ref = paste0(reference$first_word[i], ', ', reference$second_word[i]),
                ID = as.numeric(reference$ID[i]),
                loop = 'first loop')
      
      #Break after first hit
      break
    }
  }
  
  #Look for foods not identified by both first and second word i ref
  if(!ingredient %in% results$Ingredients){
    
    for(i in 1:nrow(reference)){
      
      if (str_detect(ingredient, regex(paste0('\\b', reference$first_word[i], '\\b'), ignore_case = TRUE))  &
          isFALSE(str_detect(ingredient, regex(paste0('\\b', reference$second_word[i], '\\b|\\b', reference$second_word[i], '\\w+'), ignore_case = TRUE)) )){
        
        print('second loop')
        results <- results %>%
          add_row(Ingredients = ingredient,
                  ref = reference$first_word[i],
                  ID = as.numeric(reference$ID[i]),
                  loop = 'second loop')
        
        #Break after first hit
        break
        
      } else if (str_detect(ingredient, regex(paste0('\\b', reference$first_word[i], '\\w+'), ignore_case = TRUE)) &
                 reference$second_word[i] == 'nothing') {
        
        print('third loop')
        results <- results %>%
          add_row(Ingredients = ingredient,
                  ref = reference$first_word[i],
                  ID = as.numeric(reference$ID[i]),
                  loop = 'third loop')
        
        break
      }
      
    }
    
    
  }
  
  results
  
}
checkRefList <- function(df){
  
  results <- lapply(df$Ingredients, checkRef, reference = ref)
  
}

#Get ID
temp <- checkRefList(composite_ingredients) %>% bind_rows() %>% full_join(composite_ingredients) %>% unique()

#Calculate the weight of each ingredient
various$weights <- readRDS('../porsjoner_vekt_næringsinnhold/all_weights.Rds')
#Ingredients to turn from tbsp to dl
various$to_dl <- c('pepper', 'peanøttsmør', 'olje', 'oil', 'smør', 'butter', 'margarin',
                   'ghee', 'Garlic')

weights <- various$weights %>%
  #Set netto as default value pr stk, as SHARP takes edible portion and cooking losses into account when calculating environmental impact
  mutate(
    g = case_when(
      str_detect(Ingredients, regex(paste0(various$to_dl, collapse ='|'), ignore_case = TRUE)) &
        unit_enhet == 'tbsp' ~ g * 6.67,
      TRUE ~ g),
    unit_enhet = case_when(
      unit_enhet == 'netto' ~ 'stk',
      str_detect(Ingredients, regex(paste0(various$to_dl, collapse ='|'), ignore_case = TRUE)) &
        unit_enhet == 'tbsp' ~ 'dl',
      unit_enhet %in% c('cm rot', 'cm of root') ~ 'cm',
      TRUE ~ unit_enhet
    )) %>%
  #Only keep those necessary
  filter(unit_enhet %in% composite_ingredients$unit_enhet)

various$ingredients_weight <- right_join(weights, temp, by = c('ID', 'unit_enhet')) %>% #Doing this gives final three more rows than temp...?
  
  #Turn weights into kilo
  mutate(Amounts_kg = case_when(
    unit_enhet == 'kg' ~ Amounts,
    !unit_enhet == 'kg' ~ Amounts*g/1000
  )) %>%
  
  #Cleanup
  select(Name, Ingredients.y, Amounts_kg, ref, Amounts, unit_enhet) %>%
  unique() %>% #Got some values twice as inner_join got both norwegian and english translated names
  rename(Ingredients = Ingredients.y) %>% select(-ref)

#Run through SHARP indicators to calculate CO2 and landuse
ref <- readRDS('../sharp_ref.Rds')
SHARP <- readRDS('../sharp_db.Rds')

various$with_Sharp_ref <- checkRefList(various$ingredients_weight) %>% bind_rows() %>% unique()

#Final df with co2/landuse pr kg
final <- inner_join(various$with_Sharp_ref, various$ingredients_weight) %>% inner_join(., SHARP, by ='ID') %>%
  select(Name, Ingredients.x, Amounts_kg, `GHGE of 1 kg food as consumed_kgCO2eq`, `Land use of 1 kg food as consumed_m2/yr`) %>%
  
  #CO2/Landuse by each ingredient in the composite ingredient
  mutate(CO2 = `GHGE of 1 kg food as consumed_kgCO2eq`*Amounts_kg,
         Landuse = `Land use of 1 kg food as consumed_m2/yr`*Amounts_kg) %>%
  select(-c(`GHGE of 1 kg food as consumed_kgCO2eq`, `Land use of 1 kg food as consumed_m2/yr`)) %>%
  
  #Sum CO2/landuse for whole composite ingredient
  group_by(Name) %>%
  summarise(weight = sum(Amounts_kg, na.rm = TRUE),
          CO2 = sum(CO2, na.rm = TRUE),
          Landuse = sum(Landuse, na.rm = TRUE)) %>%
  ungroup() %>%
  
  #Values pr 1kg
  mutate(`GHGE of 1 kg food as consumed_kgCO2eq` = CO2/weight,
         `Land use of 1 kg food as consumed_m2/yr` = Landuse/weight) %>%
  select(-c(Landuse, CO2, weight)) %>%
  
  #Rename columns
  rename(Ingredients = Name)

#Save
saveRDS(final, 'composite_ingredients.Rds')
