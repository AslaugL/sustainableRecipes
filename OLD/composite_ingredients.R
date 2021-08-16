library(tidyverse)
library(stringi)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data/oppskrifter')

#Composite ingredient ingredients
Name = c('condensed cream of mushroom soup', 'condensed cream of chicken soup',
         'condensed cream of celery soup', 'refrigerated buttermilk biscuit dough',
         'fish cakes coarse', 'worcestershire sauce', 'fish sauce', 'taco sauce',
         'oyster sauce', 'hot pepper sauce', 'hoisin sauce', 'pesto', 'pizza sauce',
         'chunky salsa', 'mango chutney', 'guacamole', 'cranberry sauce', 'tomato sauce',
         'potato flatbread', 'duck sauce', 'shrimp paste', 'chili sauce sweet',
         'chili sauce', 'shrimp salad')
Ingredients = c(
#Concentrated cream of soups
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
 
#Refrigerated buttermilk biscuits               
#From https://www.finecooking.com/recipe/buttermilk-biscuit-dough
                '8 ounce wheat flour
2 tsp baking powder
1 tsp granulated sugar
0.5 tsp baking soda
0.5 tsp table salt
6 tbsp cold unsalted butter
0.75 cup milk',
  
#Fish cakes              
#From https://www.matprat.no/oppskrifter/familien/grove-fiskekaker-med-karristuede-rotgronnsaker/
                '600 g haddock
1 tsp salt
1.5 tbsp potato starch
1 stk egg
1 dl milk
0.25 tsp pepper
2 tbsp leek
2 tbsp butter',

#Worcestershire sauce
#From https://practicalselfreliance.com/homemade-worcestershire-sauce/
'0.25 cup raisins
0.25 cup boiling water
0.5 cup molasses
0.25 cup tamarind paste
2 ounce can anchovies
1 stk onion, coarsely chopped
2.5 cm of fresh ginger
6 clove garlic, crushed
2 cup white vinegar, divided
2 cardamom pods
2 tbsp salt
2 tbsp brown sugar
1 tbsp red pepper flakes
1 tbsp dry mustard
1 tsp whole cloves
1 tsp black pepper whole
0.5 tsp ground cinnamon',

#Fish sauce
#From https://nourishingjoy.com/homemade-fish-sauce/
'6 clove garlic
3 tbsp  salt
6 bay leaf
2 tsp whole black pepper whole
1.5 pound small herring
1.5 cup water, as needed
1 tsp salt',

#Taco sauce
#From https://www.kidspot.com.au/kitchen/recipes/homemade-taco-sauce/2onnimeh
'10 ml olive oil
1 stk onion, finely chopped
1 stk sweet pepper, finely chopped
1 clove garlic, minced
1 pinch salt
400 g diced tomatoes
125 ml Vegetable broth
20 ml white vinegar
2 tsp cumin
2 tsp smokey paprika powder
1 tsp sugar',

#Oyster sauce
#From https://www.wikihow.com/Make-Oyster-Sauce
'0.5 pound shucked oysters with liquid
1 tbsp water
1 tsp salt
2 tbsp light soy sauce
0.5 tbsp dark soy sauce',

#Hot pepper sauce
#From https://www.chilipeppermadness.com/recipes/cayenne-pepper-sauce/
'10 ounce chili peppers
5 clove garlic
0.5 cup white wine vinegar
1 tsp salt',

#Hoisin sauce
#From https://www.bbcgoodfood.com/recipes/hoisin-sauce
'4 tbsp soy sauce
2 tbsp smooth peanut butter
1 tbsp dark brown sugar
2 tsp rice wine vinegar
1 clove garlic, finely crushed
2 tsp sesame seed oil
hot sauce, to taste
0.125 tsp black pepper',

#Pesto
#From https://www.simplyrecipes.com/recipes/fresh_basil_pesto/
#Exchanging half of the basil for baby spinach as per options to have greens found in SHARP-ID
'1 cup fresh basil leaf
1 cup spinach
2 ounce Parmesan cheese
0.5 cup extra virgin olive oil
0.33 cup pine nuts
3 clove garlic
0.25 tsp salt
0.125 tsp black pepper',

#Pizza sauce
#From https://www.bbcgoodfood.com/recipes/pizza-sauce
'2 tbsp olive oil
1 stk onion, finely chopped
1 clove garlic crushed
800 g canned chopped tomatoes
3 tbsp tomato purée
1 bay leaf
2 tbsp dried oregano
2 tsp brown sugar
1 bunch basil, finely chopped',

#Chunky salsa
#From https://www.tine.no/oppskrifter/lunsj-og-smaretter/salater/hjemmelaget-salsa
'4 stk tomatoes
1 clove garlic
2 stk scallion
1 stk red chili
0.5 stk red onion
1 stk lime
0.5 tsp salt
2 tbsp olive oil
1 bunch fresh coriander',

#Mango chutney
#From https://www.matprat.no/oppskrifter/gjester/mangochutney/
'1 stk mango
1 clove garlic
1 tsp ginger
1 stk red chili
1 tbsp vinegar
1 tbsp sugar
0.5 tsp salt',

#Guacamole
#From https://www.matprat.no/oppskrifter/kos/guacamole1/
'2 stk avocado
1.5 tbsp lime juice
1.5 stk tomato
1 clove garlic
0.5 stk red chili
2 tbsp coriander
0.5 tsp salt
0.25 tsp pepper',

#Cranberry sauce
#From https://www.rhubarbarians.com/how-to-make-jellied-cranberry-sauce/
'12 ounce fresh cranberry
1 cup sugar
1 cup water',

#Tomato sauce
#From https://www.simplyrecipes.com/recipes/basic_tomato_sauce/
'2 tbsp extra virgin olive oil
0.5 stk onion, finely chopped
1 stk carrot 
1 stalk celery
2 tbsp parsley
1 clove garlic
0.5 tsp dried basil
1.75 pound of tomato
1 tsp tomato paste
Salt and freshly ground black pepper to taste',

#Potato flatbread
#From https://thegardeningfoodie.com/2-ingredient-potato-flatbread/
'120 g wheat flour
220 g potato',

#Duck sauce
#From https://www.thespruceeats.com/chinese-duck-sauce-plum-recipe-1806745
'1 pound plum
1 pound apricot 
1 cup vinegar
0.75 cup water
0.25 cup balsamic vinegar
1 cup cider vinegar
1 cup brown sugar
1 cup white granulated sugar
0.5 cup  lemon juice
0.25 cup peeled and chopped ginger
1 stk onion (sliced thin)
1 chili
2 clove garlic
4 tsp salt
1 tbsp toasted mustard seeds
1 stk cinnamon stick',

#Shrimp paste
#From https://www.saveur.com/article/Recipes/Shrimp-Paste/
'0.5 pound unsalted butter
1 pound shrimp
0.5 tsp salt
0.5 tsp ground black pepper
0.25 cup sherry
2 tbsp lemon juice
0.25 tsp cayenne pepper',

#Chili sauce sweet
#From
'3 stk red chili
1 tsp salt
2 clove garlic
1 tbsp vegetable oil
3 dl pineapple juice
0.5 dl vinegar
3 dl sugar
4 tbsp chili flakes
2 tbsp corn starch
4 tbsp water',

#Chili sauce
#From https://www.tine.no/oppskrifter/sauser-og-dressinger/kalde-sauser-og-dressinger/verdens-sterkeste-chilisaus
'450 g strong chili
4 clove garlic
0.5 dl basil
2.5 dl vinegar
1 tsp salt',

#Shrimp salad
#From https://mills.no/oppskrift/mills/rekesalat/
'0.5 dl mayonnaise sauce
0.5 dl sour cream
1 tbsp dill
0.5 tsp lemon juice
0.25 tsp pepper
500 g shrimp')

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
reference <- readRDS('../porsjoner_vekt_næringsinnhold/food_weight_ref.Rds')

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
  
  results <- lapply(df$Ingredients, checkRef, reference = reference)
  
}

#Get ID
temp <- checkRefList(composite_ingredients) %>% bind_rows() %>% full_join(composite_ingredients) %>% unique()

#Fix errors
temp <- temp %>%
  mutate(
  ref = case_when(
    Ingredients == 'red pepper flakes' ~ 'chili flake',
    Ingredients == 'rice wine vinegar' ~ 'vinegar',
    Ingredients == 'hot sauce, to taste' ~ 'hot sauce',
    Ingredients == 'lime juice' ~ 'lime juice',
    TRUE ~ ref),
  ID = case_when(
    Ingredients == 'red pepper flakes' ~ reference %>% filter(first_word == 'chili' & second_word == 'flake') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'vinegar') ~ reference %>% filter(first_word == 'vinegar' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'spinach') ~ reference %>% filter(first_word == 'spinach' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'stock|broth|hot sauce') & !str_detect(Ingredients, 'cube|dice') ~ 0,
    TRUE ~ ID)) 

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
  rename(Ingredients = Ingredients.y) %>% select(-ref) %>%
  
  #Change some names to fit the SHARP database
  mutate(Ingredients = Ingredients %>%
          str_replace('can anchovies', 'anchovy canned') %>%
          str_replace('cranberry', 'cranberries') %>%
           str_replace('sherry', 'fortified and liqueur wines'))

#Run through SHARP indicators to calculate CO2 and landuse
reference <- readRDS('../sharp_ref.Rds')
SHARP <- readRDS('../sharp_db.Rds')

various$with_Sharp_ref <- checkRefList(various$ingredients_weight) %>% bind_rows() %>% unique()

#Fix errors
various$with_Sharp_ref <- various$with_Sharp_ref %>%
  mutate(
    ref = case_when(
      Ingredients == 'red pepper flakes' ~ 'chili flake',
      Ingredients == 'rice wine vinegar' ~ 'vinegar',
      Ingredients == 'hot sauce, to taste' ~ 'hot sauce',
      Ingredients == 'vegetable broth' ~ 'vegetable broth',
      Ingredients == 'bunch basil, finely chopped' ~ 'basil',
      TRUE ~ ref),
    ID = case_when(
      #Fix the double vinegar entry in SHARP
      str_detect(Ingredients, 'vinegar') ~ reference %>% filter(first_word == 'vinegar' & second_word == 'nothing') %>% select(ID) %>% filter(ID == min(ID)) %>% as.numeric(.),
      str_detect(Ingredients, 'stock|broth|red pepper flakes|basil') & !str_detect(Ingredients, 'cube|dice') ~ 0,
      TRUE ~ ID)) 
  

#Final df with co2/landuse pr kg
final <- full_join(various$with_Sharp_ref, various$ingredients_weight) %>% left_join(., SHARP, by ='ID') %>%
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
  rename(Ingredients = Name) %>%
  
  #Add foodgroup/L1 from sharp
  mutate(L1 = case_when(
    str_detect(Ingredients, 'salsa|soup|sauce|chutney|guacamole|pesto|paste') ~ 'Seasoning, sauces and condiments',
    str_detect(Ingredients, 'fish') ~ 'Fish, seafood, amphibians, reptiles and invertebrates',
    str_detect(Ingredients, 'dough|bread') ~ 'Grains and grain-based products',
    Ingredients == 'shrimp salad' ~ 'Composite dishes'
  ))

#Save
saveRDS(final, 'composite_ingredients.Rds')

#Recheck the oyster sauce recipe
