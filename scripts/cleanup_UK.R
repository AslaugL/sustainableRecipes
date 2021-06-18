library(tidyverse)
library(readxl)
library(stringi)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data/oppskrifter')

#read raw data
raw <- read_xlsx('Data_UK_100.xlsx')

#Various objects to keep environment clean
various <- list(
  
  #Units to keep in the recipes
  #Recipes from Kolonialen has translated 'pk/pakke/stykk' to 'hp'
  #What is hp?
  'units' = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf',
              'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice',
              'tassel', 'neve', 'ml', 'bunch', 'pack', 'plate', 'pot', 'drop', 'clove',
              'pound', 'ounce', 'sprig') %>%
    #Add whitespace on both sides to only match a unit in a string
    sapply(., function(x) {paste0('\\s', x, '\\s')}),
  
  #Ingredients that are countes as individual pieces/stk
  'stk' = c('anchovy', 'anise', 'apple', 'apricot', 'avocado',
            
            'banana', 'brea', 'baguette', 'bok choi', 'broccoli', 
            
            'cabbage', 'cardamom', 'carrot', 'cauliflower', 'Celariac root', 'celery', 'celery stalk',
            'champignons', 'chicken', 'chicory', 'chili', 'cinnamon', 'clementine', '\\bcloves\\b', 'cod', 
            'cod fillet', 'crab', 'cream cracker', 'cucumber',
            
            'duck',
            
            'egg', 'entrecôtekam',
            
            'fennel', 'fig',
            
            'grapes', 
            
            'jordskokk', 
            
            'lamb chop', 'leaf', 'leek', 'lemon', 'lemongrass', 'lettuce', 'lime', 
            
            'mango', 'mushroom',
            
            'nut',
            
            'olive', 'onion', 'orange',
            
            'paprika', 'pear', 'pepper', 'peppercorns', 'pineapple', 'pomegranate', 'plate', 'pork', 
            'potato', 'prawn', 'prune', 
            
            'radish', 'roll',
            
            'salad', 'salmon', 'scallion', 'scallop', 'scampi', 'shallot', 'sheep', 'sheet', 'shrimp',
            'smoked herring', 'squid', 'stock cube',
            
            'tenderloin', 'tomato', 'tortilla', 'trout', 'turkey'),
  'bouquet_garni' = c('1 bay leaf/2 sprigs of thyme/2 sprig parsley stems')) # From Hugh's River Cottage

#Clean up the UK data
UK <- raw %>%
  #Columns needed
  select(`Selected Meals`, Ingredients, `No. Of portion`, Source) %>%
  drop_na(Ingredients) %>%
  
  #Separate ingredients into separate rows
  separate_rows(., Ingredients, sep = '\n') %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Remove some unnecessary characters in the ingredients column, and make sure there is a space between numbers and words ('2 dl', not '2dl)
  mutate(Ingredients = Ingredients %>%
           
           str_replace_all('["()]|possibly', '') %>%
           str_replace('1,000|1 000', '1000') %>%
           str_replace('(?<=\\d)(?=[^\\d\\s\\.-])', ' ') %>%
           str_replace('(?<=\\d) ½', '.5') %>%
           str_replace('(?<=\\d) ⅘', '.8') %>%
           str_replace('(?<=\\d) ⅕', '.2') %>%
           str_replace('(?<=\\d) ⅓', '.33') %>%
           str_replace('(?<=\\d) ¼', '.25') %>%
           str_replace('⅓', '0.33') %>%
           str_replace('½', '0.5') %>%
           str_replace('¼', '0.25') %>%
           str_replace('¾', '0.75') %>%
           str_replace('⅔', '0.67') %>%
           str_replace('2 -3|2-3', '3') %>% #Use three of each of the two foods with 2-3 written in the recipe
           str_replace('half(?= pac)', '0.5') %>%
           str_trim() %>%
           
           #Turn cups and punds into singular form
           str_replace('cups', 'cup') %>%
           str_replace('pounds', 'pound') %>%
           str_replace('ounces', 'ounce')
  ) %>%
  
#Reformat all the units in the different recipes to the same name----
mutate(Ingredients = Ingredients %>%
         
         #Missing spaces
         str_replace('\\bg(?=[^ |^arlic|^uacamole|^hee])', 'g ') %>%
         str_replace('\\bkg(?=\\w)', 'kg ') %>%
         str_replace('\\bpcs(?=\\w)', 'pcs ') %>%
         str_replace('\\btbsp(?=\\w)', 'tbsp ') %>%
         str_replace('\\bkg(?=\\w)', 'kg ') %>%
         str_replace('\\btablespoon(?=\\w)', 'tablespoon ') %>%
         str_replace('\\bdl|\\bDL(?=\\w)', 'dl ') %>%
         str_replace('\\bpiece(?=\\w)', 'piece ') %>%
         str_replace('slice(?=[^s|^\\s|^d])', 'slice ') %>%
         str_replace('(?<=\\d)slice', ' slice ') %>%
         str_replace('(?<=\\w|,)slice', ' slice') %>%
         
         #Change units of ingredients
         str_replace('bacon slice|pieces of bacon|pieces of good sliced bacon', 'slice bacon') %>%
         str_replace('tins of canned', 'can') %>%
         str_replace('parts(?! sugar| water)|DL', 'dl') %>%
         str_replace('packs|packet|pakke', 'pack') %>%
         str_replace('\\sbags\\s|\\sbag\\s', ' pack ') %>%
         str_replace('\\stb\\s', ' tbsp ') %>%
         str_replace('tablespoons|tablespoon|tbsp|table spoons|table spoon|\\bss\\b', 'tbsp') %>%
         str_replace('\\st\\s', ' tsp ') %>%
         str_replace('teaspoons|teaspoon|tsp|\\bts\\b', 'tsp') %>%
         str_replace('stk kilo', 'kg') %>%
         str_replace('\\sgr\\s', ' g ') %>%
         str_replace('stk ltr|liter|litre', 'l') %>%
         str_replace('portions|servings|serving', 'portion') %>%
         str_replace('pinches|pinched|knife-wiped', 'pinch') %>%
         str_replace('\\bbt\\b|tassel', 'bunch') %>%
         str_replace('boxes', 'box') %>%
         str_replace('cups', 'cup') %>%
         str_replace('glasses', 'glass') %>%
         str_replace('drops', 'drop') %>%
         str_replace('a squeezed', '1 stk') %>%
         str_replace('pieces|piece|pcs', 'stk') %>%
         str_replace('\\spc\\s', ' stk ') %>%
         str_replace('skiver|skive|slices', 'slice') %>%
         str_replace('fists|fist|handfuls|handful', 'neve') %>%
         str_replace('leaves', 'leaf') %>%
         str_replace_all('twigs|twig|sprigs', 'sprig') %>%
         str_replace('garlic clove', 'clove garlic') %>%
         str_replace('16 figs, each cut into 6 stk s', '16 stk figs') %>%
         str_replace('1 neve aroma mushrooms', '5 stk aroma champignon') %>%
         str_replace('half a bunch of spring onions, white bit only, trimmed and finely', '0.5 bunch scallions') %>%
         str_replace('double kipper fillets', 'smoked herring') %>%
         str_trim()
) %>%
  
  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'rosemary|basil') ~ str_replace(Ingredients, 'pot', 'bunch'),
    str_detect(Ingredients, 'hvitløk|garlic') ~ str_replace(Ingredients, 'båter|båt|boats|boat|cloves|fedd', 'clove'),
    str_detect(Ingredients, 'juice of') &
      !str_detect(Ingredients, paste0(various$units, collapse = '|')) ~ str_replace(Ingredients, '(?<=\\d)\\s', ' stk '),
    str_detect(Ingredients, 'bouquet garni') ~ as.vector(various$bouquet_garni),
    str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'cheese') ~ 'goat cheese',
    TRUE ~ Ingredients)) %>%
    
    #Split rows of boquet garni
    separate_rows(Ingredients, sep = '/') %>%
  
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
  
#Rename some ingredients----
  mutate(Ingredients = Ingredients %>%
           str_replace('all-purpose flour|plain flour|strong white bread flour, plus extra for dusting', 'wheat flour') %>%
           str_replace('celery root', 'Celariac root') %>%
           str_replace('extra-virgin olive', 'olive oil') %>%
           str_replace('of green onions, finely sliced', 'scallion') %>%
           str_replace('smoked lardons', 'bacon') %>%
           str_replace('soft tortillas', 'tortilla') %>%
           str_replace('chickpea', 'chick pea') %>%
           str_replace('fava bean', 'broad bean') %>%
           str_replace('parlsey', 'parsley') %>%
           str_replace('pinenut', 'pine nut') %>%
           str_replace('hazelnut', 'hazel nut') %>%
           str_replace('beansprout', 'bean sprout') %>%
           str_replace('red bean', 'kidney bean') %>%
           str_replace('chilli', 'chili') %>%
           str_replace('chuck steak, excess fat and sinew removed, cut into', 'beef chuck steak') %>%
           str_replace('canned cannellini beans, drained and rinsed', 'canned white beans') %>%
           str_replace('spring onions', 'scallion'),
         
         #Conditionals
         Ingredients = case_when(
           Ingredients == 'fish fillets' ~ 'cod fillet',
           str_detect(Ingredients, 'firm white fish') ~ 'cod fillet',
           Ingredients == 'spaghetti' ~ 'pasta', #Pasta is used in SHARP
           TRUE ~ Ingredients),
         
         
         #Add stk to the amounts of all the items listed in 'stk', fix some mistakes
         Amounts = case_when(
           !str_detect(Amounts, '[:alpha:]') &
             str_detect(Ingredients, regex(paste0(various$stk, collapse = '|'), ignore_case = TRUE)) ~ paste0(Amounts, ' stk'),
           Ingredients == 'bunt coriander' ~ '1 bunch',
           TRUE ~ Amounts)
         ) %>%
  
  mutate(Amounts = case_when(
    str_detect(Ingredients, '1.2 kg') ~ '1.2 kg',
    Ingredients == 'tube of good tomato purée' ~ '130 g', #Kolonial, Mutti
    Ingredients == 'small bunch of fresh thyme, leaf only' ~ '1 bunch',
    str_detect(Ingredients, 'celery') & str_detect(Ingredients, 'stalk') ~ str_replace(Amounts, 'stk', 'stalk'),
    TRUE ~ Amounts
  )) %>%
  #Remove empty Ingredient rows
  drop_na(Ingredients) %>%

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
      str_replace('\\bg\\b', 'kg'),
    
    #Fix some missing units
    unit_enhet = case_when(
      Ingredients %in% c('sprigs of thyme', 'large sprig of thyme') ~ 'sprig',
      TRUE ~ unit_enhet
    ))

#Turn volume units into weight----
#Find the ingredients in the recipes that also can be found in the food weight and portion size data----
#Reference volume to weight comparisons for different foods
ref <- readRDS('../porsjoner_vekt_næringsinnhold/food_weight_ref.Rds')

#Look at the ingredients that are missing amounts
various$no_amounts <- UK %>%
  filter(is.na(Amounts))

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

#Look for ingredients in ref
various$with_reference <- checkRefList(UK)

saveRDS(various$with_reference, 'with_ref_UK.Rds')

various$with_reference <- readRDS('with_ref_UK.Rds')

references <- ref %>% mutate_at(c('first_word', 'second_word'), ~tolower(.))

#Look through and fix errors, several corn cobs have not been properly translated
temp <- bind_rows(various$with_reference) %>%
  full_join(UK) %>% unique() %>%
  filter(!is.na(Amounts)) %>%
  mutate(
    ref = case_when(
      str_detect(Ingredients, 'eggplant') ~ 'Eggplant',
      str_detect(Ingredients, 'chick pea flour') ~ 'chick pea, flour',
      Ingredients == 'canned chick peas' ~ 'Chick pea, canned',
      Ingredients %in% c('lemongrass root, chopped', 'sitrongressrot, hakket', '–3 lemongrass, thin slice',
                         'finely cut lemongrass can be looped', 'lemongrass', 'of lemongrass,', 'lemongrass, finely chop the white part') ~ 'lemongrass',
      Ingredients == 'chili powder, red' ~ 'chili, powder',
      Ingredients == 'spinach' ~ 'Spinach',
      Ingredients == 'salad' ~ 'salad',
      Ingredients == 'heart salad or other salad' ~ 'salad',
      Ingredients == 'peanuts, salty, coarsely chopped' ~ 'Peanuts',
      Ingredients == 'grilled sweet pepper, canned' ~ 'sweet pepper, grilled',
      str_detect(Ingredients, 'breadcrumb') ~ 'bread, crumb',
      str_detect(Ingredients, 'tomat|Tomat') & str_detect(unit_enhet, 'box|can') ~ 'Tomatoes, canned',
      str_detect(Ingredients, 'vinegar') ~ 'Vinegar',
      str_detect(Ingredients, regex('orange', ignore_case = TRUE)) &
        str_detect(Ingredients, 'juice') ~ 'orange juice',
      str_detect(Ingredients, regex('apple', ignore_case = TRUE)) &
        str_detect(Ingredients, 'juice') ~ 'apple juice',
      str_detect(Ingredients, regex('grape', ignore_case = TRUE)) &
        str_detect(Ingredients, 'juice') ~ 'grape juice',
      str_detect(Ingredients, 'lime|Lime') & unit_enhet == 'kg' ~ 'lime juice',
      str_detect(Ingredients, 'lemon|Lemon') & unit_enhet == 'kg' ~ 'lemon juice',
      Ingredients == 'pickled sweet pepper' ~ 'pickled sweet pepper',
      Ingredients == 'cloves' ~ 'cloves',
      Ingredients == 'butter dough' ~ 'puff, pastry',
      str_detect(Ingredients, 'stock|broth') & !str_detect(Ingredients, 'cube|dice') ~ 'stock/broth',
      str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'cube|dice') ~ 'broth, cube',
      str_detect(Ingredients, 'wine') & is.na(ref) ~ 'wine',
      str_detect(Ingredients, 'water') & !str_detect(Ingredients, 'chestnut') ~ 'water',
      str_detect(Ingredients, 'breadsticks') ~ '',
      str_detect(Ingredients, 'buttermilk') ~ 'buttermilk',
      Ingredients == 'savoy cabbage' ~ paste0(ref, ' savoy'),
      Ingredients == "lamb's lettuce" ~ "lamb's lettuce", #Lmb's lettuce is found in the SHARP database 
      Ingredients == 'fresh pig’s liver' ~ 'pork liver',
      Ingredients == 'calf liver' ~ 'calf liver',
      str_detect(Ingredients, 'smoked pollock') ~ 'smoked pollock',
      Ingredients == 'medium-sweet cider' ~ 'cider',
      Ingredients == 'bulgar wheat' ~ 'bulgur wheat',
      str_detect(Ingredients, 'pancetta') ~ 'ham, smoked',
      Ingredients == 'sea bass' ~ 'sea bass',
      str_detect(Ingredients, 'goat cheese') & !str_detect(Ingredients, 'blue') ~ 'goat cheese',
      Ingredients == 'wheat flour tortillas' ~ 'tortilla, fin',
      str_detect(Ingredients, 'prosciutto') ~ 'ham, cured',
      str_detect(Ingredients, 'squidges of honey') ~ 'honey',
      Ingredients == 'green bird’s-eye chilies, chopped with seeds' ~ 'chili',
      Ingredients == 'pineapple juice' ~ 'pineapple juice',
      Ingredients == 'hazel nut oil' ~ 'hazelnut oil',
      Ingredients == 'canned chick peas or mixed beans, rinsed and drained in a sieve' ~ 'chick pea canned',
      Ingredients == 'peas' ~ 'pea',
      str_detect(Ingredients, 'cheddar|jarlsberg') & str_detect(Ingredients, 'cheese') ~ 'hard to semi-hard cheese',
      str_detect(Ingredients, 'watercress') ~ 'watercress',
      TRUE ~ ref),
    ID = case_when(
      str_detect(Ingredients, 'eggplant') ~ as.numeric(references[str_which(tolower(references$first_word), 'eggplant'),1]),
      str_detect(Ingredients, 'chick pea flour') ~ references %>% filter(first_word == 'chick pea' & second_word == 'flour') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'canned chick peas' ~ references %>% filter(first_word == 'chick pea' & second_word == 'canned') %>% select(ID) %>% as.numeric(.),
      Ingredients %in% c('lemongrass root, chopped', 'sitrongressrot, hakket', '–3 lemongrass, thin slice',
                         'finely cut lemongrass can be looped', 'lemongrass', 'of lemongrass,', 'lemongrass, finely chop the white part') ~ as.numeric(references[str_which(tolower(references$first_word), 'lemongrass'),1]),
      Ingredients == 'chili powder, red' ~ references %>% filter(first_word == 'chili' & second_word == 'powder') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'spinach' ~ references %>% filter(first_word == 'spinach' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients %in% c('field salad', 'salad', 'heart salad or other salad', 'lettuce') ~ references %>% filter(first_word == 'salad' & second_word == 'mix') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'peanuts, salty, coarsely chopped' ~ references %>% filter(first_word == 'peanuts' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'potato') & !str_detect(Ingredients, 'starch') ~ references %>% filter(first_word == 'potato' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'grilled sweet pepper, canned' ~ references %>% filter(first_word == 'sweet pepper' & second_word == 'grilled') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'breadcrumbs or panko' ~ references %>% filter(first_word == 'bread' & second_word == 'crumb') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'tomat|Tomat') & str_detect(unit_enhet, 'box|can') ~ references %>% filter(first_word == 'tomatoes' & second_word == 'canned') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'vinegar') ~ references %>% filter(first_word == 'vinegar' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, regex('orange', ignore_case = TRUE)) &
        str_detect(Ingredients, 'juice') ~ 0,
      str_detect(Ingredients, regex('apple', ignore_case = TRUE)) &
        str_detect(Ingredients, 'juice') ~ 0,
      str_detect(Ingredients, regex('grape', ignore_case = TRUE)) &
        str_detect(Ingredients, 'juice') ~ 0,
      str_detect(Ingredients, 'lime|Lime') & unit_enhet == 'kg' ~ 0,
      str_detect(Ingredients, 'lemon|Lemon') & unit_enhet == 'kg' ~ 0,
      Ingredients == 'pickled sweet pepper' ~ references %>% filter(first_word == 'pickled' & second_word == 'pepper') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'cloves' ~ references %>% filter(first_word == 'cloves' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'butter dough' ~ references %>% filter(first_word == 'puff' & second_word == 'pastry') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'stock|broth') &
        !str_detect(Ingredients, 'cube|dice') ~ 0,
      str_detect(Ingredients, 'stock|broth') &
        str_detect(Ingredients, 'cube|dice') ~ references %>% filter(first_word == 'broth' & second_word == 'cube') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'wine') & is.na(ref) ~ 0,
      str_detect(Ingredients, 'thyme') &
        !str_detect(Ingredients, 'dried') ~ references %>% filter(first_word == 'thyme' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'mint') &
        !str_detect(Ingredients, 'chutney') ~ references %>% filter(first_word == 'mint' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'breadcrumb') ~ references %>% filter(first_word == 'bread' & second_word == 'crumb') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'water') & !str_detect(Ingredients, 'chestnut') ~ 0,
      str_detect(Ingredients, 'breadsticks') ~ 0,
      str_detect(Ingredients, 'buttermilk') ~ 0,
      Ingredients == "lamb's lettuce" ~ 0,
      Ingredients == 'fresh pig’s liver' ~ 0,
      Ingredients == 'calf liver' ~ 0,
      str_detect(Ingredients, 'smoked pollock') ~ 0,
      Ingredients == 'medium-sweet cider' ~ 0,
      Ingredients == 'bulgar wheat' ~ references %>% filter(first_word == 'bulgur' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'pancetta') ~ references %>% filter(first_word == 'ham' & second_word == 'smoked') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'sea bass' ~ 0,
      str_detect(Ingredients, 'goat cheese') & !str_detect(Ingredients, 'blue') ~ 0,
      Ingredients == 'wheat flour tortillas' ~ references %>% filter(first_word == 'tortilla' & second_word == 'fin') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'prosciutto') ~ references %>% filter(first_word == 'ham' & second_word == 'cured') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'squidges of honey') ~ as.numeric(references[str_which(tolower(references$first_word), 'honey'),1]),
      Ingredients %in% c('ginger root', 'grated fresh ginger') ~ references %>% filter(first_word == 'ginger' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'green bird’s-eye chilies, chopped with seeds' ~ references %>% filter(first_word == 'chili' & second_word == 'red') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'pineapple juice' ~ 0,
      Ingredients == 'hazel nut oil' ~ references %>% filter(first_word == 'hazelnut' & second_word == 'oil') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'canned chick peas or mixed beans, rinsed and drained in a sieve' ~ references %>% filter(first_word == 'chick pea' & second_word == 'canned') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'peas' ~ references %>% filter(first_word == 'pea' & second_word == 'dry') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'cheddar|jarlsberg') & str_detect(Ingredients, 'cheese') ~ references %>% filter(first_word == 'hard to semi-hard cheese' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'watercress') ~ references %>% filter(first_word == 'watercress' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      TRUE ~ ID)) %>%
  
  #mutate(Ingredients = Ingredients %>%
  mutate(unit_enhet = case_when(
    Ingredients == 'cloves' ~ 'stk',
    
    #One handful of salad ~ about one dl
    str_detect(Ingredients, 'salad') & unit_enhet == 'neve' ~ 'dl',
    TRUE ~ unit_enhet
  )) 

various$not_found_in_ref <- anti_join(UK, temp %>% select(`Selected Meals`, Ingredients))

#Calculate the weights
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
  filter(unit_enhet %in% UK$unit_enhet)

final <- right_join(weights, temp, by = c('ID', 'unit_enhet')) %>% #Doing this gives final three more rows than temp...?
  
  #Turn weights into kilo
  mutate(Amounts_kg = case_when(
    unit_enhet == 'kg' ~ Amounts,
    !unit_enhet == 'kg' ~ Amounts*g/1000
  )) %>%
  
  #Cleanup
  select(`Selected Meals`, Ingredients.y, Amounts_kg, ref, Amounts, unit_enhet) %>%
  unique() %>% #Got some values twice as inner_join got both norwegian and english translated names
  rename(Ingredients = Ingredients.y) %>% drop_na(Ingredients)

#Save
saveRDS(final, '../oppskrifter/UK_clean.Rds')

#Save some of the dataframes in various
various$units <- NULL
various$stk <- NULL
various$bouquet_garni <- NULL
various$with_reference <- NULL
various$weights <- NULL
various$to_dl <- NULL
saveRDS(various, 'various_UK.Rds')

