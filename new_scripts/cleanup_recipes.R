library(tidyverse)
library(readxl)
library(stringi)
library(thesisToolsOmics)

#Get the nutrient and sustainability information for each recipe per 100g of the recipe

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data')

#GGplot theme
theme_set(theme_bw())

#Cleanup Norway, UK & US----

#Different databases to search through to find amounts in kilos, nutrient content and sustainability measurements
references <- list(
  'volume_weight' = readRDS('./porsjoner_vekt_næringsinnhold/food_weight_ref.Rds') %>% filter(language == 'english'),
  'sustainability' = readRDS('sharp_ref.Rds'),
  'nutrients' = readRDS('nutrient_reference.Rds')
)
databases <- list(
  'volume_weight' = readRDS('./porsjoner_vekt_næringsinnhold/all_weights.Rds'),
  'nutrients' = readRDS('nutrients_df.Rds') ,
  'sustainability' = readRDS('sharp_db.Rds')
)

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
                 reference$second_word[i] == '\\') {
        
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
checkRefList <- function(df, reference){
  
  results <- lapply(df$Ingredients, checkRef, reference = reference) %>% bind_rows()
  
}
fixRefID <- function(reference, first_w, second_w = '\\') {
  
  ID <- reference %>% filter(first_word == first_w & second_word == second_w) %>% select(ID) %>% as.numeric(.)
  
}

#Various objects to keep environment clean----
various <- list(
  
  #Units to keep in the recipes
  #Recipes from Kolonialen has translated 'pk/pakke/stykk' to 'hp'
  #What is hp?
  'units' = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf', 'can',
              'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice', '\\bclove\\b',
              'neve', 'ml', 'cl', 'bunch', 'pack', 'plate', 'drop', 'twig', 'pound', 'ounce', 'stalk') %>%
    #Add whitespace on both sides to only match a unit in a string
    sapply(., function(x) {paste0('\\s', x, '\\s')}),
  
  #Ingredients that are countes as individual pieces/stk
  'stk' = c('anchovy', 'anise', 'apple', 'apricot', 'avocado',
            
            'banana', 'bay leaf', 'bean', 'baguette', 'bok choi', 'broccoli', 'broth cube', 'basil',
            'bread',
            
            'cabbage', 'cardamom', 'carrot', 'cauliflower', 'Celariac root', 'celery', 'celery stalk',
            'champignons', 'chicken', 'chicory', 'chili', 'ciabatta', 'cinnamon', 'clementine', '\\bcloves\\b', 'cod',
            'cod fillet', 'crab', 'cracker cream', 'cucumber', 'coriander',
            
            'duck',
            
            'egg', 'entrecôtekam',
            
            'fennel', 'fig',
            
            'garlic', 'grapes',
            
            'herring smoked',
            
            'jerusalem artichoke', 'juniper berry',
            
            'lamb chop', 'leek', 'lemon', 'lemongrass', 'lettuce', 'lime',
            
            'mango', 'mushroom',
            
            'nori seaweed', 'nut',
            
            'olive', 'onion', 'orange',
            
            'paprika', 'pear', 'pepper', 'peppercorns', 'pineapple', 'pomegranate', 'plate', 'pork',
            'potato', 'prawn', 'prune',
            
            'radish', 'roll',
            
            'salad', 'salmon', 'scallion', 'scallop', 'scampi', 'shallot', 'sheep', 'sheet', 'shrimp',
            'squid', 'stock cube',
            
            'tenderloin', 'thyme', 'tomato', 'tortilla', 'trout', 'turkey')
  
)
#Raw data----
raw <- list() #Empty list to fill with the raw data from the different countries

#From each individual Norwegian dataset
raw_list_NO <- list(
  'aperitif' = read_csv('./oppskrifter/aperitif.csv'),
  'klikk' = read_csv('./oppskrifter/klikk.csv'),
  'tine' = read_csv('./oppskrifter/tine.csv'),
  'kolonial' = read_csv('./Oppskrifter/kolonialen.csv')
) %>%
  #Columns needed for my work
  bind_rows(.) %>% select(No, `Selected Meals`, Ingredients, Source, Country)

  #Recipes needed
  various$NO_recipes <- read_xlsx('./oppskrifter/Data_NO_100.xlsx') %>%
    select(`Selected Meals`, Source) %>%
  
    #Rename some recipe names that are different in the datasets
    mutate(`Selected Meals` = `Selected Meals` %>%
           str_replace('PinnekjÃ¸tt', 'Pinnekjøtt') %>%
           str_replace('Biff with BearnÃ© saus', 'Biff with Bearné saus') %>%
           str_replace('sauted reindeer', 'sautéed reindeer')
    ) %>%
  
    #Add muslim steak curry as it won't be matched by the name
    left_join(raw_list_NO %>% filter(str_detect(`Selected Meals`, 'steak curry'))) %>%
    #Add the rest
    left_join(raw_list_NO %>% select(`Selected Meals`, Source), by = c('Selected Meals', 'Source')) %>%
    select(`Selected Meals`, Source)
  
#Save and remove words describing parts of the recipe, such as the side dishes, do some light clean-up of ingredients including translating from Norwegian to English
#write_csv(raw_list_NO, 'all_norwegian_recipes.csv')
#reread
raw$Norway <- read_xlsx('./oppskrifter/all_norwegian_recipes.xlsx') %>%
  mutate(`Selected Meals` = `Selected Meals` %>%
           str_replace('PinnekjÃ¸tt', 'Pinnekjøtt') %>%
           str_replace('Biff with BearnÃ© saus', 'Biff with Bearné saus') %>%
           str_replace('sautÃ©ed reindeer', 'sautéed reindeer')
         ) %>%
  right_join(various$NO_recipes)
#UK
raw$UK <- read_xlsx('./oppskrifter/Data_UK_100.xlsx') %>%
  select(c(`Selected Meals`, Ingredients, `No. Of portion`, Source)) %>%
  mutate(Country = 'UK')  %>%
  drop_na(Ingredients)

raw$US <- readRDS('./oppskrifter/US_clean.Rds') %>%
  select(-c(ingredient_id, ingredient_amount)) %>%
  mutate(Country = 'US',
         Source = 'Allrecipes') %>%
  rename(Amounts = Amounts_kg) %>%
  mutate(Amounts = paste0(Amounts, ' kg'))

various$raw_ingredients <- bind_rows(raw) %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Separate ingredients into separate rows and remove empty rows----
separate_rows(., Ingredients, sep = '\n') %>%
  mutate(Ingredients = str_trim(Ingredients, side = 'both'),
         Ingredients = str_squish(Ingredients)) %>%
  filter(Ingredients != '' | is.na(Ingredients))

#Clean up the ingredients
clean <- bind_rows(raw) %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Separate ingredients into separate rows and remove empty rows----
  separate_rows(., Ingredients, sep = '\n') %>%
  mutate(Ingredients = str_trim(Ingredients, side = 'both'),
         Ingredients = str_squish(Ingredients)) %>%
  filter(Ingredients != '' | is.na(Ingredients)) %>% #Filter away empty rows
  
  #Change a few composite ingredient into individual ingredients----
  mutate(Ingredients = Ingredients %>%
         str_replace('500 g ground pork and beef', '250 g ground pork\n250 g ground meat beef') %>%
         str_replace('1 bouquet garni \\(a bay leaf, 2 sprigs of thyme, and some parsley stems, tied together with string\\)|1 stk bouquet garni', '1 stk bay leaf\n2 twig of thyme\n2 twig parsley stems') %>% # From Hugh's River Cottage
         #Change the 'pizza base mix' from a Kolonial recipe to a piza base recipe from the same site
         #https://oda.com/no/recipes/2520-spoon-pizzadeig/
         str_replace('1 piece pizza base, mix|1 hp pizza base, mix', '450 g	wheat flour\n1 tsp	dry yeast\n3 dl	water\n2 tbsp	olive oil\n1 tsp	salt\n1 tsp	sugar') %>%
         #Change the Tine ekte grated cheese to 50% Jarlsberg, and 50% Norvegia as pr ingredient list, the 'grated cheese' in Greek Moussaka is the Tine grated cheese according to recipe site
         str_replace('100 g of grated cheese', '50 g norvegia\n50 g jarlsberg') %>%
         str_replace('1 dl tine genuine grated cheese original', '0.5 dl norvegia\n0.5 dl jarlsberg') %>%
         str_replace('100 g tine genuine grated cheese original', '50 g norvegia\n50 g jarlsberg') %>%
         str_replace('300 g tine genuine grated cheese original', '150 g norvegia\n150 g jarlsberg') %>%
         str_replace('4 dl tine genuine grated cheese original', '2 dl norvegia\n2 dl jarlsberg') %>%
         str_replace('2 dl tine genuine grated cheese original', '1 dl norvegia\n1 dl jarlsberg') %>%
         str_replace('2 tbsp tine genuine torn cheese original', '1 tbsp norvegia\n 1 tbsp jarlsberg') %>%
         str_replace('1 hp rema frozen vegetables', '170 g carrot\n165 g broccoli\n165 g baby corn') %>% #Familiefavoritt grovkuttet in the recipe
         str_replace('frozen mixed vegetables', '40.75 g potato\n40.75 g sweet pepper red\n40.75 g green peas\n40.75 g carrot') %>% #Mix of vegetables typically found in the dish
         str_replace('wok-vegetables', '110 g bean green asparagus\n110 g broccoli\n100 g mango\n85 g onion\n75 g water chestnut\n10 g rapeseed oil') %>% #Thai inspired wok REMA1000
              
         #Refrigerated buttermilk dough
         #From https://www.finecooking.com/recipe/buttermilk-biscuit-dough
         str_replace('refrigerated buttermilk biscuit dough', '8 ounce wheat flour
2 tsp baking powder
1 tsp granulated sugar
0.5 tsp baking soda
0.5 tsp table salt
6 tbsp cold unsalted butter
0.75 cup milk') %>%
         
         #Fish cakes              
         #From https://www.matprat.no/oppskrifter/familien/grove-fiskekaker-med-karristuede-rotgronnsaker/
         str_replace('600 g fish cakes, coarse', '600 g haddock
1 tsp salt
1.5 tbsp potato starch
1 stk egg
1 dl milk
0.25 tsp pepper
2 tbsp leek
2 tbsp butter') %>%
         
         #Potato flatbread
         #From https://www.matprat.no/oppskrifter/tradisjon/potetlomper/ uses a mix of rye and wheat flour originally
         str_replace('30 pcs lomper',
         '5 dl wheat flour
         2000 g potato
         2 tsp salt')) %>%
  
  #Remove amounts for compiste ingredients from the us recipes
  mutate(Amounts = case_when(!str_detect(Ingredients, '\n') ~ Amounts)) %>%

  #Separate again
  separate_rows(Ingredients, sep = '\n') %>%
  
  #Remove some unnecessary characters in the ingredients column, and make sure there is a space between numbers and words ('2 dl', not '2dl)----
  mutate(Ingredients = Ingredients %>%
           str_replace_all('["()]|possibly', '') %>%
           str_replace(' s ', '') %>%
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
           str_replace('half(?= pac)', '0.5'))

clean <- clean %>%
  
  #Reformat all the units in the different recipes to the same name, fix some names----
  mutate(Ingredients = Ingredients %>%
         
         #Missing spaces
         str_replace('\\bg(?=[^ |^arlic|^uacamole|^hee])', 'g ') %>%
         str_replace('\\bkg(?=\\w)', 'kg ') %>%
         str_replace('\\bpacks(?=\\w)', 'pack ') %>%
         str_replace('\\bpack(?=[a-rt-z])', 'pack ') %>%
         str_replace('\\bpcs(?=\\w)', 'pcs ') %>%
         str_replace('\\bpieces(?=\\w)', 'piece ') %>%
         str_replace('\\bpiece(?=[a-rt-z])', 'piece ') %>%
         str_replace('\\btbsp(?=[a-rt-z])', 'tbsp ') %>%
         str_replace('\\bkg(?=[a-rt-z])', 'kg ') %>%
         str_replace('\\btablespoons(?=\\w)', 'tablespoon ') %>%
         str_replace('\\btablespoon(?=[a-rt-z])', 'tablespoon ') %>%
         str_replace('\\bdl|\\bDL(?=\\w)', 'dl ') %>%
         str_replace('slice(?=[^s|^\\s|^d])', 'slice ') %>%
         str_replace('(?<=\\d)slice', ' slice ') %>%
         str_replace('(?<=\\w|,)slice', ' slice') %>%
         str_replace('paprikai', 'paprika i') %>%
         
         #Change units of ingredients
         str_replace('bacon slice|pieces of bacon|pieces of good sliced bacon|thin slices of bacon|slices of bacon thin|rashers of smoked streaky bacon', 'slice bacon') %>%
         str_replace('large garlic cloves|cloves of garlic|pieces minced garlic|ts garlic minced|fat garlic|clove of garlic|cloves garlic|garlic cloves|garlic clove|garlic boats|garlic boat|piece garlic|pieces of pressed garlic cloves|pieces of garlic|teaspoon garlic clove|tsp finely chopped garlic|tsp finely grated garlic|boats finely chopped garlic|cloves finely chopped garlic|cloves with garlic', 'clove garlic') %>% #A tsp garlic is about a clove
         str_replace('0.5 stem celery', '0.5 stk celery') %>%
         str_replace('celery root', 'celeriac root') %>% #Must be fixed before changing the units of celery stalks)
         str_replace('stk of celery|stalks of celery|celery rod|celery stalks|celery stalk|chopped bar celery|celery bars|stems celery|stem celery|pieces celery|piece celery|pieces of celery|piece of celery|stk celery stalks|stk celery stalk|rod celery|twig celery', 'stalk celery') %>%
         str_replace('0.5 fl', '0.325 dl') %>% #One half bottle of white wine
         str_replace('1 bottle', '0.75 dl') %>% #One bottle of red wine
         str_replace('4 slices entrecote', '4 portion entrecote') %>%
         str_replace('1 piece of pinched', '1 pinch') %>%
         str_replace('1 stem of spring onion', '1 stk spring onion') %>%
         str_replace('coat corn, or bread grater from 2 slices of bread', '2 slice bread') %>%
         str_replace('tins of canned', 'can') %>%
         str_replace('parts|DL', 'dl') %>%
         str_replace('packs|packet|pakke', 'pack') %>%
         str_replace('\\sbags\\s', ' pack ') %>%
         str_replace('\\sbag\\s', ' pack ') %>%
         str_replace('\\stb\\s', ' tbsp ') %>%
         str_replace('tablespoons|tbsps|table spoons', 'tbsp') %>%
         str_replace('tablespoon|tbsp|table spoon|\\bss\\b', 'tbsp') %>%
         str_replace('\\st\\s', ' tsp ') %>%
         str_replace('teaspoons|tsps', 'tsp') %>%
         str_replace('teaspoon|tsp|\\bts\\b', 'tsp') %>%
         str_replace('kilo', 'kg') %>%
         str_replace('\\sgr\\s', ' g ') %>%
         str_replace('grams', 'g') %>%
         str_replace('stk ltr|liter|litre', 'l') %>%
         str_replace('portions|servings', 'portion') %>%
         str_replace('portion|serving', 'portion') %>%
         str_replace('pinches|pinched|knife-wiped', 'pinch') %>%
         str_replace('\\bbts\\b|tassels|bundles', 'bunch') %>%
         str_replace('\\bbt\\b|tassel|bundle', 'bunch') %>%
         str_replace('boxes', 'box') %>%
         str_replace('cups', 'cup') %>%
         str_replace('glasses', 'glass') %>%
         str_replace('drops', 'drop') %>%
         str_replace('pieces|pcs', 'stk') %>%
         str_replace('piece', 'stk') %>%
         str_replace('\\spcs\\s|\\spc\\s', ' stk ') %>%
         str_replace('slices', 'slice') %>%
         str_replace('thick slice', 'slice') %>%
         str_replace('fists|handfuls', 'neve') %>%
         str_replace('fist|handful', 'neve') %>%
         str_replace('leaves', 'leaf') %>%
         str_replace('sage leaves|sage leaf', 'leaf sage') %>%
         str_replace('twigs fresh thyme|thyme twigs|stk s of thyme twigs|twigs thyme|thyme sprig|fresh thyme sprigs dried can also be used|sprigs of fresh thyme|sprig of fresh thyme|sprig thyme|sprig of thyme|large sprig of thyme', 'twig thyme') %>%
         str_replace('stk of neve fresh thyme', 'neve fresh thyme') %>%
         str_replace('small bunch of fresh thyme, leaf only', 'bunch fresh thyme') %>%
         str_replace('stk of parsley sprig|pieces of parsley stalks|stk of parsley stalks', 'twig parsley') %>%
         str_replace('stk of rosemary sprig|stems fresh rosemary|piece of rosemary sprig|pieces of rosemary|pieces of fresh rosemary', 'twig rosemary') %>%
         str_replace('1 mug of parsley', '1 bunch parsley') %>%
         str_replace('1 mug of parsley', '1 bunch parsley') %>%
         str_replace('piece basil, fresh', 'twig basil fresh') %>%
         str_replace('stk of neve coriander', 'neve coriander') %>%
         str_replace('stk leaf sage', 'leaf sage') %>%
         str_replace('stk small dill bunch', 'bunch dill') %>%
         str_replace('sprigs|sprig|\\bstems\\b|\\bstem\\b|twigs', 'twig') %>%
         str_replace('pieces rosemary, fresh|stk rosemary fresh', 'bunch rosemary') %>%
         str_replace('basil leaf|fresh basil leaf, cut into thin strips', 'leaf basil') %>%
         str_replace('pounds', 'pound') %>%
         str_replace('ounces', 'ounce') %>%
         str_replace('chili stk', 'stk chili') %>%
           
         #Ingredients that specify amounts in the text or lack some
         str_replace('4 chicken fillets about 100 g per person', '400 g chicken breast') %>%
         str_replace('4 stk of angler fish fillets 170 - 200 g', '800 g angler fish') %>%
         str_replace('4 stk steaks of 200 g of tenderloin beef ', '800 g beef tenderloin') %>%
         str_replace('8 stk of salmon fillet about 100 g', '800 g salmon fillet') %>%
         str_replace('4 salmon fillets about 140 g', '560 g salmon fillets') %>%
         str_replace('1 stk turkey fillet, about 1.5 kg with leather', '1.5 kg turkey breast') %>%
         str_replace('4 chickens, about 500 g', '2 kg whole chicken') %>%
         str_replace('1 stk thai ginger kha about 2 cm or 1 tbsp laos powder', '2 cm thai ginger') %>%
         str_replace('1 4-bone 1.2 kg 20.5lb stk of pork, cleaned', '1.2 kg pork rib roast') %>%
         str_replace('1 stk of lamb thighs, approx. 2.5 kg', '2.5 kg lamb leg roast') %>%
         str_replace('4 stk of cod back per approx. 180 g', '720 g cod fillets') %>%
         str_replace('4 turkey steaks', '4 portion turkey breast') %>% #A turkey steak is a portion of a turkey breast
         str_replace('grated shell of 1 lemon|the shell of 1 lemon', '1 stk lemon, the zest') %>%
         str_replace('3 plates', '225 g') %>% #One pack of six plates butterdough has 450 g's worth
         str_replace('12 small bouquets of broccoli', '300 g broccoli') %>% #1 bouquet is 25g according to the Weight and measurement database
         str_replace('16 figs, each cut into 6 stk s', '16 stk figs') %>%
         str_replace('1 neve aroma mushrooms', '5 stk aroma champignon') %>%
         str_replace('half a bunch of spring onions, white bit only, trimmed and finely', '0.5 bunch scallion') %>%
         str_replace('4 stk steaks of 200 g of tenderloin beef', '800 g beef tenderloin') %>%
         str_replace('4 stk of beef fillet, 200 g', '800 g beef tenderloin') %>%
           
         #Some names to change
         str_replace('double kipper fillets', 'smoked herring') %>%
         str_replace('chilli', 'chili') %>%
         str_replace('pen or other paste', 'penne or other pasta') %>%
         str_replace('calf text', 'calf steak') %>%
         str_replace('calf tail', 'calf leg') %>% #The pieces of calf tail in the osso bucco recipe from klikk is calf leg https://oppskrift.klikk.no/ossobuco-med-gremolata-og-risotto/1068/
         str_replace('wild pot base', 'toro jegergryte') %>%
         str_replace('dried leaf basil', 'basil dried') %>%
         str_replace('korean-style short ribs beef chuck flanken, cut 1 /3 to 1/2 inch thick across bones', 'beef short rib') %>%
         str_replace('chile peppers|chile pods', 'chili pepper') %>%
         str_replace('pimento|sport peppers', 'sweet pepper') %>%
         str_replace('large shrimp, peeled and deveined and butterflied', 'large shrimp') %>%
         str_replace('dill pickle spear|dill pickle slice', 'cucumber pickle') %>%
         str_replace('dried rice vermicelli', 'vermicelli pasta') %>%
         str_replace('corn tortilla chips', 'nacho') %>%
         str_replace('recao, or culantro', 'coriander') %>%
         str_replace('parlsey', 'parsley') %>%
         str_replace('salvie', 'sage')
         
         ) %>%
  
  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'rosemary|basil|tarragon|coriander') ~ str_replace(Ingredients, 'pot|bunt', 'bunch'),
    !str_detect(Ingredients, '^[:digit:]') & (str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'lemon|lime')) ~ str_replace(Ingredients, '(?<=\\d)\\s', ' stk '),
    `Selected Meals` %in% c('Duck Breast With PasTine Pure', 'Cod With Bacon, Spinach And Rotmoss') & Ingredients == 'parsley' ~ 'parsnip root', #Mistake in translation
    `Selected Meals` == 'Pollock with herb onion and baked potatoes' & str_detect(Ingredients, 'fish') ~ str_replace(Ingredients, 'fish fillet', 'pollock'), #It says in the name it's pollock
    TRUE ~ Ingredients))
    
  #Extract the amounts to their own column----
clean <- clean %>%
  
  mutate(Amounts = case_when(
    
    Country == 'US' & !is.na(Amounts) ~ Amounts,
    
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
      TRUE ~ Ingredients)) %>%
  
    #Trim and squish
    mutate(Ingredients = Ingredients %>%
             str_trim(side = 'both') %>%
             str_squish()) %>%
  
  #Fix the 'hp' amounts from Kolonialen and Klikk, using the weights from their webstore. Hp means either 'pack' or 'portion'
  mutate(Amounts = case_when(
    #In the 100
    Ingredients == 'chickpeas' & Amounts == '4 hp' ~ '1640 g', #Four cans, one can is 410 g in the recipe ingredient shopping cart they supply
    Ingredients == 'vossa sausage' ~ '800 g', #The one sold at kolonial is 400g each
    Ingredients == 'tomatoes, chopped' & Amounts == '2 hp' ~ '2 stk',
    str_detect(Ingredients, 'jasmine rice') & Amounts == '2 hp' ~ '240 g',
    Ingredients == 'halloumi' ~ '200 g',
    Ingredients == 'toro greek moussaka' ~ '136 g', #Mostly potatoes, some tomatoes, wheat flour, cornstach and oil
    Ingredients == 'salad mix' & Amounts == '1 hp' ~ '175 g', #Babyleaf salad mix is their default salad mix in the shoppin cart for the recipes
    Ingredients == 'rema frozen vegetables' ~ '450 g',
    Ingredients == 'peas, frozen' & Amounts == '0.75 hp' ~ '263 g',
    Ingredients == 'peas, frozen' & Amounts == '1 hp' ~ '350 g',
    Ingredients == 'mustard for sausages' ~ '490 g',
    Ingredients %in% c('ketchup', 'tomato ketchup') & Amounts == '1 hp' ~ '450 g', #Heinz in Kolonialem recipes
    str_detect(Ingredients, 'noodle') & Amounts == '1 hp' ~ '250 g',
    str_detect(Ingredients, 'crispy bread') & Amounts == '1 hp' ~ '520 g', #This is a lot of crisp bread for four persons?
    str_detect(Ingredients, 'parsley') & Amounts == '0.5 hp' ~ '10 g',
    str_detect(Ingredients, 'parsley') & Amounts == '1 hp' ~ '20 g',
    Ingredients == 'stalk celery' & Amounts %in% c('0.20 hp', '0.2 hp') ~ '76 g',
    Ingredients == 'cherry tomatoes, red' & Amounts == '0.5 hp' ~ '100 g',
    str_detect(Ingredients, 'risotto rice with mushrooms') ~ '500 g',
    str_detect(Ingredients, 'ham, in cubes') ~ '80 g', #Smårettstopping skinke
    Ingredients == 'of flatbread' ~ '275 g', #Tine recipe, Korni flatbread 
    
    #In the not used
    Ingredients == 'anchovies, can be looped' ~ '55 g', #One pack of anchovies is 50g
    Ingredients == 'asparagus' & str_detect(Amounts, 'hp') ~ '500 g', #Two packs of asparagus is 250*2
    str_detect(Ingredients, 'bearnaise') ~ '29 g', #Amount in one pack of Toro bearnaise
    Ingredients == 'broccoli, frozen' ~ '450 g', #In Norway, frozen broccoli is typically sold in packs of either 400, 450 or 500g depending on supermarket chain.
    Ingredients == 'celery stalk' & Amounts == '0.25 hp' ~ '95 g', #Net weight of one celery is 380g
    Ingredients == 'celery stalk' & Amounts == '1 stk' ~ '380 g',
    Ingredients == 'cherry tomatoes, canned' ~ '400 g',
    Ingredients == 'cherry tomatoes, red' & Amounts == '1 hp' ~ '200 g', #It's their 200g product they show in the recipes
    Ingredients == 'frozen frozen peas' & Amounts == '1 hp' ~ '350 g', #Rema 1000 frozen peas
    Ingredients == 'guacamole spice mix' ~ '20 g', #Olde el paso
    Ingredients == 'heart salad' & Amounts == '1 hp' ~ '2 stk',
    str_detect(Ingredients, 'jasmine rice') & Amounts == '1 hp' ~ '120 g', #One boil-in-bag is 120 g
    Ingredients == 'mango chutney' & Amounts == '1 hp' ~ '50 g', #Recipe says 50 g
    Ingredients == 'mango chutney' & Amounts == '0.5 hp' ~ '175 g',
    Ingredients == 'naan bread' & Amounts == '1 hp' ~ '120 g', #From kolonial
    Ingredients == 'naan bread' & Amounts == '2 hp' ~ '240 g',
    Ingredients == 'pasta' & Amounts == '2 hp' ~ '400 g', #Default large portion of pasta for four people, as other recipes at the same site. Couldn't find the specific recipe
    Ingredients == 'pearl barley' & Amounts == '4 hp' ~ '300 g', #One portion of pearl barley is default 75g
    Ingredients == 'peas, frozen' & Amounts == '2 hp' ~ '700 g',
    Ingredients == 'pizza filling' ~ '55 g', #Tomatoes, onions, corn starch, sugar, oil and spices
    Ingredients == 'pork chops' & Amounts == '2 hp' ~ '1000 g', #One portion of pork chops is 250g, four portions in the recipe
    Ingredients == 'mashed potatoes' ~ '800 g', #Default portion size of mashed potatoes is 200 g
    Ingredients == 'radish' & Amounts == '0.5 hp' ~ '65 g',
    Ingredients == 'radish' & Amounts == '1 hp' ~ '130 g',
    Ingredients == 'radish' & Amounts == '2 hp' ~ '260 g',
    Ingredients == 'rod celery' & Amounts == '0.2 hp' ~ '0.2 stk', #350 g is the weight at Meny.no,
    Ingredients == 'rod celery' & Amounts == '0.25 hp' ~ '0.25 stk',
    str_detect(Ingredients, 'shimeji mushrooms') ~ '200 g', #Pack of rare mushrooms
    Ingredients == 'sour cream, light' & Amounts == '1 hp' ~ '300 g', #Tine default
    Ingredients == 'spinach' & Amounts == '1 hp' ~ '200 g', #BAMA
    str_detect(Ingredients, 'sugar snap peas') & Amounts == '1 hp' ~ '200 g',
    Ingredients == 'taco seasoning' & Amounts == '1 hp' ~ '28 g',
    Ingredients == 'tikka masala sauce' ~ '360 g',
    Ingredients == 'yoghurt, Greek' & Amounts == '1 hp' ~ '200 g', #That's what the recipe says
    Ingredients == 'blueberry basket' ~ '125 g',
    TRUE ~ Amounts
  ))

  #Standardize some ingredient names----
clean <- clean %>%
  
  mutate(Ingredients_standardized = case_when(
    
    #Vegetables/fruit----
    str_detect(Ingredients, 'acorn squash') ~ 'winter squash acorn',
    str_detect(Ingredients, 'apple') & !str_detect(Ingredients, 'juice|vinegar|butter|pine|wasabi') ~ 'apple',
    str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'sauce') ~ 'apple sauce',
    str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'pine') ~ 'apple juice',
    str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'dried') ~ 'apricot dried',
    str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'jam') ~ 'apricot jam',
    str_detect(Ingredients, 'artichoke') & str_detect(Ingredients, 'heart') & str_detect(Ingredients, 'drain|can') ~ 'artichoke heart canned',
    str_detect(Ingredients, 'asparagus') & str_detect(Ingredients, 'white') ~ 'asparagus white',
    str_detect(Ingredients, 'asparagus') & str_detect(Ingredients, 'bean') ~ 'bean green asparagus',
    str_detect(Ingredients, 'asparagus') ~ 'asparagus',
    str_detect(Ingredients, 'avocado') & !str_detect(Ingredients, 'wok') ~ 'avocado',
    
    str_detect(Ingredients, 'banan') & !str_detect(Ingredients, 'shallot') ~ 'banana',
    str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'yellow') ~ 'beetroot yellow',
    str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'cooked') ~ 'beetroot cooked',
    str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'root') ~ 'beetroot',
    str_detect(Ingredients, 'black curran') & !str_detect(Ingredients, 'juice') ~ 'black currant',
    str_detect(Ingredients, 'blueberr') ~ 'blueberries',
    str_detect(Ingredients, 'broccolini') ~ 'broccolini',
    str_detect(Ingredients, 'broccoli') ~ 'broccoli', #The 'broccoli peas' are broccoli florets according to the recipe site
    str_detect(Ingredients, 'brussel') & str_detect(Ingredients, 'sprout') ~ 'brussel sprout',
    str_detect(Ingredients, 'butternut') ~ 'winter squash butternut',
    
    str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'red') ~ 'cabbage red',
    str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'china') ~ 'cabbage china',
    str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'savoy') ~ 'cabbage savoy',
    str_detect(Ingredients, 'cabbage') & !str_detect(Ingredients, 'meat|root') ~ 'cabbage',
    str_detect(Ingredients, 'bok choi') ~ 'cabbage bok choi',
    str_detect(Ingredients, 'carrot|raw yellow') & !str_detect(Ingredients, 'paste|wok|mire') ~ 'carrot',
    str_detect(Ingredients, 'cauliflower') & !str_detect(Ingredients, 'butter') ~ 'cauliflower',
    str_detect(Ingredients, 'celery') & !str_detect(Ingredients, 'salt|soup') ~ 'celery', #Use celery for stangselleri
    str_detect(Ingredients, 'celeriac') & !str_detect(Ingredients, 'mire') ~ 'celariac root',
    str_detect(Ingredients, 'chard') & !str_detect(Ingredients, 'wine') ~ 'mangold',
    str_detect(Ingredients, 'cherry tomato') & str_detect(Ingredients, 'can') ~ 'cherry tomato canned',
    str_detect(Ingredients, 'cherry tomato') ~ 'cherry tomato',
    str_detect(Ingredients, 'cherry') & !str_detect(Ingredients, 'tomato') ~ 'cherries', #Name used in SHARP and Matvaretabellen
    str_detect(Ingredients, 'chicory') & str_detect(Ingredients, 'white') ~ 'chicory white',
    str_detect(Ingredients, 'chicory') & str_detect(Ingredients, 'red') ~ 'chicory red',
    str_detect(Ingredients, 'chicory') ~ 'chicory',
    str_detect(Ingredients, 'jalap') ~ 'chili pepper jalapeno',
    str_detect(Ingredients, 'chili|chile') & str_detect(Ingredients, 'green') ~ 'chili pepper green',
    ((str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'chili')) | (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'red|rød'))) & !str_detect(Ingredients, 'powder') |
      str_detect(Ingredients, 'mild chili') & !str_detect(Ingredients, 'sauce') | str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'chop') |
      str_detect(Ingredients, 'chili') & str_detect(Amounts, 'stk')  ~ 'chili pepper red',
    str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'dried') & !str_detect(Ingredients, 'flake') ~ 'chili pepper dried',
    str_detect(Ingredients, 'clemen') ~ 'clementine',
    str_detect(Ingredients, 'coconut') & !str_detect(Ingredients, 'milk|cream') ~ 'coconut',
    str_detect(Ingredients, 'minima|baby corn') ~ 'corn baby',
    str_detect(Ingredients, 'corn') & str_detect(Amounts, 'g') & str_detect(Ingredients, 'can') | str_detect(Ingredients, 'kernel') & str_detect(Ingredients, 'drained') ~ 'sweet corn kernels canned',
    str_detect(Ingredients, 'corn') & str_detect(Amounts, 'g') & !str_detect(Ingredients, 'starch|tortilla|oil') | str_detect(Ingredients, 'corn kernel') ~ 'sweet corn kernels',
    str_detect(Ingredients, 'corn') & str_detect(Amounts, 'stk') & !str_detect(Ingredients, 'pepper') ~ 'corn cob',
    str_detect(Ingredients, 'cranberr') & str_detect(Ingredients, 'jam') ~ 'cranberries jam',
    str_detect(Ingredients, 'cranberr') & !str_detect(Ingredients, 'sauce') ~ 'cranberries',
    str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'snake') ~ 'cucumber snake',
    str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'jam|pickle') ~ 'cucumber pickled',
    str_detect(Ingredients, 'cucumber') ~ 'cucumber',
    
    str_detect(Ingredients, 'eggplant') ~ 'eggplant',
    
    str_detect(Ingredients, 'fennel') & !str_detect(Ingredients, 'seed') ~ 'fennel',
    str_detect(Ingredients, 'fig') ~ 'fig',
    
    str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'chinese') ~ 'garlic chinese',
    str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'whole') & !str_detect(Ingredients, 'salt|powder') ~ 'whole garlic',
    str_detect(Ingredients, 'garlic') & !str_detect(Ingredients, 'pickle|sauce|paste|oil|baguette|cheese') ~ 'garlic',
    str_detect(Ingredients, 'grape') & str_detect(Ingredients, 'juice') ~ 'grape juice',
    str_detect(Ingredients, 'grape') ~ 'grape',
    
    str_detect(Ingredients, 'horseradish') & !str_detect(Ingredients, 'sauce') ~ 'horseradish',
    
    str_detect(Ingredients, 'jerusalem artichoke') ~ 'jerusalem artichoke',
    
    str_detect(Ingredients, 'kale') ~ 'kale',
    str_detect(Ingredients, 'kimchi') ~ 'kimchi',
    str_detect(Ingredients, 'kiwi') ~ 'kiwi',
    
    str_detect(Ingredients, 'leek') & !str_detect(Ingredients, 'mire|onion|tortilla') ~ 'leek',
    str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'grass') ~ 'lemongrass',
    str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'peel|zest') ~ 'lime, the juice and zest',
    str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'peel|sheel|zest') & !str_detect(Ingredients, 'juice') ~ 'lime, the zest',
    str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice|pressed') & !str_detect(Ingredients, 'peel') ~ 'lime, the juice',
    str_detect(Ingredients, 'lime') & str_detect(Amounts, 'tsp|tbsp|dl') ~ 'lime, the juice',
    str_detect(Ingredients, 'lime') & !str_detect(Ingredients, 'sheet|lemon|leaf') ~ 'lime',
    str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'peel|zest|shell') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice and zest',
    str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice',
    str_detect(Ingredients, 'lemon') & (str_detect(Amounts, 'tsp|tbsp|dl') | str_detect(Ingredients, 'drop')) & !str_detect(Ingredients, 'peel|shell|zest') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice',
    str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'shell|peel|zest') & !str_detect(Ingredients, 'pepper') | str_detect(Ingredients, 'sitronskall') ~ 'lemon, the zest',
    str_detect(Ingredients, 'of lime or lemon') ~ 'lemon', #Recipe has lemon in the name
    str_detect(Ingredients, 'lemon') & !str_detect(Ingredients, 'lime|balm') ~ 'lemon',
    str_detect(Ingredients, 'lychee') ~ 'lychee',
    
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'lettuce') ~ 'salad lamb lettuce',
    
    str_detect(Ingredients, 'mango') & !str_detect(Ingredients, 'chutney') ~ 'mango',
    
    str_detect(Ingredients, 'tamarind juice') ~ 'tamarind juice',
    str_detect(Ingredients, 'tomat') & (str_detect(Amounts, 'can|box|hp|drain') | str_detect(Ingredients, 'can|box')) & !str_detect(Ingredients, 'water|mackerel|beans|sauce') ~ 'tomato canned',
    str_detect(Ingredients, 'tomat') & !str_detect(Ingredients, 'canned|alsa|sauce|ketchup|canned|cherry|can|box|purée|puree|paste|mackerel|tube|vegetable|sun|beans|bunch') ~ 'tomato',
    str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'pur') ~ 'tomato puree',
    str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'bunch') ~ 'tomato bunch',
    str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'sun') ~ 'tomato sun dried',
    str_detect(Ingredients, 'ketchup') ~ 'tomato ketchup',
    str_detect(Ingredients, 'turnip') ~ 'turnip',
    
    str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'black') ~ 'olive black',
    str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'green') ~ 'olive green',
    str_detect(Ingredients, 'onion') & !str_detect(Ingredients, 'pickle|spring|green|pearl|leek|mire|garlic|powder|soup') ~ 'onion',
    str_detect(Ingredients, 'pearl onion') ~ 'pearl onion',
    str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'zest|peel|shell') ~ 'orange the juice and zest',
    str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'shell|zest|peel') ~ 'orange, the zest',
    str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') ~ 'orange juice',
    str_detect(Ingredients, 'orange') ~ 'orange',
    
    str_detect(Ingredients, 'parsley') & str_detect(Ingredients, 'root') ~ 'parsley root',
    str_detect(Ingredients, 'parsnip') ~ 'parsnip',
    str_detect(Ingredients, 'pea') & !str_detect(Ingredients, 'chick|broccoli|nut|sugar|asparagus|onion|pearl|horse|peach|dill') ~ 'peas green',
    str_detect(Ingredients, 'peach') ~ 'peach',
    str_detect(Ingredients, 'pear') & !str_detect(Ingredients, 'onion|barley') ~ 'pear',
    str_detect(Ingredients, 'pineapple') & str_detect(Ingredients, 'can') ~ 'pineapple canned',
    str_detect(Ingredients, 'pineapple') & str_detect(Ingredients, 'juice') ~ 'pineapple juice',
    str_detect(Ingredients, 'pineapple') ~ 'pineapple',
    str_detect(Ingredients, 'plantain') ~ 'plantain',
    str_detect(Ingredients, 'pomegranat') & str_detect(Ingredients, 'kernel') ~ 'pomegranate kernel',
    str_detect(Ingredients, 'pomegranat') ~ 'pomegranate',
    str_detect(Ingredients, 'potato') & !str_detect(Ingredients, 'rice|bread|sweet|mash|flour|mash') ~ 'potato',
    str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'mash') & !str_detect(Ingredients, 'cook|boil') ~ 'potato mash',
    str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'cook|boil') ~ 'potato boiled',
    str_detect(Ingredients, 'pumpkin') & !str_detect(Ingredients, 'seed|butternut') ~ 'winter squash pumpkin',
    str_detect(Ingredients, 'prune') ~ 'prune',
    
    str_detect(Ingredients, 'radish') & !str_detect(Ingredients, 'horse') ~ 'radish',
    str_detect(Ingredients, 'raisin') ~ 'raisin',
    str_detect(Ingredients, 'raspbe') ~ 'raspberries',
    str_detect(Ingredients, 'rhubarb') & str_detect(Ingredients, 'juice') ~ 'rhubarb juice',
    str_detect(Ingredients, 'rhubarb') ~ 'rhubarb',
    
    str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'heart') ~ 'salad heart',
    str_detect(Ingredients, 'ruccula|rocket salad|arugula|ruccola|peppery salad') ~ 'salad rocket',
    str_detect(Ingredients, 'lettuce') & !str_detect(Ingredients, 'lamb')  ~ 'salad lettuce',
    str_detect(Ingredients, 'salad') &str_detect(Ingredients, 'crispi') ~ 'salad crispi',
    str_detect(Ingredients, 'lollo rosso') ~ 'salad lollo rosso',
    str_detect(Ingredients, 'salad') & !str_detect(Ingredients, 'shrimp') ~ 'salad',
    str_detect(Ingredients, 'sauerkraut') ~ 'sauerkraut',
    str_detect(Ingredients, 'scallion|green onion|spring onion') ~ 'scallion',
    str_detect(Ingredients, 'shallot') ~ 'shallot',
    str_detect(Ingredients, 'sorrel') ~ 'sorrel',
    str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'baby') ~ 'spinach baby',
    str_detect(Ingredients, 'spinach') ~ 'spinach',
    str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'alfalfa') ~ 'sprouts alfalfa',
    str_detect(Ingredients, 'strawberr') ~ 'strawberries',
    str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'pea') ~ 'sugar snap pea',
    str_detect(Ingredients, 'swede') | (str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'root')) | str_detect(Ingredients, 'rutabaga') ~ 'swede',
    str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'green') ~ 'sweet pepper green',
    str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'yellow') ~ 'sweet pepper yellow',
    str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & !str_detect(Ingredients, 'grilled|pickled|powder|spice') ~ 'sweet pepper red',
    str_detect(Ingredients, 'pickled red pepper') ~ 'sweet pepper pickled',
    Country == "Norway" & str_detect(Ingredients, 'red pepper') ~ 'sweet pepper red',
    str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') ~ 'sweet pepper grilled',
    str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'potato') ~ 'sweet potato',
    str_detect(Ingredients, 'squash|zucchini') & !str_detect(Ingredients, 'butter|acorn') ~ 'summer squash zucchini', #Standard
    
    str_detect(Ingredients, 'watermelon') ~ 'watermelon',
    str_detect(Ingredients, 'water chestnut') ~ 'water chestnut',
    
    #Dairy / substitutes----
    str_detect(Ingredients, 'ghee|clarified butter') ~ 'butter clarified ghee',
    str_detect(Ingredients, 'butter|smør') & str_detect(Ingredients, 'unsalted|usalted') ~ 'unsalted butter',
    str_detect(Ingredients, 'butter|smør') & !str_detect(Ingredients, 'frying|peanut|dough|unsalted|browning|brushing|pepper|sour cream|roasting|oil|butternut|pastry|greasing|milk|beans') ~ 'butter',
    str_detect(Ingredients, 'butter') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') & !str_detect(Ingredients, 'oil') ~ 'butter for cooking',
    str_detect(Ingredients, 'buttermilk') & !str_detect(Ingredients, 'dough') ~ 'buttermilk',
    
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'norman') ~ 'cheese blue normanna', #Tine cheese
    str_detect(Ingredients, 'norzola') ~ 'cheese blue norzola', 
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'blue') ~ 'cheese blue',
    str_detect(Ingredients, 'brie') ~ 'cheese brie',
    str_detect(Ingredients, 'camembert') ~ 'cheese camembert',
    str_detect(Ingredients, 'real goat cheese') ~ 'goat brown cheese',
    str_detect(Ingredients, 'cheddar') ~ 'cheese cheddar',
    str_detect(Ingredients, 'garlic cheese') ~ 'cheese garlic',
    str_detect(Ingredients, 'chevre') ~ 'cheese goat chevre white',
    str_detect(Ingredients, 'feta|fat cheese in cubes|semi-solid cheese in cubes') & str_detect(Ingredients, 'cheese') ~ 'cheese feta', #Fat cheese is a translation error
    str_detect(Ingredients, 'halloumi') ~ 'cheese halloumi',
    str_detect(Ingredients, 'jarlsberg') ~ 'cheese jarlsberg',
    str_detect(Ingredients, 'manchego') ~ 'cheese manchego',
    str_detect(Ingredients, 'mascarpone') ~ 'cheese mascarpone',
    str_detect(Ingredients, 'mozzarella') ~ 'cheese mozzarella',
    str_detect(Ingredients, 'norvegia') ~ 'cheese norvegia',
    str_detect(Ingredients, 'port salut') ~ 'cheese port salut',
    str_detect(Ingredients, 'ricotta') ~ 'cheese ricotta salata',
    str_detect(Ingredients, 'romano') & str_detect(Ingredients, 'cheese') ~ 'cheese romano',
    str_detect(Ingredients, 'american cheese') ~ 'cheese american',
    str_detect(Ingredients, 'swiss') & str_detect(Ingredients, 'cheese') ~ 'cheese swiss',
    str_detect(Ingredients, 'provolone') & str_detect(Ingredients, 'cheese') ~ 'cheese provolone',
    str_detect(Ingredients, 'monterey jack|ppperjack') & str_detect(Ingredients, 'cheese') ~ 'cheese monterey jack',
    str_detect(Ingredients, 'neufchatel') & str_detect(Ingredients, 'cheese') ~ 'cheese neufchatel',
    str_detect(Ingredients, 'asiago') & str_detect(Ingredients, 'cheese') ~ 'cheese asiago',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'hard') ~ 'cheese hard goat',
    str_detect(Ingredients, 'snøfrisk') ~ 'cheese cream goat snøfrisk',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cream') | str_detect(Ingredients, 'kremgo') ~ 'cheese cream',
    (str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'skinny|low fat|lean')) | str_detect(Ingredients, 'paneer cheese') ~ 'cheese cottage low fat', #Paneer is a cheese like low fat cc
    str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'cheese') ~ 'cheese cottage',
    str_detect(Ingredients, 'parmesan') ~ 'parmesan cheese',
    str_detect(Ingredients, 'cheese') & !str_detect(Ingredients, 'yogurt|yoghurt') ~ 'cheese semi-hard',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'whip|heavy') ~ 'cream whipped 37 %',
    str_detect(Ingredients, 'cream') & (str_detect(Ingredients, 'food') | !str_detect(Ingredients, 'cheese|sour|cracker|sauce|coconut|light|condensed')) ~ 'cream household 18 %', #Standard
    str_detect(Ingredients, 'crème fraîche 18 %') ~ 'crème fraîche 18 %',
    str_detect(Ingredients, 'crème fraîche 10 %') ~ 'crème fraîche 10 %',
    str_detect(Ingredients, 'crème fraîche') ~ 'crème fraîche 35 %', #The original
    
    str_detect(Ingredients, 'kefir') ~ 'kefir',
    str_detect(Ingredients, 'kesam') & str_detect(Ingredients, 'low fat|1 %') ~ 'quark, 1 %',
    str_detect(Ingredients, 'kesam') ~ 'quark, 7 %',
    
    str_detect(Ingredients, 'margarin') & str_detect(Ingredients, 'frying') ~ 'margarine for cooking',
    str_detect(Ingredients, 'margarin') ~ 'margarine',
    str_detect(Ingredients, 'milk|tinemelk') & !str_detect(Ingredients, 'whole|full-fat|coconut|butter|extra|almond') ~ 'milk 1 %', #Standard
    str_detect(Ingredients, 'milk|melk') & str_detect(Ingredients, 'whole|full-fat') ~ 'whole milk 3.5 %',
    str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'extra light|skim milk') ~ 'milk 0.1 %',
    str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'evaporated') ~ 'milk evaporated',
    str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'coconut') ~ 'milk coconut',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'coconut') ~ 'milk coconut cream full fat',
    
    str_detect(Ingredients, 'lightroom 10 %|light flow 10 %') ~ 'sour cream 10 %',
    str_detect(Ingredients, 'lightroom 18 %|light stream|light flow 18%|lightroom 18%|light flow 18 %') | (str_detect(Ingredients, 'sour cream') & !str_detect(Ingredients, '%')) | (str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'light') & !str_detect(Ingredients, 'alternativ')) ~ 'sour cream 18 %', #Standard
    str_detect(Ingredients, 'seatroom|seat cream') ~ 'sour cream 35 %',
    
    str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'greek') ~ 'yogurt greek',
    str_detect(Ingredients, 'yogurt|yoghurt') ~ 'yoghurt',
   
    #Herbs and spices----
    str_detect(Ingredients, 'adobo seasoning') ~ 'adobo seasoning',
    str_detect(Ingredients, 'allspice|of all kinds') ~ 'allspice',
    str_detect(Ingredients, 'anis') & !str_detect(Ingredients, 'star') ~ 'anise ground',
    
    str_detect(Ingredients, 'basil') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'thyme|rosemary|oregano|parsley|chives|dill') ~ 'basil fresh herbs',
    str_detect(Ingredients, 'thaibasil') ~ 'basil fresh herbs',
    str_detect(Ingredients, 'basil') & str_detect(Ingredients, 'dried') ~ 'basil dried',
    str_detect(Ingredients, 'bay leaf') ~ 'bay leaf',
    str_detect(Ingredients, 'burrito spice') ~ 'burrito spice mix',
    
    str_detect(Ingredients, 'cajun') & str_detect(Ingredients, 'spice') ~ 'cajun spice',
    str_detect(Ingredients, 'caraway') ~ 'caraway seed',
    str_detect(Ingredients, 'cardamom') & str_detect(Ingredients, 'fruit|pod') ~ 'cardamom pod',
    str_detect(Ingredients, 'cardamom') ~ 'cardamom',
    str_detect(Ingredients, 'chervil') & !str_detect(Ingredients, 'parsley|rosemary|thyme|mint|basil|oregano|dill|coriander|tarragon') ~ 'chervil fresh herb', #All chives are fresh
    str_detect(Ingredients, 'chives') & !str_detect(Ingredients, 'parsley|rosemary|thyme|mint|basil|oregano|dill|coriander') ~ 'chives fresh herb', #All chives are fresh
    (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'dried') & str_detect(Ingredients, 'flake')) | str_detect(Ingredients, 'red pepper flate') | str_detect(Ingredients, 'chili flake') ~ 'chili flake dried',
    str_detect(Ingredients, 'chinese') & str_detect(Ingredients, 'spice') ~ 'chinese five spice',
    str_detect(Ingredients, 'cinnamon') & str_detect(Ingredients, 'bar|rod') ~ 'cinnamon bar',
    str_detect(Ingredients, 'cinnamon') ~ 'cinnamon',
    str_detect(Ingredients, 'cloves|carnation') & !str_detect(Ingredients, 'garlic') ~ 'cloves',
    str_detect(Ingredients, 'coriander|cilantro') & !str_detect(Ingredients, 'seed') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf|malt') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|thyme|oregano|basil|chives|mint|dill') ~ 'coriander fresh herbs',
    str_detect(Ingredients, 'coriander|cilantro') & str_detect(Ingredients, 'dried') ~ 'coriander dried',
    str_detect(Ingredients, 'coriander') & str_detect(Ingredients, 'seed') ~ 'coriander seed',
    str_detect(Ingredients, 'cress') ~ 'cress fresh herbs',
    str_detect(Ingredients, 'cumin') ~ 'cumin',
    str_detect(Ingredients, 'curry') ~ 'curry powder',
    
    str_detect(Ingredients, 'dill') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|thyme|oregano|basil|chives|mint|coriander') ~ 'dill fresh herbs',
    str_detect(Ingredients, 'dill') & str_detect(Ingredients, 'dried') ~ 'dill dried',
    
    str_detect(Ingredients, 'fajita') & str_detect(Ingredients, 'spice') ~ 'fajita spice mix',
    str_detect(Ingredients, 'fennel') & str_detect(Ingredients, 'seed') ~ 'fennel seed',
    str_detect(Ingredients, 'fenugreek leaf') & str_detect(Ingredients, 'dried') ~ 'fenugreek leaf dried',
    str_detect(Ingredients, 'fenugreek seed') ~ 'fenugreek seed',
    
    str_detect(Ingredients, 'garam') ~ 'garam masala',
    str_detect(Ingredients, 'pav bhaji masala') ~ 'pav bhaji masala',
    Ingredients == 'italian seasoning' ~ 'italian seasoning',
    str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'powder') ~ 'garlic powder',
    str_detect(Ingredients, 'ginger') & (str_detect(Ingredients, 'fresh|grated|chopped') | str_detect(Amounts, 'cm')) ~ 'fresh herbs ginger',
    str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'pickle') ~ 'ginger pickled',
    str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'paste') ~ 'paste ginger',
    str_detect(Ingredients, 'ginger') ~ 'dried ginger',
    str_detect(Ingredients, 'zedoari') ~ 'ginger zedoari', #In the same family
    str_detect(Ingredients, 'guacamole spice') ~ 'guacamole spice mix',
    
    str_detect(Ingredients, 'juniper') ~ 'juniper berry',
    
    str_detect(Ingredients, 'lemon balm') ~ 'lemon balm',
    
    str_detect(Ingredients, 'mint') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|thyme|oregano|basil|chives|dill|coriander') ~ 'mint fresh herbs',
    str_detect(Ingredients, 'mint') & str_detect(Ingredients, 'dried') ~ 'mint dried',
    
    str_detect(Ingredients, 'nutmeg') ~ 'nutmeg',
    
    str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'powder') ~ 'onion powder',
    str_detect(Ingredients, 'oregano') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|thyme|mint|basil|chives|dill|coriander') ~ 'oregano fresh herbs',
    str_detect(Ingredients, 'oregano') & str_detect(Ingredients, 'dried|spice') ~ 'oregano dried',
    str_detect(Ingredients, 'oregano') & !str_detect(Ingredients, 'parsley|rosemary|thyme|mint|basil|chives|dill|coriander') ~ 'oregano dried', #Standard
    
    str_detect(Ingredients, 'paprika|pepper') & str_detect(Ingredients, 'powder|spice') & !str_detect(Ingredients, 'spice seasoning pepper') ~ 'paprika powder',
    str_detect(Ingredients, 'parsley|mug') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'thyme|rosemary|oregano|mint|basil|chives|dill|coriander') ~ 'parsley fresh herbs',
    str_detect(Ingredients, 'parsley') & str_detect(Ingredients, 'dried') ~ 'parsley dried',
    
    str_detect(Ingredients, 'rosemary') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'thyme|basil|oregano|parsley') ~ 'rosemary fresh herbs',
    str_detect(Ingredients, 'rosemary') & str_detect(Ingredients, 'dried') ~ 'rosemary dried',
    
    str_detect(Ingredients, 'saffron') ~ 'saffron',
    str_detect(Ingredients, 'sage') & !str_detect(Ingredients, 'sau') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|thyme|oregano|basil|chives|dill|coriander') ~ 'sage fresh herbs',
    str_detect(Ingredients, 'sage') & !str_detect(Ingredients, 'sau') ~ 'sage dried',
    str_detect(Ingredients, 'salvie') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'thyme|basil|oregano|parsley') ~ 'salvie fresh herbs',
    str_detect(Ingredients, 'sazon seasoning') ~ 'sazon seasoning',
    str_detect(Ingredients, 'star anis') ~ 'star anise',
    
    str_detect(Ingredients, 'taco') & str_detect(Ingredients, 'spice|season') ~ 'taco spice mix',
    str_detect(Ingredients, 'tandoori') ~ 'tandoori spice mix',
    str_detect(Ingredients, 'tarragon') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|thyme|chervil|mint|basil|chives|dill|coriander') ~ 'tarragon fresh herbs',
    str_detect(Ingredients, 'tarragon') & str_detect(Ingredients, 'dried') ~ 'tarragon dried',
    str_detect(Ingredients, 'tarragon') & !str_detect(Ingredients, 'parsley|rosemary|thyme|chervil|mint|basil|chives|dill|coriander') ~ 'tarragon dried', #Standard
    str_detect(Ingredients, 'thyme') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) & !str_detect(Ingredients, 'parsley|rosemary|oregano|mint|basil|chives|dill|coriander') ~ 'thyme fresh herbs',
    str_detect(Ingredients, 'thyme') & str_detect(Ingredients, 'dried') ~ 'thyme dried',
    str_detect(Ingredients, 'turmeric') ~ 'turmeric',
    
    #Mushrooms----
    str_detect(Ingredients, 'aroma') & str_detect(Ingredients, 'champignon|mushroom|soup') ~ 'mushroom aroma champignon',
    str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'porcini') & str_detect(Ingredients, 'dried') ~ 'mushroom porcini dried',
    str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'dry|dried') ~ 'mushroom dried',
    str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'shiitake|shitake') ~ 'mushroom shiitake',
    str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'shimeji') ~ 'mushroom shimeji',
    str_detect(Ingredients, 'portebello') ~ 'mushroom portebello',
    str_detect(Ingredients, 'chanterelle') ~ 'mushroom chanterelle',
    str_detect(Ingredients, 'champig') ~ 'mushroom champignon',
    str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'chestnut') ~ 'mushroom chestnut',
    str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'drained') ~ 'mushroom canned',
    str_detect(Ingredients, 'mushroom') & !str_detect(Ingredients, 'rice|condensed') ~ 'mushroom',
    
    #Flours, grains/nuts/legumes----
    str_detect(Ingredients, 'almond') & !str_detect(Ingredients, 'potato') ~ 'almond',
    
    str_detect(Ingredients, 'bagel') ~ 'rolls white bagel',
    str_detect(Ingredients, 'rolls|poppyseed hot dog bun') ~ 'rolls white',
    str_detect(Ingredients, 'baguette') & str_detect(Ingredients, 'garlic') ~ 'rolls white baguette garlic',
    str_detect(Ingredients, 'baguette') ~ 'rolls white baguette',
    str_detect(Ingredients, 'canned chickpeas or mixed beans|garbanzo beans, drained') ~ 'chick pea canned', #Chickpeas are in the name of the recipe
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'sprout') ~ 'bean sprout',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'black') &
      (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean black canned',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|navy|cannellini|butter') &
      (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean white canned',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'kidney|red') &
      (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean kidney canned',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'black') ~ 'bean black',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|navy|cannellini|butter') ~ 'bean white',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'kidney|red') ~ 'bean kidney',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'green|french|break') ~ 'bean green',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'horse|broad|fava|brew') ~ 'bean broad',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'tomat') ~ 'bean white tomato',
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'can') ~ 'bean canned',
    (str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crumb|grate') & str_detect(Ingredients, 'white')) | str_detect(Ingredients, 'grilling flour') & str_detect(Ingredients, 'white') ~ 'bread crumb white',
    (str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crumb|grate')) | str_detect(Ingredients, 'grilling flour') ~ 'bread crumb',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'stick') ~ 'breadstick',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crisp') & str_detect(Ingredients, 'coarse') ~ 'crisp bread coarse',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crisp') ~ 'crisp bread',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'coarse') ~ 'bread coarse',
    str_detect(Ingredients, 'chapati') ~ 'bread brown chapati',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'white') ~ 'bread white',
    str_detect(Ingredients, 'flatbread') ~ 'bread flat hard',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'nan|naan') ~ 'bread naan',
    str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'nan|naan') ~ 'bread crisp',
    str_detect(Ingredients, 'bread') & !str_detect(Ingredients, 'flat|hamburger|rolls|pita') ~ 'bread',
    str_detect(Ingredients, 'bulgur|bulgar') ~ 'bulgur wheat',
    
    str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'nut') ~ 'cashew nut salt',
    str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'roast') ~ 'cashew nut roasted',
    str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'nut') ~ 'cashew nut',
    str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'chick') &
      (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'chick pea canned',
    str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'chick') ~ 'chick pea flour',
    str_detect(Ingredients, 'chickpea|chick pea') & !str_detect(Ingredients, 'lentil') ~ 'chick pea',
    str_detect(Ingredients, 'ciabatta') ~ 'ciabatta',
    (str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'flour|starch')) | str_detect(Ingredients, 'corn') & !str_detect(Ingredients, 'oil|pepper|crispy|cob|minim|coat|water to the') & str_detect(Amounts, 'tbsp|tsp') ~ 'corn starch',
    str_detect(Ingredients, 'polenta') ~ 'corn flour polenta',
    str_detect(Ingredients, 'cous') ~ 'couscous',
    str_detect(Ingredients, 'cream cracker') ~ 'cracker cream',
    
    str_detect(Ingredients, 'hamburger') & str_detect(Ingredients, 'bread|bun') ~ 'hamburger bun',
    str_detect(Ingredients, 'hazelnut') & !str_detect(Ingredients, 'oil') ~ 'hazelnut',
    
    str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'red') ~ 'lentil red',
    str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'green') ~ 'lentil green',
    str_detect(Ingredients, 'lentil') & str_detect(Amounts, 'box|can') ~ 'lentil canned',
    str_detect(Ingredients, 'lentil') ~ 'lentil',
    
    str_detect(Ingredients, 'nacho') ~ 'nacho',
    
    str_detect(Ingredients, 'oatmeal') ~ 'oatmeal',
    
    str_detect(Ingredients, 'pasta|paste|spagetti|spaghetti') & str_detect(Ingredients, 'whole') ~ 'pasta whole grain',
    str_detect(Ingredients, 'lasagna|lasagne') & str_detect(Ingredients, 'plate|sheet') ~ 'lasagna plate pasta',
    str_detect(Ingredients, 'pasta|spagetti|spaghetti|tagli|pens|macaroni') & !str_detect(Ingredients, 'lasagna') & str_detect(Ingredients, '\\bcooked') ~ 'pasta cooked',
    str_detect(Ingredients, 'pasta|spagetti|spaghetti|tagli|pens|macaroni') & !str_detect(Ingredients, 'lasagna') & !str_detect(Ingredients, 'sauce') ~ 'pasta',
    str_detect(Ingredients, 'lasagna noodles') ~ 'pasta',
    str_detect(Ingredients, 'barley') ~ 'pearl barley',
    str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'butter') ~ 'peanut butter',
    str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'oil') & !str_detect(Ingredients, 'sunflower|rapeseed') ~ 'peanut oil',
    str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'salt') ~ 'peanut salt',
    str_detect(Ingredients, 'peanut') & !str_detect(Ingredients, 'oil') ~ 'peanut',
    str_detect(Ingredients, 'pie dough') ~ 'pie dough',
    str_detect(Ingredients, 'pine') & str_detect(Ingredients, 'nut|seed|kernel') & !str_detect(Ingredients, 'apple') ~ 'pine nut',
    str_detect(Ingredients, 'pistachio') ~ 'pistachio nut',
    str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'flour') ~ 'potato starch',
    str_detect(Ingredients, 'lompe') ~ 'potato flatbread lompe',
    str_detect(Ingredients, 'puff pastry|butter dough') ~ 'puff pastry',
    str_detect(Ingredients, 'pumpkin seed') ~ 'pumpkin seed',
    
    str_detect(Ingredients, 'quinoa') ~ 'quinoa',
    
    str_detect(Ingredients, 'rice|ris') & str_detect(Ingredients, 'basmati') ~ 'rice basmati',
    str_detect(Ingredients, 'rice|ris') & str_detect(Ingredients, 'risotto|arbori|paella') | str_detect(Ingredients, 'vialone nano') ~ 'rice risotto',
    str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'jasmin') ~ 'rice jasmin',
    str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked') ~ 'rice cooked',
    str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'whole') ~ 'rice brown long grain',
    str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'sushi') ~ 'rice sushi',
    str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'noodle') ~ 'rice noodle',
    str_detect(Ingredients, 'rice') & !str_detect(Ingredients, 'beef|potato|vinegar|wine') ~ 'rice white long grain',
    
    str_detect(Ingredients, 'sesame') & str_detect(Ingredients, 'seed') ~ 'sesame seed',
    str_detect(Ingredients, 'shortcrust pastry') ~ 'shop-bought shortcrust pastry',
    
    str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'whole') ~ 'tortilla coarse',
    str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'corn') ~ 'tortilla corn',
    str_detect(Ingredients, 'tortilla|wraps') & !str_detect(Ingredients, 'pita') ~ 'tortilla',
    
    str_detect(Ingredients, 'walnut') & !str_detect(Ingredients, 'oil') ~ 'walnut',
    str_detect(Ingredients, 'wheat flour|all-purpose flour|plain flour|flour|durum wheat') & !str_detect(Ingredients, 'whole|tortilla|potato|corn') ~ 'wheat flour',
    
    #Poultry----
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'breast|fillet|filet') & str_detect(Ingredients, 'without|skinless|no skin') & str_detect(Ingredients, 'cooked') & !str_detect(Amounts, 'stk') ~ 'chicken breast without skin cooked',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'breast|fillet|filet') & str_detect(Ingredients, 'without|skinless|no skin') ~ 'chicken breast without skin',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'breast|fillet|filet') & str_detect(Ingredients, 'cooked') & !str_detect(Amounts, 'stk') ~ 'chicken breast cooked',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'breast|fillet|filet') ~ 'chicken breast',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'thigh|leg') & str_detect(Ingredients, 'without|skinless|no skin') ~ 'chicken thigh without skin',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'thigh|leg') & str_detect(Ingredients, 'grilled|cooked') & !str_detect(Amounts, 'stk') ~ 'chicken thigh cooked', #This is 
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'thigh|leg') ~ 'chicken thigh',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'grilled|cooked') & !str_detect(Amounts, 'stk') ~ 'chicken cooked',
    str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'drum') ~ 'chicken drumstick',
    str_detect(Ingredients, 'chicken') & !str_detect(Ingredients, 'power|condensed|broth|stock') ~ 'chicken whole',
    
    str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'breast') ~ 'duck breast',
    str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'leg') ~ 'duck leg',
    
    str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'boil|hard cook|hard-cook') & !str_detect(Amounts, 'stk') ~ 'egg boiled',
    str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'noodle') ~ 'egg noodle',
    str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'white') ~ 'egg white',
    str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'yolk') ~ 'egg yolk',
    str_detect(Ingredients, 'egg') & !str_detect(Ingredients, 'plant') ~ 'egg',
    
    str_detect(Ingredients, 'grouse') ~ 'hen breast fillet grouse',
    
    str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'ground') ~ 'turkey minced meat',
    str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'breast') ~ 'turkey breast',
    str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'club') ~ 'turkey drumstick chicken', #Add chicken to use to calculate nutrition values
    str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'ham') ~ 'turkey ham canned',
    str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'grill') ~ 'sausage turkey chicken', #Prior turkey chicken grill sausage
    str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'cooked') ~ 'turkey meat cooked',
    str_detect(Ingredients, 'turkey') & !str_detect(Ingredients, 'broth|stock|fund') ~ 'whole turkey',
    
    #Meat----
    
    str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') & str_detect(Ingredients, 'cooked') ~ 'bacon cooked',
    str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') ~ 'bacon',
    str_detect(Ingredients, 'lard') ~ 'lard pork fat',
    str_detect(Ingredients, 'bankekjøtt|beef round roast|bottom round roast') ~ 'beef bottom round',
    str_detect(Ingredients, 'roast beef') ~ 'beef bottom round roast beef',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'sirloin') ~ 'beef sirloin',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'comb|entre') | str_detect(Ingredients, 'entrecote|cote de boef|scotch or black welsh beef|standing rib roast, bone in') | `Selected Meals` == 'Steak with potato salad' & Ingredients == 'steak' ~ 'beef rib-eye steak',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'tenderloin|fillet') & !str_detect(Ingredients, 'outer') ~ 'beef tenderloin',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'rump') | str_detect(Ingredients, 'top round steak') ~ 'beef roast of knuckle',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'high back|high-roast|stew meat') | str_detect(Ingredients, 'chuck steak|cubed beef') | Ingredients == 'of beef' ~ 'beef chuck roll',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shoulder') | str_detect(Ingredients, 'shin of beef') ~ 'beef shoulder', #Shoulder and shin are similar
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'brisket|short rib') ~ 'beef brisket',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'tongue') ~ 'beef tongue',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'bone') ~ 'beef bones',
    str_detect(Ingredients, 'hanger steak|flank steak') ~ 'beef flank steak', #Cut from the same are of the animal
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'tongue') ~ 'beef tongue',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'flat') & !str_detect(Ingredients, 'elk|deer') ~ 'beef topside', #Also some veal meat
    str_detect(Ingredients, 'beef|angus') & str_detect(Ingredients, 'outer') ~ 'beef striploin',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince') & str_detect(Ingredients, 'lean') ~ 'beef minced meat 6 %',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince|all-beef hot dog') ~ 'beef minced meat',
    str_detect(Ingredients, 'minced meat') & !str_detect(Ingredients, 'pork|deer') ~ 'beef minced meat', #Standard
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shredded') | str_detect(Ingredients, 'steak strips') | `Selected Meals` == 'Beef quesadillas' & str_detect(Ingredients, 'steak') ~ 'beef sirloin butt',
    str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince') | str_detect(Ingredients, 'ground') & str_detect(Ingredients, 'meat') & !str_detect(Ingredients, 'pork|turkey|deer') | str_detect(Ingredients, 'meat dough') ~ 'beef minced meat',
    str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'liver') ~ 'beef calf liver',
    str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'leg') ~ 'beef calf shoulder', #Actually hind leg but not in database,
    str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'steak') ~ 'beef veal for roast',
    
    str_detect(Ingredients, 'deer') & str_detect(Ingredients, 'ground') & !str_detect(Ingredients, 'rein|rain') ~ 'roe deer minced meat',
    
    str_detect(Ingredients, 'elk') & str_detect(Ingredients, 'flat') ~ 'elk moose inside round',
    str_detect(Ingredients, 'elk') & str_detect(Ingredients, 'thigh') ~ 'elk shoulder',
    
    str_detect(Ingredients, 'game meat with bone') ~ 'game beef elk shoulder',
    
    str_detect(Ingredients, 'ham') & str_detect(Ingredients, 'cured') | str_detect(Ingredients, 'pancetta|prosciutto') ~ 'ham cured',
    str_detect(Ingredients, 'ham') & str_detect(Ingredients, 'smoked') ~ 'ham smoked',
    str_detect(Ingredients, 'ham') & !str_detect(Ingredients, 'bacon|tenderloin|hamburger|champignon|pork from|turkey|steak|bone') ~ 'ham',
    
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'shoulder|in slice|neck|with bone') ~ 'lamb shoulder', #Shoulder and neck meat can be interchanged
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'rib') ~ 'lamb cured rib',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') & str_detect(Ingredients, 'smoke') ~ 'lamb leg smoked',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') ~ 'lamb leg roast',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'stew|pot') ~ 'lamb stew meat',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'shank') ~ 'lamb shank',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'carree') ~ 'lamb hind saddle',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'chop') ~ 'lamb chop',
    str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'cooked') ~ 'lamb shoulder cooked', #Default
    str_detect(Ingredients, 'lamb') & !str_detect(Ingredients, 'power') ~ 'lamb shoulder', #Default
    str_detect(Ingredients, 'sheep cabbage meat') ~ 'lamb sheep cabbage stew meat',
    str_detect(Ingredients, 'sheep head') ~ 'lamb sheep head',
    
    str_detect(Ingredients, 'meatball') ~ 'meatball',
    
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'butt') ~ 'pork shoulder',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ground|minced') ~ 'pork minced meat',
    str_detect(Ingredients, 'neck') & str_detect(Ingredients, 'chop') ~ 'pork neck chop',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'chop') ~ 'pork chop',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'neck') ~ 'pork neck',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'tenderloin|fillet|escalope') | Ingredients == 'thinly sliced pork loin' ~ 'pork tenderloin',
    Ingredients == 'cold roast pork loin' ~ 'pork tenderloin cooked',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'shred') | `Selected Meals` == 'Satay of pigs with honey and ginger' & str_detect(Ingredients, 'pork') | Ingredients == 'pork steak, cut into strips' ~ 'pork inside round', #What is used to create shredded pork in Norway
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'hock') ~ 'pork hock',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'stew|shoulder') ~ 'pork shoulder', 
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'pulled') ~ 'pork shoulder cooked', 
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'salt') ~ 'pork shoulder salt', 
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'kidney') ~ 'pork kidney', 
    str_detect(Ingredients, 'pig') & str_detect(Ingredients, 'liver') ~ 'pork liver',
    str_detect(Ingredients, 'pork belly') | str_detect(`Selected Meals`, 'Ribbe|ribbe') & str_detect(Ingredients, 'rib') ~ 'pork belly',
    str_detect(Ingredients, 'pork rib roast') ~ 'pork rib roast',
    str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ham') | str_detect(Ingredients, 'ham steak') ~ 'pork ham roast',
    str_detect(Ingredients, 'bone-in ham') ~ 'pork ham roast bone in',
    str_detect(Ingredients, 'spare rib') & !str_detect(Ingredients, 'beef') | `Selected Meals` == 'Juicy ribs with homemade tortillas' & Ingredients == 'ribs' | str_detect(Ingredients, 'pork sparerib') ~ 'pork spare rib',
    
    str_detect(Ingredients, 'rabbit') ~ 'rabbit',
    str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'fillet') ~ 'reindeer tenderloin',
    str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'pot meat') ~ 'reindeer chuck roll',
    str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'flat') ~ 'reindeer inside round',
    str_detect(Ingredients, 'reindeer') ~ 'reindeer',
    
    str_detect(Ingredients, 'salami') ~ 'salami',
    str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'chorizo') | str_detect(Ingredients, 'chorizo') ~ 'sausage chorizo',
    str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'vossa') ~ 'sausage vossa',
    str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'chipolata') ~ 'sausage chipolata',
    str_detect(Ingredients, 'sausage') & !str_detect(Ingredients, 'mustard|sauce') ~ 'sausage',
    
    #Seafood----
    str_detect(Ingredients, 'anchovy fillet|sardines or anchovies') ~ 'anchovy fillet',
    str_detect(Ingredients, 'anchovies') ~ 'anchovy canned',
    str_detect(Ingredients, 'angler fish|anglerfish') ~ 'anglerfish',
    
    str_detect(Ingredients, 'catfish') ~ 'catfish',
    str_detect(Ingredients, 'ishavsrøye') ~ 'char ishavsrøye fatty fish',
    str_detect(Ingredients, 'clam') ~ 'clam',
    str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'lutefisk') ~ 'cod lutefisk',
    str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'clip') ~ 'cod clipfish',
    str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'cooked') ~ 'cod cooked',
    str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'dried|dry') ~ 'cod dried',
    str_detect(Ingredients, 'fish fillet|firm white fish|different fillets of white fish|optional fish without skin and bone') & !str_detect(Ingredients, 'angler|cat|cod|pollock') ~ 'cod',
    str_detect(Ingredients, 'cod') ~ 'cod fillet',
    str_detect(Ingredients, 'crab') & str_detect(Ingredients, 'shell') ~ 'crab shell',
    str_detect(Ingredients, 'crab') ~ 'crab',
    
    str_detect(Ingredients, 'fish cake') & str_detect(Ingredients, 'coarse') ~ 'fish cake coarse',
    str_detect(Ingredients, 'fish, head, back bone') ~ 'fish scraps for broth',
    
    str_detect(Ingredients, 'grouper') ~ 'grouper',
    
    str_detect(Ingredients, 'haddock') ~ 'haddock',
    str_detect(Ingredients, 'halibut') ~ 'halibut',
    str_detect(Ingredients, 'herring') & str_detect(Ingredients, 'smoked') ~ 'herring smoked',
    str_detect(Ingredients, 'herring') ~ 'herring',
    
    str_detect(Ingredients, 'lobster') & !str_detect(Ingredients, 'shell') ~ 'lobster',
    
    str_detect(Ingredients, 'mackerel') & str_detect(Ingredients, 'tomato') ~ 'mackerel tomato canned',
    str_detect(Ingredients, 'mackerel') ~ 'mackerel',
    str_detect(Ingredients, 'mussels') & !str_detect(Ingredients, 'power') ~ 'mussels',
    
    str_detect(Ingredients, 'oyster') & !str_detect(Ingredients, 'sauce') ~ 'oyster',
    
    str_detect(Ingredients, 'pollock') & str_detect(Ingredients, 'smoked') ~ 'pollock smoked',
    str_detect(Ingredients, 'pollock|pollack|chop fillet, without skins and bones') ~ 'pollock',
    str_detect(Ingredients, 'prawn') ~ 'prawn',
    
    str_detect(Ingredients, 'redfish') ~ 'redfish',
    
    str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'smoked') ~ 'salmon smoked',
    str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'roe') ~ 'salmon roe',
    str_detect(Ingredients, 'salmon') ~ 'salmon',
    str_detect(Ingredients, 'sandshell') ~ 'sandshell',
    str_detect(Ingredients, 'sardine') ~ 'sardine',
    str_detect(Ingredients, 'scallop') ~ 'scallop',
    str_detect(Ingredients, 'scampi') ~ 'scampi',
    str_detect(Ingredients, 'sea bass') ~ 'sea bass',
    str_detect(Ingredients, 'sea urchin') ~ 'sea urchin',
    str_detect(Ingredients, 'shellfish') & !str_detect(Ingredients, 'borth|stock') ~ 'shellfish',
    str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, 'lake') ~ 'shrimp in brine',
    str_detect(Ingredients, 'shrimp') & !str_detect(Ingredients, 'paste|salad|shellfish') ~ 'shrimp',
    str_detect(Ingredients, 'squid') & !str_detect(Ingredients, 'honey') ~ 'squid',
    
    str_detect(Ingredients, 'trout') & str_detect(Ingredients, 'cured') ~ 'cured trout',
    str_detect(Ingredients, 'trout') ~ 'trout',
    str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'oil') ~ 'tuna in oil canned',
    str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'water|drained') ~ 'tuna in water canned',
    str_detect(Ingredients, 'tuna') ~ 'tuna',
    
    #Oils----
    str_detect(Ingredients, 'canola oil|rapeseed oil') & !str_detect(Ingredients, 'frying|olive|corn') ~ 'rapeseed oil',
    str_detect(Ingredients, 'canola oil|rapeseed oil') & str_detect(Ingredients, 'frying') & !str_detect(Ingredients, 'corn') ~ 'rapeseed oil for cooking',
    str_detect(Ingredients, 'coconut oil') ~ 'coconut oil',
    str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'oil') & !str_detect(Ingredients, 'rapeseed') ~ 'oil corn',
    
    str_detect(Ingredients, 'olive oil|olivenolje|extra-virgin olive') & !str_detect(Ingredients, 'canola|frying|butter') ~ 'olive oil',
    str_detect(Ingredients, 'olive oil') & str_detect(Ingredients, 'frying') ~ 'olive oil for cooking',
    
    str_detect(Ingredients, 'sesame oil') ~ 'sesame oil',
    str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') ~ 'soybean oil',
    str_detect(Ingredients, 'sunflower oil') & !str_detect(Ingredients, 'peanut|frying|waking|duck') ~ 'sunflower oil',
    str_detect(Ingredients, 'sunflower oil') & str_detect(Ingredients, 'frying|waking') ~ 'sunflower oil for cooking',
    
    str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'truffle') ~ 'oil truffle',
    
    str_detect(Ingredients, 'garlic oil') ~ 'garlic oil',
    
    str_detect(Ingredients, 'hazelnut oil') ~ 'hazelnut oil',
    
    
    str_detect(Ingredients, 'oil for deep frying') | Ingredients == 'frying oil' ~ 'vegetable oil for deep frying',
    Ingredients %in% c('oil for frying', 'oil for brushing', 'lubricating and brushing oil') ~ 'vegetable oil for cooking',
    str_detect(Ingredients, 'vegetable oil|salad oil|oil, neutral|vegetabie oil') | Ingredients %in% c('oil', 'of oil')  ~ 'vegetable oil',
    
    str_detect(Ingredients, 'mayo') ~ 'mayonnaise',
    
    #Div----
    str_detect(Ingredients, 'agar') ~ 'agar',
    
    str_detect(Ingredients, 'baking powder') ~ 'baking powder',
    str_detect(Ingredients, 'baking soda') ~ 'baking soda',
    str_detect(Ingredients, 'beer') & str_detect(Ingredients, 'dark|amber|christmas') ~ 'beer dark',
    str_detect(Ingredients, 'beer|ale') ~ 'beer',
    Ingredients == 'black truffle or 2 tbsp s truffle oil' ~ 'black truffle',
    str_detect(Ingredients, 'brandy') ~ 'spirits 40 vol-% alcohol brandy',
    str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'base') ~ 'bolognese base',
    str_detect(Ingredients, 'bearnaise') & str_detect(Ingredients, 'base') ~ 'bearnaise base',
    
    str_detect(Ingredients, 'caper|hijack') ~ 'caper',
    (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'powder|spice')) | str_detect(Ingredients, 'ground red pepper') ~ 'chili powder',
    str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'sweet') ~ 'sweet chili sauce',
    str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sauce') ~ 'chili sauce',
    str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mango') ~ 'chutney mango',
    str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mint') ~ 'chutney mint',
    str_detect(Ingredients, 'cider') & !str_detect(Ingredients, 'vinegar') ~ 'cider',
    str_detect(Ingredients, 'cocoa powder') ~ 'cocoa powder',
    str_detect(Ingredients, 'cognac') ~ 'spirits 40 vol-% alcohol cognac',
    str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'celery') ~ 'condensed cream of celery soup',
    str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'chicken') ~ 'condensed cream of chicken soup',
    str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'mushroom') ~ 'condensed cream of mushroom soup',
    str_detect(Ingredients, 'cream sauce') & str_detect(Ingredients, 'base') ~ 'cream sauce base',
    
    str_detect(Ingredients, 'fish soup') & str_detect(Ingredients, 'base') ~ 'fish soup base',
    
    str_detect(Ingredients, 'guacamole') ~ 'guacamole',
    
    str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'base') ~ 'hollandaise base',
    str_detect(Ingredients, 'honey') & !str_detect(Ingredients, 'mustard') ~ 'honey',
    
    str_detect(Ingredients, 'kirsch') ~ 'spirits 40 vol-% alcohol kirsch',
    
    str_detect(Ingredients, 'madeira') ~ 'madeira fortified wine 15 vol-% alcohol',
    str_detect(Ingredients, 'marsala') ~ 'marsala fortified wine 20 vol-% alcohol',
    str_detect(Ingredients, 'miso') & str_detect(Ingredients, 'white') & str_detect(Ingredients, 'paste') ~ 'miso paste white',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'seed') ~ 'mustard seed',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'powder') ~ 'mustard powder',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'whole|grain|coarse') ~ 'mustard whole grain',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'dijon') ~ 'mustard dijon',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'honey') ~ 'mustard honey',
    str_detect(Ingredients, 'mustard') & !str_detect(Ingredients, 'cheese') ~ 'mustard',
    
    str_detect(Ingredients, 'nori') & str_detect(Ingredients, 'flak|seaweed') ~ 'nori seaweed',
    
    str_detect(Ingredients, 'olive paste tapenade') ~ 'olive paste tapenade',
    str_detect(Ingredients, 'dry onion soup mix') ~ 'onion soup mix',
    
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'chili') | str_detect(Ingredients, 'sambal') ~ 'paste chili',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'carrot') ~ 'paste carrot',
    str_detect(Ingredients, 'paste|pasta') & str_detect(Ingredients, 'curry') ~ 'paste curry',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'garlic') ~ 'paste garlic',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'shrimp') ~ 'paste shrimp',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'sun') ~ 'paste tomato sun-dried',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
    str_detect(Ingredients, 'pesto') ~ 'pesto',
    str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'white') ~ 'white pepper',
    str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'cayenne') ~ 'cayenne pepper',
    str_detect(Ingredients, 'pepper|Pepper') & !str_detect(Ingredients, 'chili|white|sweet|cayenne|spice|bell|salad|sauce') & !str_detect(Ingredients, 'salt') ~ 'black pepper',
    str_detect(Ingredients, 'pizza filling') ~ 'pizza filling',
    
    str_detect(Ingredients, 'sake') ~ 'sake',
    str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'oil') ~ 'salt and pepper and oil',
    str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'butter') ~ 'salt and pepper and butter',
    str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') ~ 'salt and pepper',
    str_detect(Ingredients, 'salt') & !str_detect(Ingredients, 'peanut|lamb|pork|anchovy|soy|flesk|cashew') ~ 'salt',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'barbeque|barbecue') ~ 'sauce barbeque',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'bearnaise') ~ 'sauce bearnaise',
    str_detect(Ingredients, 'bearnaise') ~ 'sauce bearnaise',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot pepper') ~ 'sauce hot pepper',
    str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'chunky') ~ 'salsa chunky',
    str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'tomato') ~ 'salsa tomato',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'cheese') ~ 'sauce cheese',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sweet') ~ 'sauce sweet chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'hot') ~ 'sauce hot chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sour') ~ 'sauce sour chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'salt') ~ 'sauce salt chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'mild') ~ 'sauce mild chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'garlic') ~ 'sauce chili garlic',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') ~ 'sauce chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'cream') ~ 'sauce cream',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'fish|fisk') ~ 'sauce fish',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'hoisin') ~ 'sauce hoisin',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'horseradish') ~ 'sauce horseradish',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'mint') ~ 'sauce mint',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'teriyaki') ~ 'sauce teriyaki',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'tomat') ~ 'sauce tomato',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'oyster') ~ 'sauce oyster',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'pad thai') ~ 'sauce pad thai',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'piri-piri') ~ 'sauce piri-piri',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'sweet') ~ 'sauce sweet soy',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'soy') ~ 'sauce soy',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'taco') ~ 'sauce taco',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'tikka masala') ~ 'sauce tikka masala',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'white') ~ 'sauce white',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'cranberr') ~ 'sauce cranberry',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'worcestershire') ~ 'sauce worcestershire',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pasta|spagetti|spaghetti') ~ 'sauce pasta',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') ~ 'sauce pizza',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot') ~ 'sauce hot',
    str_detect(Ingredients, 'sherry') & !str_detect(Ingredients, 'vinegar') ~ 'sherry fortified wine 15 vol-% alcohol',
    str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'brown|castor') ~ 'sugar brown',
    str_detect(Ingredients, 'sugar') & !str_detect(Ingredients, 'asparagus|pea') ~ 'sugar',
    str_detect(Ingredients, 'syrup') & str_detect(Ingredients, 'maple') ~ 'syrup maple',
    str_detect(Ingredients, 'syrup') ~ 'syrup',
    
    str_detect(Ingredients, 'tabasco') ~ 'tabasco',
    str_detect(Ingredients, 'tofu') ~ 'tofu',
    str_detect(Ingredients, 'toro jegergryte') ~ 'toro jegegryte',
    str_detect(Ingredients, 'toro moussaka') ~ 'toro moussaka',
    
    str_detect(Ingredients, 'vermouth') ~ 'vermouth fortified wine 15 vol-% alcohol',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'apple|cider') ~ 'vinegar apple cider',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'balsamic') ~ 'vinegar balsamic',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'red wine') ~ 'vinegar red wine',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'rice') ~ 'vinegar rice',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'sherry') ~ 'vinegar sherry',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'white wine') ~ 'vinegar white wine',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ 'vinegar wine',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'brown') ~ 'vinegar brown',
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'clear|light|5%') ~ 'vinegar',
    str_detect(Ingredients, 'vinegar') ~ 'vinegar',
    str_detect(Ingredients, 'vodka') ~ 'spirits 40 vol-% alcohol vodka',
    
    str_detect(Ingredients, 'water') & !str_detect(Ingredients, 'corn|beef|tuna|coffee|chili|cream|cress|chestnut|melon') ~ 'water',
    str_detect(Ingredients, 'water to the corn') ~ 'water',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'vegetable') ~ 'broth cube vegetable',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'fish') ~ 'broth cube fish',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'beef|meat') ~ 'broth cube beef',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'chicken') ~ 'broth cube chicken',
    str_detect(Ingredients, 'stock|broth|borth') & str_detect(Ingredients, 'cube|dice') ~ 'broth cube',
    str_detect(Ingredients, 'stock|broth|power') & str_detect(Ingredients, 'chicken') ~ 'water broth chicken',
    str_detect(Ingredients, 'stock|broth|bouilljon') & str_detect(Ingredients, 'beef|meat') ~ 'water broth beef',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'shellfish') ~ 'water broth shellfish',
    str_detect(Ingredients, 'stock|broth|power') & str_detect(Ingredients, 'game|wild') ~ 'water broth game',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'turkey') ~ 'water broth turkey',
    str_detect(Ingredients, 'stock|broth') & str_detect(Ingredients, 'vegetable') ~ 'water broth vegetable',
    str_detect(Ingredients, 'stock|broth|power') & str_detect(Ingredients, 'lamb') ~ 'water broth lamb',
    str_detect(Ingredients, 'stock|broth|power') & str_detect(Ingredients, 'fish') ~ 'water broth fish',
    str_detect(Ingredients, 'stock|broth|frying pan|power') ~ 'water broth',
    str_detect(Ingredients, 'wasabi') ~ 'wasabi',
    str_detect(Ingredients, 'whisky|whiskey') ~ 'whisky spirits 40 vol-% alcohol',
    str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'rice') ~ 'wine rice',
    str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'white') & !str_detect(Ingredients, 'vinegar') ~ 'wine white',
    str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'red') & !str_detect(Ingredients, 'vinegar') ~ 'wine red',
    str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'port') ~ 'wine port fortified wine 20 vol-% alcohol',
    str_detect(Ingredients, 'mirin japanese sweet wine') ~ 'wine mirin',
    
    str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'dry|dried') ~ 'yeast dry',
    str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'nutritional') ~ 'yeast nutritional',
    str_detect(Ingredients, 'yeast') ~ 'yeast',
    
    TRUE ~ Ingredients
  )) %>%
  
  #Remove some ingredients not in recipe when checked (bacon in Turkey fillet on party bed/KALKUNFILET PÅ PARTYSENG, and 'Refractory' ingredients from Salmon Shape With Pepper Root and 'Pepper Beef With Chopped Tomato And Chevre Salad')
  filter(!(`Selected Meals` == 'Turkey fillet on party bed' & Ingredients == 'bacon') & !str_detect(Ingredients, 'Refractory')) %>%

  #Add stk to the amounts of all the items listed in 'stk'
  mutate(Amounts = case_when(
    !str_detect(Amounts, '[:alpha:]') &
      str_detect(Ingredients_standardized, regex(paste0(various$stk, collapse = '|'), ignore_case = TRUE)) ~ paste0(Amounts, ' stk'),
    TRUE ~ Amounts)) %>%

#Turn volume units to dl and weight unit to kg----
#Split Amounts into amounts and units
separate(., Amounts, c('Amounts', 'unit'), sep = ' ') %>%
  #Turn amounts into numeric
  mutate_at('Amounts', ~as.numeric(.)) %>%
  
  #Turn volume units to dl,  ounce/pound to g, beef to portions
  mutate(Amounts = case_when(
    Ingredients_standardized == 'cider' & unit == 'glass' ~ 3.41,
    unit == 'cup' ~ Amounts * 2.45,
    unit == 'l' ~ Amounts * 10,
    unit == 'ml' ~ Amounts / 100,
    unit == 'tbsp' ~ Amounts / 6.67,
    unit == 'tsp' ~ Amounts / 20,
    unit == 'krm' ~ Amounts / 100,
    unit == 'drop' ~ Amounts / 2000, #One drop is 0.05ml 
    unit == 'pinch' ~ Amounts / (20*16), #A pinch is usually defined as 1/16 of a tsp
    unit == 'ounce' ~ Amounts * 28.35,
    unit == 'pound' ~ Amounts * 453.59,
    TRUE ~ Amounts
  )) %>%
  mutate(unit = case_when(
    unit %in% c('cup', 'l', 'ml', 'tsp', 'tbsp', 'krm', 'drop', 'pinch') | Ingredients_standardized == 'cider' & unit == 'glass' ~ 'dl',
    unit %in% c('ounce', 'pound') ~ 'g',
    Ingredients_standardized %in% c('shrimp', 'salad rocket') ~ str_replace(unit, 'neve', 'dl'),
    str_detect(Ingredients, 'beef') & !str_detect(Ingredients, 'tongue|cube') ~ str_replace(unit, 'stk', 'portion'),
    str_detect(Ingredients_standardized, 'fresh herb|coriander|basil|thyme') ~ str_replace(unit, 'stk', 'twig'),
    str_detect(Ingredients_standardized, 'salad|caper|parsley') ~ str_replace(unit, 'neve', 'dl'),
    TRUE ~ unit
  )) %>%
  
  #Rename column for later
  rename(org_ingredients = Ingredients,
         Ingredients = Ingredients_standardized) %>%
  
  #Turn juice, water, vinegar and other liquids with similar density to water from dl/l to grams as they are all about 100g/dl
  mutate(
    Amounts = case_when(
      (str_detect(Ingredients, 'water|beer|madeira|marsala|cognac|cider|juice|Juice|broth|kraft|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
         unit == 'dl') &
        !str_detect(Ingredients, 'sugar|cheese|flour') ~ Amounts * 100,
      TRUE ~ Amounts),
    unit = case_when(
      (str_detect(Ingredients, 'water|beer|madeira|marsala|cognac|cider|juice|Juice|broth|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
         unit == 'dl') &
        !str_detect(Ingredients, 'sugar|cheese|flour') ~ 'g',
      TRUE ~ unit)) %>%
  
  #Turn grams into kilos 
  mutate(
    Amounts = case_when(
      unit == 'g' ~ Amounts/1000,
      TRUE ~ Amounts),
    unit = unit %>%
      str_replace('\\bg\\b', 'kg'))

#Save the orginal and standardized ingredient names for comparisons-----
various$org_ingredients <- clean %>% select(`Selected Meals`, Ingredients, org_ingredients)
  
  #Sum the amounts for all the ingredients with the same name for each recipe----
clean <- clean %>%
  
  group_by(`Selected Meals`, Ingredients, Country, Source, unit) %>%
  summarise(Amounts = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
  #Using na.rm gives ingredients with no amounts a '0' value, change back to NA
  mutate(Amounts = case_when(
    !is.na(unit) ~ Amounts
  ))

rm(raw, raw_list_NO)
various$units <- NULL
various$stk <- NULL
various$NO_recipes <- NULL

#Turn cooked products into their raw equivalents, using the convertion factors from Helsedirekttortatet Mål Vekt og Porsjonsstørrelser----
clean <- clean %>%
  
  mutate(
    Amounts = case_when(
      Ingredients == 'bacon cooked' ~ Amounts/0.31,
      Ingredients %in% c('chicken breast cooked', 'chicken breast without skin cooked') ~ Amounts/0.8,
      Ingredients %in% c('chicken cooked', 'turkey meat cooked') ~ Amounts/0.4,
      Ingredients == 'lamb shoulder cooked' ~ Amounts/0.56,
      Ingredients == 'pork tenderloin cooked' ~ Amounts/0.75,
      Ingredients == 'pasta cooked' ~ Amounts/2.63,
      Ingredients == 'rice cooked' ~ Amounts/2.94,
      Ingredients == 'beetroot cooked' ~ Amounts/0.95, #Use values for parlsey root
    
      TRUE ~ Amounts),
    #Remove cooked from ingredient name
    Ingredients = str_replace(Ingredients, ' cooked', '')
    )

various$unique_ingredients <- clean %>% select(Ingredients) %>% unique()

#Get the ingredients that don't have amounts in kg already and map to volume/weight database----
various$get_amounts_kg <- clean %>%
  filter(unit != 'kg')
#See which ingredients have no unit or amounts
various$missing <- clean %>% filter(is.na(Amounts))

#Map volume/weight database
temp <- checkRefList(various$get_amounts_kg %>% select(Ingredients) %>% unique(), reference = references$volume_weight)
#Save
saveRDS(temp, 'with_weight_volume_ref.Rds')
temp <- readRDS('with_weight_volume_ref.Rds')

temp2 <- temp %>%
  full_join(various$get_amounts_kg) %>% unique() %>%
  filter(!is.na(Amounts)) %>%

  #Fix some errors
  mutate(ID = case_when(
    Ingredients == 'butter clarified ghee' ~ fixRefID(reference = references$volume_weight, 'ghee'),
    Ingredients == 'cheese brie' ~ fixRefID(reference = references$volume_weight, 'soft'),
    Ingredients == 'eggplant' ~ fixRefID(reference = references$volume_weight, 'eggplant'),
    Ingredients == 'sugar' ~ fixRefID(reference = references$volume_weight, 'sugar', 'white'),
    Ingredients == 'almond' ~ fixRefID(reference = references$volume_weight, 'almonds'),
    Ingredients == 'apricot dried' ~ fixRefID(reference = references$volume_weight, 'apricots', 'dried'),
    Ingredients == 'bread flat hard' ~ fixRefID(reference = references$volume_weight, 'flatbread', 'hard'),
    Ingredients == 'caper' ~ fixRefID(reference = references$volume_weight, 'capers'),
    Ingredients == 'cheese brie' ~ fixRefID(reference = references$volume_weight, 'soft ripened cheese'),
    str_detect(Ingredients, 'cheddar|jarlsberg|norvegia|semi-hard') ~ fixRefID(reference = references$volume_weight, 'hard to semi-hard cheese'),
    Ingredients == 'cheese mozzarella' ~ fixRefID(reference = references$volume_weight, 'mozzarella'),
    Ingredients == 'parmesan cheese' ~ fixRefID(reference = references$volume_weight, 'parmesan'),
    Ingredients == 'chili pepper green' ~ fixRefID(reference = references$volume_weight, 'chili', 'red'), #Same in volume
    Ingredients == 'grape' ~ fixRefID(reference = references$volume_weight, 'grapes'), #Same in volume
    Ingredients == 'hazelnut' ~ fixRefID(reference = references$volume_weight, 'hazel', 'nut'), #Same in volume
    Ingredients == 'mackerel tomato canned' ~ fixRefID(reference = references$volume_weight, 'mackerel', 'fillet'),
    Ingredients == 'of olives' ~ fixRefID(reference = references$volume_weight, 'olive', 'green'),
    Ingredients == 'peas green' ~ fixRefID(reference = references$volume_weight, 'pea', 'frozen'),
    Ingredients == 'pork neck chop' ~ fixRefID(reference = references$volume_weight, 'pork', 'neck'),
    Ingredients == 'sweet pepper grilled' & unit == 'stk' ~ fixRefID(reference = references$volume_weight, 'sweet', 'pepper'),
    Ingredients == 'sweet pepper grilled' ~ fixRefID(reference = references$volume_weight, 'sweet pepper', 'grilled'),
    Ingredients == 'turkey chicken drumstick' ~ fixRefID(reference = references$volume_weight, 'turkey', 'drumstick'),
    Ingredients == 'soy' ~ fixRefID(reference = references$volume_weight, 'soy', 'sauce'), #This seems most likely as it is part of a marinade for sashimi
    Ingredients == 'lemongrass' ~ fixRefID(reference = references$volume_weight, 'lemongrass'),
    Ingredients == 'fig' ~ fixRefID(reference = references$volume_weight, 'fig'),
    Ingredients == 'shrimp' ~ fixRefID(reference = references$volume_weight, 'shrimps', 'in'),
    Ingredients == 'bean white canned' ~ fixRefID(reference = references$volume_weight, 'bean white', 'canned'),
    Ingredients == 'bean kidney canned' ~ fixRefID(reference = references$volume_weight, 'bean kidney', 'canned'),
    Ingredients == 'bean black canned' ~ fixRefID(reference = references$volume_weight, 'bean black', 'canned'),
    Ingredients == 'chick pea canned' ~ fixRefID(reference = references$volume_weight, 'chick pea', 'canned'),
    Ingredients == 'mustard powder' ~ fixRefID(reference = references$volume_weight, 'mustard', 'powder'),
    Ingredients == 'mustard powder' ~ fixRefID(reference = references$volume_weight, 'mustard', 'powder'),
    Ingredients %in% c('salad', 'salad lettuce') & unit == 'stk' ~ fixRefID(reference = references$volume_weight, 'heart', 'salad'),
    Ingredients %in% c('lettuce', 'salad lettuce') & unit == 'dl' ~ fixRefID(reference = references$volume_weight, 'iceberg', 'lettuce'),
    Ingredients == 'chopped parsley or generous sprinkling dill fronds, or mixture optional' ~ fixRefID(reference = references$volume_weight, 'parsley', 'fresh'),
    Ingredients == 'basil' & unit == 'twig' ~ fixRefID(reference = references$volume_weight, 'basil', 'fresh'),
    Ingredients == 'coriander' & unit == 'twig' ~ fixRefID(reference = references$volume_weight, 'coriander', 'fresh'),
    
    #Ingredients with no references
    Ingredients %in% c('fish soup base', 'mustard powder', 'burrito spice mix', 'bolognese base',
                       'chinese five spice', 'of lime sheet, shredded', 'of dip mix') ~ 0,
    
    TRUE ~ ID
  ))

#Ingredients to turn from tbsp to dl
various$to_dl <- c('pepper', 'peanøttsmør', 'olje', 'oil', 'smør', 'butter', 'margarin',
                   'ghee', 'garlic', 'tomato paste')

various$weights <- databases$volume_weight %>%
  #Set brutto as default value pr stk, as SHARP takes edible portion and cooking losses into account when calculating environmental impact
  mutate(
    g = case_when(
      str_detect(Ingredients, regex(paste0(various$to_dl, collapse ='|'), ignore_case = TRUE)) &
        unit_enhet == 'tbsp' ~ g * 6.67,
      TRUE ~ g),
    unit_enhet = case_when(
      unit_enhet == 'brutto' ~ 'stk',
      
      #These ingredients doesn't have brutto values, use netto
      unit_enhet == 'netto' & str_detect(Ingredients, 'tomat|egg yolk|egg white') ~ 'stk',
      
      str_detect(Ingredients, regex(paste0(various$to_dl, collapse ='|'), ignore_case = TRUE)) &
        unit_enhet == 'tbsp' ~ 'dl',
      unit_enhet %in% c('cm rot', 'cm of root') ~ 'cm',
      TRUE ~ unit_enhet
    )) %>%
  #Only keep those necessary
  filter(unit_enhet %in% clean$unit) %>% select(-reference) %>% unique() %>% filter(language != 'norwegian') %>%
  rename(unit = unit_enhet) %>% select(ID, unit, g)

#Join them together and calculate the weight in kilos
various$with_weights <- inner_join(temp2, various$weights, by = c('ID', 'unit')) %>%
  
  mutate(Amounts_kg = (Amounts * g) / 1000,
         unit = 'kg') %>%
  #Remove and rename columns
  select(-c(g, ID, ref, loop, Amounts)) %>%
  rename(Amounts = Amounts_kg)
  
#Add the new units to clean df
clean <- clean %>%
  filter(unit == 'kg') %>%
  full_join(various$with_weights) %>%
  group_by(`Selected Meals`, Ingredients, Country, Source) %>%
  
  summarise(Amounts = sum(Amounts, na.rm = TRUE)) %>%
  ungroup()

#Remove some unnecessary df's
various$get_amounts_kg <- NULL
various$to_dl <- NULL
various$weights <- NULL 
various$with_weights <- NULL 

#Calculate the total weight of each recipe
various$recipe_weight <- clean %>%
  group_by(`Selected Meals`) %>%
  summarise(Weight = sum(Amounts))

#Impute the mean value of the same ingredient for the missing ingredients----
#Calculate the mean value of each type of ingredient pr 100g of a recipe first
various$mean_values <- clean %>%
  #Add total weight of recipe in kg
  inner_join(various$recipe_weight) %>%
  #Amount of ingredient pr 100g of a recipe
  mutate(value_100g = (Amounts/Weight)/10) %>%
  #Calculate the mean value for each ingredient
  group_by(Ingredients) %>%
  summarise(value = mean(value_100g)) %>% ungroup()

#Add to the missing amounts
temp <- various$mean_values %>%
  #Inner join with the ingredients missing amounts
  inner_join(., various$missing) %>%
  #Add the weight of these recipes
  inner_join(various$recipe_weight) %>%
  #Calculate the amounts based on weight
  mutate(Amounts = value*Weight*10,
         unit = 'kg')

#Add back to clean df
clean <- full_join(clean, temp) %>%
  select(-c(value, Weight, unit)) %>%
  #if a recipe has more than two occurences of a recipe (example_ butter used both for frying and as part of a dough), sum them together
  group_by(`Selected Meals`, Ingredients, Country, Source) %>%
  summarise(Amounts = sum(Amounts)) %>% ungroup()

#Which ingrediens still don't have amounts?
various$missing <- anti_join(various$missing %>% select(`Selected Meals`, Ingredients),clean %>% select(`Selected Meals`, Ingredients))

#Fill in values for these ingredients based on similar ingredients
temp <- various$missing %>%
  mutate(value = case_when(
    Ingredients %in% c('peanut oil', 'sunflower or peanut oil for frying') ~ various$mean_values %>% filter(Ingredients == 'sunflower oil') %>% select(value) %>% as.numeric(.),
    Ingredients %in% c('a little canola or olive oil', 'extra-virgin olive or more canola oil, to finish') ~ various$mean_values %>% filter(Ingredients == 'canola or olive oil') %>% select(value) %>% as.numeric(.),
    Ingredients == 'oil / butter for frying' ~ various$mean_values %>% filter(Ingredients == 'butter for cooking') %>% select(value) %>% as.numeric(.),
    Ingredients %in% c('a few fresh mint or flat-leaf parsley leaf, finely chopped', 'parsley') ~ various$mean_values %>% filter(Ingredients == 'parsley fresh herbs') %>% select(value) %>% as.numeric(.),
    Ingredients == 'grape juice' ~ various$mean_values %>% filter(Ingredients == 'vinegar balsamic') %>% select(value) %>% as.numeric(.), #Used "to taste"
    Ingredients == 'rice sushi' ~ various$mean_values %>% filter(Ingredients == 'rice white long grain') %>% select(value) %>% as.numeric(.),
    Ingredients == 'sauce horseradish' ~ various$mean_values %>% filter(Ingredients == 'yoghurt') %>% select(value) %>% as.numeric(.), #As conditment
    
  )) %>%
  
  #Drop those without amounts
  drop_na(value) %>%
  
  #Add the weight of these recipes
  inner_join(various$recipe_weight) %>%
  #Calculate the amounts based on weight
  mutate(Amounts = value*Weight*10,
         unit = 'kg')

#Add back to clean df
clean <- full_join(clean, temp) %>%
  select(-c(value, Weight, unit)) %>%
  #if a recipe has more than two occurences of a recipe (example_ butter used both for frying and as part of a dough), sum them together
  group_by(`Selected Meals`, Ingredients, Country, Source) %>%
  summarise(Amounts = sum(Amounts)) %>% ungroup()

#Fill in missing Country and source info for the missing ingredients
clean <- clean %>%
  group_by(`Selected Meals`) %>%
  fill(c(Country, Source), .direction = 'downup') %>% ungroup()

#New total weight of recipes
various$recipe_weight <- clean %>%
  select(`Selected Meals`, Amounts) %>%
  group_by(`Selected Meals`) %>%
  summarise(Weight = sum(Amounts, na.rm = TRUE)) %>%
  ungroup()

#Weight of each ingredient pr 100g
various$ingredients_weight <- clean %>%
  select(`Selected Meals`, Ingredients, Amounts) %>%
  inner_join(various$recipe_weight) %>%
  mutate(Amounts_kg = Amounts/Weight) %>%
  select(-c(Amounts, Weight)) %>%
  rename(sample_id = `Selected Meals`)

#Map to nutrients database----
temp <- clean %>%
  select(Ingredients) %>% unique() %>%
  checkRefList(., reference = references$nutrients)

#See which ingredients haven't been picked up
t <- anti_join(clean %>% select(Ingredients) %>% unique(), temp %>% select(Ingredients))

#Save
saveRDS(temp, 'with_nutrients_ref.Rds')
temp <- readRDS('with_nutrients_ref.Rds')

temp2 <- temp %>%
  select(-loop) %>%
  
  full_join(., clean) %>% unique() %>%
  
  #Fix some errors
  mutate(ID = case_when(
    
    Ingredients == 'eggplant' ~ fixRefID(references$nutrients, 'eggplant'),
    str_detect(Ingredients, 'vinegar') & !str_detect(ref, 'vinegar') ~ fixRefID(references$nutrients, 'vinegar'),
    Ingredients == 'chick pea' ~ fixRefID(references$nutrients, 'chick pea'),
    Ingredients == 'chopped parsley or generous sprinkling dill fronds, or mixture optional' ~ fixRefID(references$nutrients, 'parsley', 'fresh'),
    Ingredients == 'cod lutefisk' ~ fixRefID(references$nutrients, 'lutefisk'),
    ref == 'mushroom' & !str_detect(Ingredients, 'condensed cream of mushroom soup') ~ fixRefID(references$nutrients, 'mushroom'),
    Ingredients == 'of olives' ~ fixRefID(references$nutrients, 'olive', 'green'),
    Ingredients == 'parsley' ~ fixRefID(references$nutrients, 'parsley', 'fresh'),
    Ingredients == 'peach' ~ fixRefID(references$nutrients, 'peach'),
    Ingredients == 'peanut' ~ fixRefID(references$nutrients, 'peanut, raw'),
    Ingredients == 'pork neck chop' ~ fixRefID(references$nutrients, 'pork', 'neck chop'),
    Ingredients == 'rice white long grain' ~ fixRefID(references$nutrients, 'rice white long grain'),
    Ingredients == 'sausage' ~  fixRefID(references$nutrients, 'sausage'),
    Ingredients == 'sugar' ~  fixRefID(references$nutrients, 'sugar'),
    Ingredients == 'sweet corn kernels' ~ fixRefID(references$nutrients, 'sweet corn', 'canned'),
    Ingredients == 'sweet potato' ~ fixRefID(references$nutrients, 'sweet potato'),
    str_detect(Ingredients, 'water broth') ~ fixRefID(references$nutrients, 'water'),
    Ingredients == 'butter clarified ghee' ~ fixRefID(references$nutrients, 'ghee'),
    Ingredients %in% c('cashew nut salt', 'cashew nut roasted') ~ fixRefID(references$nutrients, 'cashew', 'salt'),
    Ingredients == 'chili pepper dried' ~ fixRefID(references$nutrients, 'chili pepper', 'red'),
    Ingredients == 'mackerel tomato canned' ~ fixRefID(references$nutrients, 'mackerel', 'tomato canned'),
    Ingredients %in% c('potato', 'potato boiled') ~ fixRefID(references$nutrients, 'potato'), 
    Ingredients %in% c('bread crumb', 'bread', 'bread naan', 'breadstick') ~ fixRefID(references$nutrients, 'bread'),
    Ingredients == 'cheese cottage low fat' ~ fixRefID(references$nutrients, 'cottage cheese', 'low fat'),
    Ingredients == 'cheese cottage' ~ fixRefID(references$nutrients, 'cottage cheese'),
    Ingredients == 'cheese asiago' ~  fixRefID(references$nutrients, 'parmesan'), #Can be substituted for eachother in recipes
    Ingredients == 'cheese blue' ~ fixRefID(references$nutrients, 'gorgonzola', 'blue cheese'), #Use as standard for time being
    Ingredients == 'cheese goat chevre white' ~ fixRefID(references$nutrients, 'chevre'),
    Ingredients == 'cheese cream' ~ fixRefID(references$nutrients, 'cream cheese'),
    Ingredients == 'cheese hard goat' ~ fixRefID(references$nutrients, 'hard goat cheese', 'kvitlin'), #Use as standard for time being
    Ingredients == 'cheese jarlsberg' ~ fixRefID(references$nutrients, 'jarlsberg'), 
    Ingredients == 'cheese manchego' ~ fixRefID(references$nutrients, 'cheddar'), #Can be substituted in recipes
    Ingredients == 'cheese mozzarella' ~ fixRefID(references$nutrients, 'mozzarella'),
    Ingredients == 'cheese norvegia' ~ fixRefID(references$nutrients, 'norvegia'),
    Ingredients == 'cheese ricotta salata' ~ fixRefID(references$nutrients, 'ricotta salata'),
    Ingredients == 'cheese port salut' ~ fixRefID(references$nutrients, 'port salut'),
    Ingredients == 'cheese semi-hard' ~ fixRefID(references$nutrients, 'norvegia'), #Use as standard for time being
    Ingredients == 'chicken' ~ fixRefID(references$nutrients, 'chicken', 'whole'),
    Ingredients == 'condensed cream of celery soup' ~ fixRefID(references$nutrients, 'condensed cream of celery soup'),
    Ingredients == 'condensed cream of chicken soup' ~ fixRefID(references$nutrients, 'condensed cream of chicken soup'),
    Ingredients %in% c('crisp bread', 'crisp bread coarse') ~ fixRefID(references$nutrients, 'crisp bread', 'coarse'),
    Ingredients == 'goat brown cheese' ~ fixRefID(references$nutrients, 'goat cheese brown'),
    Ingredients == 'jerusalem artichoke' ~ fixRefID(references$nutrients, 'jerusalem artichoke'),
    Ingredients == 'lentil' ~ fixRefID(references$nutrients, 'lentil', 'green'), #Use as standard
    Ingredients == 'mangold' ~ fixRefID(references$nutrients, 'mangold'),
    Ingredients == 'oil corn' ~ fixRefID(references$nutrients, 'vegetable', 'oil'),
    Ingredients == 'onion soup mix' ~ fixRefID(references$nutrients, 'onion soup mix'),
    Ingredients == 'sauce hot pepper' ~ fixRefID(references$nutrients, 'hot pepper sauce'),
    Ingredients == 'sauce pasta' ~ fixRefID(references$nutrients, 'tomato', 'sauce'), #Use as substitute for time being
    Ingredients == 'whole turkey' ~ fixRefID(references$nutrients, 'turkey', 'meat'),
    Ingredients == 'sauce hot' ~ fixRefID(references$nutrients, 'hot pepper sauce'),
    
    Ingredients %in% c('duck or goose fat for confit', 'homemade beef gravy', 'of lime sheet, shredded',
                       'olive paste tapenade', 'cooking spray', 'red food coloring',
                       'pack high quality charcoal briquettes', 'pomegranate kernel', 'yeast nutritional',
                       'salmon roe', 'spice seasoning pepper', 'toro greek moussaka'
                       ) ~ 0,
    
    #Substitutions or ingredients not found in Matvaretabellen
    Ingredients %in% c('garlic oil', 'oil truffle') ~ fixRefID(references$nutrients, 'olive', 'oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
    Ingredients == 'frying oil' ~ fixRefID(references$nutrients, 'vegetable', 'oil'),
    Ingredients == 'hazelnut oil' ~ fixRefID(references$nutrients, 'walnut', 'oil'), #Another nut oil
    Ingredients == 'bean canned' ~ fixRefID(references$nutrients, 'bean black', 'canned'),
    Ingredients == 'scampi' ~ fixRefID(references$nutrients, 'shrimp'),
    Ingredients == 'ciabatta' ~ fixRefID(references$nutrients, 'bread', 'white'),
    
    #Find a solution for the lemon/lime juice/zest
    
    TRUE ~ ID
  )) %>%
  
  #Join with nutrient db
  inner_join(databases$nutrients %>% mutate(ID = as.numeric(ID)), by = 'ID') %>%
  select(-ref) %>%
  replace(is.na(.), 0)

#See which ingredients haven't been picked up
t <- anti_join(clean %>% select(Ingredients) %>% unique(), temp2 %>% select(Ingredients))

#Save the raw nutrient values in 100g recipe by ingredients
various$ingredients_nutrients <- temp2 %>%
  select(Ingredients, ID) %>%
  inner_join(., clean, by = 'Ingredients') %>%
  unique() %>%
  inner_join(various$recipe_weight) %>%
  
  #Amount of each ingredient per 100 g of recipe
  mutate(temp_amounts = Amounts/Weight) %>%
  select(-c(Amounts, Weight)) %>%
  
  #Join with nutrient database
  full_join(databases$nutrients, by = 'ID') %>%
  
  #Turn wide and calculate nutrient contents
  select(-c(ID)) %>%
  pivot_longer(
    cols = -c(`Selected Meals`, Ingredients, Country, Source, temp_amounts),
    names_to = 'feature',
    values_to = 'value'
  ) %>%
  
  #Calculate nutrient content pr ingredients
  mutate(value = temp_amounts*value) %>% #Nutrient amount from each ingredient
  select(-temp_amounts) %>%
  
  #Rename for later
  rename(sample_id = `Selected Meals`,
         group = Country)

#Sum nutrient content for whole recipe
various$recipes_nutrients <- various$ingredients_nutrients %>%
  
  #Sum for each recipe
  group_by(sample_id, group, feature) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
  drop_na(group) 
  
#Calculate the CO2 and landuse----
temp <- clean %>%
  select(Ingredients) %>% unique() %>%
  checkRefList(., references$sustainability)

t <- anti_join(clean %>% select(Ingredients) %>% unique(), temp %>% select(Ingredients))

#Fix some errors
temp2 <- temp %>%
  
  full_join(., clean %>% select(Ingredients)) %>% unique() %>%
  
  mutate(ID = case_when(
    
    #Grains, nuts, seeds, legumes
    Ingredients == 'almond' ~ fixRefID(references$sustainability, 'almonds', 'sweet'),
    Ingredients == 'hazelnut' ~ fixRefID(references$sustainability, 'nut', 'hazel'),
    Ingredients %in% c('lentil', 'lentil green') ~ fixRefID(references$sustainability, 'lentil', 'dry'),
    Ingredients == 'peas green' ~ fixRefID(references$sustainability, 'pea', 'garden'),
    Ingredients %in% c('bean green asparagus', 'bean green') ~ fixRefID(references$sustainability, 'bean with pods', 'with'),
    str_detect(Ingredients, 'bean') & !str_detect(Ingredients, 'canned|sprout') ~ fixRefID(references$sustainability, 'beans', 'dry'),
    str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'canned') ~ fixRefID(references$sustainability, 'bean', 'canned'),
    str_detect(Ingredients, 'noodle') ~ fixRefID(references$sustainability, 'noodle'),
    Ingredients == 'pistachio nut' ~ fixRefID(references$sustainability, 'pistachio'),
    
    #Veggies
    str_detect(Ingredients, 'pickled') & !str_detect(Ingredients, 'ginger') ~ fixRefID(references$sustainability, 'vegetables', 'pickled'),
    str_detect(Ingredients, 'endive|chicory') ~ fixRefID(references$sustainability, 'curly', 'endives'),
    Ingredients == 'peach' ~ fixRefID(references$sustainability, 'peaches', 'other'),
    Ingredients == 'sorrel' ~ fixRefID(references$sustainability, 'lettuce', 'other'),
    str_detect(Ingredients, 'winter squash') ~ fixRefID(references$sustainability, 'pumpkin'),
    str_detect(Ingredients, 'eggplant') ~ fixRefID(references$sustainability, 'eggplant'),
    Ingredients == 'garlic chinese' ~ fixRefID(references$sustainability, 'garlic'),
    Ingredients == 'corn baby' ~ fixRefID(references$sustainability, 'sweet', 'corn'),
    Ingredients == 'mangold' ~ fixRefID(references$sustainability, 'chard'),
    Ingredients == 'olive black' ~ fixRefID(references$sustainability, 'olives', 'canned'),
    Ingredients %in% c('olive green', 'of olives') ~ fixRefID(references$sustainability, 'olives', 'fresh'),
    
    #Red meat
    str_detect(Ingredients, 'reindeer') ~ fixRefID(references$sustainability, 'mammals', 'meat'),
    str_detect(Ingredients, 'pork') & !str_detect(Ingredients, 'lard') ~ fixRefID(references$sustainability, 'pork'),
    
    #Poultry
    str_detect(Ingredients, 'turkey') ~ fixRefID(references$sustainability, 'turkey'),
    
    #Seafood
    Ingredients == 'scampi' ~ fixRefID(references$sustainability, 'prawn'),
    Ingredients == 'char ishavsrøye fatty fish' ~ fixRefID(references$sustainability, 'trout'), #Look up other alternatives
    
    #Div
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ fixRefID(references$sustainability, 'vinegar', 'wine'),
    str_detect(Ingredients, 'vinegar') ~ fixRefID(references$sustainability, 'vinegar'),
    Ingredients == 'condensed cream of celery soup' ~ fixRefID(references$sustainability, 'condensed cream of celery soup'),
    Ingredients == 'condensed cream of chicken soup' ~ fixRefID(references$sustainability, 'condensed cream of chicken soup'),
    str_detect(Ingredients, 'mushroom') & !str_detect(Ingredients, 'dried|canned') ~ fixRefID(references$sustainability, 'mushroom'),
    Ingredients %in% c('garlic oil', 'oil truffle') ~ fixRefID(references$sustainability, 'olive', 'oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
    Ingredients == 'sauce hot' ~ fixRefID(references$sustainability, 'hot', 'pepper'),
    Ingredients == 'sesame oil' ~ fixRefID(references$sustainability, 'seed', 'oil'),
    Ingredients == 'hazelnut oil' ~ fixRefID(references$sustainability, 'walnut', 'oil'),
    Ingredients == 'sauce pasta' ~ fixRefID(references$sustainability, 'tomato', 'sauce'),
    Ingredients == 'sweet chili sauce' ~ fixRefID(references$sustainability, 'chili', 'sweet'),
    str_detect(Ingredients, 'cognac') ~ fixRefID(references$sustainability, 'brandy'),
    
    #Dairy
    str_detect(Ingredients, 'cheddar|romano|parmigiano-reggiano|parmesan|parmigiano-reggiano|cheese hard goat') ~ fixRefID(references$sustainability, 'hard cheese', 'hard cheese'),
    str_detect(Ingredients, 'halloumi|manchego|havarti|swiss|monterey jack|pepperjack|asiago|mozzarella|goat brown cheese|jarlsberg|cheese semi-hard|provolone|norvegia') ~ fixRefID(references$sustainability, 'hard to semi-hard cheese'),
    str_detect(Ingredients, 'ricotta|cheese blue|camembert|chevre|neufchatel|port salut|brie') ~ fixRefID(references$sustainability, 'soft-ripened cheese'),
    Ingredients == 'cheese american' ~ fixRefID(references$sustainability, 'processed cheese and spreads'),
    Ingredients == 'yogurt greek' ~ fixRefID(references$sustainability, 'yoghurt'),

    #Bread and rolls
    Ingredients %in% c('bread', 'bread coarse', 'tortilla coarse', 'crisp bread coarse', 'bread crumb') ~ fixRefID(references$sustainability, 'wheat bread and rolls', 'brown'),
    Ingredients %in% c('hamburger bun', 'bread white', 'tortilla', 'crisp bread', 'breadstick', 'ciabatta',
                       'rolls white', 'cracker cream') ~ fixRefID(references$sustainability, 'wheat bread and rolls', 'white'),
    
    #Herbs and spices
    str_detect(Ingredients, 'saffron|turmeric|sazon seasoning|ginger|caraway|lemongrass|basil|rosemary|thyme|tarragon|pepper|sage|masala|oregano|spice mix|nutmeg|cloves|coriander|cumin|dill|fenugreek leaf|juniper berry|cinnamon|chives|chervil|cardamom|caper|allspice|bay leaf|paprika powder|fennel seed') &
      !str_detect(Ingredients, 'sau|sweet') | str_detect(Ingredients, 'chili') & !str_detect(Ingredients, 'pepper') ~ fixRefID(references$sustainability, 'mixed', 'herbs'),
    
    #Not in ref
    Ingredients %in% c('homemade beef gravy', 'water broth beef', 'yeast nutritional', 'paste chili', 'cocoa powder', 'nacho', 'agar', 'gluten',
                       'corn starch', 'tortilla corn', 'nori seaweed', 'olive paste tapenade', 'salmon roe', 'sweet green pickle relish',
                       'plantain', 'tabasco', 'tapioca', 'miso paste white', 'sake', 'liquid smoke flavoring', 'pack high quality charcoal briquettes',
                       'cooking spray', 'quinoa', 'red food coloring', 'toro greek moussaka', 'sprouts alfalfa') ~ 0,
    str_detect(Ingredients, 'water broth|broth cube') ~ 0,
    
    TRUE ~ ID
    
  ))

#All sustainability data
various$ingredients_sustainability <- temp2 %>% select(Ingredients, ID) %>%
  inner_join(., clean, by = 'Ingredients') %>%
  full_join(various$recipe_weight) %>% unique() %>% #Add the total weight of each recipe in kg
  
  #Join with SHARP database
  full_join(databases$sustainability, by = 'ID') %>%
  select(-Ingredients.y) %>%
  #Rename to fit the other df's
  rename(Ingredients = Ingredients.x,
         sample_id = `Selected Meals`,
         group = Country) %>%
  
  #Turn long and calculate sustainability markers for each ingredient
  select(-c(ID)) %>%
  pivot_longer(
    cols = -c(sample_id, Ingredients, group, Source, Amounts, Weight, L1),
    names_to = 'feature',
    values_to = 'sustainability_value_kg' #SHARP has sustainability values pr 1kg of an ingredient
  ) %>%
  #Value pr 100g
  mutate(temp = (sustainability_value_kg*Amounts)) %>% #How much does that amount of ingredients contribute to CO/Landuse
  select(-c(sustainability_value_kg, Amounts)) %>%
  drop_na(Ingredients) %>%
  
  #How much does the ingredient contribute pr 100g recipe
  mutate(value = temp/Weight/10) %>%
  select(-c(temp, Weight)) %>%
  
  #Turn wide again
  pivot_wider(.,
              names_from = 'feature',
              values_from = 'value') %>%
  #Rename column
  rename(CO2 = `GHGE of 1 kg food as consumed_kgCO2eq`,
         Landuse = `Land use of 1 kg food as consumed_m2/yr`) %>%
  
  #Keep in long format
  #Pivot long to do the calculations
  pivot_longer(
    cols = -c(sample_id, Ingredients, group, Source, L1),
    names_to = 'feature',
    values_to = 'value'
  )

#Sum total per 100g recipe
various$recipe_sustainability <- various$ingredients_sustainability %>%
  
  #Calculate the whole recipe
  group_by(sample_id, group, feature) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() 

#Per foodgroup
various$foodgroup_sustainability <- various$ingredients_sustainability %>%
  
  #Calculate the whole recipe
  group_by(group, L1, feature) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() 

#Final df's to run analyses on----
#Df with all nutrients and sustainability markers pr 100 g recipe
final_recipes <- full_join(various$recipes_nutrients, various$recipe_sustainability) %>%
  
  #Turn wide
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  
  #Recalculate Salt (g) column from Sodium (mg) as it seems to have had different units in the nutrient db
  select(-Salt) %>%
  mutate(Salt = Sodium*2.5/1000)


saveRDS(final_recipes, 'recipe_data_ready_for_analysis.Rds')

#With individual ingredients, join the nutrient information with the sustainability one
final_ingredients <- full_join(various$ingredients_nutrients, various$ingredients_sustainability) %>%
                               
  drop_na(Ingredients) %>%
  #Add weight of each ingredient
  inner_join(various$ingredients_weight) %>%
  
  #Rename amounts to show it is in kg, and rename L1 to foodgroup
  rename(Foodgroup  = L1) %>%
  #Fill in missing values in foodgroup 
  group_by(Ingredients) %>%
  fill(Foodgroup, .direction = 'up') %>%
  ungroup() %>%
  
  #Turn wide
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  
  #Recalculate Salt (g) column from Sodium (mg) as it seems to have had different units in the nutrient db
  select(-Salt) %>%
  mutate(Salt = Sodium*2.5/1000)

saveRDS(final_ingredients, 'ingredients_data_ready_for_analysis.Rds')  
  
