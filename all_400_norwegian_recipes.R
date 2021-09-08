library(tidyverse)
library(stringi)
library(sustainableRecipes)

#Setup----
#Different databases to search through to find amounts in kilos, nutrient content and sustainability measurements
references <- list(
  'volume_weight' = readRDS('./Data/output/food_weight_ref.Rds') %>% filter(language == 'english'),
  'sustainability' = readRDS('./Data/output/sharp_ref.Rds'),
  'nutrients' = readRDS('./Data/output/nutrient_reference.Rds')
)
databases <- list(
  'volume_weight' = readRDS('./Data/output/all_weights.Rds'),
  'nutrients' = readRDS('./Data/output/nutrients_df.Rds') ,
  'sustainability' = readRDS('./Data/output/sharp_db.Rds')
)

#Some various datafraes to use in the cleanup
various <- list(
  #Units to keep in the recipes
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

#Read in the recipes
raw <- list.files(path = './Data/recipes/', pattern = "*cleaned.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.)) %>%
  #Columns to work with
  select(`Selected Meals`, Ingredients, Source, Country)

#Clean up the recipe ingredients----
recipes <- raw %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Separate ingredients into separate rows, trim whitespace and remove empty rows
  separate_rows(., Ingredients, sep = '\n') %>%
  mutate(Ingredients = str_trim(Ingredients, side = 'both'),
         Ingredients = str_squish(Ingredients)) %>%
  filter(Ingredients != '' | is.na(Ingredients)) %>%
  
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
         #Change vegetable/herb mixes to vegetables/herbs
         str_replace('1 hp rema frozen vegetables', '170 g carrot\n165 g broccoli\n165 g baby corn') %>% #Familiefavoritt grovkuttet in the recipe
         str_replace('frozen mixed vegetables', '40.75 g potato\n40.75 g sweet pepper red\n40.75 g green peas\n40.75 g carrot') %>% #Mix of vegetables typically found in the dish
         str_replace('600 g frozen vegetable mixture|600 g of frozen stew mix', '180 g swede\n150 g carrot\n150 g celeriac root\n60 g red onion\n60 g parsnip root') %>%
         str_replace('wok-vegetables', '110 g bean green asparagus\n110 g broccoli\n100 g mango\n85 g onion\n75 g water chestnut\n10 g rapeseed oil') %>% #Thai inspired wok REMA1000
         str_replace('400 g wok mixture with asparagus beans, carrot and leek', '133.33 g asparagus beans\n133.33 g carrot\n133.33 g leek') %>%
         str_replace('4 dl vegetables in cubes. eg. tomato, cucumber and corn', '1.33 dl tomato\n1.33 dl cucumber\n1.33 dl corn') %>%
         str_replace('1 cup fresh herbs eg basil, rosemary and leaf parsley', '0.33 cup basil\n0.33 cup rosemary\n0.33 cup parsley') %>%
         str_replace('60 g of fresh herbs e.g . parsley, basil, chives and coriander', '15 g fresh parsley\n15 g fresh basil\n15 g fresh chives\n15 g fresh coriander') %>%
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
  
  #Remove amounts for composite ingredients from the us recipes
  #mutate(Amounts = case_when(!str_detect(Ingredients, '\n') ~ Amounts)) %>%
  
  #Separate again
  separate_rows(Ingredients, sep = '\n') %>%
  
  #Remove some unnecessary characters in the ingredients column, and make sure there is a space between numbers and words ('2 dl', not '2dl)
  #Reformat all units to the same and fix some ingredient names for later----
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
         str_replace('half(?= pac)', '0.5')) %>%
  
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
         str_replace('1 slice, 4-5 cm thick', '300 g') %>% #Large portion of ribeye steak
         str_replace('2 packs of Prior Grill skewer turkey', '1200 g of Prior Grill skewer turkey') %>% #One pack is 600 g according to webshop
         
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
         str_replace('dill pickle spear|dill pickle slice|pickle, sliced, finely chopped', 'cucumber pickle') %>%
         str_replace('dried rice vermicelli', 'vermicelli pasta') %>%
         str_replace('corn tortilla chips', 'nacho') %>%
         str_replace('recao, or culantro', 'coriander') %>%
         str_replace('parlsey', 'parsley') %>%
         str_replace('mug, fresh', 'parsley, fresh') %>%
         str_replace('broccoli peas, frozen', 'broccoli, frozen') %>%
         str_replace('salvie', 'sage'))
  
  #Conditionals
  recipes <- recipes %>%
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'rosemary|basil|tarragon|coriander') ~ str_replace(Ingredients, 'pot|bunt', 'bunch'),
    !str_detect(Ingredients, '^[:digit:]') & (str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'lemon|lime')) ~ str_replace(Ingredients, '(?<=\\d)\\s', ' stk '),
    `Selected Meals` %in% c('Duck Breast With PasTine Pure', 'Cod With Bacon, Spinach And Rotmoss') & Ingredients == 'parsley' ~ 'parsnip root', #Mistake in translation
    `Selected Meals` == 'Pollock with herb onion and baked potatoes' & str_detect(Ingredients, 'fish') ~ str_replace(Ingredients, 'fish fillet', 'pollock'), #It says in the name it's pollock
    TRUE ~ Ingredients))

  #Extract the amounts to their own column----
  recipes <- recipes %>%
  mutate(Amounts = case_when(
    
    #Country == 'US' & !is.na(Amounts) ~ Amounts,
    
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
    #In the 100 previously used
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
    
    #In the other 300
    Ingredients == 'anchovies, can be looped' ~ '55 g', #One pack of anchovies is 50g
    Ingredients == 'asparagus' & str_detect(Amounts, 'hp') ~ '500 g', #Two packs of asparagus is 250*2
    str_detect(Ingredients, 'bearnaise') ~ '29 g', #Amount in one pack of Toro bearnaise
    Ingredients == 'broccoli, frozen' ~ '450 g', #In Norway, frozen broccoli is typically sold in packs of either 400, 450 or 500g depending on supermarket chain.
    Ingredients %in% c('celery stalk', 'stalk celery', 'rod celery') & Amounts == '0.25 hp' ~ '0.25 stk',
    Ingredients == 'rod celery' & Amounts == '0.2 hp' ~ '0.2 stk',
    Ingredients == 'celery stalk' & Amounts == '1 stk' ~ '380 g',
    Ingredients == 'cherry tomatoes, canned' ~ '400 g',
    Ingredients == 'cherry tomatoes, red' & Amounts == '1 hp' ~ '200 g', #It's their 200g product they show in the recipes
    Ingredients == 'frozen frozen peas' & Amounts == '1 hp' ~ '350 g', #Rema 1000 frozen peas
    Ingredients == 'guacamole spice mix' ~ '20 g', #Olde el paso
    Ingredients == 'heart salad' & Amounts == '1 hp' ~ '2 stk',
    str_detect(Ingredients, 'jasmine rice') & Amounts == '1 hp' ~ '120 g', #One boil-in-bag is 120 g
    Ingredients == 'mango chutney' & Amounts == '1 hp' ~ '50 g', #Recipe says 50 g
    Ingredients == 'mango chutney' & Amounts == '0.5 hp' ~ '175 g',
    Ingredients %in% c('naan bread', 'of naan bread') & Amounts == '1 hp' ~ '120 g', #From kolonial
    Ingredients %in% c('naan bread', 'nan bread') & Amounts == '2 hp' ~ '240 g',
    Ingredients == 'pasta' & Amounts == '2 hp' ~ '400 g', #Default large portion of pasta for four people, as other recipes at the same site. Couldn't find the specific recipe
    Ingredients == 'pearl barley' & Amounts == '4 hp' ~ '300 g', #One portion of pearl barley is default 75g
    Ingredients == 'peas, frozen' & Amounts == '2 hp' ~ '700 g',
    Ingredients == 'pizza filling' ~ '55 g', #Tomatoes, onions, corn starch, sugar, oil and spices
    Ingredients == 'pork chops' & Amounts == '2 hp' ~ '1000 g', #One portion of pork chops is 250g, four portions in the recipe
    Ingredients == 'mashed potatoes' ~ '800 g', #Default portion size of mashed potatoes is 200 g
    Ingredients == 'radish' & Amounts == '0.5 hp' ~ '65 g',
    Ingredients == 'radish' & Amounts == '1 hp' ~ '130 g',
    Ingredients == 'radish' & Amounts == '2 hp' ~ '260 g',
    str_detect(Ingredients, 'shimeji mushrooms') ~ '200 g', #Pack of rare mushrooms
    Ingredients == 'sour cream, light' & Amounts == '1 hp' ~ '300 g', #Tine default
    Ingredients == 'spinach' & Amounts == '1 hp' ~ '200 g', #BAMA
    str_detect(Ingredients, 'sugar snap peas') & Amounts == '1 hp' ~ '200 g',
    Ingredients == 'taco seasoning' & Amounts == '1 hp' ~ '28 g',
    Ingredients == 'tikka masala sauce' ~ '360 g',
    Ingredients %in% c('yoghurt, greek', 'yogurt, greek') & Amounts == '1 hp' ~ '200 g', #That's what the recipe says
    Ingredients %in% c('blueberry basket', 'raspberry basket') ~ '125 g',
    Ingredients == 'strawberry basket' ~ '400 g',
    Ingredients %in% c('sugar peas', 'sugar peas, in strips') & Amounts == '1 hp' ~ '150 g',
    Ingredients == 'potato steps' ~ '450 g',
    Ingredients == 'tagliatelle' & Amounts == '2 hp' ~ '400 g',
    TRUE ~ Amounts
  ))

#Standardise ingredients and units----
recipes <- recipes %>% standardiseIngredients() %>%
    
  #Remove some ingredients not in recipe when checked (bacon in Turkey fillet on party bed/KALKUNFILET PÅ PARTYSENG, and 'Refractory' ingredients from Salmon Shape With Pepper Root and 'Pepper Beef With Chopped Tomato And Chevre Salad')
  filter(!(`Selected Meals` == 'Turkey fillet on party bed' & Ingredients == 'bacon') & !str_detect(Ingredients, 'Refractory')) %>%
    
  #Add stk to the amounts of all the items listed in 'stk'
  mutate(Amounts = case_when(
    !str_detect(Amounts, '[:alpha:]') &
      str_detect(Ingredients_standardised, regex(paste0(various$stk, collapse = '|'), ignore_case = TRUE)) ~ paste0(Amounts, ' stk'),
    TRUE ~ Amounts)) %>%
    
  #Split Amounts into amounts and units
  separate(., Amounts, c('Amounts', 'unit'), sep = ' ') %>%
  #Turn amounts into numeric
  mutate_at('Amounts', ~as.numeric(.)) %>%
  #Standardise weight units to grams and volume units to dl
  standardiseUnits() %>%
    
  #Rename column for later
  rename(org_ingredients = Ingredients,
         Ingredients = Ingredients_standardised) %>%
    
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
  
  #Change the amount of lemon/orange/lime juice/zest from whole pieces of fruit to dl when applicable
  temp <- recipes %>%
    filter(str_detect(Ingredients, 'the juice|the zest')) %>%
    
    #If it says 'juice and zest' treat as a whole fruit
    mutate(Ingredients = str_replace(Ingredients, ', the juice and zest', '')) %>%
    
    #Turn whole fruits into the equivalent amount of juice in kg and zest in dl
    mutate(
      Amounts = case_when(
        #Juice (info from https://www.webstaurantstore.com/blog/2760/juice-in-citrus-fruits.html)
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(unit, 'stk') ~ (Amounts*3)*0.15,
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(unit, 'stk') ~ (Amounts*2)*0.15,
        #Zest (info from https://bakingbites.com/2017/01/how-much-zest-does-citrus-lemon-yield/)
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ (Amounts*(1/3))*0.15,
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ (Amounts*1)*0.15,
        str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ (Amounts*1.5)*0.15,
        
        TRUE ~ Amounts),
      unit = case_when(
        str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'stk') ~ 'kg',
        str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ 'dl',
        
        TRUE ~ unit)
    )
  
  #Add back
  recipes <- recipes %>%
    filter(!str_detect(Ingredients, 'the juice|the zest')) %>%
    bind_rows(., temp)
  
  #Split rows with 'salt and pepper and oil' and similar
  recipes <- recipes %>%
    separate_rows(Ingredients, sep = ' and ') %>%
    #Remove "slice" of pepper/salt, mistaken translation from Norwegian
    mutate(unit = case_when(
      !(Ingredients %in% c('salt', 'pepper') & unit == 'slice') ~ unit
    ))
  
  #Turn cooked products into their raw equivalents, using the convertion factors from Helsedirekttortatet Mål Vekt og Porsjonsstørrelser----
  recipes <- recipes %>%
      mutate(
        Amounts = case_when(
          Ingredients == 'bacon cooked' ~ Amounts/0.31,
          Ingredients %in% c('chicken breast cooked', 'chicken breast without skin cooked') ~ Amounts/0.8,
          Ingredients %in% c('chicken cooked', 'turkey meat cooked') ~ Amounts/0.4,
          Ingredients == 'lamb shoulder cooked' ~ Amounts/0.56,
          Ingredients == 'pork tenderloin cooked' ~ Amounts/0.75,
          Ingredients == 'pasta cooked' ~ Amounts/2.63,
          Ingredients == 'rice cooked' ~ Amounts/2.94,
          Ingredients == 'beetroot cooked' ~ Amounts/0.95, #Use values for parsley root
          
          TRUE ~ Amounts),
        #Remove cooked from ingredient name
        Ingredients = str_replace(Ingredients, ' cooked', '')
      )
  
  #Split broth into water and broth cubes
  #Pull out broth ingredients
  temp <- recipes %>%
    filter(str_detect(Ingredients, 'water broth')) %>%
    #Add / to split the rows into two separate ingredients, and add "cube" to broth
    mutate(Ingredients = str_replace(Ingredients, 'water broth', 'water/broth cube')) %>%
    separate_rows(Ingredients, sep = '/') %>%
    #Turn the broth cube amounts into number of broth cubes, 1 cube per 5 dl/0.5 kg water
    mutate(
      Amounts = case_when(
        str_detect(Ingredients, 'broth cube') ~ Amounts/0.5,
        TRUE ~ Amounts),
      unit = case_when(
        str_detect(Ingredients, 'broth cube') ~ 'stk',
        TRUE ~ unit)
    )
  
  #Add back to recipes
  recipes <- recipes %>%
    #Remove broth wthout broth cubes
    filter(!str_detect(Ingredients, 'water broth')) %>%
    #Add back with broth cues
    bind_rows(., temp)
  
#Save the orginal and standardized ingredient names for comparisons-----
various$org_ingredients <- recipes %>% select(`Selected Meals`, Ingredients, org_ingredients)
  
#Sum the amounts for all the ingredients with the same name for each recipe----
recipes <- recipes %>%
    group_by(`Selected Meals`, Ingredients, Country, Source, unit) %>%
    summarise(Amounts = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
    #Using na.rm gives ingredients with no amounts a '0' value, change back to NA
    mutate(Amounts = case_when(
      !is.na(unit) ~ Amounts
    ))
  
#Map to databases, find the amount of each ingredient by weight,the nutritional values and environmental impact
#Ingredients with amounts that are not in weight
various$get_amounts_kg <- recipes %>%
  filter(unit != 'kg')
  #See which ingredients have no unit or amounts whatsoever
  various$missing <- recipes %>% filter(is.na(Amounts))
  
  #Map to volume/weight database
  temp <- checkRef(various$get_amounts_kg %>% select(Ingredients) %>% unique(), reference = references$volume_weight)
  
  temp2 <- temp %>%
    full_join(various$get_amounts_kg) %>% unique() %>%
    filter(!is.na(Amounts)) %>%
    
    #Fix some errors
    mutate(ID = case_when(
      Ingredients == 'butter clarified ghee' ~ fixRefID(reference = references$volume_weight, 'ghee'),
      Ingredients %in% c('cheese brie', 'cheese mascarpone') ~ fixRefID(reference = references$volume_weight, 'soft'),
      Ingredients == 'eggplant' ~ fixRefID(reference = references$volume_weight, 'eggplant'),
      Ingredients == 'sugar' ~ fixRefID(reference = references$volume_weight, 'sugar', 'white'),
      Ingredients == 'almond' ~ fixRefID(reference = references$volume_weight, 'almonds'),
      Ingredients == 'apricot dried' ~ fixRefID(reference = references$volume_weight, 'apricots', 'dried'),
      Ingredients == 'bread flat hard' ~ fixRefID(reference = references$volume_weight, 'flatbread', 'hard'),
      Ingredients == 'caper' ~ fixRefID(reference = references$volume_weight, 'capers'),
      str_detect(Ingredients, 'cheddar|jarlsberg|norvegia|semi-hard') ~ fixRefID(reference = references$volume_weight, 'hard to semi-hard cheese'),
      Ingredients == 'cheese mozzarella' ~ fixRefID(reference = references$volume_weight, 'mozzarella'),
      Ingredients == 'parmesan cheese' ~ fixRefID(reference = references$volume_weight, 'parmesan'),
      Ingredients %in% c('chili pepper green', 'chili pepper jalapeno') ~ fixRefID(reference = references$volume_weight, 'chili', 'red'), #Same in volume
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
      Ingredients %in% c('salad', 'salad lettuce') & unit == 'stk' ~ fixRefID(reference = references$volume_weight, 'heart', 'salad'),
      Ingredients %in% c('lettuce', 'salad lettuce') & unit == 'dl' ~ fixRefID(reference = references$volume_weight, 'iceberg', 'lettuce'),
      Ingredients == 'chopped parsley or generous sprinkling dill fronds, or mixture optional' ~ fixRefID(reference = references$volume_weight, 'parsley', 'fresh'),
      Ingredients == 'basil' & unit == 'twig' ~ fixRefID(reference = references$volume_weight, 'basil', 'fresh'),
      Ingredients == 'coriander' & unit == 'twig' ~ fixRefID(reference = references$volume_weight, 'coriander', 'fresh'),
      Ingredients %in% c('bean canned', 'bean black') ~ fixRefID(reference = references$volume_weight, 'bean black', 'canned'),
      Ingredients == 'rice brown long grain' ~ fixRefID(reference = references$volume_weight, 'rice'),
      str_detect(Ingredients, 'polenta') ~ fixRefID(references$volume_weight, 'cornmeal', 'polenta'),
      Ingredients == 'corn' & unit == 'dl' ~ fixRefID(references$volume_weight, 'corn', 'kernel'),
      Ingredients == 'cranberries jam' ~ fixRefID(references$volume_weight, 'jam', 'marmelade'),
      Ingredients == 'hamburger bun' ~ fixRefID(references$volume_weight, 'hamburger', 'bread'),
      Ingredients == 'mustard honey' ~ fixRefID(references$volume_weight, 'mustard'),
        
      #Ingredients with no references
      Ingredients %in% c('fish soup base', 'mustard powder', 'burrito spice mix', 'bolognese base',
                         'chinese five spice', 'of lime sheet, shredded', 'of dip mix', 'of coffee lime leaf, dried, soaked in warm water 15 min') ~ 0,
      
      TRUE ~ ID
    ))


  
  