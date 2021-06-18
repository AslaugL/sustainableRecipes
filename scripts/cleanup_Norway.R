library(tidyverse)
library(readxl)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data')

#Cleanup Norway ----
#Various objects to keep environment clean
various <- list(
  
  #Units to keep in the recipes
  #Recipes from Kolonialen has translated 'pk/pakke/stykk' to 'hp'
  #What is hp?
  'units' = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf',
              'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice',
              'tassel', 'neve', 'ml', 'bunch', 'pack', 'plate', 'pot', 'drop') %>%
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
            'squid', 'stock cube',
            
            'tenderloin', 'tomato', 'tortilla', 'trout', 'turkey')
  
)

#Raw data
#From each individual dataset
raw_list <- list(
  'aperitif' = read_csv('./oppskrifter/aperitif.csv'),
  'klikk' = read_csv('./oppskrifter/klikk.csv'),
  'tine' = read_csv('./oppskrifter/tine.csv'),
  'kolonial' = read_csv('./Oppskrifter/kolonialen.csv')
) %>%
  #Columns needed for my work
  bind_rows(.) %>% select(No, `Selected Meals`, Ingredients, Source, Country)
temp <- read_csv('./oppskrifter/Data_Norway.csv') %>%
  select(`Nr. Of portion`, `Selected Meals`) %>%
  full_join(raw_list) %>%
  group_by(`Selected Meals`) %>%
  fill(`Nr. Of portion`) %>%
  ungroup() %>% unique()

#Get a cleaned up version of the ingredients for the 100 recipes (without words such as "for the vegetables" etc)
raw <- read_xlsx('./oppskrifter/Data_NO_100.xlsx') %>%
  select(`Selected Meals`, Ingredients, Source, Country) %>%
  
  #Rename some recipe names that are different in the datasets
  mutate(`Selected Meals` = `Selected Meals` %>%
           str_replace('PinnekjÃ¸tt', 'Pinnekjøtt') %>%
           str_replace('Biff with BearnÃ© saus', 'Biff with Bearné saus') %>%
           str_replace('sauted reindeer', 'sautéed reindeer') %>%
           str_replace('Muslim steak curry', 'Muslim steak curry')
         ) %>%
  
  inner_join(temp %>% select(No, `Selected Meals`, `Nr. Of portion`, Source), by = c('Selected Meals', 'Source'))

rm(temp)
rm(raw_list)

#Clean the data
clean <- raw %>%

  #Separate ingredients into separate rows
  separate_rows(., Ingredients, sep = '\n') %>%
  #Remove some unnecessary characters in the ingredients column, and make sure there is a space between numbers and words ('2 dl', not '2dl)
  mutate(Ingredients = Ingredients %>%
           str_replace_all('["()]|possibly', '') %>%
           str_replace('1,000|1 000', '1000') %>%
           str_replace('(?<=\\d)(?=[^\\d\\s\\.-])', ' ') %>%
           str_replace('(?<=\\d) ½', '.5') %>%
           str_replace('(?<=\\d) ⅘', '.8') %>%
           str_replace('(?<=\\d) ⅕', '.2') %>%
           str_replace('½', '0.5') %>%
           str_replace('¼', '0.25') %>%
           str_replace('¾', '0.75') %>%
           str_replace('2 -3|2-3', '3') %>% #Use three of each of the two foods with 2-3 written in the recipe
           str_replace('half(?= pac)', '0.5') %>%
           str_trim()
  ) %>%

  #Remove rows with no ingredients or that include names of part of the meal (ie 'vegetables:')
  filter(!Ingredients == '') %>%
  filter(!str_detect(Ingredients, ':|Serve with|FOR SERVING|For serving|For portion|Acce|ACCE|kewer|Sauce|Saus|Salad|Lake|Fill|:ALPHA:')) %>% #Used in Tine recipes

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
         str_replace('paprikai', 'paprika i') %>%
         str_replace_all('hvitløkbåt|hvitløkfedd', 'garlic clove') %>%

         #Change units of ingredients
         str_replace('bacon slice|pieces of bacon|pieces of good sliced bacon', 'slice bacon') %>%
         str_replace('0.5 stk fl tørr hvitvin', '0.75 dl tørr hvitvin') %>% #One bottle of white wine is 750ml
         str_replace('4 tynne', '4 slice') %>% #Spekeskinke is counted by slices
         str_replace('4 slices entrecote', '4 portion entrecote') %>%
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
         str_replace('stk ltr|liter', 'l') %>%
         str_replace('portions|servings|serving', 'portion') %>%
         str_replace('pinches|pinched|knife-wiped', 'pinch') %>%
         str_replace('\\bbt\\b|tassel', 'bunch') %>%
         str_replace('boxes', 'box') %>%
         str_replace('cups', 'cup') %>%
         str_replace('glasses', 'glass') %>%
         str_replace('3 plates', '225 g') %>% #One pack of six plates butterdough has 450 g's worth
         str_replace('drops', 'drop') %>%
         str_replace('a squeezed', '1 stk') %>%
         str_replace('pieces|piece|pcs', 'stk') %>%
         str_replace('\\spc\\s', ' stk ') %>%
         str_replace('skiver|skive|slices', 'slice') %>%
         str_replace('fists|fist|handfuls|handful', 'neve') %>%
         str_replace('leaves', 'leaf') %>%
         str_replace('twigs|twig', 'sprig') %>%
         str_replace('12 small bouquets of broccoli', '300 g broccoli') #1 bouquet is 25g according to the Weight and measurement database
) %>%

  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'rosemary|basil') ~ str_replace(Ingredients, 'pot', 'bunch'),
    str_detect(Ingredients, 'hvitløk|garlic') ~ str_replace(Ingredients, 'båter|båt|boats|boat|cloves|fedd', 'clove'),
    str_detect(Ingredients, 'juice of') &
      !str_detect(Ingredients, paste0(various$units, collapse = '|')) ~ str_replace(Ingredients, '(?<=\\d)\\s', ' stk '),
    TRUE ~ Ingredients)) %>%

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
    TRUE ~ Ingredients)) %>%

  #Fix some amounts and ingredients that either became wrong after last step, are missing, or specifics are added in the text----
mutate(
  Ingredients = case_when(
    Amounts == '1 turkey' ~ paste0('turkey ', Ingredients),
    Amounts == '4 chicken' ~ paste0('chicken ', Ingredients),
    Amounts == '4 salmon' ~ paste0('salmon ', Ingredients),
    TRUE ~ Ingredients),
  Amounts = case_when(
    Amounts == '1 turkey' ~ '1 stk',
    Amounts == '4 chicken' ~ '4 stk',
    Amounts == '4 salmon' ~ '0.56 kg', #It says in the recipe to use four salmon fillets at 140g each
    str_detect(Ingredients, 'well hung') ~ '0.4 kg', #One slice of , 4-5 cm thick, well hung beef comb should be one portion of filetkam, 3-400g https://www.matprat.no/oppdelingsguiden/storfe/filetkam/
    Ingredients == '400 g canned tomatoes' ~ '0.4 kg',
    Ingredients == 'chicken about 1 kg' ~ '1 kg',
    Ingredients == 'lollo rosso, leaf' ~ '2 leaf',
    str_detect(Ingredients, 'skrelt mandelpotet') ~ paste0(Amounts, ' g'),
    str_detect(Ingredients, 'chicken  fillets about 100 g per person') ~ '0.4 kg', #4 fillets of 100g each
    str_detect(Ingredients, 'approx. 2.5 kg') ~ '2.5 kg',
    str_detect(Ingredients, 'of cod back per approx. 180 g') ~ '0.72 kg', #4 bacj fillets of 180 g each
    str_detect(Ingredients, 'of salmon fillet about 100 g , without skins and bones') ~ '0.8 kg', #8 of them
    str_detect(Ingredients, 'about .5 kg') ~ '0.5 kg',
    Ingredients == 'bookcase ham' ~ paste0(Amounts, ' skive'), #Bogskinke are counted in slices
    str_detect(Ingredients, '\\bclove\\b') ~ str_replace(Amounts, ' stk| tsp', ''), #one tsp garlic is about a clove/fedd
    `Selected Meals` == 'Satay spear' & Amounts == '9 cup' ~ '9 dl', #Checked the recipe https://oppskrift.klikk.no/satayspyd/646/
    str_detect(`Selected Meals`, 'urkey steak') & str_detect(Ingredients, 'turkey breast') ~ paste0(Amounts, ' portion'), #1 turkey steak is one portion of turkey breast

    #Add stk to the amounts of all the items listed in 'stk'
    !str_detect(Amounts, '[:alpha:]') &
      str_detect(Ingredients, regex(paste0(various$stk, collapse = '|'), ignore_case = TRUE)) ~ paste0(Amounts, ' stk'),
    TRUE ~ Amounts),
  Amounts = case_when(
    str_detect(Ingredients, '\\bclove\\b') ~ paste0(Amounts, ' clove'), #Garlic
    TRUE ~ Amounts )) %>%

  #Remove some fill-words
  mutate(Ingredients = Ingredients %>%
           str_trim() %>% #Remove leading whitespace
           str_replace_all('^of |^s |^s of', '')) %>% #Remove leading of and s

  #Clean up some ingredient names and translation errors----
  mutate(Ingredients = Ingredients %>%

         #Meat
         str_replace('(?<=\\w)filet', ' filet') %>%
         str_replace('bouilljon', 'broth') %>%
         str_replace('beef leg', 'beef bone') %>% #Stock bones to make broth
         str_replace('beef comb', 'T-bone') %>% #Filetkam/parts of the lower neck with bone
         str_replace('beef, elk or reindeer', 'beef') %>%
         str_replace('book or beef back', 'chuck roll') %>%
         str_replace('bookcase ham', 'bogskinke') %>%
         str_replace('calf text', 'calf steak') %>%
         str_replace('calf tail', 'calf leg') %>% #The pieces of calf tail in the osso bucco recipe from klikk is calf leg https://oppskrift.klikk.no/ossobuco-med-gremolata-og-risotto/1068/
         str_replace('cattle, slices', 'beef') %>%
         str_replace('chicken or broth water + 0.5 broth cube', 'chicken broth') %>%
         str_replace('chicken fillets|chicken fillet|kylling fileter|kylling filet', 'chicken breast') %>%
         str_replace('chicken, divided into eight or four chicken breasts divided into stk s|whole chicken, or breast \\/ thigh in piece s', 'whole chicken') %>%
         str_replace('Christmas ham', 'boiled ham') %>%
         str_replace('clubs', 'drumstick') %>%
         str_replace('cote de boef of angus', 'entrecôtekam') %>% #Name used in Mål vekt og porsjonsstørrelser'
         str_replace('dice meat broth', 'broth cube meat') %>%
         str_replace('farce lean', 'meat low fat') %>%
         str_replace('frying pan|power', 'stock') %>% #From https://oppskrift.klikk.no/m%C3%B8rbradstek-med-sherrysaus/64/ and other recipes
         str_replace('high-roast beef or other reasonably priced meat|beef from leg or high back', 'Beef Rib roast') %>%
         str_replace('knocked meat', 'bottom round, outside flat, meat') %>% #English/american name for the piece of beef
         str_replace('lamb meat with bone, in slice book or roast', 'lamb shoulder') %>%
         str_replace('lardo or lightly salted bottle', 'bacon') %>% #saltflesk is not exactly the same as bacon, but close
         str_replace('minced meat|minced beef|chop dough|carbonated dough|meat dough', 'ground meat') %>%
         str_replace('outer fillet', 'tenderloin') %>%
         str_replace('parmask', 'spekeskinke') %>% #parmaskinke
         str_replace('pork fillet chops', 'pork chops') %>%
         str_replace('steak strips', 'beef steak strips') %>%
         str_replace('turkey fillet|turkey steak', 'turkey breast') %>%
         str_replace('turkey  calculate 300 g whole meat with bones per portion, 200 g pure fillet', 'whole turkey') %>%
         str_replace('whole chicken, or as in soweto hen', 'whole chicken') %>%
         str_replace('carbonated dough', '') %>%
         str_replace('meat dough', 'meat') %>%

         #Fish
         str_replace('anchovy fillet', 'anchovy fillets') %>%
         str_replace('catfish', 'Atlantic catfish, lean fish') %>% #Atlantic catfish is the correct name for the fish we usually eat in Norway 'steinbit'
         str_replace('chop fillet', 'pollock') %>% #Pollock not sausage in the recipe https://www.aperitif.no/oppskrifter/oppskrift/ovnsbakt-sei-i-sitron-og-eplesaus,1974362
         str_replace('kreps, evt. krepsehaler/reker', 'kreps') %>%
         str_replace('makrell fileter', 'makrell fatty fish') %>%
         str_replace('medium-sized polar sea urchins', 'ishavsrøye, fatty fish') %>% #Not urchins but ishavsrøye in this recipe
         str_replace('pollack', 'cod') %>%
         str_replace('rake fish', 'trout') %>% #All recipes with rakfisk is with trout
         str_replace('rekepasta', 'shrimp paste') %>%
         str_replace('from one salmon side per stk', 'salmon') %>%
         str_replace('slice or fillets of cod, saithe or onion.', 'cod fillet') %>%

         #Plants
         str_replace('(?<!r)aisins', 'raisins') %>%
         str_replace('all kinds of malt|all kinds', 'allspice') %>%
         str_replace('bagel or other pastry', 'bagel') %>%
         str_replace('barley pig|barley', 'pearl barley') %>%
         str_replace('bell pepper|shredded pepper|of baked pepper, rind and shredded|red pepper|green pepper in strips', 'sweet pepper') %>%
         str_replace('bread grater', 'brødrasp') %>% #Should find a good english term
         str_replace('broccoli peas', 'broccoli') %>%
         str_replace('brunsukker', 'brown sugar') %>%
         str_replace('of minima, in  pieces', 'baby corn') %>%
         str_replace('bulb', 'pear') %>%
         str_replace('cabbage head', 'cabbage') %>%
         str_replace('cabbage root|Cabbage root', 'swede') %>%
         str_replace('celery root', 'Celariac root') %>%
         str_replace('chana dal, or chick peas', 'chick peas') %>%
         str_replace('chickpea', 'chick pea') %>%
         str_replace('chilli|Chilli', 'chili') %>%
         str_replace('coarse tortilla leeks', 'tortilla fullkorn') %>%
         str_replace('coat corn, or bread grater from  of bread', 'bread') %>%
         str_replace('corn corn|corn maize|corn starch|corn flour|light corn stalks|light corn evenly or corn|dark corn evenness|maisennajevning', 'maisstivelse') %>% #Translations of maisenna in aperitif and klikk recipes
         str_replace('corn flask|corn colbs|corn colb', 'corn cob') %>%
         str_replace('crispy bread, coarse', 'crisp bread, brown, rye') %>% #Husman Wasa
         str_replace('earth shocks', 'jordskokk') %>%
         str_replace('garlic baguettes|baguettes, fine, half fried', 'baguette') %>%
         str_replace('glue', 'lime') %>%
         str_replace('grilling flour', 'griljermel') %>%
         str_replace('head oak leaf salad or other salad', 'lettuce') %>% #Head lettuce?
         str_replace('hijackers|\\bcaper\\b', 'capers') %>%
         str_replace('juniper berries|juniper, crushed', 'juniper berry') %>%
         str_replace('kokosfløte', 'coconut milk') %>%
         str_replace('kfedd', 'k fedd') %>%
         str_replace('large disc shape loff', 'baguette') %>%
         str_replace('leaf parsley', 'parsley') %>%
         str_replace('lentils, green or alternatively cooked chick peas', 'lentils') %>%
         str_replace('lemon grass', 'lemongrass') %>%
         str_replace('lemons, squeezed|sitronsaft', 'lemon juice') %>%
         str_replace('mangochutney', 'mango chutney') %>%
         str_replace('mild chili', 'chili') %>%
         str_replace('mold', 'loff') %>%
         str_replace('mug, fresh|finely chopped mug|mug of parsley|mugs, fresh', 'kruspersille') %>%
         str_replace('nan bread', 'naan bread') %>%
         str_replace('olive oil, plus extra for frying bread', 'olive oil') %>%
         str_replace('oil / butter', 'oil') %>%
         str_replace('painted juniper berries', 'knuste einebær') %>%
         str_replace('paprika spice', 'paprika powder') %>%
         str_replace('toasted pine kernels or pumpkin kernels', 'pine seed') %>%
         str_replace('potato steps', 'mashed potatoes') %>%
         str_replace('potato flour', 'potato starch') %>%
         str_replace('raw yellowed|mini yellow roots', 'gulrot') %>%
         str_replace('red curry pasta', 'red curry paste') %>%
         str_replace('redlk', 'onion') %>% #rødløk
         str_replace('ripped tomatoes', 'tomatoes canned') %>%
         str_replace('rolls or good bread', 'rolls') %>%
         str_replace('ruccula or salad mix', 'salad mix') %>%
         str_replace('saffron thread or turmeri', 'saffron') %>%
         str_replace('spring onions|spring onion', 'Scallion') %>%
         str_replace('stem celery|celery bar|celery rod|bar celery|rod celery|of celery finely chopped', 'celery stalk') %>%
         str_replace('sugar asparagus|sugar peas', 'sugar snap peas') %>% #Recipe says sugar peas or asparagus
         str_replace('sultanarosins', 'sultana raisins') %>%
         str_replace('tagliatelle', 'pasta') %>%
         str_replace('tortilla lefts medium|tortilla leaf medium', 'tortilla medium') %>%
         str_replace('tomato pasta', 'tomato paste') %>%
         str_replace('tomato puree', 'Tomato purée') %>%
         str_replace("ukoctris uncle ben's etc", "rice") %>%
         str_replace('whole cloves|whole carnations|carnation nails|nellikspiker', 'cloves') %>%
         str_replace('zedoari', 'zedoary') %>%
         str_replace('Sunniva® Pressed Orange Juice with Fruit Meat', 'orange juice') %>%
         str_replace('ruccula', 'ruccola') %>%
         str_replace('brewed beans|horse beans Belgian peas', 'broad beans') %>%

         #Dairy
         str_replace('Cream Fraîche', 'crème fraîche') %>%
         str_replace('clarified butter, or oil', 'ghee') %>%
         str_replace('fat cheese in cubes 2 x2 cm', 'feta cheese') %>%
         str_replace('light streams', 'lett rømme') %>%
         str_replace('TINE Dairy butter for roasting apple|Thyme and juniper butter|TINE Dairy butter for the peas', 'butter') %>%
         str_replace('TINE Food cream', 'fløte') %>% #Actually matfløte, a bit different nutrient composition than regular
         str_replace('TINE Genuine Grated Cheese Original|TINE Genuine Torn Cheese Original', 'hvitost, raspet') %>%
         str_replace('TINE Kremgo® Natural', 'cream cheese') %>%
         str_replace('TINE Light flow 10 %|TINE Lightroom 10 %|Tine Lightroom 10 %', 'TINE lett rømme 10%') %>%
         str_replace('TINE Lightroom 18 %|TINE Light flow 18 %|Tine Lightroom 18 %', 'Tine lett rømme 18%') %>%
         str_replace('TINE Light 2 % a good alternative to sour cream', 'lett rømme 2%') %>%
         str_replace('TINE Light Food Cream or TineMelk Whole', 'helmelk') %>%
         str_replace('Seatroom', 'seter rømme') %>%
         str_replace('Jarlsberg® Tear', 'Jarlsberg Revet') %>%
         str_replace('Norman cheese|Norzola', 'blåmuggost') %>% #Norman cheese is actually normanna cheese from Tine, a blåmuggost. Norzola is also a blåmuggost
         str_replace('\\bbrie\\b', 'hvitmuggost') %>% #Brie is a hvitmuggost
         str_replace('yogurt|Yogurt', 'yoghurt') %>%

         #Div
         str_replace('boats', '') %>%
         str_replace('dice only', 'cube') %>%
         str_replace('fresh', '') %>%
         str_replace('\\bfry\\b', 'broth') %>% #Stekesjy
         str_replace_all('small|large|medium|unsalted|soft', '') %>% #Remove sizing and unnecessary adjectives, use medium size for everything if reference has sizing categories
         str_replace('tørr hvitvin, evt. 4 dl  hønsebuljong og saften av 0.5 sitron to hvitvin', 'hvitvin') %>%
         str_replace('olive oil / butter', 'olive oil') %>%
         str_replace('sunflower oil and roasting oil', 'sunflower oil') %>%
         str_replace('tacos 2 tablespoons is about the same as 1 pack of tacos|tacos can be looped', 'taco seasoning') %>%
         str_replace('tacos sauce or salsa', 'taco sauce') %>%
         str_replace('water to the corn', 'water') %>%
         str_replace('blocks of Italian spice broth or other spice broth', 'broth cube') %>%
         str_replace('thaibasilikum', 'basilikum') %>%
         str_replace("Old El Paso Thick'n Chunky Salsa", "Olde El Paso Tomato Salsa") %>%
         str_replace('aroma soup', 'aroma champignon')
         ) %>%
  #Trim whitespace
  mutate_at('Ingredients', ~str_trim(.)) %>%

  #Conditionals
  mutate(
    Ingredients = case_when(
      str_detect(Ingredients, 'broadleaf|knit flab') ~ 'anglerfish, lean fish',
      Ingredients == 'sei' ~ 'pollock, lean fish',
      Ingredients == 'soy' ~ 'soy sauce', #This seems most likely as it is part of a marinade for sashimi
      str_detect(Ingredients, 'cardamom|kardemommebelger') & str_detect(Amounts, 'stk') ~ 'cardamom pod',
      str_detect(Ingredients, 'brødrasp') & str_detect(Amounts, 'slice') ~ 'bread',
      Ingredients == 'corn' & str_detect(Amounts, 'stk') ~ 'corn cob',
      str_detect(Ingredients, 'chilisaus') ~ 'chili sauce',
      str_detect(Ingredients, 'ground') & !str_detect(Ingredients, 'meat') ~ str_replace(Ingredients, 'ground', ''),
      str_detect(`Selected Meals`, 'asta') ~ str_replace(Ingredients, 'Penne|penne|Pens|pens|pen|paste', 'pasta'),
      str_detect(Ingredients, 'juice') ~ str_replace(Ingredients, 'paste', 'lime'),
      str_detect(Ingredients, 'herring fillet') ~ 'herring fillet', #Choice between salmon and herring in the recipe, recipe is called "herring with..."
      str_detect(Ingredients, 'beef|elk|lamb') ~ str_replace(Ingredients, 'thigh', 'leg'),
      str_detect(`Selected Meals`,
                 'Cowboy Casserole With Beef|Russian chicken|Pollack with shrimp and asparagus') &
        Ingredients == 'corn' ~ 'maisstivelse',
      Ingredients %in% c('of baby corn',' of corn', 'corn') & str_detect(Amounts, 'dl') ~ 'corn kernel',
      str_detect(Ingredients, 'ortilla')& !str_detect(Ingredients, 'fullkorn|whole grain') ~ 'tortilla fin', #Use white flour unless it says wholemeal
      str_detect(Ingredients, 'ortilla') & str_detect(Ingredients, 'fullkorn|whole grain') ~ 'tortilla fullkorn wheat',
      str_detect(Ingredients, 'orvegia|arlsberg|semi-solid') ~ 'hard to semi-hard cheese',
      str_detect(Ingredients, 'grated') & str_detect(Ingredients, 'cheese') ~ 'hard to semi-hard cheese',
      Ingredients == 'onion' & str_detect(Amounts, 'slice') ~ 'Leeks', #The two slices of onio n in Pot au Feu is actually two stk leeks, and the lettuce is endives/sikori https://www.aperitif.no/oppskrifter/oppskrift/pot-au-feu,2102492
      Ingredients == 'jam' & Amounts == '8 slice' ~ 'pickled beetroot', #From recipe
      `Selected Meals` == 'Herring with apple and beet salad and eggs' & Ingredients == 'beetroot' ~ 'pickle juice', #From recipe
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'pasta') ~ str_replace(Ingredients, 'paste', ''),
      Ingredients == 'turkey' ~ 'whole turkey',
      `Selected Meals` == 'Faith Spiced Salmon With Quinoa Salad' & Ingredients == 'salsa' ~ 'tomato salsa',
      Ingredients == 'sambal oelek' ~ 'chili paste',
      Ingredients %in% c('kyllinger', 'spring chickens', 'spring chicken', 'chicken, grilled', 'chicken') ~ 'whole chicken',
      `Selected Meals` == 'Shrimp el diablo' & Ingredients == 'shrimp' ~ 'scampi',
      str_detect(Ingredients, 'mackerel|herring') & str_detect(Amounts, 'stk') ~ paste0(Ingredients, ' fatty fish'),
      Ingredients == 'curry' ~ 'curry powder',
      str_detect(Ingredients, 'lime|Lime') & str_detect(Amounts, 'stk') ~ 'lime',
      Ingredients %in% c('lime', 'lime, pressed') & str_detect(Amounts, 'dl') ~ 'lime juice',
      str_detect(Ingredients, 'oregano spice') ~ 'oregano dried',
      Ingredients == 'celery' ~ 'celery stalk',
      Ingredients == 'sitron' & str_detect(Amounts, 'dl|tbsp') ~ 'lemon juice',
      str_detect(Ingredients, 'lemon\\b|Lemon\\b') & str_detect(Amounts, 'stk') ~ 'lemon',
      str_detect(Ingredients, 'karriblader eller 3 laurbærblader') ~ 'bay leaf',
      Ingredients == 'ketchup' ~ 'tomato ketchup',
      `Selected Meals` == 'Baked turkey with puff pastry' & str_detect(Ingredients, 'beef') ~ 'beef stock',
      `Selected Meals` == 'Christmas Ribbe' & str_detect(Ingredients, 'rib') ~ 'pork belly',
      str_detect(Ingredients, 'hvitvin') ~ 'white vine',
      str_detect(Ingredients, 'eddik') ~ 'vinegar',
      TRUE ~ Ingredients),
    Amounts = case_when(
      Ingredients == 'bagel' ~ '4 stk', #Mistake in recipe
      Ingredients == 'Leeks' & str_detect(Amounts, 'slice') ~ '2 stk',
      Ingredients == 'spice butter' ~ '4 tbsp', #In Tine recipes, 'spice butter' is not one pack of butter but actually a tablespoon for each person
      str_detect(Ingredients, 'gground') ~ '250 g',
      str_detect(Ingredients, 'Cottage Cheese of 250 grams') ~ '250 g',
      Ingredients == 'jordskokk' ~ '4 stk',
      Ingredients == 'of crab claws' ~ '4 portion', #Cannot find weight of individual crab claws, use portion size instead
      Ingredients == 'kreps' ~ '6 portion', #Cannot find weight of individual kreps, use portion size instead
      str_detect(Ingredients, 'scampi|makrell|mackerel|herring|salmon chops|quick rice|entrecôtekam') & str_detect(Amounts, 'stk|pack') ~ paste0(`Nr. Of portion`, ' portion'), #Use portion size
      str_detect(Ingredients, 'cod fillets') & is.na(Amounts) ~ paste0(`Nr. Of portion`, ' portion'),
      str_detect(Ingredients, 'calf') & str_detect(Amounts, 'slice') ~ paste0(`Nr. Of portion`, ' portion'), #Use portion size
      str_detect(Ingredients, 'beef steak|beef tenderloin') & str_detect(Amounts, 'stk') ~ str_replace(Amounts, 'stk', 'portion'),
      str_detect(Ingredients, 'baguette') & `Selected Meals` == 'Parisian steak' ~ '4 stk',
      str_detect(Ingredients, 'ishavsrøye') ~ paste0(`Nr. Of portion`, ' portion'),
      str_detect(Ingredients, 'can tomatoes, roughly chopped') ~ '2 can',
      str_detect(Ingredients, 'kha') ~ '2 cm',
      Ingredients == 'neck chops' & Amounts == '4' ~ '4 portion',
      Ingredients == 'of TINE Dairy butter for frying' ~ '2 tbsp',
      Ingredients == 'loff, whole, slice without crust' ~ '10 slice',
      Ingredients == 'broth cube' & Amounts == '2' ~ '2 stk',
      Ingredients == 'Olde El Paso Tomato Salsa' ~ '226 g',
      str_detect(Ingredients, 'chickens, about 500 g') ~ '2000 g', #Four stk
      Ingredients %in% c('grated shell of 1 lemon', 'juice and peel of 1 lime') ~ '1 stk',
      Ingredients == 'cherry tomato' & is.na(Amounts) ~ '12 stk', #Two for each person
      Ingredients == 'cl peanut oil or walnut oil for frying' ~ '3 dl',
      `Selected Meals` == 'Fish and Chips' & str_detect(Ingredients, 'anglerfish') ~ '800 g',
      TRUE ~ Amounts) ) %>%

  #The 'Half fermented trout' recipe were lacking in the amounts, filled in by this recipe https://oppskrift.klikk.no/forrett-med-rakfisk/3862/
  #Not part of the 100 recipes in the analyses
  #mutate(
   # `Nr. Of portion` = case_when(
    #  `Selected Meals` == 'Half-fermented trout' ~ 2,
     # TRUE ~ `Nr. Of portion`
    #),
    #Ingredients = case_when(
     # `Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'lefse') ~ 'potetlefse', #It is potetlefse that is used with rakfisk
      #`Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'butter') ~ 'sour cream', #This is the tradition
      #`Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'onion') ~ 'onion', #Recipe says red onion, but red/yellow onion are the same in most food databases
      #TRUE ~ Ingredients
    #),
    #Amounts = case_when(
     # `Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'lefse') ~ '4 stk',
    #  `Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'sour') ~ '4 tbsp',
     # `Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'onion') ~ '0.5 stk',
      #`Selected Meals` == 'Half-fermented trout' & str_detect(Ingredients, 'potatoes') ~ '4 stk',
      #TRUE ~ Amounts
    #)
  #) %>%
  #Add a new row for the leeks (previously shared row with onion)
  #add_row(`Nr. Of portion` = 2, `Selected Meals` = 'Half-fermented trout', Ingredients = 'Leek', Amounts = '0.5 stk') %>%

  #Remove some ingredients not in recipe when checked (bacon in Turkey fillet on party bed/KALKUNFILET PÅ PARTYSENG, and 'Refractory' ingredients from Salmon Shape With Pepper Root and 'Pepper Beef With Chopped Tomato And Chevre Salad')
  filter(!(`Selected Meals` == 'Turkey fillet on party bed' & Ingredients == 'bacon') & !str_detect(Ingredients, 'Refractory')) %>%
  filter(!(str_detect(Ingredients, 'mashed potatoes') & str_detect(Amounts, 'neve|pinch'))) %>% #Can't find these in the recipes, and the volume unit makes no sense for mashed potatoes
  filter(!(str_detect(Ingredients, 'salmon') & Amounts == 'cm')) %>% #Duplicate

  #Fix the 'hp' amounts from Kolonialen and Klikk, using the weights from their webstore. Hp means either 'pack' or 'portion'
  mutate(Amounts = case_when(
    Ingredients == 'anchovies, can be looped' ~ '55 g', #One pack of anchovies is 50g
    Ingredients == 'asparagus' & str_detect(Amounts, 'hp') ~ '500 g', #Two packs of asparagus is 250*2
    str_detect(Ingredients, 'bearnaise') ~ '29 g', #Amount in one pack of Toro bearnaise
    Ingredients == 'broccoli, frozen' ~ '450 g', #In Norway, frozen broccoli is typically sold in packs of either 400, 450 or 500g depending on supermarket chain.
    Ingredients == 'celery stalk' & Amounts == '0.25 hp' ~ '95 g', #Net weight of one celery is 380g
    Ingredients == 'celery stalk' & Amounts %in% c('0.20 hp', '0.2 hp') ~ '76 g',
    Ingredients == 'celery stalk' & Amounts == '1 stk' ~ '380 g',
    Ingredients == 'cherry tomatoes, canned' ~ '400 g',
    Ingredients == 'cherry tomatoes, red' & Amounts == '1 hp' ~ '200 g', #It's their 200g product they show in the recipes
    Ingredients == 'cherry tomatoes, red' & Amounts == '0.5 hp' ~ '100 g',
    Ingredients == 'chick peas' & Amounts == '4 hp' ~ '1640 g', #Four cans, one can is 410 g in the recipe ingredient shopping cart they supply
    str_detect(Ingredients, 'crisp bread, brown') & Amounts == '1 hp' ~ '520 g', #This is a lot of crisp bread for four persons?
    str_detect(Ingredients, 'noodles') & Amounts == '1 hp' ~ '250 g',
    Ingredients == 'frozen frozen peas' & Amounts == '1 hp' ~ '350 g', #Rema 1000 frozen peas
    Ingredients == 'guacamole spice mix' ~ '20 g', #Olde el paso
    Ingredients == 'halloumi' ~ '200 g',
    Ingredients == 'heart salad' & Amounts == '1 hp' ~ '2 stk',
    str_detect(Ingredients, 'jasmine rice') & Amounts == '1 hp' ~ '120 g', #One boil-in-bag is 120 g
    str_detect(Ingredients, 'jasmine rice') & Amounts == '2 hp' ~ '240 g',
    Ingredients %in% c('ketchup', 'tomato ketchup') & Amounts == '1 hp' ~ '450 g', #Heinz in Kolonialem recipes
    str_detect(Ingredients, 'kruspersille') & Amounts == '0.5 hp' ~ '10 g',
    str_detect(Ingredients, 'kruspersille') & Amounts == '1 hp' ~ '20 g',
    Ingredients == 'mango chutney' & Amounts == '1 hp' ~ '50 g', #Recipe says 50 g
    Ingredients == 'mango chutney' & Amounts == '0.5 hp' ~ '175 g',
    Ingredients == 'mustard for sausages' ~ '490 g',
    Ingredients == 'naan bread' & Amounts == '1 hp' ~ '120 g', #From kolonial
    Ingredients == 'naan bread' & Amounts == '2 hp' ~ '240 g',
    Ingredients == 'pasta' & Amounts == '2 hp' ~ '400 g', #Default large portion of pasta for four people, as other recipes at the same site. Couldn't find the specific recipe
    Ingredients == 'pearl barley' & Amounts == '4 hp' ~ '300 g', #One portion of pearl barley is default 75g
    Ingredients == 'peas, frozen' & Amounts == '0.75 hp' ~ '263 g',
    Ingredients == 'peas, frozen' & Amounts == '1 hp' ~ '350 g',
    Ingredients == 'peas, frozen' & Amounts == '2 hp' ~ '700 g',
    Ingredients == 'pizza base, mix' ~ '370 g', #Mostly white wheat flour, a bit of oil and yeast
    Ingredients == 'pizza filling' ~ '55 g', #Tomatoes, onions, corn starch, sugar, oil and spices
    Ingredients == 'pork chops' & Amounts == '2 hp' ~ '1000 g', #One portion of pork chops is 250g, four portions in the recipe
    Ingredients == 'mashed potatoes' ~ '800 g', #Default portion size of mashed potatoes is 200 g
    Ingredients == 'radish' & Amounts == '0.5 hp' ~ '65 g',
    Ingredients == 'radish' & Amounts == '1 hp' ~ '130 g',
    Ingredients == 'radish' & Amounts == '2 hp' ~ '260 g',
    Ingredients == 'rod celery' & Amounts == '0.2 hp' ~ '0.2 stk', #350 g is the weight at Meny.no,
    Ingredients == 'rod celery' & Amounts == '0.25 hp' ~ '0.25 stk',
    Ingredients == 'salad mix' & Amounts == '1 hp' ~ '175 g', #Babyleaf salad mix is their default salad mix in the shoppin cart for the recipes
    str_detect(Ingredients, 'shimeji mushrooms') ~ '200 g', #Pack of rare mushrooms
    Ingredients == 'sour cream, light' & Amounts == '1 hp' ~ '300 g', #Tine default
    Ingredients == 'spinach' & Amounts == '1 hp' ~ '200 g', #BAMA
    str_detect(Ingredients, 'sugar snap peas') & Amounts == '1 hp' ~ '200 g',
    Ingredients == 'taco seasoning' & Amounts == '1 hp' ~ '28 g',
    Ingredients == 'tikka masala sauce' ~ '360 g',
    Ingredients == 'tomatoes, chopped' & Amounts == '2 hp' ~ '2 stk',
    Ingredients == 'toro greek moussaka' ~ '136 g', #Mostly potatoes, some tomatoes, wheat flour, cornstach and oil
    Ingredients == 'vossa sausage' ~ '800 g', #The one sold at kolonial is 400g each
    Ingredients == 'yoghurt, Greek' & Amounts == '1 hp' ~ '200 g', #That's what the recipe says
    Ingredients == 'blueberry basket' ~ '125 g',
    Ingredients == 'rema frozen vegetables' ~ '450 g',
    TRUE ~ Amounts
  )) %>%

  #Clear up some amounts that has not been picked up earlier
  mutate(
    Amounts = case_when(
      Ingredients == 'chicken  fillets about 100 g per person' ~ '400 g',
      Ingredients == 'chicken about 1 kg' ~ '1 kg',
      Ingredients == 'salmon fillets about 140 g' ~ '560 g',
      Ingredients == 'steaks of 200 g of tenderloin beef' ~ '800 g',
      Ingredients == 'thai ginger kha about 2 cm or 1 tbsp laos powder' ~ '2 cm',
      Ingredients == 'turkey breast, about 1.5 kg with leather' ~ '1.5 kg',
      Ingredients == 'turkey breasts' ~ '800 g', #Four portions of turkey breast meat
      Ingredients == 'lasagna plates' & Amounts == '2 stk' ~ '12 stk', #Rechecked recipe as 2 plates seemed very little, real number is 12.https://oppskrift.klikk.no/lasagne/1667/
      Ingredients == 'of beef fillet, 200 grams' ~ '800 g', #4 stk a 200 g
      Ingredients %in% c('cans of canned tomatoes about 400','can tomatoes, roughly chopped') ~ '800 g', #Two cans
      Ingredients == 'of canned tomatoes in pieces, without liquid' ~ '1600 g',
      str_detect(Ingredients, 'pinch cayenne pepper') ~ '1 pinch',
      Ingredients == 'dill bundle' ~ str_replace(Amounts, 'stk', 'bunch'),
      Ingredients %in% c('can of canned tomatoes, chopped','boks hermetiske tomater', '400 g canned tomatoes') ~ '400 g',
      str_detect(Ingredients, 'hvittmuggost') ~ str_replace(Amounts, 'stk', 'slice'),
      Ingredients == 'flatbread' ~ '520 g', #One pack Mors hjemmebakte
      Ingredients == 'taco seasoning' & str_detect(Amounts, 'pack') ~ '28 g', #Santa Maria
      Ingredients == 'egg noodles' & str_detect(Amounts, 'stk') ~ '100 g', #0.5 pack Santa Maria, from recipe site
      Ingredients == 'ham, in cubes' ~ '325 g', #Picnic skinke, from recipe site
      `Selected Meals` == 'Christmas ham' & Ingredients == 'boiled ham' ~ str_replace(Amounts, '1', '2 kg'), #From recipe site
      `Selected Meals` == 'Sausage Lunch' & Ingredients == 'sausage' ~ '2 kg', #From recipe site
      Ingredients == 'of risotto rice with mushrooms' ~ '500 g', #From recipe site
      Ingredients == 'tomato salsa' & str_detect(Amounts, 'glass') ~ '690 g', #Santa Maria, 1 glass 230g
      Ingredients %in% c('kidney beans', 'beans, white') & Amounts == '1 stk' ~ '290 g', #Øko, Kolonial
      Ingredients == 'of broccolini' ~ '100 g', #0.5 pack a 200g (Kolonial)
      Ingredients == 'lollo rosso, leaf' ~ '2 leaf',
      Ingredients == 'spaghetti' & Amounts == '0.5 pack' ~ '500 g', #0.5 pack Barilla a 1kg
      Ingredients == 'neve  thyme' ~ '1 neve',
      Ingredients == 'pasta pipe' ~ '500 g', #One Sopps pasta
      Ingredients == 'g tomat, stilkfestet fjernet' ~ '200 g',
      Ingredients == 'chick peas' & str_detect(Amounts, 'box') ~ '410 g',
      Ingredients == 'kidney beans' & str_detect(Amounts, 'box') ~ '380 g', #3/4 kidney bean boxes/cans at Meny are 380g
      Ingredients == 'strawberry basket' ~ '400 g', #From the recipe at kolonial
      Ingredients == 'of bacon' ~ str_replace(Amounts, 'stk', 'slice'),
      Ingredients == 'neve coriander' ~ str_replace(Amounts, 'stk', 'neve'),
      str_detect(Ingredients, 'vitmuggost') ~ str_replace(Amounts, 'stk', 'slice'),
      Ingredients == 'tortilla fin' & is.na(Amounts) ~ paste0(`Nr. Of portion`*2, ' stk'), #Two tortillas each
      Ingredients == 'The shell of 1 lemon' ~ '1 stk',
      Ingredients == 'pasta for 4 people' ~ '4 portion',
      `Selected Meals` %in% c('Meatballs with chili and pasta', 'Pasta with Mexican shrimp sauce', 'fried chicken with honey and soy',
                              'Creamy pasta with mushrooms and dill') & Ingredients == 'pasta' ~ paste0(`Nr. Of portion`, ' stk'),
      Ingredients == 'black olives for portion' ~ paste0(`Nr. Of portion`, ' portion'),
      Ingredients == 'cooked potatoes for portion	' ~ paste0(`Nr. Of portion`, ' portion'),
      Ingredients == 'salad for portion' ~ paste0(`Nr. Of portion`, ' portion'),
      str_detect(Ingredients, 'celery') & str_detect(Ingredients, 'stalk') ~ str_replace(Amounts, 'stk', 'stalk'), #To not mistake between the whole bunch of celery stalks and using individual stalks

      #Amounts taken from thespicetrain
      str_detect(Ingredients, 'of rosemary|of  rosemary|stems  rosemary') ~ '2 tsp',
      Ingredients == 'rosemary squid' ~ '1 tsp',
      Ingredients == 'rosemary,' & Amounts == '0.5 stk' ~ '0.5 tsp',
      str_detect(Ingredients, 'parsley stalk\\b') ~ '1 tbsp',
      str_detect(Ingredients, 'parsley stalks|kruspersille|Parsley') & !str_detect(Ingredients, 'root') ~ str_replace(Amounts, 'stk', 'tbsp'),
      str_detect(Ingredients, 'sprig') & str_detect(Amounts, '2') | Ingredients == 'the leaf of 2 sprig  thyme' ~ '0.5 tsp',
      str_detect(Ingredients, 'sprig') & str_detect(Amounts, '1') ~ '0.25 tsp',
      Ingredients == 'thyme' & Amounts == '2' ~ '0.5 tsp',
      Ingredients == 'of parsley, torn into  pieces' ~ '10 tbsp',
      str_detect(Ingredients, 'mackerel') & str_detect(Ingredients, 'tomato') ~ '160 g', #One portion of tomato mackerel is 40g, four portions
      Ingredients == 'cloves' ~ paste0(Amounts, ' stk'),
      TRUE ~ Amounts
    )
  ) %>%

  #Turn volume units to dl----
#(use 2.45dl for one cup, as 1 cup is 2.5dl in Norway and 2.4 in US), and turn into grams for water and other liquids with 100g/dl
#Split Amounts into amounts and units
separate(., Amounts, c('Amounts', 'unit_enhet'), sep = ' ') %>%
  #Turn amounts into numeric
  mutate_at('Amounts', ~as.numeric(.)) %>%

  #Turn volume units to dl
  mutate(Amounts = case_when(
    unit_enhet == 'cup' ~ Amounts * 2.45,
    unit_enhet == 'l' ~ Amounts * 10,
    unit_enhet == 'ml' ~ Amounts / 10,
    unit_enhet == 'tbsp' ~ Amounts / 6.67,
    unit_enhet == 'tsp' ~ Amounts / 20,
    unit_enhet == 'krm' ~ Amounts / 100,
    unit_enhet == 'drop' ~ Amounts / 2000, #One drop is 0.05ml
    unit_enhet == 'pinch' ~ Amounts / (20*16), #A pinch is usually defined as 1/16 of a tsp
    TRUE ~ Amounts
  )) %>%
  mutate(unit_enhet = case_when(
    unit_enhet %in% c('cup', 'l', 'ml', 'tsp', 'tbsp', 'krm', 'drop', 'pinch') ~ 'dl',
    TRUE ~ unit_enhet
  )) %>%

  #Turn juice, water, vinegar and other liquids with similar density to water from dl/l to grams as they are all about 100g/dl
  mutate(
    Amounts = case_when(
      (str_detect(Ingredients, 'water|Beer|cognac|cider|juice|Juice|broth|kraft|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
         unit_enhet == 'dl') &
        !str_detect(Ingredients, 'sugar|cheese|sour|flour') ~ Amounts * 100,
      TRUE ~ Amounts),
    unit_enhet = case_when(
      (str_detect(Ingredients, 'water|Beer|cognac|cider|juice|Juice|broth|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
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

    #Some ingredients needing cleaning up
    Ingredients = Ingredients %>%
      str_replace('kyllinger|chicken, grilled', 'whole chicken') %>%
      str_replace('calf leg', 'calf tail') %>% #As per recipe
      str_replace('tomato pasta', 'tomato paste') %>%
      str_replace('eggs, whipped together', 'egg') %>%
      str_replace('head cabbage', 'cabbage') %>%
      str_replace('honey mustard', 'mustard') %>%
      str_replace('kvernet hvit pepper', 'pepper, ground') %>%
      str_replace('white asparagus canned or', 'white asparagus, in can') %>%
      str_replace('finely chopped  basil or 1 tsp dried', 'basil') %>%
      str_replace('Greek yogurt or cheese', 'greek yogurt') %>%
      str_replace('onion corn, germ meal or dried bread crumbs', 'bread crumb') %>%
      str_replace('lettuce leaf', 'salad mix') %>%
      str_replace('chili spice', 'chili powder') %>%
      str_replace('spring chickens', 'whole chicken')
  ) %>% mutate(Ingredients = case_when(
    Ingredients == 'beef' & `Selected Meals` == 'Baked turkey with puff pastry' ~ 'beef stock',
    str_detect(Ingredients, 'sjalottløk') ~ 'shallot',
    TRUE ~ Ingredients
  ))


#Turn volume units into weight----
#Find the ingredients in the recipes that also can be found in the food weight and portion size data----
#Reference volume to weight comparisons for different foods
ref <- readRDS('./porsjoner_vekt_næringsinnhold/food_weight_ref.Rds')

#Look at the data
various$no_amounts <- clean %>%
  filter(is.na(Amounts))
#Get the recipe names for the 100 recipes to use
various$names_norway <- read_xlsx('./oppskrifter/Data_NO_100.xlsx') %>%
  select(`Selected Meals`, Source)

#Ingredients to run query ref, all code behind # unnecessary when using the Data_NO_100 raw file
toRef <- clean #%>% inner_join(various$names_norway)

#recipes with different names in the different datasets
#Get the recipe names for the 100 recipes to use
#various$names_norway <- read_xlsx('./oppskrifter/Data_NO_100.xlsx') %>%
#  select(`Selected Meals`, Source)

#Some are missing, add them
#various$missing <- anti_join(various$names_norway, raw_list)
#toRef <- full_join(toRef, clean %>%
#                     filter(str_detect(`Selected Meals`, 'Pinnekjøtt|Biff with Bearné saus|sautéed reindeer|Muslim steak curry')) )


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
various$with_reference <- checkRefList(toRef)

saveRDS(various$with_reference, 'with_ref_NOR.Rds')

#various$with_reference <- readRDS('with_ref_NOR.Rds')

references <- ref %>% mutate_at(c('first_word', 'second_word'), ~tolower(.))

#Look through and fix errors, several corn cobs have not been properly translated
temp <- bind_rows(various$with_reference) %>%
  full_join(toRef) %>% unique() %>%
  filter(!is.na(Amounts)) %>%
  mutate(
    ref = case_when(
      str_detect(Ingredients, 'eggplant') ~ 'Eggplant',
      str_detect(Ingredients, 'chick pea flour') ~ 'chick pea, flour',
      Ingredients == 'korianderfrø' ~ 'korianderfrø',
      Ingredients == 'tortilla fullkorn wheat' ~ 'Tortilla, fullkorn',
      Ingredients == 'canned chick peas' ~ 'Chick pea, canned',
      Ingredients %in% c('malt cardamom', 'cardamom') ~ 'cardamom',
      Ingredients == 'sjalottløk eller 1 løk' ~ 'Sjalottløk',
      Ingredients %in% c('lemongrass root, chopped', 'sitrongressrot, hakket', '–3 lemongrass, thin slice',
                         'finely cut lemongrass can be looped', 'lemongrass', 'of lemongrass,', 'lemongrass, finely chop the white part') ~ 'lemongrass',
      Ingredients == 'eplemos'  ~ 'eplemos',
      Ingredients == 'chili powder, red' ~ 'chili, powder',
      Ingredients == 'spinach' ~ 'Spinach',
      Ingredients == 'salad' ~ 'salad',
      Ingredients == 'heart salad or other salad' ~ 'salad',
      Ingredients == 'peanuts, salty, coarsely chopped' ~ 'Peanuts',
      Ingredients == 'grilled sweet pepper, canned' ~ 'sweet pepper, grilled',
      Ingredients == 'breadcrumbs or panko' ~ 'bread, crumb',
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
      Ingredients == 'fingersalt' ~ 'salt',
      Ingredients == 'filet av Ishavsrøye, uten ben og skinn' ~ 'fatty fish',
      Ingredients == 'cloves' ~ 'cloves',
      Ingredients == 'butter dough' ~ 'puff, pastry',
      Ingredients == 'onion - pickled' ~ '',
      str_detect(Ingredients, 'stock|broth') & !str_detect(Ingredients, 'cube|dice') ~ 'stock/broth',
      str_detect(Ingredients, 'wine') & is.na(ref) ~ 'wine',
      Ingredients == 'tenderloin, cattle, slice' ~ 'Beef',
      Ingredients == 'Tine Genuine Grated Cheese Original' ~ 'hard to semi-hard cheese',
      Ingredients == 'skrelt mandelpotet' ~ 'Potato',
      Ingredients == 'chickens, about 500 g' ~ 'Chicken, whole',
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'potato') ~ 'potato',
      str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'cheese') ~ 'goat cheese',
      Ingredients == 'rice noodles' ~ 'rice noodles',
      Ingredients == 'shrimp salad' ~ 'shrimp salad',
      Ingredients == 'peas' ~ 'pea',
      str_detect(Ingredients, 'maisstivelse') ~ 'maisstivelse',
      TRUE ~ ref),
    ID = case_when(
      str_detect(Ingredients, 'eggplant') ~ as.numeric(references[str_which(tolower(references$first_word), 'eggplant'),1]),
      str_detect(Ingredients, 'chick pea flour') ~ references %>% filter(first_word == 'chick pea' & second_word == 'flour') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'korianderfrø' ~ as.numeric(references[str_which(tolower(references$first_word), 'korianderfrø'),1]),
      Ingredients == 'tortilla fullkorn wheat' ~ references %>% filter(first_word == 'tortilla' & second_word == 'fullkorn') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'canned chick peas' ~ references %>% filter(first_word == 'chick pea' & second_word == 'canned') %>% select(ID) %>% as.numeric(.),
      Ingredients %in% c('malt cardamom', 'cardamom') ~ references %>% filter(first_word == 'cardamom' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'sjalottløk eller 1 løk' ~ as.numeric(references[str_which(tolower(references$first_word), 'sjalottløk'),1]),
      Ingredients %in% c('lemongrass root, chopped', 'sitrongressrot, hakket', '–3 lemongrass, thin slice',
                         'finely cut lemongrass can be looped', 'lemongrass', 'of lemongrass,', 'lemongrass, finely chop the white part') ~ as.numeric(references[str_which(tolower(references$first_word), 'lemongrass'),1]),
      Ingredients == 'eplemos'  ~ as.numeric(references[str_which(tolower(references$first_word), 'eplemos'),1]),
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
      Ingredients == 'fingersalt' ~ references %>% filter(first_word == 'salt' & second_word == 'grovt') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'filet av Ishavsrøye, uten ben og skinn' ~ references %>% filter(first_word == 'fatty' & second_word == 'fish') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'cloves' ~ references %>% filter(first_word == 'cloves' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'ingefær') ~ references %>% filter(first_word == 'ingefærrot' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'butter dough' ~ references %>% filter(first_word == 'puff' & second_word == 'pastry') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'onion - pickled' ~ 0,
      str_detect(Ingredients, 'stock|broth') & !str_detect(Ingredients, 'cube|dice') ~ 0,
      str_detect(Ingredients, 'wine') & is.na(ref) ~ 0,
      Ingredients == 'tenderloin, cattle, slice' ~ references %>% filter(first_word == 'beef' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'Tine Genuine Grated Cheese Original' ~ references %>% filter(first_word == 'hard to semi-hard cheese' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'skrelt mandelpotet' ~ references %>% filter(first_word == 'potato' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'chickens, about 500 g' ~ references %>% filter(first_word == 'chicken' & second_word == 'whole') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'thyme') & !str_detect(Ingredients, 'dried') ~ references %>% filter(first_word == 'thyme' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'mint') & !str_detect(Ingredients, 'chutney') ~ references %>% filter(first_word == 'mint' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'potato') ~ references %>% filter(first_word == 'potato' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'cheese') ~ references %>% filter(first_word == 'brown' & second_word == 'cheese') %>% select(ID) %>% as.numeric(.), #The cheese that is sliced is a brown goat cheese
      Ingredients == 'rice noodles' ~ 0,
      Ingredients == 'shrimp salad' ~ 0,
      Ingredients == 'peas' ~ references %>% filter(first_word == 'pea' & second_word == 'dry') %>% select(ID) %>% as.numeric(.),
      Ingredients == 'tomato paste' ~ references %>% filter(first_word == 'tomato' & second_word == 'paste') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'bay leaf') ~ references %>% filter(first_word == 'bay' & second_word == 'leaf') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'naan bread') ~ references %>% filter(first_word == 'naan' & second_word == 'bread') %>% select(ID) %>% as.numeric(.),
      Ingredients %in% c('ginger', 'finely chopped,  ginger', 'ginger,', 'grated  ginger') ~ references %>% filter(first_word == 'ginger' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      str_detect(Ingredients, 'maisstivelse') ~ references %>% filter(first_word == 'maisstivelse' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
      TRUE ~ ID)) %>%

  mutate(unit_enhet = case_when(
           Ingredients == 'cloves' ~ 'stk',
           TRUE ~ unit_enhet
         ))

various$not_found_in_ref <- anti_join(toRef, temp %>% select(`Selected Meals`, Ingredients, Amounts, unit_enhet))

#Calculate the weights
various$weights <- readRDS('./porsjoner_vekt_næringsinnhold/all_weights.Rds')
#Ingredients to turn from tbsp to dl
various$to_dl <- c('pepper', 'peanøttsmør', 'olje', 'oil', 'smør', 'butter', 'margarin',
           'ghee', 'garlic', 'tomato paste')

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
    filter(unit_enhet %in% clean$unit_enhet)

final <- right_join(weights, temp, by = c('ID', 'unit_enhet')) %>% #Doing this gives final three more rows than temp...?

  #Turn weights into kilo
  mutate(Amounts_kg = case_when(
    unit_enhet == 'kg' ~ Amounts,
    !unit_enhet == 'kg' ~ Amounts*g/1000
  )) %>%

  #Cleanup
  select(No, `Selected Meals`, Ingredients.y, Amounts_kg, ref, Amounts, unit_enhet, ID) %>% #Keep ID to translate norwegian to english later
  unique() %>% #Got some values twice as inner_join got both norwegian and english translated names
  rename(Ingredients = Ingredients.y,
         weight_ID = ID) %>% drop_na(Ingredients)

#Save
saveRDS(final, './oppskrifter/NOR_clean.Rds')

#Save some of the dataframes in various
various$units <- NULL
various$stk <- NULL
various$bouquet_garni <- NULL
various$with_reference <- NULL
various$weights <- NULL
various$to_dl <- NULL
saveRDS(various, 'various_NOR.Rds')

