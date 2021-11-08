#Setup----
devtools::load_all(path = '.')

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

#Some various datafraems to use in the cleanup
various <- list()

#Read in the recipes
raw <- list(
  
  'norway' = list.files(path = './Data/recipes/', pattern = "*cleaned.csv", full.names = TRUE) %>% 
    map_df(~read_csv(.)) %>%
    #Columns to work with
    select(`Selected Meals`, Ingredients, Source, Country),
  
  'uk' = read_xlsx('./Data/recipes/Data_UK_100.xlsx') %>%
    select(`Selected Meals`, Ingredients, Source, Country) %>% drop_na(Ingredients),
  
  'us' = readRDS('./Data/recipes/US_clean.Rds') %>%
    #Format like the others
    #Remove columns
    select(-c(ingredient_id, ingredient_amount)) %>%
    #Add columns
    mutate(Country = 'US',
           Source = 'allrecipes',
           unit = 'kg') %>%
    #Reformat Ingredient rows, one row for each ingredient with amount before ingredient name
    rename(temp = Ingredients) %>%
    unite(., col = 'Ingredients', c(Amounts_kg, unit, temp), sep = ' ')
) %>%

#All together
bind_rows(.) %>%
  #Rename `Selected Meals` to sample_id
  rename(sample_id = `Selected Meals`) %>%
  #create unique names for the recipes with identical names
  mutate(sample_id = case_when(
  sample_id == 'Christmas Ribbe' & Source == 'Tine' ~ 'Christmas Ribbe 2',
  sample_id == 'Spagetti Bolognese' & Source == 'Klikk' ~ 'Spagetti Bolognese 2',
  TRUE ~ sample_id
))

#Metadata for recipes
recipe_metadata <- raw %>% select(sample_id, Country, Source) %>% unique() %>% rename(group = Country)
saveRDS(recipe_metadata, './Data/output/recipe_metadata.Rds')

#Clean up the recipe ingredients----
recipes <- raw %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Separate ingredients into separate rows, trim whitespace and remove empty rows
  separate_rows(., Ingredients, sep = '\n') %>%
  mutate(Ingredients = str_trim(Ingredients, side = 'both'),
         Ingredients = str_squish(Ingredients)) %>%
  filter(Ingredients != '' | is.na(Ingredients)) %>%

  #Change a few composite ingredient into individual ingredients
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
         str_replace('4 dl vegetables in cubes. eg. tomato, cucumber and corn', '1.33 dl tomato\n1.33 dl cucumber\n1.33 dl corn kernels') %>%
         str_replace('1 cup fresh herbs eg basil, rosemary and leaf parsley', '0.33 cup basil\n0.33 cup rosemary\n0.33 cup parsley') %>%
         str_replace('60 g of fresh herbs e.g . parsley, basil, chives and coriander', '15 g fresh parsley\n15 g fresh basil\n15 g fresh chives\n15 g fresh coriander') %>%
         str_replace('0.406 kg diced tomatoes with green chile peppers, drained', '0.406 g canned tomatoes') %>%
           
        #Lomper (Potato flatbread)
        #From https://www.matprat.no/oppskrifter/tradisjon/potetlomper/ uses a mix of rye and wheat flour originally
        str_replace('30 pcs lomper', '5 dl wheat flour\n2000 g potato\n2 tsp salt') %>%
           
        #Storebought pie dough
        #From https://oda.com/no/recipes/1075-spoon-quiche-med-spinat-sopp-og-feta/
        str_replace('1 piece pie dough, finished', '125 g butter\n3 dl wheat flour\n3 tbsp water')) %>%
  
  #Separate rows again
  separate_rows(Ingredients, sep = '\n') %>%
  
  #Remove some unnecessary characters in the ingredients column, and make sure there is a space between numbers and words ('2 dl', not '2dl)
  #Reformat all units to the same and fix some ingredient names for later
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
         str_replace('finely chopped herbs', 'fresh parsley') %>% #Most common herb used
         str_replace('mug, fresh', 'parsley, fresh') %>%
         str_replace('broccoli peas, frozen', 'broccoli, frozen') %>% #riginal recipe says it's broccoli
         str_replace('salvie', 'sage') %>%
         str_replace('duck or goose fat for confit', '500 g goose fat')) %>% #Goose fat is in nutrition database, duck fat is not. 500 g is a conservative estimate for fat used in confit.
  
  mutate(Ingredients = Ingredients %>%
    #HP in recipes from Norway means either pack, portion, can, glass depending on recipe and ingredient.
    #Real amounts found by looking in Kolonial's webshop
    str_replace('1 hp egg noodles', '250 g egg noodles') %>%
      str_replace('1 hp shimeji mushrooms in chunks', '200 g shimeji mushroom') %>%
      str_replace('1 hp frozen frozen peas|1 hp peas, frozen', '350 g frozen peas') %>%
      str_replace('1 hp sugar peas|1 hp sugar peas, in strips', '150 g sugar peas') %>%
      str_replace('1 hp spinach', '200 g fresh spinach') %>%
      str_replace('hp parsley', 'bunch parsley') %>%
      str_replace('1 hp anchovies, can be looped', '55 g anchovies canned') %>%
      str_replace('1 hp taco seasoning', '1 pack taco seasoning') %>%
      str_replace('1 hp guacamole spice mix', '20 g guacamole spice mix') %>%
      str_replace('0.75 hp peas, frozen', '263 g peas frozen') %>%
      str_replace('1 hp toro greek moussaka', '136 g toro greek moussaka') %>%
      str_replace('1 hp bearnaise sauce, mix|1 hp bearnaise base', '29 g bearnaise sauce base mix') %>%
      str_replace('1 hp halloumi', '200 g halloumi') %>%
      str_replace('0.5 hp mango chutney|1 hp mango chutney', '50 g mango chutney') %>% #Recipe says 50 g
      str_replace('1 hp of naan bread', '120 g naan bread') %>%
      str_replace('4 hp chickpeas', '1640 g canned chickpeas') %>%
      str_replace('1 hp potato steps', '450 g potatoes') %>%
      str_replace('2 hp peas, frozen', '700 g frozen peas') %>%
      str_replace('1 hp crispy bread, coarse', '520 g cripsy bread coarse') %>%
      str_replace('1 hp salad mix', '175 g salad mix') %>%
      str_replace('2 hp pork chops', '1000 g pork chops') %>%
      str_replace('2 hp asparagus', '500 g asparagus') %>%
      str_replace('1 hp cherry tomatoes, red', '200 g cherry tomatoes') %>%
      str_replace('1 hp jasmine rice boil-in-bag', '120 g parboiled rice jasmine') %>%
      str_replace('1 hp heart salad', '2 stk heart salad') %>%
      str_replace('0.5 hp radish', '65 g radish') %>%
      str_replace('2 hp radish', '260 g radish') %>%
      str_replace('4 hp pearl barley', '300 g pearl barley') %>% #One portion of pearl barley is default 75g
      str_replace('0.2 hp stalk celery', '80 g celery') %>%
      str_replace('2 hp tomatoes, chopped', '2 stk tomatoes chopped') %>%
      str_replace('0.5 hp cherry tomatoes, red', '100 g cherry tomatoes') %>%
      str_replace('1 hp tikka masala sauce', '360 g tikka masala sauce') %>%
      str_replace('1 hp mango chutney', '50 g mango chutney') %>%
      str_replace('1 hp yogurt, greek', '2 dl yoghurt greek') %>%
      str_replace('2 hp nan bread', '240 g naan bread') %>%
      str_replace('1 hp sour cream, light', '300 g sour cream light') %>%
      str_replace('1 hp cherry tomatoes, canned', '400 g cherry tomatoes canned') %>%
      str_replace('2 hp vossa sausage', '800 g vossa sausage') %>%
      str_replace('1 hp pizza filling', '55 g pizza filling') %>%
      str_replace('1 hp ketchup', '450 g ketchup') %>%
      str_replace('1 hp mustard for sausages', '490 g mustard') %>%
      str_replace('1 hp radish', '130 g radish') %>%
      str_replace('0.25 hp stalk celery', '100 g celery') %>%
      str_replace('2 hp tagliatelle', '400 g tagliatelle') %>%
      str_replace('2 hp jasmine rice boil-in-bag', '240 g parboiled rice jasmine') %>%
      str_replace('1 hp broccoli, frozen', '450 g broccoli frozen') %>%
      str_replace('1 stk of ham, in cubes', '80 g ham') %>%
      str_replace('2 stk of risotto rice with mushrooms', '500 g risotto rice') %>%
      str_replace('1 pack of flatbread', '275 g flatbread') %>%
      str_replace('1 stk strawberry basket', '400 g strawberries') %>%
      str_replace('1 stk raspberry basket', '125 g raspberries') %>%
      str_replace('1 stk blueberry basket', '125 g blueberries'))
  
  #Conditionals
  recipes <- recipes %>%
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'rosemary|basil|tarragon|coriander') ~ str_replace(Ingredients, 'pot|bunt', 'bunch'),
    !str_detect(Ingredients, '^[:digit:]') & (str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'lemon|lime')) ~ str_replace(Ingredients, '(?<=\\d)\\s', ' stk '),
    sample_id %in% c('Duck Breast With PasTine Pure', 'Cod With Bacon, Spinach And Rotmoss') & Ingredients == 'parsley' ~ 'parsnip root', #Mistake in translation
    sample_id == 'Pollock with herb onion and baked potatoes' & str_detect(Ingredients, 'fish') ~ str_replace(Ingredients, 'fish fillet', 'pollock'), #It says in the name it's pollock
    
    TRUE ~ Ingredients))

  #Standardise recipe ingredients and units
  recipes <- recipes %>% standardiseRecipes()
    
#Save the orginal and cleaned up ingredient names for comparisons-----
various$org_ingredients <- recipes %>% select(sample_id, Country, Ingredients, org_ingredients)
various$org_amounts <- recipes %>% select(sample_id, Country, Ingredients, Amounts, unit) %>%
  unite(., 'original_amounts', c(Amounts, unit), sep = ' ') %>%
  mutate(original_amounts = str_replace(original_amounts, 'NA NA', '-')) #Should get the original volume amounts from US recipes as well

t <- various$org_ingredients %>% select(Ingredients, org_ingredients) %>% unique()

#Map to databases, find the amount (Weight) of each ingredient by weight,the nutritional values and environmental impact----
  #Sum the amounts for all the ingredients with the same name for each recipe
  recipes <- recipes %>%
    select(-org_ingredients) %>%
    group_by(sample_id, Ingredients, Country, Source, unit) %>%
    summarise(Amounts = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
    #Using na.rm gives ingredients with no amounts a '0' value, change back to NA
    mutate(Amounts = case_when(
      !is.na(unit) ~ Amounts
    ))
  
# Weight/volume database----  
  #Ingredients with amounts that are not in weight
  various$get_amounts_kg <- recipes %>%
    filter(unit != 'kg')
  #See which ingredients have no unit or amounts whatsoever
  various$missing <- recipes %>% filter(is.na(Amounts))
  #Map to volume/weight database
  temp <- checkRef(various$get_amounts_kg, reference = references$volume_weight)

  #Calculate weight in kilo
  #Ingredients that need to have weight per dl in database
  various$to_dl <- c('pepper', 'peanøttsmør', 'olje', 'oil', 'smør', 'butter', 'margarin',
                     'ghee', 'garlic', 'tomato paste', 'baking powder', 'taco spice mix',
                     'tahini')
  
  various$weights <- databases$volume_weight %>%
    #Set brutto as default value pr stk, as it used in both Matvaretabellen and SHARP
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
    filter(unit_enhet %in% recipes$unit) %>% select(-reference) %>% unique() %>% filter(language != 'norwegian') %>%
    rename(unit = unit_enhet) %>% select(ID, unit, g)
  
  #Join them together and calculate the weight in kilos
  various$with_weights <- inner_join(temp, various$weights, by = c('ID', 'unit')) %>%
    
    mutate(Amounts_kg = (Amounts * g) / 1000,
           unit = 'kg') %>%
    #Remove and rename columns
    select(-c(g, ID, ref, Amounts)) %>%
    rename(Amounts = Amounts_kg)
  
  #What has not been picked up? Add to various$missing
  various$missing <- various$missing %>%
    bind_rows(., anti_join(temp %>% select(sample_id, Ingredients),
                                        various$with_weights %>% select(sample_id, Ingredients)))
    
  #Add the ingredients with weights found back to recipes df
  recipes <- recipes %>%
    filter(unit == 'kg') %>%
    bind_rows(various$with_weights) %>%
    group_by(sample_id, Ingredients, Country, Source) %>%
    summarise(Amounts = sum(Amounts, na.rm = TRUE)) %>%
    ungroup()
  
  #Remove some unnecessary df's
  various$get_amounts_kg <- NULL
  various$to_dl <- NULL
  various$weights <- NULL 
  various$with_weights <- NULL 
  
  #Calculate the total weight of each recipe
  various$recipe_weight <- recipes %>%
    group_by(sample_id) %>%
    summarise(Weight = sum(Amounts)) %>%
    mutate(unit = 'kg')
  
  #Impute the mean value of the same ingredient for the missing ingredients
  #Calculate the mean value of each type of ingredient normalized by recipe weight
  various$mean_values <- recipes %>%
    #Add total weight of recipe in kg
    inner_join(various$recipe_weight) %>%
    #Amount of ingredient normalized by recipe weight
    mutate(amount_normalized = (Amounts/Weight)) %>%
    #Calculate the mean value for each ingredient
    group_by(Ingredients) %>%
    summarise(value = mean(amount_normalized)) %>% ungroup()
  
  #Add to the Ingredients with missing amounts
  temp <- various$mean_values %>%
    #Inner join with the ingredients missing amounts
    inner_join(., various$missing, by = 'Ingredients') %>%
    #Add the weight of these recipes
    inner_join(various$recipe_weight %>% select(-unit), by = 'sample_id') %>%
    #Calculate the amounts based on weight
    mutate(Amounts = value*Weight,
           unit = 'kg')
  
  #Add back to recipes df
  recipes <- full_join(recipes, temp) %>%
    select(-c(value, Weight, unit)) %>%
    #if a recipe has more than two occurences of a recipe (example: butter used both for frying and as part of a dough), sum them together
    group_by(sample_id, Ingredients, Country, Source) %>%
    summarise(Amounts = sum(Amounts)) %>% ungroup()
  
  #Which ingrediens still don't have amounts?
  various$missing <- anti_join(various$missing %>% select(sample_id, Ingredients), recipes %>% select(sample_id, Ingredients))
  
  #Fill in values for these ingredients based on similar ingredients
  temp <- various$missing %>%
    mutate(value = case_when(
      Ingredients %in% c('peanut oil', 'sunflower or peanut oil for frying') ~ various$mean_values %>% filter(Ingredients == 'sunflower oil') %>% select(value) %>% as.numeric(.),
      Ingredients %in% c('a little canola or olive oil', 'extra-virgin olive or more canola oil, to finish') ~ various$mean_values %>% filter(Ingredients == 'canola or olive oil') %>% select(value) %>% as.numeric(.),
      Ingredients == 'oil or butter for frying' ~ various$mean_values %>% filter(Ingredients == 'butter for cooking') %>% select(value) %>% as.numeric(.),
      Ingredients %in% c('a few fresh mint or flat-leaf parsley leaf, finely chopped', 'parsley') ~ various$mean_values %>% filter(Ingredients == 'parsley fresh herbs') %>% select(value) %>% as.numeric(.),
      Ingredients == 'grape juice' ~ various$mean_values %>% filter(Ingredients == 'vinegar balsamic') %>% select(value) %>% as.numeric(.), #Used "to taste"
      Ingredients == 'rice sushi' ~ various$mean_values %>% filter(Ingredients == 'rice white long grain') %>% select(value) %>% as.numeric(.),
      Ingredients == 'sauce horseradish' ~ various$mean_values %>% filter(Ingredients == 'yoghurt') %>% select(value) %>% as.numeric(.), #As conditment
      Ingredients == 'oil' ~ various$mean_values %>% filter(Ingredients == 'vegetable oil') %>% select(value) %>% as.numeric(.),
      Ingredients == 'orange, the zest' ~ various$mean_values %>% filter(Ingredients == 'lemon, the zest') %>% select(value) %>% as.numeric(.),
      Ingredients == 'sauce piri-piri' ~ various$mean_values %>% filter(Ingredients == 'chili sauce') %>% select(value) %>% as.numeric(.),
      Ingredients == 'salami' ~ various$mean_values %>% filter(Ingredients == 'ham') %>% select(value) %>% as.numeric(.),
      Ingredients %in% c('sage dried', 'dill dried', 'herbs', 'lemon balm') ~ various$mean_values %>% filter(Ingredients == 'coriander dried') %>% select(value) %>% as.numeric(.),
      Ingredients == 'chopped pickles' ~ various$mean_values %>% filter(Ingredients == 'cucumber pickled') %>% select(value) %>% as.numeric(.),
      Ingredients == 'pickled garlic' ~ various$mean_values %>% filter(Ingredients == 'garlic') %>% select(value) %>% as.numeric(.),
      str_detect(Ingredients, 'spice') ~ various$mean_values %>% filter(Ingredients == 'guacamole spice mix') %>% select(value) %>% as.numeric(.),
      Ingredients == 'wasabi' ~ various$mean_values %>% filter(Ingredients == 'paste chili') %>% select(value) %>% as.numeric(.),
      Ingredients == 'sauerkraut' & str_detect(sample_id, 'Christmas Ribbe') ~ various$mean_values %>% filter(Ingredients == 'cabbage red') %>% select(value) %>% as.numeric(.), #Exchangeable in the meal
      Ingredients == 'potetlefse' ~ various$mean_values %>% filter(Ingredients == 'tortilla') %>% select(value) %>% as.numeric(.),
      Ingredients == 'cheese garlic' ~ various$mean_values %>% filter(Ingredients == 'cheese semi-hard') %>% select(value) %>% as.numeric(.),
      Ingredients == 'ham smoked' ~ various$mean_values %>% filter(Ingredients == 'ham') %>% select(value) %>% as.numeric(.),
      Ingredients == 'anise ground' ~ various$mean_values %>% filter(Ingredients == 'star anise') %>% select(value) %>% as.numeric(.),
      Ingredients == 'apricot jam' ~ various$mean_values %>% filter(Ingredients == 'cranberries jam') %>% select(value) %>% as.numeric(.)
      
    )) %>%
    
    #Drop those without amounts
    drop_na(value) %>%
    
    #Add the weight of these recipes
    inner_join(various$recipe_weight) %>%
    #Calculate the amounts based on weight
    mutate(Amounts = value*Weight,
           unit = 'kg')
  
  #Add back to recipes df
  recipes <- full_join(recipes, temp) %>%
    select(-c(value, Weight, unit)) %>%
    #if a recipe has more than two occurences of a recipe (example_ butter used both for frying and as part of a dough), sum them together
    group_by(sample_id, Ingredients, Country, Source) %>%
    summarise(Amounts = sum(Amounts)) %>% ungroup()
  
  #Fill in missing Country and source info for the missing ingredients
  recipes <- recipes %>%
    group_by(sample_id) %>%
    fill(c(Country, Source), .direction = 'downup') %>% ungroup()
  
  #New total weight of recipes
  various$recipe_weight <- recipes %>%
    select(sample_id, Amounts) %>%
    group_by(sample_id) %>%
    summarise(Weight = sum(Amounts, na.rm = TRUE)) %>%
    ungroup()
  
  #Weight of each ingredient normalized by recipe weight
  various$ingredients_weight <- recipes %>%
    select(sample_id, Source, Ingredients, Amounts) %>%
    inner_join(various$recipe_weight) %>%
    mutate(normalized_weight = Amounts/Weight) %>%
    select(-c(Amounts, Weight))
  
  #Still missing ingredients
  various$no_amounts <- various$missing %>%
    select(sample_id, Ingredients) %>%
    anti_join(temp %>% select(sample_id, Ingredients)) 
  various$missing <- NULL
  
  
# Map to nutrition database----
  temp <- checkRef(recipes, reference = references$nutrients) %>%

    #Join with nutrient db
    inner_join(databases$nutrients %>% mutate(ID = as.numeric(ID)), by = 'ID') %>%
    select(-ref) %>%
    replace(is.na(.), 0)
  
  #See which ingredients haven't been picked up
  t <- anti_join(recipes %>% select(sample_id, Ingredients) %>% unique(), temp %>% select(sample_id, Ingredients))
  
  #Save the raw nutrient values in 100g recipe by ingredients
  various$ingredients_nutrients <- temp %>%
    select(Ingredients, ID) %>%
    #Add amounts of ingredients
    inner_join(., recipes, by = 'Ingredients') %>%
    unique() %>%
    
    #Amount of each ingredient per 100 g of recipe
    inner_join(., various$ingredients_weight, by = c('sample_id', 'Source', 'Ingredients')) %>%
    unique() %>%
    
    #Join with nutrient database
    full_join(databases$nutrients, by = 'ID') %>%
    
    #Turn wide and calculate nutrient contents
    select(-c(ID, Amounts)) %>%
    pivot_longer(
      cols = -c(sample_id, Ingredients, Country, Source, normalized_weight),
      names_to = 'feature',
      values_to = 'value'
    ) %>%
    
    #Calculate nutrient content pr ingredients
    mutate(value = normalized_weight*value) %>% #Nutrient amount from each ingredient
    select(-normalized_weight) %>%
    
    #Rename for later
    rename(group = Country) %>%
    #Remove empty rowns
    drop_na(Ingredients)
  
  #Sum nutrient content for whole recipe
  various$recipes_nutrients <- various$ingredients_nutrients %>%
    
    #Sum for each recipe
    group_by(sample_id, group, Source, feature) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
    drop_na(group) 
  
  #Ingredients that has not been mapped regardless of reason
  various$no_nutrient_info <- t %>%
    inner_join(recipes %>% select(sample_id, Ingredients, Amounts)) %>%
    
    #Calculate amount pr 100 g
    inner_join(., various$recipe_weight) %>% #Total weight of recipe
    mutate(`pct_of_full_recipe` = round(Amounts/Weight*100, 2)) %>%
    select(-c(Amounts, Weight))
  
# Sustainability database-----
  temp <- checkRef(recipes, reference = references$sustainability) %>%
  
    #Join with sustainability db
    inner_join(databases$sustainability %>% mutate(ID = as.numeric(ID)), by = 'ID') %>%
      select(-ref) %>%
      replace(is.na(.), 0)
  
  #See which ingredients haven't been picked up
  t <- anti_join(recipes %>% select(sample_id, Ingredients) %>% unique(), temp %>% select(sample_id, Ingredients))
  
  #Save the raw environmental impact values in 100g recipe by ingredients
  various$ingredients_sustainability <- temp %>%
    
    #Add the amounts of ingredients normalized to recipe weight
    inner_join(., various$ingredients_weight) %>%
    unique() %>%
    
    #Turn wide to calculate environmental impact
    select(-c(ID, Amounts)) %>%
    pivot_longer(
      cols = -c(sample_id, Ingredients, Country, Source, L1, normalized_weight),
      names_to = 'feature',
      values_to = 'sustainability_value_kg' #SHARP has sustainability values pr 1kg of an ingredient
    ) %>%
    
    #Value pr 100g
    mutate(value = (sustainability_value_kg*normalized_weight/10)) %>% #Divide by 10 to get environmental impact per 100 g
    select(-c(sustainability_value_kg, normalized_weight)) %>%
    drop_na(Ingredients) %>%
    
    #Turn wide again
    pivot_wider(.,
                names_from = 'feature',
                values_from = 'value') %>%
    #Rename column
    rename(CO2 = `GHGE of 1 kg food as consumed_kgCO2eq`,
           Landuse = `Land use of 1 kg food as consumed_m2/yr`,
           group = Country) %>%
    
    #Keep in long format
    #Pivot long to do the calculations
    pivot_longer(
      cols = -c(sample_id, Ingredients, group, Source, L1),
      names_to = 'feature',
      values_to = 'value'
    )
    
  
  #Sum total envirnomental impact per 100g recipe
  various$recipe_sustainability <- various$ingredients_sustainability %>%
    
    #Calculate the whole recipe
    group_by(sample_id, group, Source, feature) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() 
  
  #Per foodgroup
  various$foodgroup_sustainability <- various$ingredients_sustainability %>%
    
    #Calculate the whole recipe
    group_by(group, Source, L1, feature) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() 
  
  #Ingredients that are still without sustainability indicators
  various$no_sustainability_indiactors <- t %>%
    inner_join(recipes) %>% select(sample_id, Ingredients, Amounts) %>%
    
    #Calculate amount pr 100 g
    inner_join(., various$recipe_weight) %>% #Total weight of recipe
    mutate(`pct_of_full_recipe` = round(Amounts/Weight*100, 2)) %>%
    select(-c(Amounts, Weight))
  
#Save for analysis ----
#Remove recipes with >10% missing data, either from nutrition or sustainability
missing_data <- list(
  'no_amounts' = various$no_amounts,
  'no_nutrient_info' = various$no_nutrient_info,
  'no_sustainability_indicators' = various$no_sustainability_indiactors
  )
  saveRDS(missing_data, './Data/output/missing_data2.Rds')
  
to_remove <- bind_rows(
  missing_data$no_nutrient_info %>% filter(pct_of_full_recipe >10),
  missing_data$no_sustainability_indicators %>% filter(pct_of_full_recipe >10)
) %>% unique()
 
#All recipes with summed nutrients and sustainability 
recipes_analysis <- full_join(
  various$recipe_sustainability %>% pivot_wider(names_from = feature, values_from = value),
  various$recipes_nutrients %>% pivot_wider(names_from = feature, values_from = value)
) %>% filter(!sample_id %in% to_remove$sample_id) #Remove recipes with too much missing data
saveRDS(recipes_analysis, './Data/output/recipes_for_analysis.Rds') 

ingredients_analysis <- full_join(
  various$ingredients_sustainability %>% pivot_wider(names_from = feature, values_from = value),
  various$ingredients_nutrients %>% pivot_wider(names_from = feature, values_from = value)
) %>%
  #Add weight of each ingredient
  inner_join(various$ingredients_weight) %>% rename(Amounts = normalized_weight) %>%
  filter(!sample_id %in% to_remove$sample_id) #Remove recipes with too much missing data
saveRDS(ingredients_analysis, './Data/output/ingredients_for_analysis.Rds')


to_share_ingredients <- ingredients_analysis %>%
  rename(food_group = L1) %>%
  select(group, Source, sample_id, Ingredients, food_group, Amounts, everything())

to_share_recipes <- recipes_analysis %>%
  inner_join(., recipes %>%
               select(sample_id, Country, Source) %>%
               rename(group = Country) %>% unique()) %>%
  select(group, Source, sample_id, everything())

features_metadata <- read_xlsx('./Data/databases/matvaretabellen_metadata.xlsx') %>%
  select(-c(starts_with('Ref'), starts_with('Food'), starts_with('C1'), starts_with('C20:3'), starts_with('C20:4'), `Edible part`, Water)) %>%
  rename(EPA = `C20:5n-3 (EPA)`,
         DPA = `C22:5n-3 (DPA)`,
         DHA = `C22:6n-3 (DHA)`) %>%
  mutate(CO2 = 'consumed_kgCO2eq',
         Landuse = 'consumed_m2/yr',
         Amounts = 'Normalized by total weight of recipe',
         Weight = 'kg') %>%
  pivot_longer(.,
               cols = everything(),
               names_to = 'feature',
               values_to = 'unit')

write_csv(to_share_ingredients, 'all_ingredients_100g.csv')
write_csv(to_share_recipes, 'all_recipes_100g.csv')
write_csv(features_metadata, 'features_metadata.csv')
write_csv(various$recipe_weight, 'total_weight_recipe.csv')
write_csv(various$org_ingredients, 'original_ingredients')

