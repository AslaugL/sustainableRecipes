library(tidyverse)
library(readxl)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data')

#Load raw data helsedir
raw_data <- read_xlsx('./porsjoner_vekt_næringsinnhold/vekt_porsjonsstørrelser_helsedir.xlsx')

#List to keep various dataframes and keep envronment clean
various <- list()

#Reformat----
all_weights <- raw_data %>% rename(
  portion = contains('porsjon'),
  tbsp = contains('ss,'),
  tsp = contains('ts,'),
  ms = contains('ms,'),
  dl = contains('dl,'),
  unit_enhet = EnhetUnit,
  g = contains('g/'),
  brutto = `Brutto g`,
  netto = `Netto g`
  ) %>%

  #Turn into long format
  select(-`Spiselig del % Edible part %`) %>% #Don't need this column
  pivot_wider(., #Turn into long format first to get all the different unit types in the name row
              names_from = unit_enhet,
              values_from = g) %>%
  pivot_longer(.,
               cols = -contains('Matvare'),
               names_to = 'unit_enhet',
               values_to = 'g') %>%
  drop_na() %>%

  #Rename for easier use later
  rename(Ingredients = `Matvare\r\nFood item`,
         Foodgroup = `Matvaregruppe\r\nFoodgroup`) %>%
  
  #Remove some not needed foodgroups
  filter(!str_detect(Foodgroup, regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter'))) %>%
  
  #Create unique IDs for each ingredients
  group_by(Ingredients) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  
  #Split into Norwegian and English
  separate_rows(., c(Ingredients, Foodgroup, unit_enhet), sep = '\n') %>%
  #Remove whitespace
  mutate_at(c('Ingredients', 'Foodgroup', 'unit_enhet'), ~str_trim(.)) %>%
           
  #Split rows of foods with multiple weights
  #First extract the references to a separate column
  separate_rows(., g, sep = '/') %>%
  #Split the weight column into grams and the reference letter from the original table
  mutate(g = g %>%
           str_replace('([:alpha:])', ' \\1') %>%
           str_replace(',', '.')) %>%
  separate(g, into = c('g', 'reference'), sep = ' ') %>%
  mutate_at('g', ~ as.numeric(.)) %>%
  
  #Use the mean weight for these foods and remove the created duplicates
  group_by(Ingredients, unit_enhet, Foodgroup) %>%
  mutate(g = mean(g),
       reference = paste(reference, collapse = ',')) %>%
  ungroup() %>% unique()

#Split the 'Rømme, crème fraîche, kesam' and butter/margarine rows into individual rows, create new IDs for rømme/kesam etc
temp <- all_weights %>%
  filter(str_detect(Ingredients, 'margarin|crème fraîche')) %>%
  #Add ghee, similar to butter and oil
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'margarin') & !str_detect(Ingredients, 'iquid|lytende') ~ paste0(Ingredients, ', ghee'),
    TRUE ~ Ingredients
  )) %>%
  separate_rows(., Ingredients, sep = ',') %>% mutate(Ingredients = str_trim(Ingredients)) %>%
  #Remove liquid/flytende margarine
  filter(!str_detect(Ingredients, 'iquid|lytende')) %>%
  #New ID's
  mutate(ID = case_when(
    str_detect(Ingredients, 'ømme|our') ~ as.integer(607),
    str_detect(Ingredients, 'crème') ~ as.integer(608),
    Ingredients %in% c('kesam', 'quark') ~ as.integer(609),
    TRUE ~ ID
  ))
  
all_weights <- all_weights %>%
  #Remove the rømme/kesam row from all_weights and add the individual rows
  filter(!str_detect(all_weights$Ingredients,'crème fraîche|margarin')) %>%
  full_join(., temp) %>%
  
  #Fill in reference letters that went missing when separating multiple weight ingredients
  group_by(Ingredients, unit_enhet) %>%
  fill(reference, .direction = 'downup') %>%
  ungroup() %>%
  
  #Change names of the different units so they are in line with the recipes
  mutate(unit_enhet = unit_enhet %>%
           str_replace_all('desiliter|decilitre', 'dl') %>%
           str_replace_all('spiseskje|tablespoon', 'tbsp') %>%
           str_replace_all('one piece|a piece|piece|per item|stk one|one', 'stk') %>%
           str_replace_all('porsjon', 'portion') %>%
           str_replace_all('fedd \\(med skall\\)|clove \\(with skin\\)', 'clove') %>%
           str_replace_all('handful', 'neve') %>%
           str_replace('rasher', 'slice') %>%
           str_replace('dl \\(in cubes\\)', 'dl') %>%
           str_replace('cm rot|cm of root', 'cm')
  ) %>%
  #Conditional
  mutate(unit_enhet = case_when(
    str_detect(unit_enhet, 'slice|skive') ~ 'slice',
    str_detect(Ingredients, 'Blåmuggost|Blue cheese') ~ str_replace(unit_enhet, 'portion', 'slice'),
    TRUE ~ unit_enhet
  )) %>%

#Remove some ingredients not used
  filter(!(str_detect(Ingredients, 'Celery') & unit_enhet %in% c('brutto', 'netto') ))

#Use the mean values for different types of crisp bread
temp <- all_weights %>%
  filter(str_detect(tolower(Ingredients), 'crisp bread')) %>%
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'bread') ~ 'crisp bread'
  )) %>%
  group_by(Ingredients, unit_enhet) %>%
  mutate(g = mean(g),
         ID = median(ID),
         reference = 'i, t, u') %>%
  ungroup() %>% unique()

all_weights <- all_weights %>%
  filter(!str_detect(tolower(Ingredients), 'crisp bread')) %>%
  full_join(temp)
  

#Ingredients from recipes not present in the Norwegian food weight and measurement database
temp <- list(
  
    #Meat
    c('Bacon', 'pack', '140', 'Gilde, stjernebacon'),
    c('Beef tongue', 'stk', '1200', 'Gilde'),
    c('calf tail', 'portion', '200', 'f'),
    c('Turkey whole', 'stk', '6000', 'https://www.matprat.no/oppdelingsguiden/kalkun/hel-kalkun/'),
    c('Turkey drumstick', 'stk', '600', 'https://engrosnett.no/lokal-mat/homlagarden/kalkun-fryst'),
    c('Duck breast', 'stk', '250', 'Gårdsand/Holte'),
    c('Duck leg', 'stk', '280', 'Gårdsand/Holte'),
    c('sheep head', 'stk', '1100', 'Nortura, Eldhus Smalahove'),
    c('sausage cumberland', 'stk', '56.75', 'Tesco British Cumberland Sausages'),
    c('chicken diced', 'dl', '57.14', 'https://www.eatthismuch.com/food/nutrition/chicken-breast,454/'),
    
    #Bread
    c('Naan bread', 'stk', '130', 'Santa Maria'),
    c('crisp bread', 'pack', '520', 'WASA'),
    
    #Vegetables
    c('Asparagus', 'bunch', '250', 'Meny'),
    c('Asparagus beans', 'dl', '80', 'Assumed similar to other beans in database'),
    c('baby corn', 'stk', '10', 'BAMA, mais mini'),
    c('bagel', 'stk', '85', 'Meny, Hatting'),
    c('bay leaf', 'stk', '0.2', 'Yahoo Answers'),
    c('Bean sprout', 'dl', '97.3', 'FoodData Central'),
    c('Break beans', 'stk', '7', 'Assumed twice the weight of a sugar snap pea'),
    c('brødrasp/griljermel/bread crumb', 'dl', '67.6', 'FoodData Central'),
    c('Bread, semi-coarse', 'stk', '700', 'Meny, assortert utvalg'), #Rewrite to keep only Bread, coarse and Loff as bread 
    c('Loff, formstekt', 'stk', '700', 'Meny, assortert utvalg'), #Rewrite to keep only Bread, coarse and Loff as bread 
    c('butternut squash', 'stk', '1360.8', 'River cottage every day'),
    c('cardamom pod', 'stk', '0.14', 'thespiceguide.com'),
    c('cardamom', 'dl', '39.2', 'FoodData Central'),
    c('Celariac root', 'slice', '40', 'Assume same as one dl'),
    c('carrot paste', 'dl', '101.4', 'Assumed same density as other pastes'),
    c('cayenne pepper', 'dl', '35.8', 'FoodData Central'),
    c('cherry tomato', 'neve', '65', 'Assume one handful is about one dl'),
    #c('Coriander', 'dl', '6.7', 'FoodData Central'),
    c('Coconut milk', 'stk', '400', 'Rema 1000, kokosmelk'),
    c('corn cob', 'stk', '250', 'Grønn&Frisk, Meny'),
    c('corn kernel', 'dl', '105.7', 'FoodData Central'),
    c('chapati', 'stk', '60', 'Assume weight is the same as a tortilla'),
    c('chick pea flour', 'dl', '38.9', 'FoodData Central'),
    c('Chicory', 'stk', '85', 'CooksInfo.com'),
    c('Chicory, endive', 'stk', '85', 'CooksInfo.com'),
    c('chili flake', 'dl', '48.7', 'FoodData Central'),
    c('chili paste', 'dl', '101.4', 'FoodData Central'),
    c('chili powder', 'dl', '54.1', 'FoodData Central'),
    c('Chili sauce/chilisaus', 'dl', '111.6', 'FoodData Central'),
    c('cinnamon', 'dl', '52.6', 'FoodData Central'),
    c('curry powder', 'dl', '42.6', 'FoodData Central'),
    c('cumin', 'dl', '40.6', 'FoodData Central'),
    c('crispis salad', 'stk', '150', 'Meny, Gartner'),
    c('eplemos/apple sauce', 'dl', '108.2', 'FoodData Central'),
    c('curry paste', 'dl', '108.2', 'FoodData Central'),
    c('garam masala', 'dl', '30.4', 'FoodData Central'),
    c('Garlic', 'tsp', '2.5', 'a'), #Rule of thumb says one clove is equal to one tsp
    c('Garlic', 'dl', '50', 'Twenty teaspoons'),
    c('garlic paste', 'dl', '94.7', 'FoodData Central'),
    c('grilled sweet pepper', 'glass', '290', 'Meny, gaea'),
    c('ginger paste', 'dl', '101.4', 'FoodData Central'),
    c('Ginger, pickled', 'dl', '101.4', 'FoodData Central'),
    c('Ginger root', 'dl', '35.2', 'FoodData Central'), #This is ground, not grated ginger though
    c('heart salad', 'stk', '150', 'Meny, Gartner'),
    c('Horse-radish', 'dl', '101.4', 'FoodData Central'),
    c('jalapeno/jalapeño', 'dl', '101.4', 'FoodData Central'),
    c('korianderfrø/coriander seed', 'dl', '33.8', 'FoodData Central'),
    c('lasagna plate', 'stk', '18', 'Barilla'),
    c('lemon peel', 'dl', '40.6', 'FoodData Central'),
    c('mango chutney', 'dl', '135.3', 'FoodData Central'),
    c('mustard seed', 'dl', '61.5', 'FoodData Central'),
    c('mint chutney', 'dl', '115', 'FoodData Central'),
    c('nutmeg', 'dl', '47.3', 'FoodData Central'),
    c("onion pearl", "stk", "5", "Melissa's produce"), #https://www.melissas.com/products/pearl-onions
    c('paprika powder', 'dl', '46', 'FoodData Central'),
    c('Pumpkin Seeds', 'dl', '50.7', 'FoodData Central'),
    c('saffron', 'dl', '14.2', 'FoodData Central'),
    c('salad mix', 'leaf', '15', 'Recipeland'),
    c('lettuce/lollo rosso', 'leaf', '15', 'Recipeland'),
    c('salad', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset'),
    c('salad mix', 'stk', '175', 'Kolonial, Baby leaf salad mix'),
    c('salad mix', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset'),
    c('scallion, spring onion', 'bunch', '150', 'Meny, Gartner'),
    c('Pearl barley', 'portion', '65', 'same as other grains used for dinner'),
    c('pickled pepper', 'dl', '101.4', 'FoodData Central'),
    c('puff pastry', 'stk', '75', 'Meny, Bakeverket'),
    c('ruccola', 'dl/neve', '8.5', 'FoodData Central'),
    c('Taco sauce', 'dl', '101.4', 'FoodData Central'),
    c('tandoori spice', 'dl', '106.8', 'FoodData Central'),
    c('Tomatoes, canned', 'box', '400', 'Mutti, Eldorado'),
    c('Tomatoes, canned', 'can', '400', 'Mutti, Eldorado'),
    c('Tomato Paste', 'dl', '101.4', 'FoodData Central'),
    c('Tomato purée', 'glass', '140', 'Mutti, Eldorado'),
    c('tomato salsa', 'dl', '101.4', 'FoodData Central'),
    c('Tomatoes sun-dried', 'dl', '101.4', 'FoodData Central'), #In oil, a bit less without oil 
    c('Tomato beans', 'box', '410', 'Eldorado'),
    c('cherry tomato', 'bunch', '250', 'Kolonial, cherrytomat'),
    c('turmeric', 'dl', '63.6', 'FoodData Central'),
    c('Worcestershire sauce', 'dl', '116.2', 'FoodData Central'),
    c('zedoary', 'dl', '63.6', 'Assume same as turmeric, zedoary also known as "white turmeric"'),
    c('fennel seed', 'dl', '39.22', 'FoodData Central'),
    c('garlic oil', 'dl', '94.68', 'As other oils'),
    c('mint dried', 'dl', '10.82', 'FoodData Central'),
    c('mint sauce', 'dl', '101.4', 'FoodData Central'),
    c('Mushroom, shiitake', 'stk', '19', 'https://hannaone.com/Recipe/weightmushrooms.html'),
    c('Mushroom, portebello', 'stk', '160.75', 'https://hannaone.com/Recipe/weightmushrooms.html'),
    c('champignon', 'stk', '18', 'https://hannaone.com/Recipe/weightmushrooms.html'),
    c('hazelnut oil', 'dl', '94.68', 'As other oils'),
    c('peanut oil', 'dl', '94.68', 'As other oils'),
    c('pesto', 'glass', '185', 'Meny, Eldorado'),
    c('peas, dry', 'neve', '80', 'A handful is about 1 dl'),
    c('bean broad', 'dl', '109.9', 'FoodData Central'),
    c('bok choi', 'stk', '125', 'Meny'),
    c('Mushroom, chestnut', 'stk', '10', 'https://hannaone.com/Recipe/weightmushrooms.html'),
    c('caraway seed', 'dl', '45.31', 'FoodData Central'),
    c('allspice', 'dl', '40.58', 'FoodData Central'),
    c('pine nuts', 'neve', '20', 'As other nuts'),
    c('radish', 'bunch', '130', 'Kolonial'),
    
    #Seafood
    c('Anchovies, canned', 'box', '55', 'Abba'),
    c('fish sauce/fiskesaus', 'dl', '121.7', 'FoodData Central'),
    c('crab', 'stk', '500', 'Meny, portion sizes seafood'),
    c('crab shell', 'stk', '150', 'Meny, Lerøy seafood'),
    c('crab claw', 'portion', '500', 'Meny, portion sizes seafood'),
    c('kreps', 'portion', '500', 'Meny, portion sizes seafood'),
    c('Mackerel fillet, in tomato sauce, canned', 'stk', '170', 'Stabburet'),
    c('scampi', 'portion', '500', 'Meny portion size seafood'),
    c('shrimp paste', 'dl', '101.4', 'FoodData Central'),
    c('tuna oil', 'box', '185', 'Eldorado'),
    c('tuna water', 'box', '185', 'Eldorado'),
    c('tuna oil', 'stk', '185', 'Eldorado'),
    c('tuna water', 'stk', '185', 'Eldorado'),
    c('squid baby', 'stk', '85.05', 'http://www.clovegarden.com/ingred/seasquidc.html'),
    c('herring smoked', 'stk', '300', 'DOMSTEIN Sjømat'),
    
    #Dairy
    c('Blue cheese', 'dl', '47.34', 'FoodData Central'),
    c('Cream cheese', 'box', '125', 'Tine'),
    c('Cream cheese', 'dl', '108.2', 'FoodData Central'),
    c('ricotta salata', 'dl', '104.8', 'FoodData Central'),
    c('Feta cheese', 'stk', '200', 'Meny, Apetina/Kolios'),
    c('Parmesan', 'neve', '40', 'Assume about the same as one dl'),
    
    #Div
    c('broth cube', 'stk', '10', 'TORO klar kjøttbuljong'),
    c('taco seasoning', 'dl', '60.9', 'FoodData Central'),
    c('taco seasoning', 'pack', '28', 'Santa Maria'),
    c('tabasco', 'dl', '101.4', 'FoodData Central'),
    c('oyster sauce', 'dl', '304.3', 'FoodData Central'),
    c('Walnuts', 'stk', '7', 'Google'),
    c('wine', 'glass', '110', 'vinmonopolet'),
    
    #MISSING
    c('Einebær/juniper berry', 'stk', '', ''),
    c('Einebær/juniper berry', 'dl', '', ''),
    c('lemongrass/sitrongressrot', 'stk', '', ''),
    c('pickled onion', 'dl', '', ''),
    c('salmon roe', 'dl', '', ''),
    c('asparagus white', 'dl', '', ''),
    c('potetlefse', 'stk', '', ''),
    c('prawn', 'stk', '', '')
    
)

#Add new ingredients to all_weights
createIngredientRow <- function(vector){
  
  row <- tibble(
    'Ingredients' = vector[1],
    'unit_enhet' = vector[2],
    'g' = as.numeric(vector[3]),
    'reference' = vector[4]
  )
  
}
new_ingredients <- lapply(temp, createIngredientRow) %>%
  bind_rows()

#Add different herbs
herbs <- tibble(
  'Ingredients' = c('rosemary', 'tarragon', 'Basil/Basilikum', 'Chives',
                    'Parsley/Kruspersille/Persille', 'thyme', 'Coriander/Koriander',
                    'Oregano', 'Chervil/Kjørvel', 'Dill', 'mint', 'sorrel', 'cilantro',
                    'cress/watercress')) %>%
  mutate(unit_enhet = 'bunch/dl/neve',
         g = 20,
         reference = case_when(
           !Ingredients %in% c('sorrel', 'cilantro') ~ 'Fersk i Pose, Netfresh/Kolonial',
           TRUE ~ 'As other herbs')) %>%
  separate_rows(unit_enhet, sep = '/') %>%
  separate_rows(Ingredients, sep = '/')
#Add sprigs of herbs
herbs_sprigs <- tibble(
  'Ingredients' = c('rosemary', 'tarragon', 'Basil/Basilikum', 'Chives',
                    'Parsley/Kruspersille/Persille', 'thyme', 'Oregano',
                    'Dill', 'mint')) %>%
  mutate(unit_enhet = c('tsp', 'tsp', 'tbsp/tbsp', 'tsp', 'tsp/tsp/tsp',
                        'tsp', 'tsp', 'tsp', 'tsp'),
         g = c('1', '1', '1/1', '0.375', '1.5/1.5/1.5', '0.125', '1', '1', '1')) %>%
  separate_rows(Ingredients, sep = '/') %>%
  separate_rows(unit_enhet, sep = '/') %>%
  separate_rows(g, sep = '/') %>%
  mutate_at('g', ~as.numeric(.)) %>%
  #Turn tsp/tbsp into dl and then to gram
  mutate(
    g = case_when(
      unit_enhet == 'tsp' ~ g*0.05,
      unit_enhet == 'tbsp' ~ g*0.15),
    unit_enhet = case_when(
      unit_enhet %in% c('tsp', 'tbsp') ~ 'dl'
    )) %>%
  separate_rows(Ingredients, sep = '/') %>%
  separate_rows(unit_enhet, sep = '/') %>%
  separate_rows(g, sep = '/') %>%
  mutate_at('g', ~as.numeric(.)) %>%
  inner_join(herbs, by = c('Ingredients', 'unit_enhet')) %>%
  unique() %>%
  
  #clean up
  mutate(unit_enhet = str_replace(unit_enhet, 'dl', 'sprig'),
         g = g.x * g.y) %>%
  select(-c(g.x, g.y))
#Dry herbs
herbs_dry <- tibble(
  'Ingredients' = c('oregano dried', 'basil dried', 'thyme dried', 'tarragon dried', 'fenugreek dried'),
  'g' = c(20.3, 30.4, 29.1, 32.5, 75.1)) %>%
  mutate(unit_enhet = 'dl',
         reference = 'FoodData Central')

#Add different oils
oils <- tibble(
  'Ingredients' = c('olive oil/olivenolje', 'coconut oil', 'soy oil/soybean oil',
                    'canola oil', 'sunflower oil', 'sesame oil/sesamolje', 'corn oil', 'salad oil',
                    'rapeseed oil', 'truffle oil', 'vegetable oil/vegetabilsk olje', 'sunflower oil')) %>%
  mutate(unit_enhet = 'dl',
         'g' = 94.7,
         reference = 'FoodData Central')

new_ingredients <- full_join(new_ingredients, herbs) %>%
  full_join(herbs_sprigs) %>%
  full_join(herbs_dry) %>%
  full_join(., oils) %>%
  mutate(Ingredients = tolower(Ingredients))

all_weights <- full_join(all_weights, new_ingredients) %>%
  
  #Turn everything to small letters
  mutate(Ingredients = tolower(Ingredients)) %>%
  
  #Unique ID
  group_by(Ingredients) %>% #For ingredients already found the the dataset
  fill(ID, .direction = 'downup') %>%
  ungroup() 

#Create unique id's for the new ingredients
temp <- all_weights %>%
  filter(is.na(ID)) %>%
  group_by(Ingredients) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = ID + 609) %>% #can't use max(ID) from all_weights as it contains NA
  ungroup() %>%
  separate_rows(Ingredients, sep = '/') 
  
#Add back
all_weights <- all_weights %>% 
  filter(!is.na(ID)) %>%
  full_join(., temp) %>%
  replace_na(list(Foodgroup = '')) %>%
  separate_rows(unit_enhet, sep = '/') %>% unique() %>% #Separate rows with multiple units keeping the same ID
  
  #Use mean unit measurement
  group_by(Ingredients, unit_enhet) %>%
  mutate(g = mean(g)) %>%
  ungroup() %>%
  unique() 

#Ingredients not needed in the final database 
various$not_needed <- all_weights %>%
  
  #Whole foodgroups not beeded
  filter(str_detect(Foodgroup,
                    regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter|cakes|candy|Desert|Dressing|hjemmelaget|homemade|Grain breakfast cereal|knekkebrød|biscuits|pølser|cold cuts|pålegg|Fish spread|Fish products|snacks|candy|Meat products')) |
           
           #Use raw eggs as the standard for egg-size, bread semi coarse as standard for bread
           ((str_detect(Ingredients, 'egg|Egg') & str_detect(Ingredients, 'store|large|små|small|mellomstore|medium|pan-fried|stekt|rambled'))) |
           
           #ID %in% c(50, 51, 319, 466) |
           
           #Ingredients not needed
           ((str_detect(Ingredients, 'aby') & str_detect(Ingredients, 'carrot|gulrot'))) |
           str_detect(Ingredients, 'fried|without skin|uten skinn|hicken and turkey|resh pasta|baguettes, fine, half fried|drink|drikke|fløteerstatning|milkshake|prim') |
           (str_detect(Ingredients, 'asagn|ulgur|uinoa|ouscous|akaroni|asta|getti|otet|otato|ønner|inser|eans|egumes|ris|Ris|Rice|rice') &
             str_detect(Ingredients, 'boiled|kokt|\\bcooked')) |
           Ingredients %in% c('Oliven, grønne, pickles', 'Olives, green, pickled',
                             'Pepper, hel', 'Pepper, malt', 'Gingersnaps', 'Ginger nuts',
                             'Almond sticks', 'Swedish almond cake macaroon tea cake', 'Loff, spiral',
                             'cream substitute, vegetable fat', 'whipped cream, canned', 'instant coffee creamer', 'coffee creamer',
                             'kaffemelk', 'krem/topping på boks', 'melkepulver', 'syrnet melk, kulturmelk, kefir', 'cultured milk, kefir milk',
                             'gammelost', 'traditional norwegian cheese, matured, gammelost', 'gomme', 'traditional norwegian cheese, gomme'
                             )) %>%
  
            #Any ingredient of the above that should be kept
            filter(!Ingredients %in% c('bread, semi-coarse', 'bread, white', 'crisp bread', 'flatbread, hard',
                                       'smoke-cured ham', 'cured ham', 'spekeskinke', 'boiled ham', 'honning', 'sukker, brunt',
                                       'sukker hvitt', 'anchovies, canned', 'anchovy fillets, canned', 'salmon, smoked, dry salted',
                                       'mackerel fillet, in tomato sauce, canned', 'cod roe', 'tuna canned', 'ground meat, raw'))
  
#Remove the not needed ingredients, add cloves
all_weights <- all_weights %>%
  filter(!Ingredients %in% various$not_needed$Ingredients) %>%
  add_row(Ingredients = 'cloves',
          unit_enhet = 'stk',
          g = 0.09,
          reference = 'https:\\/\\/forum.norbrygg.no\\/threads\\/traden-for-dumme-sporsmal.23452\\/page-62',
          ID = 700)

#Save
saveRDS(all_weights, './porsjoner_vekt_næringsinnhold/all_weights.Rds')

#Use store bought breads and rolls as default

#Change the names so they can be used to query the recipes
names <- all_weights %>%
  select(Ingredients, Foodgroup, ID) %>% unique() %>%
  
  #Change some ingredient names to better fit with the recipes
  mutate(Ingredients = Ingredients %>%
           str_replace(', rå|, raw|, uncooked|, tørr', '') %>%
           str_replace('red kidney', 'kidney') %>%
           str_replace('Tortilla, hvetelefse, fullkorn', 'tortilla, fullkorn, hvete, whole, wheat') %>%
           str_replace('tortilla, hvetelefse, fin', 'tortilla, fin, hvete, wheat') %>%
           str_replace('soft norwegian flatbread, ', '') %>%
           str_replace('\\bcelery\\b', 'celery stalk') %>%
           str_replace('with rye', 'rye') %>%
           str_replace(', spring onion', '') %>%
           str_replace(', hvetelefse,', '') %>%
           str_replace('leaf beet, mangold', 'chard') %>%
           str_replace('aubergine', 'eggplant') %>%
           str_replace('bread, semi-coarse', 'bread') %>%
           
           #Change all plural forms to singular
           str_replace('fillets', 'fillet') %>%
           str_replace('beans', 'bean') %>%
           str_replace('peas', 'pea') %>%
           str_replace('seeds', 'seed')
         ) %>%
  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'chop') & str_detect(Foodgroup, 'lamb') ~ paste0('lamb ', Ingredients),
    TRUE ~ Ingredients
  ))

#Create query
ref <- names %>%
  select(-Foodgroup) %>%
  mutate(Ingredients = str_replace_all(Ingredients, ',', '')) %>%
  
  #First two words contain the most important information to identify the ingredients
  mutate(first_word = str_extract(Ingredients, '^\\w+'),
         temp = str_extract(Ingredients, '^\\w+\\s\\w+')) %>%
  mutate(second_word = str_remove(temp, '^\\w+\\s')) %>%
  select(-temp) %>% unique() %>%
  replace_na(list(second_word = 'nothing')) %>%
  
  #Clean up some of them that's not right, always have the most generic name in the first column, then a specification in second column if present
  mutate(
    first_word = case_when(
      Ingredients == 'white or yellow hard to semi-hard cheese' ~ 'hard to semi-hard cheese',
      str_detect(Ingredients, 'gg noodle') ~ 'egg noodles',
      Ingredients == 'horse-radish' ~ 'horseradish',
      Ingredients == 'lasagne sheets uncooked' ~ 'lasagna',
      Ingredients == 'pork neck chops' ~ 'neck',
      Ingredients == 'sugar snap peas' ~ 'sugar snap peas',
      Ingredients %in% c('black pepper whole','black pepper grounded') ~ 'pepper',
      Ingredients == 'black chokeberries' ~ 'chokeberries',
      Ingredients == 'chick pea flour' ~ 'chick pea',
      Ingredients == 'chick pea canned' ~ 'chick pea',
      Ingredients == 'sugar snap pea' ~ 'pea',
      Ingredients == 'smoke-cured ham' ~ 'ham',
      Ingredients == 'cured ham' ~ 'ham',
      Ingredients == 'grilled sweet pepper' ~ 'sweet pepper',
      Ingredients == 'ground meat' ~ 'meat',
      Ingredients == 'bog blueberries' ~ 'bog blueberries',
      Ingredients == 'cured leg of mutton' ~ 'mutton',
      Ingredients == 'whole-grain pasta' ~ 'pasta',
      TRUE ~ first_word),
    
    second_word = case_when(
      Ingredients == 'white or yellow hard to semi-hard cheese' ~ 'nothing',
      Ingredients == 'chicken with skin' ~ 'whole',
      Ingredients == 'chili pepper red' ~ 'red',
      Ingredients == 'cod traditional Norwegian dish Lutefisk' ~ 'lutefisk',
      str_detect(Ingredients, 'gg noodle') & str_detect(Ingredients, 'cooked') ~ 'cooked',
      str_detect(Ingredients, 'gg noodle') & str_detect(Ingredients, 'uncooked') ~ 'nothing',
      str_detect(Ingredients, 'capers|kapers') ~ 'nothing',
      Ingredients == 'lasagne sheets uncooked' ~ 'plate',
      Ingredients == 'oil liquid margarine' ~ 'nothing',
      Ingredients == 'onion yellow/red' ~ 'nothing',
      Ingredients == 'pork neck chops' ~ 'chops',
      Ingredients == 'quinoa tørr' ~ 'nothing',
      Ingredients == 'sugar snap peas' ~ 'nothing',
      Ingredients == 'mackerel fillet, in tomato sauce, canned' ~ 'tomato',
      Ingredients %in% c('olives black in oil canned', 'oliven, svarte, i olje, hermetisk') ~ 'nothing',
      Ingredients == 'black pepper whole' ~ 'whole',
      Ingredients == 'black pepper grounded' ~ 'ground',
      Ingredients == 'chick pea flour' ~ 'flour',
      Ingredients == 'chick pea canned' ~ 'canned',
      Ingredients == 'mackerel fillet, in tomato sauce, canned' ~ 'tomato',
      Ingredients == 'smoke-cured ham' ~ 'smoked',
      Ingredients == 'cured ham' ~ 'cured',
      Ingredients == 'grilled sweet pepper' ~ 'grilled',
      Ingredients == 'ground meat' ~ 'ground',
      Ingredients == 'black chokeberries' ~ 'black',
      Ingredients == 'bog blueberries' ~ 'nothing',
      Ingredients == 'cured leg of mutton' ~ 'cured leg',
      Ingredients == 'lentils dry' ~ 'nothing',
      Ingredients == 'sugar snap pea' ~ 'sugar snap',
      Ingredients == 'hamburger bun' ~ 'bread',
      Ingredients == 'whole-grain pasta' ~ 'whole grain',
      Ingredients == 'ginger root' ~ 'nothing',
      Ingredients == 'spinach, raw' ~ 'nothing',
      Ingredients == 'mushroom common' ~ 'nothing',
      TRUE ~ second_word
    )
  ) %>%
  select(-Ingredients) 

saveRDS(ref, './porsjoner_vekt_næringsinnhold/food_weight_ref.Rds')

#Find the english terms that can be found in SHARP, to use to translate the norwegian ingredients
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
  
  results <- lapply(df$Ingredients, checkRef, reference = sharp_ref)
  
}


temp <- checkRefList(all_weights)

english_ref <- bind_rows(temp) %>% unique() %>%
  rename(sharp_ID = ID) %>%
  
  #Add ID from weght ref
  inner_join(all_weights, by = 'Ingredients') %>%
  rename(weight_ID = ID) %>%
  select(Ingredients, weight_ID, sharp_ID) %>% unique()

saveRDS(english_ref, 'translate_ingredients.Rds')

