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
  
  #Rename some ingredients
  mutate(Ingredients = Ingredients %>%
           str_replace('Loff, formstekt', 'Loff') %>%
           str_replace('Bread, white, square-shaped', 'Bread, white')) %>%
  
  #Create unique IDs for each ingredients
  group_by(Ingredients) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  
  #Split into Norwegian and English, firsh make a language column with language identifier
  mutate(language = 'norwegian\nenglish') %>%
  separate_rows(., c(Ingredients, Foodgroup, unit_enhet, language), sep = '\n') %>%
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
  group_by(Ingredients, unit_enhet, Foodgroup, language) %>%
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
    str_detect(Ingredients, 'ømme|our') ~ as.integer(807),
    str_detect(Ingredients, 'crème') ~ as.integer(808),
    Ingredients %in% c('kesam', 'quark') ~ as.integer(809),
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
    c('Bacon', 'pack', '140', 'Gilde, stjernebacon', 'english'),
    c('Beef tongue', 'stk', '1200', 'Gilde', 'english'),
    c('calf tail', 'portion', '200', 'f', 'english'),
    c('Turkey whole', 'stk', '6000', 'https://www.matprat.no/oppdelingsguiden/kalkun/hel-kalkun/', 'english'),
    c('Turkey drumstick', 'stk', '600', 'https://engrosnett.no/lokal-mat/homlagarden/kalkun-fryst', 'english'),
    c('Duck breast', 'stk', '250', 'Gårdsand/Holte', 'english'),
    c('Duck leg', 'stk', '280', 'Gårdsand/Holte', 'english'),
    c('sheep head', 'stk', '1100', 'Nortura, Eldhus Smalahove', 'english'),
    c('sausage cumberland', 'stk', '56.75', 'Tesco British Cumberland Sausages', 'english'),
    c('chicken diced', 'dl', '57.14', 'https://www.eatthismuch.com/food/nutrition/chicken-breast,454/', 'english'),
    c('rabbit', 'stk', '2500', 'http://grillogmat.blogspot.com/2013/04/kanin-med-smak-av-rosmarin-og-lime.html', 'english'),
    c('pork hock', 'stk', '1000', 'Fjellgris', 'english'),
    
    #Bread
    c('Naan bread', 'stk', '130', 'Santa Maria', 'english'),
    c('crisp bread', 'pack', '520', 'WASA', 'english'),
    
    #Vegetables/plant based
    c('Asparagus', 'bunch', '250', 'Meny', 'english'),
    c('Asparagus beans', 'dl', '80', 'Assumed similar to other beans in database', 'english'),
    c('corn baby', 'stk', '10', 'BAMA, mais mini', 'english'),
    c('bagel', 'stk', '85', 'Meny, Hatting', 'english'),
    c('bay leaf', 'stk', '0.2', 'Yahoo Answers', 'english'),
    c('Bean sprout', 'neve/dl', '97.3', 'FoodData Central', 'english'),
    c('Beans, green, raw', 'neve/dl', '40', 'FoodData Central', 'english'),
    c('olives, black, in oil, canned', 'neve', '50', 'Same as dl', 'english'),
    c('oliven, svarte, i olje, hermetisk', 'neve', '50', 'Same as dl', 'english'),
    c('Break beans', 'stk', '7', 'Assumed twice the weight of a sugar snap pea', 'english'),
    c('brødrasp/griljermel/bread crumb', 'dl', '67.6', 'FoodData Central', 'norwegian/norwegian/english'),
    c('Bread, semi-coarse', 'stk', '700', 'Meny, assortert utvalg', 'english'), #Rewrite to keep only Bread, coarse and Loff as bread 
    c('Bread, white/Loff, formstekt', 'stk', '715', 'Meny, Gammeldags loff', 'english/norwegian'),
    c('butternut squash', 'stk', '1360.8', 'River cottage every day', 'english'),
    c('cardamom pod', 'stk', '0.14', 'thespiceguide.com', 'english'),
    c('cardamom', 'dl', '39.2', 'FoodData Central', 'english'),
    c('Celariac root', 'slice', '40', 'Assume same as one dl', 'english'),
    c('carrot paste', 'dl', '101.4', 'Assumed same density as other pastes', 'english'),
    c('cayenne pepper', 'dl', '35.8', 'FoodData Central', 'english'),
    c('cherry tomato', 'neve', '65', 'Assume one handful is about one dl', 'english'),
    #c('Coriander', 'dl', '6.7', 'FoodData Central'),
    c('Coconut milk', 'stk', '400', 'Rema 1000, kokosmelk', 'english'),
    c('corn cob', 'stk', '250', 'Grønn&Frisk, Meny', 'english'),
    c('corn kernel', 'dl', '105.7', 'FoodData Central', 'english'),
    c('chapati', 'stk', '60', 'Assume weight is the same as a tortilla', 'english'),
    c('chick pea flour', 'dl', '38.9', 'FoodData Central', 'english'),
    c('Chicory', 'stk', '85', 'CooksInfo.com', 'english'),
    c('Chicory, endive', 'stk', '85', 'CooksInfo.com', 'english'),
    c('chili flake', 'dl', '48.7', 'FoodData Central', 'english'),
    c('chili paste', 'dl', '101.4', 'FoodData Central', 'english'),
    c('chili powder', 'dl', '54.1', 'FoodData Central', 'english'),
    c('Chili sauce/chilisaus', 'dl', '111.6', 'FoodData Central', 'english/norwegian'),
    c('ciabatta', 'stk', '62', 'Eldorado, Steinsovnbakt', 'english'),
    c('cardamom pod', 'stk', '0.14', 'The Spice Guide', 'english'),
    c('cloves', 'stk', '0.07', 'Charles McGuinnes medium', 'english'),
    c('cinnamon', 'dl', '52.6', 'FoodData Central', 'english'),
    c('cinnamon bar', 'stk', '4', 'Google', 'english'),
    c('curry powder', 'dl', '42.6', 'FoodData Central', 'english'),
    c('cumin', 'dl', '40.6', 'FoodData Central', 'english'),
    c('crispis salad', 'stk', '150', 'Meny, Gartner', 'english'),
    c('eplemos/apple sauce', 'dl', '108.2', 'FoodData Central', 'norwegian/english'),
    c('fig', 'stk', '62.5', 'Kolonial', 'english'),
    c('curry paste', 'dl', '108.2', 'FoodData Central', 'english'),
    c('garam masala', 'dl', '30.4', 'FoodData Central', 'english'),
    c('Garlic', 'tsp', '2.5', 'a', 'english'), #Rule of thumb says one clove is equal to one tsp
    c('Garlic', 'dl', '50', 'Twenty teaspoons', 'english'),
    c('garlic paste', 'dl', '94.7', 'FoodData Central', 'english'),
    c('grilled sweet pepper', 'glass', '290', 'Meny, gaea', 'english'),
    c('ginger paste', 'dl', '101.4', 'FoodData Central', 'english'),
    c('Ginger, pickled', 'dl', '101.4', 'FoodData Central', 'english'),
    c('Ginger root', 'dl', '35.2', 'FoodData Central', 'english'), #This is ground, not grated ginger though
    c('heart salad', 'stk', '150', 'Meny, Gartner', 'english'),
    c('Horse-radish', 'dl', '101.4', 'FoodData Central', 'english'),
    c('jalapeno/jalapeño', 'dl', '101.4', 'FoodData Central', 'english'),
    c('korianderfrø/coriander seed', 'dl', '33.8', 'FoodData Central', 'norwegian/english'),
    c('lasagna plate', 'stk', '18', 'Barilla', 'english'),
    c('lemon peel', 'dl', '40.6', 'FoodData Central', 'english'),
    c('mango chutney', 'dl', '135.3', 'FoodData Central', 'english'),
    c('mustard seed', 'dl', '61.5', 'FoodData Central', 'english'),
    c('mustard powder', 'dl', '40.6', 'FoodData Central', 'english'),
    c('mint chutney', 'dl', '115', 'FoodData Central', 'english'),
    c('nutmeg', 'dl', '47.3', 'FoodData Central', 'english'),
    c("onion pearl", "stk", "5", "Melissa's produce", 'english'), #https://www.melissas.com/products/pearl-onions
    c('paprika powder', 'dl', '46', 'FoodData Central', 'english'),
    c('Pumpkin Seeds', 'dl', '50.7', 'FoodData Central', 'english'),
    c('saffron', 'dl', '14.2', 'FoodData Central', 'english'),
    c('salad mix', 'leaf', '15', 'Recipeland', 'english'),
    c('lettuce/lollo rosso', 'leaf', '15', 'Recipeland', 'english'),
    c('salad', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset', 'english'),
    c('salad mix', 'stk', '175', 'Kolonial, Baby leaf salad mix', 'english'),
    c('salad mix', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset', 'english'),
    c('scallion, spring onion', 'bunch', '150', 'Meny, Gartner', 'english'),
    c('Pearl barley', 'portion', '65', 'same as other grains used for dinner', 'english'),
    c('pickled pepper', 'dl', '101.4', 'FoodData Central', 'english'),
    c('puff pastry', 'stk', '75', 'Meny, Bakeverket', 'english'),
    c('ruccola', 'dl/neve', '8.5', 'FoodData Central', 'english'),
    c('Taco sauce', 'glass', '230', 'Rema1000 and Old El Paso', 'english'),
    c('tandoori spice', 'dl', '106.8', 'FoodData Central', 'english'),
    c('Tomatoes, canned', 'box', '400', 'Mutti, Eldorado', 'english'),
    c('Tomatoes, canned', 'can', '400', 'Mutti, Eldorado', 'english'),
    c('Tomato Paste', 'dl', '101.4', 'FoodData Central', 'english'),
    c('Tomato purée', 'glass', '140', 'Mutti, Eldorado', 'english'),
    c('Tomato purée', 'stk', '200', 'Petti, Kolonial', 'english'),
    c('tomato salsa', 'dl', '101.4', 'FoodData Central', 'english'),
    c('Tomatoes, sun-dried', 'dl', '101.4', 'FoodData Central', 'english'), #In oil, a bit less without oil 
    c('Beans, white, in tomato sauce, canned', 'box', '410', 'Eldorado', 'english'),
    c('cherry tomato', 'bunch', '250', 'Kolonial, cherrytomat', 'english'),
    c('turmeric', 'dl', '63.6', 'FoodData Central', 'english'),
    c('Worcestershire sauce', 'dl', '116.2', 'FoodData Central', 'english'),
    c('zedoary', 'dl', '63.6', 'Assume same as turmeric, zedoary also known as "white turmeric"', 'english'),
    c('fennel seed', 'dl', '39.22', 'FoodData Central', 'english'),
    c('garlic oil', 'dl', '94.68', 'As other oils', 'english'),
    c('mint dried', 'dl', '10.82', 'FoodData Central', 'english'),
    c('mint sauce', 'dl', '101.4', 'FoodData Central', 'english'),
    c('Mushroom, shiitake', 'stk', '19', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
    c('Mushroom, portebello', 'stk', '160.75', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
    c('champignon', 'stk', '18', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
    c('hazelnut oil', 'dl', '94.68', 'As other oils', 'english'),
    c('peanut oil', 'dl', '94.68', 'As other oils', 'english'),
    c('pesto', 'glass', '185', 'Meny, Eldorado', 'english'),
    c('peas, dry', 'neve', '80', 'A handful is about 1 dl', 'english'),
    c('bean broad', 'dl', '109.9', 'FoodData Central', 'english'),
    c('Beans, white, large, canned', 'stk', '290', 'Øko kolonial', 'english'), #Kolonial/Oda recipes uses "stk" for these
    c('Beans, red kidney, canned', 'stk', '290', 'Øko kolonial', 'english'), #Kolonial/Oda recipes uses "stk" for these
    c('Beans, red kidney, canned', 'box', '380', 'Meny', 'english'), #3/4 kidney bean boxes/cans at Meny are 380g
    c('bok choi', 'stk', '125', 'Meny', 'english'),
    c('Mushroom, chestnut', 'stk', '10', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
    c('caraway seed', 'dl', '45.31', 'FoodData Central', 'english'),
    c('allspice', 'dl', '40.58', 'FoodData Central', 'english'),
    c('pine nuts', 'neve', '20', 'As other nuts', 'english'),
    c('radish', 'bunch', '130', 'Kolonial', 'english'),
    c('grapes, with seeds', 'neve', '70', 'As dl', 'english'),
    c('peas, frozen', 'neve', '60', 'As dl', 'english'),
    c('sugar snap peas', 'dl/neve', '26.95', 'FoodData Central', 'english'),
    c('lemongrass/sitrongressrot', 'stk', '50', 'Meny', 'english/norwegian'),
    c('Chick peas, canned', 'box', '290', 'Øko kolonial', 'english'),
    c('cucumber, pickled', 'stk', '70', 'Produce converter Howmuchisin', 'english'),
    c('chunky salsa', 'glass', '230', 'Santa Maria', 'english'),
    
    #Seafood
    c('Anchovies, canned', 'box', '55', 'Abba', 'english'),
    c('fish sauce/fiskesaus', 'dl', '121.7', 'FoodData Central', 'english/norwegian'),
    c('crab', 'stk', '500', 'Meny, portion sizes seafood', 'english'),
    c('crab shell', 'stk', '150', 'Meny, Lerøy seafood', 'english'),
    c('crab claw', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
    c('kreps/lobster', 'portion', '500', 'Meny, portion sizes seafood', 'norwegian/english'),
    c('Mackerel fillet, in tomato sauce, canned', 'stk', '170', 'Stabburet', 'english'),
    c('scampi', 'portion', '500', 'Meny portion size seafood', 'english'),
    c('shrimp paste', 'dl', '101.4', 'FoodData Central', 'english'),
    c('tuna oil', 'box', '185', 'Eldorado', 'english'),
    c('tuna water', 'box', '185', 'Eldorado', 'english'),
    c('tuna oil', 'stk', '185', 'Eldorado', 'english'),
    c('tuna water', 'stk', '185', 'Eldorado', 'english'),
    c('squid baby', 'stk', '85.05', 'http://www.clovegarden.com/ingred/seasquidc.html', 'english'),
    c('herring smoked', 'stk', '300', 'DOMSTEIN Sjømat', 'english'),
    c('prawn', 'stk', '17', 'Kostholdsplanleggeren//Matvaretabellen', 'english'),
    
    #Dairy
    c('Blue cheese', 'dl', '47.34', 'FoodData Central', 'english'),
    c('Cream cheese', 'box', '125', 'Tine', 'english'),
    c('Cream cheese', 'dl', '108.2', 'FoodData Central', 'english'),
    c('ricotta salata', 'dl', '104.8', 'FoodData Central', 'english'),
    c('Feta cheese', 'stk', '200', 'Meny, Apetina/Kolios', 'english'),
    c('Parmesan', 'neve', '40', 'Assume about the same as one dl', 'english'),
    
    #Div
    c('broth cube', 'stk', '10', 'TORO klar kjøttbuljong', 'english'),
    c('taco seasoning', 'dl', '60.9', 'FoodData Central', 'english'),
    c('taco seasoning', 'pack', '28', 'Santa Maria', 'english'),
    c('tabasco', 'dl', '101.4', 'FoodData Central', 'english'),
    c('oyster sauce', 'dl', '304.3', 'FoodData Central', 'english'),
    c('Walnuts', 'stk', '7', 'Google', 'english'),
    c('wine', 'glass', '110', 'vinmonopolet', 'english'),
    c('molasses', 'dl', '142.44', 'FoodData Central', 'english'),
    c('tamarind paste', 'dl', '101.44', 'FoodData Central', 'english'),
    
    #MISSING
    c('Einebær/juniper berry', 'stk', '', '', 'norwegian/english'),
    c('Einebær/juniper berry', 'dl', '', '', 'norwegian/english'),
    c('pickled onion', 'dl', '', '', 'english'),
    c('salmon roe', 'dl', '', '', 'english'),
    c('asparagus white', 'dl', '', '', 'english'),
    c('potetlefse', 'stk', '', '', 'norwegian')
    
)

#Add new ingredients to all_weights
createIngredientRow <- function(vector){
  
  row <- tibble(
    'Ingredients' = vector[1],
    'unit_enhet' = vector[2],
    'g' = as.numeric(vector[3]),
    'reference' = vector[4],
    'language' = vector[5]
  )
  
}
new_ingredients <- lapply(temp, createIngredientRow) %>%
  bind_rows() %>%
  #Separate languages
  separate_rows(., c(Ingredients, language), sep = '/')

#Add different herbs
herbs <- tibble(
  'Ingredients' = c('rosemary fresh', 'tarragon fresh', 'Basil fresh/Basilikum', 'Chives fresh',
                    'Parsley fresh/Kruspersille/Persille', 'thyme fresh', 'Coriander fresh/Koriander',
                    'Oregano fresh', 'Chervil fresh/Kjørvel', 'Dill fresh', 'mint fresh', 'sorrel', 'cilantro fresh',
                    'cress fresh/watercress fresh', 'sage fresh')) %>%
  mutate(unit_enhet = 'bunch/dl/neve',
         g = 20,
         reference = case_when(
           !Ingredients %in% c('sorrel', 'cilantro', 'salvie fresh') ~ 'Fersk i Pose, Netfresh/Kolonial',
           TRUE ~ 'As other herbs'),
         language = c('english', 'english', 'english/norwegian', 'english', 'english/norwegian/norwegian', 'english',
                      'english/norwegian', 'english/norwegian', 'english/norwegian', 'english/norwegian', 'english', 'english', 'english',
                      'english', 'english')) %>%
  separate_rows(.,c(Ingredients, language), sep = '/') %>%
  separate_rows(unit_enhet, sep = '/')
#Add sprigs/twigs of herbs
#Amounts taken from thespicetrain
herbs_sprigs <- tibble(
  'Ingredients' = c('rosemary fresh', 'tarragon fresh', 'Basil fresh/Basilikum', 'Chives fresh',
                    'Parsley fresh/Kruspersille/Persille', 'thyme fresh', 'Oregano fresh',
                    'Dill fresh', 'mint fresh', 'sage fresh')) %>%
  mutate(unit_enhet = c('tsp', 'tsp', 'tbsp/tbsp', 'tsp', 'tsp/tsp/tsp',
                        'tsp', 'tsp', 'tsp', 'tsp', 'tbsp'),
         g = c('1', '1', '1/1', '0.375', '1.5/1.5/1.5', '0.125', '1', '1', '1', '1'),
         language = c('english', 'english', 'english/norwegian', 'english',
                      'english/norwegian/norwegian', 'english', 'english/norwegian',
                      'english/norwegian', 'english', 'english')) %>%
  separate_rows(., c(Ingredients, language), sep = '/') %>% 
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
  mutate_at('g', ~as.numeric(.)) %>%
  inner_join(herbs, by = c('Ingredients', 'unit_enhet', 'language')) %>%
  unique() %>%
  
  #clean up
  mutate(unit_enhet = str_replace(unit_enhet, 'dl', 'twig'),
         g = g.x * g.y) %>%
  select(-c(g.x, g.y))
#Dry herbs
herbs_dry <- tibble(
  'Ingredients' = c('oregano dried', 'basil dried', 'thyme dried', 'tarragon dried', 'fenugreek dried', 'sage dried'),
  'g' = c(20.3, 30.4, 29.1, 32.5, 75.1, 13.52)) %>%
  mutate(unit_enhet = 'dl',
         reference = 'FoodData Central',
         language = 'english')
#Per leaf
herbs_leaf <- tibble(
  'Ingredients' = c('Basil fresh', 'sage fresh'),
  'g' = c(0.3, 0.5)) %>%
  mutate(unit_enhet = 'leaf',
         reference = 'The Spice Train',
         language = 'english')

#Add different oils
oils <- tibble(
  'Ingredients' = c('olive oil/olivenolje', 'coconut oil', 'soy oil/soybean oil',
                    'canola oil', 'sunflower oil', 'sesame oil/sesamolje', 'corn oil', 'salad oil',
                    'rapeseed oil', 'truffle oil', 'vegetable oil/vegetabilsk olje', 'sunflower oil'),
  'language' = c('english/norwegian', 'english', 'english/english', 'english', 'english', 'english/norwegian',
                 'english', 'english', 'english', 'english', 'english/norwegian', 'english')) %>%
  mutate(unit_enhet = 'dl',
         'g' = 94.7,
         reference = 'FoodData Central') %>%
  separate_rows(., c(Ingredients, language), sep = '/')

new_ingredients <- full_join(new_ingredients, herbs) %>%
  full_join(herbs_sprigs) %>%
  full_join(herbs_dry) %>%
  full_join(herbs_leaf) %>%
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
  mutate(ID = ID + 650) %>% #can't use max(ID) from all_weights as it contains NA
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
  
  #Whole foodgroups not needed
  filter(str_detect(Foodgroup,
                    regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter|cakes|candy|Desert|Dressing|hjemmelaget|homemade|Grain breakfast cereal|knekkebrød|biscuits|pølser|cold cuts|pålegg|Fish spread|Fish products|snacks|candy|Meat products')) |
           
           #Use raw eggs as the standard for egg-size, bread semi coarse as standard for bread
           ((str_detect(Ingredients, 'egg|Egg') & str_detect(Ingredients, 'store|large|små|small|mellomstore|medium|pan-fried|stekt|rambled'))) |
           
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
                             'gammelost', 'traditional norwegian cheese, matured, gammelost', 'gomme', 'traditional norwegian cheese, gomme',
                             'bread, white, spirally shaped', 'loff, spiral', 'grapes, without seeds', 'beans, green, frozen'
                             )) %>%
  
            #Any ingredient of the above that should be kept
            filter(!Ingredients %in% c('crisp bread', 'flatbread, hard','smoke-cured ham', 'cured ham', 'spekeskinke', 'boiled ham', 'honning', 'sukker, brunt',
                                       'sukker hvitt', 'anchovies, canned', 'anchovy fillets, canned', 'salmon, smoked, dry salted',
                                       'mackerel fillet, in tomato sauce, canned', 'cod roe', 'tuna canned', 'ground meat, raw', 'bread, semi-coarse', 'bread, white',
                                       'cream cracker', 'salami'))
  
#Remove the not needed ingredients, add cloves
all_weights <- all_weights %>%
  filter(!Ingredients %in% various$not_needed$Ingredients) %>%
  add_row(Ingredients = 'cloves',
          unit_enhet = 'stk',
          g = 0.09,
          reference = 'https:\\/\\/forum.norbrygg.no\\/threads\\/traden-for-dumme-sporsmal.23452\\/page-62',
          ID = 700) %>% 
  
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
           str_replace('cream cracker', 'cracker cream') %>%
           str_replace('grapes, with seeds', 'grapes') %>%
           str_replace('anchovies', 'anchovy') %>%
           
           #Change all plural forms to singular
           str_replace('fillets', 'fillet') %>%
           str_replace('beans', 'bean') %>%
           str_replace('peas', 'pea') %>%
           str_replace('seeds', 'seed') %>%
           str_replace('nuts', 'nut') %>%
           str_replace('peppers', 'pepper') %>% 
           str_replace('chops', 'chop') %>%
           str_replace('olives', 'olive') %>%
           str_replace('tomatoes', 'tomato') %>%
           str_replace('purée', 'puree')
  ) %>%
  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'chop') & str_detect(Foodgroup, 'lamb') ~ paste0('lamb ', Ingredients),
    TRUE ~ Ingredients
  )) %>%
  #Remove some duplicates (f.ex both storebought and homemade bread, or where the english and norwegian name is the same)
  select(-Foodgroup) %>% unique()

#Save
saveRDS(all_weights, './porsjoner_vekt_næringsinnhold/all_weights.Rds')

#Use store bought breads and rolls as default

#Create query
ref <- all_weights %>% select(-c(g, unit_enhet, reference)) %>% unique() %>% #Only keep names, not units
  mutate(Ingredients = str_replace_all(Ingredients, ',', '')) %>%
  
  #First two words contain the most important information to identify the ingredients
  mutate(first_word = str_extract(Ingredients, '^\\w+'),
         temp = str_extract(Ingredients, '^\\w+\\s\\w+')) %>%
  mutate(second_word = str_remove(temp, '^\\w+\\s')) %>%
  select(-temp) %>% unique() %>%
  replace_na(list(second_word = '\\')) %>%
  
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
      Ingredients == 'grapess' ~ 'grapes',
      Ingredients == 'soft-ripened cheese (brie, camembert etc)' ~ 'soft ripened cheese',
      Ingredients == 'bean white large canned' ~ 'bean white',
      Ingredients == 'bean kidney canned' ~ 'bean kidney',
      Ingredients == 'bean black canned' ~ 'bean black',
      TRUE ~ first_word),
    
    second_word = case_when(
      Ingredients == 'white or yellow hard to semi-hard cheese' ~ '\\',
      Ingredients == 'chicken with skin' ~ 'whole',
      Ingredients == 'chili pepper red' ~ 'red',
      Ingredients == 'cod traditional Norwegian dish Lutefisk' ~ 'lutefisk',
      str_detect(Ingredients, 'gg noodle') & str_detect(Ingredients, 'cooked') ~ 'cooked',
      str_detect(Ingredients, 'gg noodle') & str_detect(Ingredients, 'uncooked') ~ '\\',
      str_detect(Ingredients, 'capers|kapers') ~ '\\',
      Ingredients == 'lasagne sheets uncooked' ~ 'plate',
      Ingredients == 'oil liquid margarine' ~ '\\',
      Ingredients == 'onion yellow/red' ~ '\\',
      Ingredients == 'pork neck chops' ~ 'chops',
      Ingredients == 'quinoa tørr' ~ '\\',
      Ingredients == 'sugar snap peas' ~ '\\',
      Ingredients == 'mackerel fillet, in tomato sauce, canned' ~ 'tomato',
      Ingredients %in% c('olives black in oil canned', 'oliven, svarte, i olje, hermetisk') ~ 'black',
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
      Ingredients == 'bog blueberries' ~ '\\',
      Ingredients == 'cured leg of mutton' ~ 'cured leg',
      Ingredients == 'lentils dry' ~ '\\',
      Ingredients == 'sugar snap pea' ~ 'sugar snap',
      Ingredients == 'hamburger bun' ~ 'bread',
      Ingredients == 'whole-grain pasta' ~ 'whole grain',
      Ingredients == 'ginger root' ~ '\\',
      Ingredients == 'spinach, raw' ~ '\\',
      Ingredients == 'mushroom common' ~ '\\',
      Ingredients == 'olives black in oil canned' ~ 'black',
      Ingredients == 'bean white large canned' ~ 'canned',
      Ingredients == 'bean white in tomato sauce canned' ~ 'tomato',
      Ingredients == 'bean kidney canned' ~ 'canned',
      Ingredients == 'bean black canned' ~ 'canned',
      TRUE ~ second_word
    )
  ) %>%
  select(-Ingredients) %>% arrange(first_word, second_word)

saveRDS(ref, './porsjoner_vekt_næringsinnhold/food_weight_ref.Rds')

