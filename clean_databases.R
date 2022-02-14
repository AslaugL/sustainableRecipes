devtools::load_all(path = '.')


#Databases used to get weight of ingredients with volume/pieces amounts, 
#the nutrient content and sustainability indicators of each ingredient.

#List to keep various dataframes and keep environment clean
various <- list()

#Weight and portion size database----
#Load raw data helsedir
raw_data <- read_xlsx('./Data/databases/vekt_porsjonsstørrelser_helsedir.xlsx')

#Reformat
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
  filter(!(str_detect(Foodgroup, regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter')) & !str_detect(Ingredients, 'Pancake'))
         ) %>%
  
  #Rename some ingredients
  mutate(Ingredients = Ingredients %>%
           str_replace('Loff, formstekt', 'Loff') %>%
           str_replace('Bread, white, square-shaped', 'Bread, white') %>%
           str_replace('Grapes, with seeds', 'grapes')) %>%
  
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
    str_detect(Ingredients, 'ømme|our') ~ as.integer(10000),
    str_detect(Ingredients, 'crème') ~ as.integer(10001),
    Ingredients %in% c('kesam', 'quark') ~ as.integer(10002),
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
  c('sausage turkey chicken', 'pack', '600', 'PRIOR kylling og kalkun grillpølser', 'english'),
  c('chicken diced', 'dl', '57.14', 'https://www.eatthismuch.com/food/nutrition/chicken-breast,454/', 'english'),
  c('rabbit', 'stk', '2500', 'http://grillogmat.blogspot.com/2013/04/kanin-med-smak-av-rosmarin-og-lime.html', 'english'),
  c('pork hock', 'stk', '1000', 'Fjellgris', 'english'),
  c('grouse breast', 'stk', '100', 'https://ultimateupland.com/skewering-meathunters-the-true-cost-of-a-pound-of-game-bird/', 'english'),
  c('marrow bone', 'stk', '113.4', 'https://grassrunfarms.com/blog/benefits-of-grass-fed-beef-marrow-bones/', 'english'), #Really per serving
  c('smoke-cured ham', 'dl', '113.9', 'FoodData Central', 'english'),
  c('cured ham', 'dl', '113.9', 'FoodData Central', 'english'),
  c('boiled ham', 'dl', '113.9', 'FoodData Central', 'english'),
  
  #Bread
  c('Naan bread', 'stk', '130', 'Santa Maria', 'english'),
  c('crisp bread', 'pack', '520', 'WASA', 'english'),
  
  #Vegetables/plant based
  c('Asparagus', 'bunch', '250', 'Meny', 'english'),
  c('asparagus white', 'dl', '35.93', 'FoodData Central', 'english'),
  c('Asparagus beans', 'dl', '80', 'Assumed similar to other beans in database', 'english'),
  c('corn baby', 'stk', '10', 'BAMA, mais mini', 'english'),
  c('corn baby', 'can', '425', 'Eldorado Meny', 'english'),
  c('bagel', 'stk', '85', 'Meny, Hatting', 'english'),
  c('bay leaf', 'stk', '0.2', 'Yahoo Answers', 'english'),
  c('Bean sprout', 'neve/dl', '97.3', 'FoodData Central', 'english'),
  c('Beans, green, raw', 'neve/dl', '40', 'FoodData Central', 'english'),
  c('olives, black, in oil, canned', 'neve', '50', 'Same as dl', 'english'),
  c('oliven, svarte, i olje, hermetisk', 'neve', '50', 'Same as dl', 'norwegian'),
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
  c('Coconut milk', 'stk', '400', 'Rema 1000, kokosmelk', 'english'),
  c('Coconut milk', 'box', '400', 'Rema 1000, kokosmelk', 'english'),
  c('Coconut milk', 'can', '400', 'Rema 1000, kokosmelk', 'english'),
  c('corn cob', 'stk', '250', 'Grønn&Frisk, Meny', 'english'),
  c('corn kernel', 'dl', '105.7', 'FoodData Central', 'english'),
  c('corn kernel', 'can', '160', 'Green Giant Meny', 'english'),
  c('chapati', 'stk', '50', 'Assume weight is the same as a tortilla', 'english'),
  c('chick pea flour', 'dl', '38.9', 'FoodData Central', 'english'),
  c('Chicory', 'stk', '85', 'CooksInfo.com', 'english'),
  c('Chicory, endive', 'stk', '85', 'CooksInfo.com', 'english'),
  c('chili flake', 'dl', '48.7', 'FoodData Central', 'english'),
  c('chili paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('chili powder', 'dl', '54.1', 'FoodData Central', 'english'),
  c('chili pepper, red', 'dl', '38', 'FoodData Central', 'english'),
  c('Chili sauce/chilisaus', 'dl', '111.6', 'FoodData Central', 'english/norwegian'),
  c('ciabatta', 'stk', '62', 'Eldorado, Steinsovnbakt', 'english'),
  c('cardamom pod', 'stk', '0.14', 'The Spice Guide', 'english'),
  c('cloves', 'stk', '0.07', 'Charles McGuinnes medium', 'english'),
  c('cinnamon', 'dl', '52.6', 'FoodData Central', 'english'),
  c('cinnamon bar', 'stk', '4', 'Google', 'english'),
  c('curry powder', 'dl', '42.6', 'FoodData Central', 'english'),
  c('cumin', 'dl', '40.6', 'FoodData Central', 'english'),
  c('crispi salad', 'stk', '150', 'Meny, Gartner', 'english'),
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
  c('rhubarb', 'twig', '50', 'Assume same as dl', 'english'),
  c('ruccola', 'dl/neve', '8.5', 'FoodData Central', 'english'),
  c('ruccola', 'pack', '65', 'Grønn og frisk', 'english'),
  c('Taco sauce', 'glass', '230', 'Rema1000 and Old El Paso', 'english'),
  c('taco spice mix', 'pack', '28', 'Santa Maria', 'english'),
  c('Tomatoes, canned', 'box', '400', 'Mutti, Eldorado', 'english'),
  c('Tomatoes, canned', 'can', '400', 'Mutti, Eldorado', 'english'),
  c('Tomato Paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('Tomato purée', 'glass', '140', 'Mutti, Eldorado', 'english'),
  c('Tomato purée', 'stk', '200', 'Petti, Kolonial', 'english'),
  c('tomato salsa', 'dl', '101.4', 'FoodData Central', 'english'),
  c('tomato salsa', 'glass', '240', 'Kolonial Supermat', 'english'),
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
  c('peas, frozen', 'neve', '60', 'As dl', 'english'),
  c('sugar snap peas', 'dl/neve', '26.95', 'FoodData Central', 'english'),
  c('lemongrass/sitrongressrot', 'stk', '50', 'Meny', 'english/norwegian'),
  c('Chick peas, canned', 'box', '290', 'Øko kolonial', 'english'),
  c('cucumber, pickled', 'stk', '70', 'Produce converter Howmuchisin', 'english'),
  c('chunky salsa', 'glass', '230', 'Santa Maria', 'english'),
  c('orange zest', 'dl', '40.58', 'FoodData Central', 'english'),
  c('lemon zest', 'dl', '40.58', 'FoodData Central', 'english'),
  c('lime zest', 'dl', '40.58', 'FoodData Central', 'english'), #Assume the same as orange and lemon
  c('grapes', 'bunch', '500', 'Kolonial', 'english'),
  c('grapes', 'neve', '70', 'As dl', 'english'),
  c('nutmeg', 'stk', '7.5', 'Wikipedia', 'english'),
  c('celery', 'stk', '400', 'Meny', 'english'),
  c('cashew nuts', 'stk', '1.6', 'https://www.verywellfit.com/cashew-nutrition-facts-4586608', 'english'),
  c('rice parboiled', 'pack', '400', 'Toro', 'english'),
  c('broccolini', 'pack', '200', 'Kolonial', 'english'),
  c('lingonberry jam', 'stk', '200', 'Løiten Gourmet', 'english'), #Used in the online recipe cart
  c('lentils, canned', 'box', '180', 'GoEco', 'english'),
  c('swede', 'slice', '55', 'same as dl', 'english'),
  c('squash, zucchini', 'pack', '250', 'https://www.shoprite.co.za/All-Departments/Food/Fresh-Food/Fresh-Vegetables/Courgettes%2C-Aubergines-and-Squash/Mixed-Patty-Pans-Pack-250g/p/10145280EA', 'english'),
  c('apricot nectar', 'dl', '104.82', 'FoodData Central', 'english'),
  c('apricot preserve', 'dl', '135.26', 'FoodData Central', 'english'),
  c('flaxseed meal', 'dl', '439.48', 'FoodData Central', 'english'),
  c('chia seed', 'dl', '101.44', 'FoodData Central', 'english'),
  c('hemp seed', 'dl', '67.63', 'FoodData Central', 'english'),
  c('onion powder', 'dl', '46.66', 'FoodData Central', 'english'),
  c('poppy seed', 'dl', '59.51', 'FoodData Central', 'english'),
  c('toenjang', 'dl', '202.88', 'FoodData Central', 'english'),
  c('broccoli', 'dl', '47.9', 'FoodData Central', 'english'),
  c('vanilla extract', 'dl', '87.92', 'FoodData Central', 'english'),
  c('kale', 'bunch', '150', 'Meny', 'english'),
  c('spinach, raw', 'neve', '12', 'same as dl', 'english'),
  
  #Seafood
  c('Anchovies, canned', 'box', '55', 'Abba', 'english'),
  c('fish sauce/fiskesaus', 'dl', '121.7', 'FoodData Central', 'english/norwegian'),
  c('crab', 'stk', '500', 'Meny, portion sizes seafood', 'english'),
  c('crab shell', 'stk', '150', 'Meny, Lerøy seafood', 'english'),
  c('crab claw', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
  c('crab claw', 'stk', '400', 'Kolonial', 'english'),
  c('lobster', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
  c('lobster', 'stk', '300', 'Meny Atlantic star', 'english'),
  c('Mackerel fillet, in tomato sauce, canned', 'stk', '170', 'Stabburet', 'english'),
  c('scampi', 'portion', '200', 'Protein rich seafood', 'english'),
  c('shrimp paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('tuna oil', 'box', '185', 'Eldorado', 'english'),
  c('tuna water', 'box', '185', 'Eldorado', 'english'),
  c('tuna oil', 'stk', '185', 'Eldorado', 'english'),
  c('tuna water', 'stk', '185', 'Eldorado', 'english'),
  c('tuna oil', 'can', '185', 'Eldorado', 'english'),
  c('tuna water', 'can', '185', 'Eldorado', 'english'),
  c('squid baby', 'stk', '85.05', 'http://www.clovegarden.com/ingred/seasquidc.html', 'english'),
  c('herring smoked', 'stk', '300', 'DOMSTEIN Sjømat', 'english'),
  c('prawn', 'stk', '17', 'Kostholdsplanleggeren//Matvaretabellen', 'english'),
  c('arctic char', 'stk', '650', 'Mat i Bergen', 'english'),
  
  #Dairy
  c('Blue cheese', 'dl', '47.34', 'FoodData Central', 'english'),
  c('Cream cheese', 'box', '125', 'Tine', 'english'),
  c('Cream cheese', 'dl', '108.2', 'FoodData Central', 'english'),
  c('ricotta salata', 'dl', '104.8', 'FoodData Central', 'english'),
  c('Feta cheese', 'stk', '200', 'Meny, Apetina/Kolios', 'english'),
  c('Parmesan', 'neve', '40', 'Assume about the same as one dl', 'english'),
  c('chevre', 'stk', '190', 'Tine', 'english'),
  c('butter spice', 'pack', '125', 'Tine Grillsmør', 'english'),
  c('soft-ripened cheese (brie, camembert etc)', 'stk', '150', 'Kolonial', 'english'),
  c('soft-ripened cheese (brie, camembert etc)', 'dl', '95', 'FoodData Central', 'english'), #Mascarpone
  
  #Div
  c('broth cube', 'stk', '10', 'TORO klar kjøttbuljong', 'english'),
  c('tabasco', 'dl', '101.4', 'FoodData Central', 'english'),
  c('oyster sauce', 'dl', '304.3', 'FoodData Central', 'english'),
  c('Walnuts', 'stk', '7', 'Google', 'english'),
  c('wine', 'glass', '110', 'vinmonopolet', 'english'),
  c('molasses', 'dl', '142.44', 'FoodData Central', 'english'),
  c('tamarind paste', 'dl', '101.44', 'FoodData Central', 'english'),
  c('horseradish prepared', 'dl', '101.44', 'FoodData Central', 'english'),
  c('mayonnaise', 'dl', '87.92', 'FoodData Central', 'english'),
  c('cream sauce base', 'stk', '50', 'Toro', 'english'), #Used in the online recipe cart
  c('fish soup base', 'stk', '81', 'Toro Bergensk fiskesuppe', 'english'), #Used in online recipe cart
  c('hollandaise base', 'stk', '26', 'Toro', 'english'),
  c('bolognese base', 'stk', '45', 'Toro Kjøttdeigsaus', 'english'), #Used in online recipe cart
  c('nacho', 'pack', '185', 'Olde El Paso', 'english'),
  c('sauce white', 'pack', '38', 'Toro Hvit Saus', 'english'),
  c('sauce teriyaki', 'dl', '94.7', 'FoodData Central', 'english'),
  c('sauce hoisin', 'dl', '111.6', 'FoodData Central', 'english'),
  c('salsa', 'dl', '94.7', 'FoodData Central', 'english'),
  c('dip mix', 'pack', '22', 'Maarud', 'english'),
  c('tandoori spice', 'dl', '106.8', 'FoodData Central', 'english'),
  c('fajita spice', 'dl', '81.2', 'FoodData Central', 'english'),
  c('wasabi', 'dl', '101.4', 'FoodData Central', 'english'),
  c('yeast dry', 'dl', '81.2', 'FoodData Central', 'english'),
  c('anise star', 'stk', '0.7', 'https://www.chowhound.com/post/mass-star-anise-pod-969821', 'english'),
  c('kimchi', 'dl', '63.4', 'FoodData Central', 'english'),
  c('miso', 'dl', '116.2', 'FoodData Central', 'english'),
  c('oil chili', 'dl', '101.44', 'FoodData Central', 'english'),
  c('chocolate', 'dl', '101.44', 'FoodData Central', 'english'),
  
  #MISSING
  c('Einebær/juniper berry', 'stk', '', '', 'norwegian/english'),
  c('Einebær/juniper berry', 'dl', '', '', 'norwegian/english'),
  c('pickled onion', 'dl', '', '', 'english'),
  c('salmon roe', 'dl', '', '', 'english')
  
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
                    'Dill fresh', 'mint fresh', 'sage fresh', 'coriander fresh')) %>%
  mutate(unit_enhet = c('tsp', 'tsp', 'tbsp/tbsp', 'tsp',
                        'tsp/tsp/tsp', 'tsp', 'tsp',
                        'tsp', 'tsp', 'tbsp', 'tsp'),
         g = c('1', '1', '1/1', '0.375',
               '1.5/1.5/1.5', '0.125', '1',
               '1', '1', '1', '0.25'),
         language = c('english', 'english', 'english/norwegian', 'english',
                      'english/norwegian/norwegian', 'english', 'english/norwegian',
                      'english/norwegian', 'english', 'english', 'english')) %>%
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
  mutate(ID = ID + 1000) %>% #can't use max(ID) from all_weights as it contains NA
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
                             'cream cracker', 'salami', 'rice parboiled', 'caramels', 'marshmallows', 'ice cream', 'pancakes'))

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
           str_replace('linseeds, flax seeds', 'flax seed') %>%
           str_replace('sesame paste, tahini', 'tahini') %>%
           
           #Change all plural forms to singular
           str_replace('anchovies', 'anchovy') %>%
           str_replace('fillets', 'fillet') %>%
           str_replace('beans', 'bean') %>%
           str_replace('peas', 'pea') %>%
           str_replace('seeds', 'seed') %>%
           str_replace('nuts', 'nut') %>%
           str_replace('peppers', 'pepper') %>% 
           str_replace('chops', 'chop') %>%
           str_replace('olives', 'olive') %>%
           str_replace('tomatoes', 'tomato') %>%
           str_replace('purée', 'puree') %>%
           str_replace('grapes', 'grape') %>%
           str_replace('plums', 'plum') %>%
           str_replace('apricots', 'apricot') %>%
           str_replace('almonds', 'almond') %>%
           str_replace('prunes', 'prune') %>%
           str_replace('lentils', 'lentil') %>%
           str_replace('raisins', 'raisin') %>%
           str_replace('caramels', 'caramel') %>%
           str_replace('marshmallows', 'marshmallow') %>%
           str_replace('pancakes', 'pancake') %>%
           str_replace('oats', 'oat')
  ) %>%
  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'chop') & str_detect(Foodgroup, 'lamb') ~ paste0('lamb ', Ingredients),
    TRUE ~ Ingredients
  )) %>%
  #Remove some duplicates (f.ex both storebought and homemade bread, or where the english and norwegian name is the same)
  select(-Foodgroup) %>% unique()

#Create one ID each for butter, margarine and ghee
all_weights <- all_weights %>%
  mutate(ID = case_when(
    Ingredients %in% c('smør', 'butter') ~ ID + 2000,
    Ingredients %in% c('margarin', 'margarine') ~ ID + 2001,
    Ingredients == 'ghee' ~ ID + 2002,
    TRUE ~ ID
  ))

#Save
saveRDS(all_weights, './Data/output/all_weights.Rds')

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
      Ingredients == 'soft-ripened cheese (brie camembert etc)' ~ 'soft ripened cheese',
      Ingredients == 'bean white large canned' ~ 'bean white',
      Ingredients == 'bean kidney canned' ~ 'bean kidney',
      Ingredients == 'bean black canned' ~ 'bean black',
      Ingredients == 'cream sauce base' ~ 'cream sauce base',
      Ingredients == 'fish soup base' ~ 'fish soup base',
      Ingredients == 'sausage turkey chicken' ~ 'sausage turkey chicken',
      Ingredients == 'ice cream' ~ 'ice cream',
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
      Ingredients == 'sausage turkey chicken' ~ '\\',
      Ingredients == 'ice cream' ~ '\\',
      TRUE ~ second_word
    )
  ) %>%
  select(-Ingredients) %>% arrange(first_word, second_word)

saveRDS(ref, './Data/output/food_weight_ref.Rds')

#Nutrient content database----
#List to fill with various items to keep environment clean
various <- list()

#Load raw data helsedir
raw_data <- read_xlsx('./Data/databases/matvaretabellen2020.xlsx')

#Clean it up and use means for items with more than one addition (such as vegetables with both Norwegian and Imported values)----
clean_nutrients <- raw_data %>%
  
  #Remove empty rows
  drop_na(Foodgroup) %>%
  #Remove ref columns
  select(!contains('ref')) %>%
  
  #Rename column and remove unnecessary columns
  rename(food_item = `Food Item`) %>%
  select(FoodID, Foodgroup, food_item) %>%
  
  #Remove unnecessary food items, first turn to lowercase----
mutate(food_item = str_to_lower(food_item),
       Foodgroup = str_to_lower(Foodgroup)) %>%
  #Keep some ingredients
  filter((food_item %in% c('chocolate bar, milk', 'chocolate, white', 'chocolate, cooking, plain, minimum 35 % cocoa', 'chocolate, snickers', 'ice cream, dairy', 'chocolate, dark, 70 % cocoa')) |
           !str_detect(Foodgroup,
                      'dessert|other meat products, prepared|other meats, minced, offal, prepared|egg, prepared|cookies|cod liver oil|homemade|chocolate|instant|cake|breakfast cereals|porridge|pizza')) %>%
  
#Rename some ingredients----
mutate(Ingredients = case_when(
  
  #Beef----
  str_detect(food_item, 'beef') & str_detect(food_item, 'bottom round') ~ 'beef_bottom round',
  str_detect(food_item, 'beef') & str_detect(food_item, 'chuck roll') ~ 'beef_chuck roll',
  str_detect(food_item, 'beef') & str_detect(food_item, 'shoulder') & str_detect(food_item, 'roast') ~ 'beef_shoulder',
  str_detect(food_item, 'beef') & str_detect(food_item, 'brisket') ~ 'beef_brisket',
  str_detect(food_item, 'beef') & str_detect(food_item, 'tenderloin') ~ 'beef_tenderloin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'striploin') ~ 'beef_striploin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'sirloin') & str_detect(food_item, 'butt') ~ 'beef_sirloin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'rib-eye steak, raw') ~ 'beef_rib-eye steak',
  str_detect(food_item, 'beef') & str_detect(food_item, 'liver') ~ 'beef_liver/veal_liver',
  str_detect(food_item, 'beef') & str_detect(food_item, 'minced meat') & str_detect(food_item, '6 %') ~ 'beef minced meat_6',
  food_item == 'beef, minced meat, without salt and water, raw' ~ 'beef_minced meat',
  food_item == 'veal, for roast, raw' ~ 'beef_veal for roast',
  food_item == 'beef, roast of nuckle, raw' ~ 'beef_roast of knuckle',
  food_item == 'veal, chops, raw' ~ 'beef_veal chops',
  
  #lamb----
  food_item == 'lamb, for stewing, raw' ~ 'lamb_stew meat',
  food_item == 'lamb, breast and skirt, with bone, raw' ~ 'lamb_breast skirt',
  food_item == 'lamb, shoulder, for roast, raw' ~ 'lamb_shoulder',
  food_item == 'lamb, for mutton and cabbage stew (fårikål), raw' ~ 'lamb_cabbage stew meat',
  food_item == 'lamb, chops, with fat, raw' ~ 'lamb_chop',
  food_item == 'lamb, leg, for roast, raw' ~ 'lamb_leg roast',
  food_item == 'lamb, leg, cured, dried, smoked' ~ 'lamb_leg smoked',
  food_item == 'lamb, rib, cured, dried, smoked, raw' ~ 'lamb_cured rib',
  food_item == 'lamb, chops, cutlet, hind saddle, lean, fat trimmed, raw' ~ 'lamb_hind saddle',
  
  #Pork----
  food_item == 'pork, belly, with rind, raw' ~ 'pork_belly',
  food_item == 'pork, chops, loin with bones, raw' ~ 'pork_chop',
  food_item == 'pork, ham, boneless, with fat, for roast, without rind, raw' ~ 'pork_ham roast',
  food_item == 'pork, rib with loin, raw' ~ 'pork_rib roast',
  food_item == 'pork, hocks, raw' ~ 'pork_hock',
  food_item == 'pork, inside round, raw' ~ 'pork_inside round',
  food_item == 'pork, neck chops, raw' ~ 'pork_neck chop',
  food_item == 'liver, pork, raw' ~ 'pork_liver',
  food_item == 'pork, minced meat, max 23 % fat, raw' ~ 'pork_minced meat',
  food_item == 'pork, bacon, with rind, raw' ~ 'bacon',
  food_item == 'pork, shoulder, with fat, for roast, raw' ~ 'pork_shoulder',
  food_item == 'pork, grillbones, spare ribs, raw' ~ 'pork_spare rib',
  food_item == 'pork, tenderloin, raw' ~ 'pork_tenderloin',
  food_item == 'pork, trimmed fat, raw' ~ 'pork_lard',
  food_item == 'ham, cured' ~ 'ham_cured',
  food_item == 'ham, smoke-cured' ~ 'ham_smoked',
  food_item == 'ham, boiled' ~ 'ham',
  food_item == 'sausage, salami' ~ 'salami',
  food_item == 'sausage, chorizo' ~ 'sausage_chorizo',
  food_item == 'sausage, swedish, falukorv' ~ 'sausage_vossa', #Similar in nutrients
  food_item == 'sausage, meat, gilde' ~ 'sausage', #Standard
  
  #Poultry----
  food_item == 'chicken, leg (thigh and drumstick), with skin, raw' ~ 'chicken_thigh',
  food_item == 'chicken, with skin, raw' ~ 'chicken_whole',
  food_item == 'chicken, fillet, without skin, raw' ~ 'chicken_breast',
  food_item == 'chicken, drumstick, with skin, raw' ~ 'chicken_drumstick',
  food_item == 'chicken, minced meat, raw' ~ 'chicken_minced meat',
  food_item == 'egg white' ~ 'egg_white',
  food_item == 'egg yolk' ~ 'egg_yolk',
  food_item == 'sausage, grill, turkey and chicken' ~ 'sausage_turkey chicken',
  food_item == 'duck, meat and skin, raw' ~ 'duck',
  food_item == 'hen, fillet, raw' ~ 'hen_fillet',
  food_item == 'turkey, breast, without skin, raw' ~ 'turkey_breast',
  food_item == 'ham, turkey, smoked' ~ 'turkey_ham',
  food_item == 'turkey, meat and skin, raw' ~ 'turkey_meat',
  food_item == 'grouse, breast, without skin, raw' ~ 'grouse_breast',
  
  #Game meat----
  food_item == 'moose, roasting, raw' ~ 'elk moose',
  food_item == 'reindeer, roasting, raw' ~ 'reindeer',
  food_item == 'roe deer, meat, raw' ~ 'roe deer',
  
  #Seafood----
  food_item == 'anchovies, canned' ~ 'anchovy_canned',
  food_item == 'anchovy fillets, canned' ~ 'anchovy_fillet',
  food_item == 'angler fish, raw' ~ 'anglerfish',
  food_item == 'alaska pollock, raw' ~ 'pollock',
  food_item == 'halibut, atlantic, unspecified, raw' ~ 'halibut',
  food_item == 'herring, summer, raw' ~ 'herring',
  food_item == 'king prawns, raw' ~ 'prawn',
  food_item == 'crab, boiled' ~ 'crab',
  food_item == 'cod, split, salted and dried' ~ 'cod_clipfish',
  food_item == 'stockfish, dried fish' ~ 'cod_dried',
  food_item == 'fish, alkaline cured, dried, lutefisk' ~ 'lutefisk',
  food_item == 'crab, boiled' ~ 'crab',
  food_item == 'salmon fillet, dry salted, with sugar and spices' ~ 'trout_cured', #Similar nutritional profile
  food_item == 'lobster, boiled' ~ 'lobster',
  food_item == 'mackerel, may-june, raw' ~ 'mackerel',
  food_item == 'mussel, blue, raw' ~ 'mussel',
  food_item == 'nori, seaweed, dried' ~ 'nori_seaweed',
  food_item == 'salmon, farmed, raw' ~ 'salmon',
  food_item == 'salmon, smoked' ~ 'salmon_smoked',
  food_item == 'sprat, raw' ~ 'sardine',
  food_item == 'scallion, spring onion, raw' ~ 'scallion',
  food_item == 'oyster, common, raw' ~ 'oyster',
  food_item == 'shrimps, in brine, drained' ~ 'shrimp_in brine',
  food_item == 'shrimps, northern, boiled' ~ 'shrimp',
  food_item == 'trout, farmed, raw' ~ 'trout',
  food_item == 'tuna in water, drained, canned' ~ 'tuna_in water canned',
  food_item == 'tuna, in oil, canned' ~ 'tuna_in oil canned',
  food_item == 'halibut, atlantic' ~ 'halibut',
  food_item == 'mackerel fillet, in tomato sauce, 60 % mackerel, canned' ~ 'mackerel_tomato canned',
  food_item == 'salmon, smoked' ~ 'salmon_smoked',
  
  #Herbs and spices----
  food_item == 'anise seeds' ~ 'anise',
  food_item == 'bay leaf, dried' ~ 'bay_leaf',
  food_item == 'pepper, cayenne, red' ~ 'pepper_cayenne',
  food_item == 'cardamom, ground' ~ 'cardamom',
  food_item == 'cinnamon, ground' ~ 'cinnamon',
  food_item == 'ginger, root, raw' ~ 'ginger',
  food_item == 'ginger, ground' ~ 'ginger_dried',
  food_item == 'water-cress, raw' ~ 'cress',
  food_item == 'nutmeg, ground' ~ 'nutmeg',
  food_item == 'sweet pepper, paprika, powder' ~ 'paprika_powder',
  food_item == 'parsley, herb, norwegian, raw' ~ 'parsley_fresh',
  food_item %in% c('basil, dried', 'parsley, dried', 'rosemary, dried', 'tarragon, dried',
                   'thyme, dried', 'pepper, white', 'pepper, black', 'basil, dried', 'basil, fresh',
                   'oregano, dried') ~ str_replace(food_item, ', ', '_'),
  food_item %in% c('coriander, raw', 'dill, raw', 'rosemary, raw', 'thyme, raw') ~ str_replace(food_item, ', raw', '_fresh'),
  food_item == 'saffron, dried' ~ 'saffron',
  food_item == 'turmeric, ground' ~ 'turmeric',
  food_item %in% c('coriander seeds', 'fennel seeds', 'mustard seeds', 'caraway seeds') ~ str_replace(food_item, ' seeds', '_seed'),
  food_item == 'chili powder' ~ 'chili_powder',
  food_item == 'curry powder' ~ 'curry_powder',
  
  #Fruit and vegetables----
  FoodID == '06.114' ~ 'sprout_alfalfa',
  food_item == 'apricots, dried' ~ 'apricot_dried',
  food_item == 'beans, green, french, raw' ~ 'bean_green',
  food_item == 'beetroot, norwegian, raw' ~ 'beetroot',
  food_item == 'blackcurrants, raw' ~ 'currant_black',
  food_item == 'brussel sprouts, norwegian, raw' ~ 'brussel_sprout',
  food_item == 'cabbage, white, raw' ~ 'cabbage', #Standard
  food_item == 'cabbage, pak-choi, bok choy, raw' ~ 'cabbage_bok choi',
  food_item == 'cabbage, red, raw' ~ 'cabbage_red',
  food_item == 'cabbage, chinese, norwegian, raw' ~ 'cabbage_chinese',
  food_item == 'cauliflower, norwegian, raw' ~ 'cauliflower',
  food_item == 'celariac root, norwegian, raw' ~ 'celariac_root',
  food_item == 'celery stalk or stem, norwegian, raw' ~ 'celery_stalk',
  food_item == 'chicory, raddichio, raw' ~ 'chicory_red',
  food_item == 'chicory, endive, raw' ~ 'chicory_white',
  food_item == 'pepper, chili, green, raw' ~ 'chili pepper_green',
  food_item == 'pepper, chili, red, raw' ~ 'chili pepper_red',
  food_item == 'jalapeño, raw' ~ 'chili pepper_jalapeno',
  food_item == 'aubergine, raw' ~ 'eggplant',
  food_item == 'horse-radish, raw' ~ 'horseradish',
  food_item == 'kiwi fruit, raw' ~ 'kiwi',
  food_item == 'kohlrabi, raw' ~ 'swede',
  food_item == 'baby corn, canned' ~ 'corn_baby',
  food_item == 'sweet corn, norwegian, raw' ~ 'corn_cob',
  food_item == 'cucumber, norwegian, raw' ~ 'cucumber',
  food_item == 'cucumber, pickled' ~ 'cucumber_pickled',
  food_item == 'figs, dried' ~ 'fig_dried',
  food_item == 'tomato purée' ~ 'tomato_puree',
  food_item == 'tomato, small, cherry, imported, raw' ~ 'cherry_tomato',
  food_item == 'beans, white, in tomato sauce, canned' ~ 'bean_tomato', #White beans
  food_item == 'tomatoes, sun-dried' ~ 'tomato_sun dried',
  food_item == 'tomato, canned' ~ 'tomato_canned',
  food_item == 'leek, norwegian, raw' ~ 'leek',
  food_item == 'lemon juice, bottled' ~ 'lemon_juice',
  food_item == 'lemon peel' ~ 'lemon_zest',
  food_item == 'leaf beet, mangold, raw' ~ 'mangold',
  food_item == 'olives, black, in oil, canned' ~ 'olive_black',
  food_item == 'olives, green, pickled' ~ 'olive_green', #Standard
  food_item == 'onion, norwegian, raw' ~ 'onion',
  food_item == 'orange peel, raw' ~ 'orange_zest',
  food_item == 'parsley root, norwegian, raw' ~ 'parsley_root',
  food_item == 'peas, sugar-snap, norwegian, raw' ~ 'pea_sugar snap',
  food_item == 'peas, frozen' ~ 'pea',
  food_item == 'pineapple, canned, in natural juice' ~ 'pineapple_canned',
  food_item == 'potato flatbread, soft, lompe' ~ 'potato flatbread lompe',
  food_item == 'potatoes, storage, raw' ~ 'potato',
  food_item == 'prunes' ~ 'prune',
  food_item == 'radish, norwegian, raw' ~ 'radish',
  food_item == 'raisins' ~ 'raisin',
  food_item == 'salad, rocket, raw' ~ 'salad_rocket',
  food_item == 'romaine lettuce' ~ 'salad_romaine',
  food_item == 'lettuce leaves, norwegian, raw' ~ 'salad_lettuce',
  food_item == 'squash, zucchini, raw' ~ 'summer squash_zucchini',
  food_item == 'sweet pepper, red, raw' ~ 'sweet pepper_red',
  food_item == 'sweet pepper, yellow/orange, raw' ~ 'sweet pepper_yellow',
  food_item == 'sweet pepper, green, raw' ~ 'sweet pepper_green',
  food_item == 'turnip, norwegian, raw' ~ 'turnip',
  food_item == 'water chestnut, raw' ~ 'chestnut_water',
  food_item == 'melon, water, raw' ~ 'watermelon',
  food_item == 'pumpkin, raw' ~ 'winter squash_pumpkin',
  food_item %in% c('apple juice', 'pineapple juice', 'cranberry juice', 'grape juice',
                   'grapefruit juice', 'orange juice', 'tomato juice', 'tomato ketchup') ~ str_replace(food_item, ' ', '_'),
  food_item == 'sweet corn, canned' ~ 'sweet corn_canned',
  food_item == 'grapes, unspecified, raw' ~ 'grape',
  food_item == 'peaches, canned, in syrup' ~ 'peach_canned',
  
  #Grains----
  food_item == 'almonds' ~ 'almond',
  food_item == 'pearled barley' ~ 'pearl barley',
  food_item == 'broad beans, uncooked' ~ 'bean_broad',
  food_item == 'beans, black, canned'  ~ 'bean_black canned',
  food_item == 'beans, red (kidney), canned' ~ 'bean_kidney canned',
  food_item == 'beans, red (kidney), uncooked' ~ 'bean_kidney',
  food_item == 'mung beans, sprouted, raw' ~ 'bean_sprout', #Standard
  food_item == 'beans, white, large, canned' ~ 'bean_white canned',
  food_item == 'beans, white, uncooked' ~ 'bean_white',
  food_item == 'beans, soya, uncooked' ~ 'bean_soya',
  food_item == 'flatbread, hard' ~ 'bread flat hard',
  food_item == 'bulgur, uncooked' ~ 'bulgur_wheat',
  food_item == 'cashew nuts, salted' ~ 'cashew nut salt',
  food_item == 'peanuts, raw' ~ 'peanut', #These seem to be shelled
  food_item == 'peanuts, roasted, salted' ~ 'peanut_salt',
  food_item == 'peas, chick peas, uncooked' ~ 'chick pea',
  food_item == 'peas, chick peas, canned' ~ 'chick pea_canned',
  food_item == 'corn starch' ~ 'corn_starch',
  food_item == 'cornmeal, polenta' ~ 'corn flour_polenta',
  food_item == 'couscous, uncooked' ~ 'couscous',
  food_item == 'cracker, cream cracker' ~ 'cracker_cream',
  food_item == 'crisp bread, wholemeal flour, rye, husman' ~ 'crisp bread_coarse',
  food_item == 'noodles, with egg, uncooked' ~ 'noodle_egg',
  food_item == 'pasta, plain, macaroni, spaghetti etc., uncooked' ~ 'pasta',
  food_item == 'pasta, whole-grain, uncooked' ~ 'pasta_whole grain',
  food_item == 'rice, jasmin, uncooked' ~ 'rice_jasmin',
  food_item == 'lentils, green and brown, uncooked' ~ 'lentil_green',
  food_item == 'lentils, red/pink, uncooked' ~ 'lentil_red',
  food_item == 'lentils, green, canned' ~ 'lentil_canned',
  food_item == 'squash seeds, pumpkin seeds' ~ 'pumpkin_seed',
  food_item == 'quinoa, white, uncooked' ~ 'quinoa',
  food_item == 'noodles, rice, uncooked' ~ 'noodle_rice',
  food_item == 'rice, basmati, uncooked' ~ 'rice_basmati',
  food_item == 'rice, brown, long-grain, uncooked' ~ 'rice brown long grain',
  food_item == 'rice, arborio, risotto rice, uncooked' ~ 'rice_risotto',
  food_item == 'rice, white, long-grain, uncooked' ~ 'rice white long grain',
  food_item == 'rice, white, pre-boiled, uncooked' ~ 'rice parboiled',
  food_item == 'rolls, white, industrially made' ~ 'rolls white',
  food_item == 'sesame seeds, without shell' ~ 'sesame_seed',
  food_item == 'tortilla chips' ~ 'nacho', 
  food_item == 'wheat flour, 80 % extraction' ~ 'wheat flour',
  food_item == 'wheat flour, wholemeal' ~ 'wheat flour_wholemeal',
  food_item == 'rye flour' ~ 'wheat flour_rye',
  food_item == 'rye flour, wholemeal' ~ 'wheat flour rye_wholemeal',
  food_item == 'semolina, wheat meal' ~ 'wheat flour_semolina',
  food_item %in% c('hamburger bun', 'peanut butter', 'potato starch') ~ str_replace(food_item, ' ', '_'),
  food_item == 'oatmeal' ~ 'oatmeal',
  food_item == 'rolled oats' ~ 'oat_rolled',
  food_item == 'sunflower seeds' ~ 'sunflower_seed',
  food_item == 'linseeds, flax seeds, crushed' ~ 'flax_seed',
  food_item == 'pecan nuts' ~ 'pecan_nut',
  
  str_detect(food_item, 'bread, semi-coarse') & str_detect(food_item, '25-50') & str_detect(food_item, 'industrially made') ~ 'bread',
  str_detect(food_item, 'bread, white') & str_detect(food_item, '0-25') & str_detect(food_item, 'industrially made') & !str_detect(food_item, 'spiral|square') ~ 'bread_white',
  str_detect(food_item, 'bread, coarse') & str_detect(food_item, '50-75') & str_detect(food_item, 'industrially made') ~ 'bread_coarse',
  food_item == 'rolls, white, industrially made' ~ 'roll_white',
  food_item == 'tortilla, wheat flour' ~ 'tortilla',
  
  #oils----
  food_item == 'oil, peanut' ~ 'peanut_oil',
  food_item == 'mayonnaise, full fat, 80 % fat' ~ 'mayonnaise',
  food_item == 'oil, olive, extra virgin' ~ 'olive_oil',
  food_item == 'oil, rapeseed' ~ 'rapeseed_oil',
  food_item == 'oil, sesame' ~ 'sesame_oil',
  food_item == 'oil, soy' ~ 'soybean_oil',
  food_item == 'oil, sunflower' ~ 'sunflower_oil',
  food_item == 'oil, rapeseed, cold pressed, odelia' ~ 'vegetable_oil', #Standard
  food_item == 'oil, walnut' ~ 'walnut_oil',
  
  #Dairy and substitutes----
  food_item == 'butter' ~ 'butter',
  food_item == 'butter, unsalted' ~ 'butter_unsalted',
  food_item == 'cheese, blue mold, norzola' ~ 'norzola_blue cheese',
  food_item == 'cheese, blue mold, normanna' ~ 'normanna_blue cheese',
  food_item == 'cheese, blue mold, gorgonzola' ~ 'gorgonzola_blue cheese',
  food_item == 'cheese, blue mold, roquefort' ~ 'roquefort_blue cheese',
  food_item == 'cheese, ripened, brie' ~ 'brie',
  food_item == 'cheese, whey, goat milk' ~ 'goat cheese brown',
  food_item == 'cheese, ripened, camembert' ~ 'camembert',
  food_item == 'cheese, hard, cheddar' ~ 'cheddar',
  food_item == 'cottage cheese' ~ 'cottage cheese',
  food_item == 'cream cheese, plain' ~ 'cream cheese',
  food_item == 'cheese, goat milk, feta' ~ 'feta_cheese',
  food_item == 'goat cheese, chevre, naturell' ~ 'chevre',
  food_item == 'goat cheese, hard, white, balsfjord' ~ 'hard goat cheese_balsfjord',
  food_item == 'goat cheese, hard, white, kvitlin' ~ 'hard goat cheese_kvitlin',
  food_item == 'cream cheese, goat milk, snøfrisk' ~ 'snøfrisk_goat cream cheese',
  food_item == 'halloumi, cheese' ~ 'halloumi_cheese',
  food_item == 'cheese, hard, jarlsberg' ~ 'jarlsberg',
  food_item == 'cheese, mascarpone' ~ 'mascarpone',
  food_item == 'cheese, mozarella' ~ 'mozzarella',
  food_item == 'cheese, hard, norvegia' ~ 'norvegia',
  food_item == 'cheese, hard, parmesan' ~ 'parmesan',
  food_item == 'cheese, semihard, port salut' ~ 'port salut',
  food_item == 'cheese, ricotta' ~ 'ricotta salata',
  food_item == 'cheese, hard, sveitser' ~ 'swiss_cheese',
  food_item == 'cheese, white, unspecified' ~ 'semi-hard to hard cheese',
  food_item == 'cream, household, 18 % fat' ~ 'cream household_18',
  food_item == 'cream, whipping, 37 % fat' ~ 'cream whipped_37',
  food_item == 'cream, sour, 35 % fat, crème fraîche' ~ 'sour cream_35/crème fraîche_35',
  food_item == 'cream, sour, low-fat, 18 % fat' ~ 'sour cream_18/crème fraîche_18',
  food_item == 'cream, sour, extra low-fat, 10 % fat' ~ 'sour cream_10/crème fraîche_10',
  food_item == 'milk, cultured, whole, kefir' ~ 'kefir',
  food_item == 'margarine, hard' ~ 'margarine',
  food_item == 'milk, skimmed, tine' ~ 'milk_0.1',
  food_item == 'milk, semi-skimmed, unspecified' ~ 'milk_1',
  food_item == 'milk, whole, unspecified' ~ 'whole milk_3.5',
  food_item == 'coconut milk, canned' ~ 'milk_coconut',
  food_item == 'quark, 1 % fat' ~ 'quark_1',
  food_item == 'quark, 7 % fat' ~ 'quark_7',
  food_item == 'milk, cultured, skimmed, skummet kulturmelk' ~ 'buttermilk', #Similar in nutrient content
  food_item == 'yoghurt, whole milk, plain' ~ 'yoghurt',
  food_item == 'milk, condensed, sweetened' ~ 'milk evaporated',
  food_item == 'soy beverage, unsweetened' ~ 'milk_soy',
  food_item == 'oat beverage, with calcium and vitamins' ~ 'dairy imitate_oatmilk',
  food_item == 'ice cream, dairy' ~ 'ice cream',
  
  #Mushrooms----
  food_item == 'mushroom, chantherelle, raw' ~ 'mushroom_chanterelle',
  food_item == 'mushroom, common, norwegian, raw' ~ 'mushroom',
  food_item == 'mushroom, oyster, raw' ~ 'mushroom_oyster',
  food_item == 'mushroom, portabello' ~ 'mushroom_portebello',
  food_item == 'mushroom, shiitake, raw' ~ 'mushroom_shiitake',
  food_item == 'mushroom, common, canned, drained' ~ 'mushroom_canned',
  
  #Div----
  food_item == 'beer, dark, 4,5 - 4,7 vol-% alcohol, bayer and christmas beers' ~ 'beer_dark',
  food_item == 'capers, canned' ~ 'caper',
  food_item == 'spirits, 40 vol-% alcohol' ~ 'spirits 40 vol-% alcohol',
  food_item == 'cider, sweet, 4,5 vol-% alcohol' ~ 'cider',
  food_item == 'sugar, brown' ~ 'sugar_brown',
  food_item == 'sugar, white, caster sugar, cube sugar' ~ 'sugar',
  food_item == 'fortified wines, sweet vermouth, 15 vol-% alcohol' ~ 'fortified wine 15 vol-% alcohol',
  food_item == 'fortified wines, sweet, port, 20 vol-% alcohol' ~ 'fortified wine 20 vol-% alcohol',
  food_item == 'sesame paste, tahini' ~ 'tahini',
  food_item == 'vinegar, 7 %' ~ 'vinegar',
  food_item == 'wasabi, root, raw' ~ 'wasabi',
  food_item == 'water, tap' ~ 'water',
  food_item == 'bakers yeast, active, dry' ~ 'yeast_dry',
  food_item == 'bakers yeast, compressed' ~ 'yeast',
  food_item == 'wine, red, unspecified' ~ 'wine_red',
  food_item == 'wine, white, rosé, sparkling, unspecified' ~ 'wine_white',
  food_item == 'bouillon powder' ~ 'broth_cube',
  food_item == 'capers, canned' ~ 'caper',
  food_item == 'cocoa powder' ~ 'cocoa_powder',
  food_item == 'honey' ~ 'honey',
  food_item == 'salt, table' ~ 'salt',
  food_item == 'soy sauce' ~ 'soy_sauce',
  food_item == 'tofu, soy bean curd' ~ 'tofu',
  food_item == 'jam, 45 % berries, 25 % sugar' ~ 'jam',
  food_item == 'chocolate bar, milk' ~ 'chocolate_milk',
  food_item == 'chocolate, white' ~ 'chocolate_white',
  food_item == 'chocolate, cooking, plain, minimum 35 % cocoa' ~ 'chocolate_semi-sweet',
  food_item == 'chocolate, snickers' ~ 'chocolate_candy bar',
  food_item == 'chocolate, dark, 70 % cocoa' ~ 'chocolate_dark', #Not really, but darkest they have
  
  #Keep the unspecified ingredients
  str_detect(food_item, ', unspecified, raw') ~ str_replace(food_item, ', unspecified, raw', ''),
  
  #Remove 'raw' from certain ingredients----
  food_item %in% c('pineapple, raw', 'asparagus, raw', 'avocado, raw', 'banana, raw',
                   'blueberries, raw', 'catfish, raw', 'char, raw',
                   'cherries, raw', 'chives, raw', 'clementine, raw',
                   'coconut, raw', 'cranberries, raw', 'peanuts, raw',
                   'egg, raw', 'fennel, raw', 'garlic, raw', 'grapefruit, raw', 'haddock, raw',
                   'hare, raw', 'kale, raw', 'jerusalem artichoke, raw', 'cranberries, raw',
                   'egg, raw', 'lemon, raw', 'lime, raw', 'lychee, raw', 'orange, raw',
                   'parsnip, raw', 'pomegranate, raw', 'sweet potato, raw', 'rabbit, raw',
                   'raspberries, raw', 'redfish, raw', 'rhubarb, raw', 'scallop, raw',
                   'shallot, raw', 'strawberries, raw', 'spinach, raw', 'squid, raw',
                   'tuna, raw') ~ str_replace(food_item, ', raw', ''),
  
  #Some single food items to keep
  food_item %in% c('ghee') ~ food_item
  
)) %>%
  
  mutate(Ingredients = case_when(
    #Turn seeds and nuts and some fruits into singular form
    str_detect(food_item, 'nuts') ~ str_replace(food_item, 'nuts', 'nut'),
    #str_detect(food_item, 'seeds') ~ str_replace(food_item, 'seeds', 'seed'),
    
    #Some other fruits and vegetable
    Foodgroup %in% c('vegetables, raw and frozen', 'fruit and berries, raw/fresh') & str_detect(food_item, ', raw') & is.na(Ingredients) ~ str_replace(food_item, ', raw', ''),
    Foodgroup %in% c('vegetables, raw and frozen', 'fruit and berries, raw/fresh') & str_detect(food_item, ', ') & is.na(Ingredients) ~ str_replace(food_item, ', ', '_'),
    
    TRUE ~ Ingredients
  )) %>%
  
  mutate(Ingredients = Ingredients %>%
           str_replace('alfalfa seed, sprouted, raw', 'alfalfa_sprout') %>%
           str_replace('plums', 'plum') %>%
           str_replace('apricots', 'apricot') %>%
           str_replace('peanut, roasted, salted', 'peanut_salt') %>%
           str_replace('peanut, raw', 'peanut')) %>%
  #Separate rows with multiple food items
  separate_rows(., Ingredients, sep = '/') %>%
  
  #Remove some resulting errors
  filter(!(food_item %in% c('sweet pepper, yellow/orange, raw') & Ingredients %in% c('orange'))) %>%
  
  rename(ID = FoodID) %>%
  mutate(ID = as.numeric(ID))

#Add missing food items from FoodData Central----
#Nutrients
fromFoodDataCentral_nutrients <- read_csv('./Data/databases/food_nutrient.csv')
fromFoodDataCentral_nutrient_names <- read_csv('./Data/databases/nutrient_incoming_name.csv')
#Food items
fromFoodDataCentral_foods <- read_csv('./Data/databases/food.csv') %>%
  
  #Select missing foods
  filter(description %in% c(
    
    #Grains
    'Tempeh',
    
    #Meat products
    'Beef, variety meats and by-products, tongue, raw', 'Pork, fresh, variety meats and by-products, kidneys, raw',
    'Beef, New Zealand, imported, flank, separable lean and fat, raw',
    
    #Fruit and veg
    'Jams and preserves, apricot', 'Artichokes, (globe or french), raw', 'Plantains, yellow, raw', 'Sauerkraut, canned, solids and liquids',
    "Tomato products, canned, paste, without salt added (Includes foods for USDA's Food Distribution Program)",
    'Lime juice, raw',
    #Sorrel
    'Sourdock, young leaves (Alaska Native)',
    
    #Herbs and spices
    'Tamarind nectar, canned', 'Spices, chervil, dried', 'Spices, allspice, ground', 'Coriander (cilantro) leaves, raw',
    'Spices, cloves, ground', 'Spices, cumin seed', 'Spices, fenugreek seed', 'Lemon grass (citronella), raw',
    'Spearmint, fresh', 'Spearmint, dried', 'Mustard, prepared, yellow', 'Spices, onion powder',
    'Spices, sage, ground', 'Seasoning mix, dry, sazon, coriander & annatto',
    
    #Dairy
    'Cheese, cottage, lowfat, 2% milkfat', 'Cheese spread, pasteurized process, American',
    'Cheese, monterey', 'Cheese, neufchatel', 'Cheese, provolone', 'Cheese, romano',
    'Yogurt, Greek, plain, whole milk',
    
    #Seafood
    'Mollusks, clam, mixed species, raw', 'Fish, grouper, mixed species, raw', 'Fish, sea bass, mixed species, raw',
    
    #Div
    'Seaweed, agar, dried', 'Soup, onion, dry, mix', 'Alcoholic beverage, rice (sake)',
    'Shortening, vegetable, household, composite', 'Pickle relish, sweet', 'Syrups, maple',
    'Sauce, ready-to-serve, pepper, TABASCO', 'Tapioca, pearl, dry', 'Molasses', 'Vital wheat gluten',
    'Horseradish, prepared', 'Oil, coconut', 'Fat, goose')) %>%
  
  #Rename to fit ingredient names
  mutate(description = description %>%
           str_replace('Beef, variety meats and by-products, tongue, raw', 'beef_tongue') %>%
           str_replace('Pork, fresh, variety meats and by-products, kidneys, raw', 'pork_kidney') %>%
           str_replace('Beef, New Zealand, imported, flank, separable lean and fat, raw', 'beef_flank') %>%
           str_replace('Jams and preserves, apricot', 'apricot_jam') %>%
           str_replace('Artichokes, \\(globe or french\\), raw', 'artichoke_heart') %>%
           str_replace('Plantains, yellow, raw', 'plantain') %>%
           str_replace('Sauerkraut, canned, solids and liquids', 'sauerkraut') %>%
           str_replace('Sourdock, young leaves \\(Alaska Native\\)', 'sorrel') %>%
           str_replace('Spices, chervil, dried', 'chervil_dried') %>%
           str_replace('Spices, allspice, ground', 'allspice') %>%
           str_replace('Coriander \\(cilantro\\) leaves, raw', 'coriander_fresh') %>%
           str_replace('Lemon grass \\(citronella\\), raw', 'lemongrass') %>%
           str_replace('Spices, cloves, ground', 'cloves') %>%
           str_replace('Spices, cumin seed', 'cumin') %>%
           str_replace('Spices, fenugreek seed', 'fenugreek_seed') %>%
           str_replace('Spearmint, dried', 'mint_dried') %>%
           str_replace('Spearmint, fresh', 'mint_fresh') %>%
           str_replace('Mustard, prepared, yellow', 'mustard') %>%
           str_replace('Spices, onion powder', 'onion_powder') %>%
           str_replace('Spices, sage, ground', 'sage_dried') %>%
           str_replace('Seasoning mix, dry, sazon, coriander & annatto', 'sazon seasoning') %>%
           str_replace('Cheese, cottage, lowfat, 2% milkfat', 'cottage cheese_low fat') %>%
           str_replace('Cheese spread, pasteurized process, American', 'cheese_american') %>%
           str_replace('Cheese, monterey', 'monterey_cheese') %>%
           str_replace('Cheese, neufchatel', 'cheese_neufchatel') %>%
           str_replace('Cheese, provolone', 'cheese_provolone') %>%
           str_replace('Cheese, romano', 'cheese_romano') %>%
           str_replace('Yogurt, Greek, plain, whole milk', 'yogurt_greek') %>%
           str_replace('Mollusks, clam, mixed species, raw', 'clam') %>%
           str_replace('Fish, grouper, mixed species, raw', 'grouper') %>%
           str_replace('Fish, sea bass, mixed species, raw', 'sea bass') %>%
           str_replace('Seaweed, agar, dried', 'agar') %>%
           str_replace('Soup, onion, dry, mix', 'onion soup mix') %>%
           str_replace('Alcoholic beverage, rice \\(sake\\)', 'sake') %>%
           str_replace('Shortening, vegetable, household, composite', 'shortening') %>%
           str_replace('Pickle relish, sweet', 'sweet green pickle relish') %>%
           str_replace('Syrups, maple', 'syrup_maple') %>%
           str_replace('Sauce, ready-to-serve, pepper, TABASCO', 'tabasco') %>%
           str_replace('Tapioca, pearl, dry', 'tapioca') %>%
           str_replace('Molasses', 'molasses') %>%
           str_replace("Tomato products, canned, paste, without salt added \\(Includes foods for USDA's Food Distribution Program\\)", 'tomato_paste') %>%
           str_replace('Lime juice, raw', 'lime_juice') %>%
           str_replace('Vital wheat gluten', 'gluten') %>%
           str_replace('Horseradish, prepared', 'horseradish_prepared') %>%
           str_replace('Oil, coconut', 'coconut_oil') %>%
           str_replace('Fat, goose', 'goose_fat')
         #'Tamarind nectar, canned'
  ) %>%
  
  #Get nutrient content from db
  #First get nutrient id's
  inner_join(., fromFoodDataCentral_nutrients, by = 'fdc_id') %>%
  #Select columns
  select(description, nutrient_id, fdc_id, amount) %>%
  #Get nutrient names
  inner_join(fromFoodDataCentral_nutrient_names, by = 'nutrient_id') %>%
  
  #Filter out the nutrients of interest
  mutate(name = str_to_lower(name)) %>%
  filter(name %in% c(
    #Energy nutrients
    'calories', 'fat', 'saturated fat', 'monounsaturated fat', 'polyunsaturated fat', 'protein', 'c20:5 ecosapentaenoic',
    'c22:5 docosapentaenoic', 'c22:6 docosahexaenoic', 'carbohydrates', 'starch', 'sucrose', 'lactose', 'galactose',
    'maltose', 'glucose', 'dextrose', 'dietary  fiber', 'alcohol v/v', 
    
    #Minerals
    'sodium', 'calcium', 'iron', 'zinc', 'potassium', 'magnesium', 'selenium', 'copper', 'phosphorus', 'iodine',
    
    #Vitamins
    'vitamin a', 'beta-carotene', 'retinol', 'vitamin d', 'alpha-tocopherol', 'beta-tocopherol', 'delta-tocopherol', 'gamma-tocopherol',
    'thiamin', 'riboflavin', 'niacin', 'vitamin b6', 'folate', 'vitamin b12', 'vitamin c',
    
    #div
    'cholesterol')) %>%
  
  #Turn wide and rename nutrients to fit the names in Matvaretabellen
  #remove unneccesery columns and rename others
  select(-c(nutrient_id, id)) %>%
  
  pivot_wider(.,
              names_from = name,
              values_from = amount) %>%
  
  rename(Ingredients = description,
         Kilocalories = calories,
         Fat = fat,
         SatFa = `saturated fat`,
         MuFa = `monounsaturated fat`,
         PuFa = `polyunsaturated fat`,
         DPA = `c22:5 docosapentaenoic`,
         EPA = `c20:5 ecosapentaenoic`,
         DHA = `c22:6 docosahexaenoic`,
         Protein = protein,
         Carbo = carbohydrates,
         Starch = starch,
         Cholesterol = cholesterol,
         `Dietary fibre` = `dietary  fiber`,
         Alcohol = `alcohol v/v`,
         `Vitamin A` = `vitamin a`,
         `Beta-carotene` = `beta-carotene`,
         Retinol = retinol,
         `Vitamin D` = `vitamin d`,
         Thiamin = thiamin,
         Riboflavin = riboflavin,
         Niacin = niacin,
         `Vitamin B6` = `vitamin b6`,
         Folate = folate,
         `Vitamin B12` = `vitamin b12`,
         `Vitamin C` = `vitamin c`,
         Calcium = calcium,
         Iron = iron,
         Sodium = sodium,
         Potassium = potassium,
         Magnesium = magnesium,
         Zinc = zinc,
         Selenium = selenium,
         Phosphorus = phosphorus,
         #Iodine = iodine,
         Copper = copper
  ) %>%
  
  #Add mono- and di-saccharaides together, and the different types of vitamin E, create a Kilojoules and salt column
  mutate(`Mono+Di` = glucose + galactose + dextrose + maltose + sucrose + lactose,
         Sugar = sucrose,
         `Vitamin E` = `alpha-tocopherol` + `gamma-tocopherol` + `beta-tocopherol` + `delta-tocopherol`,
         Kilojoules = Kilocalories*4.184,
         Salt = Sodium*2.5/1000 #Unit is in mg Salt in Matvaretabellen is in g
  ) %>%
  #Remove the columns
  select(-c(glucose, galactose, dextrose, maltose, sucrose, lactose, `alpha-tocopherol`, `gamma-tocopherol`, `beta-tocopherol`, `delta-tocopherol`)) %>%
  #Add zero for NA
  replace(is.na(.), 0) %>%
  
  #rename ID column
  rename(ID = fdc_id)

#Add to clean_nutrients df, add where the foods are from
clean_nutrients <- bind_rows(clean_nutrients %>% mutate(from = 'Matvaretabellen_FoodID'), fromFoodDataCentral_foods %>% select(ID, Ingredients) %>% mutate(from = 'FoodData Central_fdc_id'))

#Add composite ingredients----
various$component_ingredients_nutrients <- readRDS('./Data/output/composite_ingredients_nutrient_content.Rds') %>%
  #Create an ID column
  group_by(Ingredients) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(ID = ID + 200) %>% mutate(from = 'Composite ingredient not in database')

clean_nutrients <- bind_rows(clean_nutrients, various$component_ingredients_nutrients %>% select(ID, Ingredients, from)) %>%
  #Add a shellfish row
  add_row(ID = 10000, Ingredients = 'shellfish', from = 'Shellfish in Matvaretabellen')

#Create the nutrient content of the shellfish ingredient by taking the mean of the shellfish in the db
various$shellfish <- raw_data %>%
  
  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', 'Water', ':0', ' sum', '2n', '3n', '4n', 'Foodgroup'))) %>%
  #Filter out shellfish
  filter(`Food Item` %in% c('Crab, plain, canned', 'King prawns, raw', 'Lobster, boiled',
                            'Mussel, blue, raw', 'Oyster, common, raw', 'Scallop, raw')) %>%
  #Rename to fit
  rename(
    ID = FoodID,
    EPA = `C20:5n-3 (EPA)`,
    DPA = `C22:5n-3 (DPA)`,
    DHA = `C22:6n-3 (DHA)`,
    Ingredients = `Food Item`) %>%
  #Turn everything to numeric
  pivot_longer(.,
               cols = -c(ID, Ingredients),
               names_to = 'feature',
               values_to = 'value') %>%
  mutate_at(c('ID', 'value'), ~as.numeric(.)) %>%
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  #Add clams
  bind_rows(., fromFoodDataCentral_foods %>% filter(Ingredients == 'clam')) %>%
  
  #Get the mean values o each nutrient
  pivot_longer(.,
               cols = -c(ID, Ingredients),
               names_to = 'feature',
               values_to = 'value') %>%
  group_by(feature) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  #Add name and ID
  mutate(Ingredients = 'shellfish',
         ID = 10000) %>%
  #Turn wide
  pivot_wider(.,
              names_from = 'feature',
              values_from = 'value')

#Create a full nutrient database----
#Nutrients of interest
nutrients_to_use <- raw_data %>%
  
  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', 'Water', ':0', ' sum', '2n', '3n', '4n'))) %>%
  
  #Rename to fit
  rename(
    ID = FoodID,
    EPA = `C20:5n-3 (EPA)`,
    DPA = `C22:5n-3 (DPA)`,
    DHA = `C22:6n-3 (DHA)`) %>%
  
  #Remove columns and rows
  select(-c(`Food Item`, Foodgroup)) %>%
  drop_na(ID) %>%
  drop_na(Kilocalories) %>%
  
  #Turn everything to numeric
  pivot_longer(.,
               cols = -ID,
               names_to = 'feature',
               values_to = 'value') %>%
  mutate_at(c('ID', 'value'), ~as.numeric(.)) %>%
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>% mutate(from = 'Matvaretabellen_FoodID')

#Add the foodDataCentral and composite ingredients
nutrients_to_use <- nutrients_to_use %>%
  bind_rows(., fromFoodDataCentral_foods) %>%
  bind_rows(., various$component_ingredients_nutrients) %>%
  bind_rows(., various$shellfish) %>% 
  full_join(., clean_nutrients, by = 'ID') %>%
  #Give sour cream, crème fraîche and veal liver individual IDs, while keeping the nutrition information rows
  mutate(ID = case_when(
    str_detect(Ingredients.y, 'crème fraîche|veal_liver') ~ ID + 300,
    
    TRUE ~ ID
  )) %>%
  select(-c(food_item, Foodgroup, Ingredients.x, Ingredients.y, from.x)) %>% rename(from = from.y)

saveRDS(nutrients_to_use, './Data/output/nutrients_df.Rds')

#Create a search reference----
reference <- clean_nutrients %>%
  select(ID, Ingredients) %>%
  #Individual ID for crème fraîche and veal_liver, so that they don't share ID's with sour cream and
  #beef liver respectively. Otherwise there will be duplicates when mapping to a recipe by ingredient ID
  mutate(ID = case_when(
    str_detect(Ingredients, 'crème fraîche|veal_liver') ~ ID + 300,
    
    TRUE ~ ID)) %>%
  drop_na(Ingredients) %>%
  
  #Create searchwords
  separate(., col = Ingredients, into = c('first_word', 'second_word'), sep = '_', remove = FALSE) %>%
  replace_na(list(second_word = '\\')) %>%
  
  #Fix some
  mutate(first_word = case_when(
    Ingredients == 'alfalfa seed, sprouted, raw' ~ 'alfalfa',
    Ingredients == 'cashew nut, salted' ~ 'cashew',
    Ingredients == 'cashew nut' ~ 'cashew',
    TRUE ~ first_word),
    
    second_word = case_when(
      Ingredients == 'alfalfa seed, sprouted, raw' ~ 'sprout',
      Ingredients == 'cashew nut, salted' ~ 'salt',
      Ingredients == 'cashew nut' ~ 'nut',
      TRUE ~ second_word)
  ) %>%
  
  #Arrange in lexogographical order
  arrange(first_word, second_word) %>%
  select(-Ingredients)

saveRDS(reference, './Data/output/nutrient_reference.Rds')

#Sustainability indicators from SHARP----
various <- list()
#Read sustainability data and change to fit recipes----
SHARP <- read_csv('./Data/databases/SHARPversion2018.csv') %>%
  
  #Rename to fit with the other datasets
  rename(Ingredients = `Food item`) %>%
  #Remove graphical characters
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  mutate(Ingredients = str_replace_all(Ingredients, '\\(|\\)', ''))

#Ingredients to remove, either because they're duplicates or just not needed
various$sharp_to_remove <- SHARP %>%
  
  #Whole groups of ingredients
  filter(L1 %in% c('Coffee, cocoa, tea and infusions', 'Composite dishes',
                   'Food products for young population', 'Eggs and egg products',
                   'Sugar and similar, confectionery and water-based sweet desserts',
                   'Grains and grain-based products', 'Milk and dairy products',
                   'Alcoholic beverages', 'Seasoning, sauces and condiments',
                   'Water and water-based beverages') |
           str_detect(Ingredients, 'rice|other wine-like fruit drinks|soft')) %>%
  #Remove ingredients in these categories that should be kept
  filter(!Ingredients %in% tolower(c('White sugar', 'Honey', 'Syrups',
                                     
                                     #Cheeses
                                     'Cream cheese', 'Cottage cheese', 'extra hard cheese parmesan, grana type',
                                     'Firm/semi-hard cheese gouda and edam type', 'Hard cheese cheddar, emmental type',
                                     'Cheese, feta', 'Soft - ripened cheese', 'Cheese, chevre frais',  'processed cheese and spreads',
                                     'ricotta', 'mozzarella', 'cheese, pecorino toscano',
                                     #Milk products
                                     'buttermilk', 'coconut milk cocos nucifera liquid', 'cow milk', 'yoghurt, cow milk',
                                     'evaporated milk liquid, unsweetened', 'cream, plain', 'cr�me fraiche and other mild variants of sour cream',
                                     'sour cream, plain', 'milk powder, skimmed', 'milk powder', 'ice cream, milk-based',
                                     
                                     #Egg products
                                     'hen eggs', 'boiled eggs', 'hen egg white', 'hen egg yolk', 'fried eggs',
                                     #Grain/grain replacements products
                                     'rice flour', 'rice drink', 'rice grain', 'rice grain, brown', 'noodle, rice', 'couscous',
                                     'puff pastry', 'biscuits', 'maize flour', 'crisp bread, rye wholemeal', 'wheat bread and rolls',
                                     'breadcrumbs', 'buns', 'wheat bread and rolls, brown or wholemeal', 'wheat flour', 'maize flour',
                                     'wheat wholemeal flour', 'dried pasta', 'pasta wholemeal', 'oat grain', 'bulgur', 'barley grain, pearled',
                                     'oat rolled grains', 'processed oat-based flakes', 'rye flour, light', 'rye flour, wholemeal',
                                     'wheat semolina',
                                     #Alcoholic beverages
                                     'beer', 'wine', 'whisky', 'fortified and liqueur wines', 'cider', 'brandy', 'vodka and vodka-like spirits',
                                     'rum',
                                     #Conditments, sauces and spices
                                     'soy sauce', 'salt', 'mixed herbs and spices', 'curry powder',
                                     'stock cubes or granulate bouillon base', 'mustard and related sauces', 'vinegar', 'vinegar, wine',
                                     'tomato ketchup and related sauces', 'barbecue or steak sauces', 'mayonnaise sauce',
                                     #Grains
                                     'short pastry doughs pate brisee',
                                     
                                     #Chocolate etc
                                     'cocoa powder', 'bitter chocolate', 'milk chocolate', 'white chocolate',
                                     
                                     #Coffee and tea
                                     'coffee ground, roasted',
                                     
                                     #Water
                                     'tap water'))) #All sugars have the same CO2 and land-use footprint, only need one, same with types of cheeses

SHARP <- SHARP %>%
  #Filter ingredients not needed
  filter(!FoodEx2 %in% c(various$sharp_to_remove$FoodEx2,
                         
                         #Potato products
                         "A011L", "A011S", "A03VG", "A0BYS", "A0BYV", "A00ZX", "A011D",
                         "A011E", "A011P", "A011R", "A0BYT", "A005A", "A00ZV",
                         #Pasta products
                         "A007G", "A007M", "A007J", "A007V", "A007P",
                         #Soyabean Sprouts
                         "A00SY",
                         #Dairy
                         "A02YE",
                         #Pork products
                         "A026R",
                         #Pigeon
                         "A01TA",
                         #Corned beef
                         "A0B9G",
                         #Div
                         "A026M",
                         #Sausages/salami
                         "A025S", "A024M", "A026C", "A024G", "A024J", "A024H",
                         "A025G", "A025L", "A024R", "A025Q", "A025B", "A025P",
                         "A026A", "A0EYP", "A025N", "A025E", "A024S", "A026D",
                         "A024Y", "A024Z", "A025V",
                         #Ham
                         "A023H", "A023K", "A022Z", "A023A", "A022S",
                         #Beer
                         "A03MG", "A03MF", "A03ME", "A03MD",
                         #Juice
                         "A03BN"
                         
                         
  )) %>%
  filter(!Ingredients %in% c('bovine and pig, minced meat', 'onion bulbs for fresh consumption',
                             'blue mussel', 'butter and margarine/oil blends', 'margarines and similar',
                             'shrimps, common', 'brown trout', 'atlantic salmon', 'herring, atlantic', 'mackerel, atlantic',
                             'carps', 'halibut, greenland', 'compote of fruit / vegetables',
                             'blended frying oil/fats', 'chickpeas without pods', 'non dairy coffee creamer',
                             'mixed vegetable juice', 'mixed fruit and vegetable juices', 'fruit and vegetable juices and nectars',
                             'peas without pods and similar-', 'garden peas without pods', 'small radishes', 'black radishes',
                             'smooth hounds', 'oranges, sweet', 'mixed dried fruits', 'currants black, red and white',
                             'black cherries', 'black mulberries', 'liquid drink bases including concentrates and home-made preparations',
                             'peanuts fresh seeds',
                             #Rakefish might fit within this category?
                             'marinated / pickled fish')) %>%
  
  #Fix some names and remove duplicate items
  mutate(Ingredients = Ingredients %>%
           
           #Dairy
           str_replace('extra hard cheese parmesan, grana type', 'parmesan/hard cheese') %>%
           str_replace('hard cheese cheddar, emmental type|firm/semi-hard cheese gouda and edam type', 'hard to semi-hard cheese') %>%
           str_replace('soft - ripened cheese', 'soft-ripened cheese') %>%
           str_replace('yoghurt, cow milk', 'yoghurt') %>%
           str_replace('cream, plain', 'cream') %>%
           str_replace('sour cream, plain', 'sour cream') %>%
           str_replace('cr�me fraiche and other mild variants of sour cream', 'crème fraîche') %>%
           str_replace('cow milk', 'milk') %>%
           str_replace('milk powder, skimmed', 'milk powder nonfat') %>%
           str_replace('ice cream, milk-based', 'ice cream') %>%
           str_replace('cheese, pecorino toscano', 'cheese pecorino') %>%
           
           #Plants
           str_replace('aubergines', 'eggplant') %>%
           str_replace('textured soy protein', 'tofu') %>%
           str_replace('barley grain, pearled', 'pearl barley') %>%
           str_replace('potato flour', 'potato starch') %>%
           str_replace('podded pea young pods', 'sugar snap pea') %>%
           str_replace('spring onions', 'scallion') %>%
           str_replace('celeriacs', 'Celariac root') %>%
           str_replace('celeries', 'celery stalk') %>%
           str_replace('common mushrooms', 'mushroom') %>%
           str_replace('coconut oil/fat', 'Coconut oil') %>%
           str_replace('white sugar', 'sugar') %>%
           str_replace('olive oil, virgin or extra-virgin', 'olive oil') %>%
           str_replace('olives, processed', 'olives canned') %>%
           str_replace('table olives ready for consumption', 'olives fresh') %>%
           str_replace('kohlrabies', 'swede') %>%
           str_replace('dried pasta', 'pasta') %>%
           str_replace('beans with pods and similar-', 'bean with pods') %>%
           str_replace('canned or jarred common beans', 'beans canned') %>%
           str_replace('unleavened or flat bread and similar', 'flatbread') %>%
           str_replace('shallots and similar-', 'shallots') %>%
           str_replace('soya bean oil, refined', 'soy oil') %>%
           str_replace('oils', 'oil') %>%
           str_replace('tomato ketchup and related sauces', 'tomato ketchup') %>%
           str_replace('mustard and related sauces', 'mustard') %>%
           str_replace('coconut milk cocos nucifera liquid', 'coconut milk') %>%
           str_replace('palm oil/fat', 'palm oil') %>%
           str_replace('soyabeans for consumption dry', 'soy bean') %>%
           str_replace('canned/jarred vegetables', 'vegetables canned') %>%
           str_replace('pickled/marinated vegetables', 'vegetables, pickled') %>%
           str_replace('florence fennels', 'fennel') %>%
           str_replace('sweet potatoes', 'potato sweet') %>%
           str_replace('red cabbages', 'cabbage red') %>%
           str_replace('head cabbages', 'cabbage') %>% #Default
           str_replace('white cabbage', 'cabbage white') %>%
           str_replace('savoy cabbages', 'cabbage savoy') %>%
           str_replace('chinese cabbages', 'cabbage chinese') %>%
           str_replace('dried vine fruits raisins etc.', 'raisin') %>%
           str_replace('dried prunes', 'prune dried') %>%
           str_replace('dried figs', 'fig dried') %>%
           str_replace('dried fruit', 'fruit dried') %>%
           str_replace('dried dates', 'date dried') %>%
           str_replace('dried bananas', 'banana dried') %>%
           str_replace('dried apricots', 'apricot dried') %>%
           str_replace('dried mushrooms', 'mushroom dried') %>%
           str_replace('dried nuts and related flours and powderes', 'nutflour') %>%
           str_replace('dried vegetables', 'vegetables dried') %>%
           str_replace('jam, sweet cherry', 'sweet cherry jam') %>%
           str_replace('jam, plums', 'plum jam') %>%
           str_replace('jam, strawberries', 'strawberry jam') %>%
           str_replace('jam, apricots', 'apricot jam') %>%
           str_replace('jam, oranges', 'orange jam') %>%
           str_replace('jam of fruit / vegetables', 'jam') %>%
           str_replace('juice, apple', 'apple juice') %>%
           str_replace('juice, lemon', 'lemon juice') %>%
           str_replace('juice, lime', 'lime juice') %>%
           str_replace('juice, orange', 'orange juice') %>%
           str_replace('juice, mango', 'mango juice') %>%
           str_replace('juice, grapefruit', 'grapefruit juice') %>%
           str_replace('juice, grape', 'grape juice') %>%
           str_replace('juice, pineapple', 'pineapple juice') %>%
           str_replace('juice, carrot', 'carrot juice') %>%
           str_replace('juice, elderberry', 'elderberry juice') %>%
           str_replace('parsley roots', 'parsley root') %>%
           str_replace('vegetable fats and oils, edible', 'vegetable oil') %>%
           str_replace('swiss chards', 'chard') %>%
           str_replace('roman rocket and similar-', 'salad rocket') %>% #What other salads are like rockets?
           str_replace('summer squashes', 'squash summer') %>%
           str_replace('kales and similar-', 'kale') %>%
           str_replace('globe artichokes', 'artichoke') %>%
           str_replace('dried prunes', 'prune dried') %>%
           str_replace('granate apples', 'pomegranat') %>%
           str_replace('radishes', 'radish') %>%
           str_replace('litchis', 'lychee') %>%
           str_replace('table grapes', 'grapes') %>%
           str_replace("lamb's lettuces", "lettuce lamb") %>%
           str_replace('barley grain, pearled', 'barley pearl') %>%
           str_replace('courgettes', 'zucchini') %>% #Name used in recipes
           str_replace('sprouts, shoots and similar', 'sprout') %>%
           str_replace('maize flour', 'corn flour') %>%
           str_replace('processed oat-based flakes', 'oatmeal') %>%
           str_replace('oat rolled grains', 'oat rolled') %>%
           str_replace('black cherries', 'cherries black') %>%
           str_replace('cherries and similar-', 'cherries') %>%
           str_replace('sour cherries', 'cherries sour') %>%
           str_replace('jam of fruit / vegetables', 'jam') %>%
           str_replace('fruit juices 100% from named source', 'fruit juice') %>%
           str_replace('canned or jarred pineapple', 'pineapple canned') %>%
           str_replace('canned or jarred peach', 'peach canned') %>%
           str_replace('canned or jarred sweet cherry', 'cherries canned') %>%
           str_replace('wheat semolina', 'wheat flour semolina') %>%
           str_replace('rye flour, light', 'wheat flour rye') %>%
           str_replace('rye flour, wholemeal', 'wheat flour rye wholemeal') %>%
           
           #Meat
           str_replace('beef tallow including processed suet', 'tallow') %>%
           str_replace('bovine, minced meat', 'beef minced meat') %>% #Used in the norwegian recipes
           str_replace('pork lard', 'lard') %>%
           str_replace('bovine tongue', 'beef tongue') %>%
           str_replace('bovine marrowbone', 'marrow bone beef') %>%
           str_replace('pig muscle', 'pork') %>% #Use as standard
           str_replace('pig liver', 'pork liver') %>%
           str_replace('pig kidney', 'pork kidney') %>%
           str_replace('chicken fresh meat', 'chicken') %>%
           str_replace('goose fresh meat', 'goose') %>%
           str_replace('sheep muscle', 'sheep') %>%
           str_replace('salami-type sausage', 'salami') %>%
           str_replace('ham, pork', 'ham') %>%
           
           #Poultry
           str_replace('hen ', '') %>%
           str_replace('eggs', 'egg') %>%
           str_replace('turkey fresh meat', 'turkey') %>%
           str_replace('cooked other poultry meat', 'chicken cooked/turkey cooked') %>%
           str_replace('fried egg', 'omelet') %>% #Only type of fried eggs in the recipe
           
           #Seafood
           str_replace('anglerfish and monkfish', 'anglerfish/monkfish') %>% #Easier to separate
           str_replace('cods, hakes, haddocks', 'cod/hake/haddock/pollock') %>% #Pollock is in the same fish family
           str_replace('canned tunas and similar', 'tuna canned') %>%
           str_replace('canned anchovies', 'anchovies canned') %>%
           str_replace('canned mackerel', 'mackerel canned') %>%
           str_replace('shrimps and prawns', 'shrimp/prawn') %>%
           str_replace('canned/jarred fish', 'fish canned') %>%
           str_replace('scallops, pectens', 'scallops') %>%
           str_replace('edible crab', 'crab') %>%
           str_replace('anchovies', 'anchovy') %>%
           str_replace('smoked salmon', 'salmon smoked') %>%
           str_replace('smoked herring', 'herring smoked') %>%
           
           #Div
           str_replace('barbecue or steak sauces', 'barbeque sauce') %>%
           str_replace(', common|, edible', '') %>%
           str_replace('p�rigord black truffles', 'truffle black') %>%
           str_replace('common mushrooms', 'mushroom common') %>%
           str_replace('canned mushrooms', 'mushroom canned') %>%
           str_replace('dried mushrooms', 'mushroom dried') %>%
           str_replace('hazelnuts', 'nuts hazel') %>%
           str_replace('maize oil', 'corn oil') %>%
           str_replace('short pastry doughs pate brisee', 'pastry dough') %>%
           str_replace('tap water', 'water') %>%
           str_replace('pasta sauce', 'tomato sauce') %>% #pasta sauces are tomato sauces
           str_replace('vodka and vodka-like spirits', 'vodka') %>%
           str_replace('bitter chocolate', 'chocolate dark') %>%
           str_replace('milk chocolate', 'chocolate milk') %>%
           str_replace('white chocolate', 'chocolate white') %>%
           str_replace('traditional margarine', 'margarine') %>%
           str_replace('dairy imitates other than milks', 'dairy imitate')
         
  ) %>%
  
  #Change 'and similar' to 'other'
  mutate(Ingredients = Ingredients %>%
           str_replace('and similar-', 'other') %>%
           str_replace('and similar', 'other')) %>%
  
  #Filter away other unnecessary ingredients
  filter(!(str_detect(Ingredients, 'canned ') |
             str_detect(Ingredients, 'biscuit'))) %>%
  filter(!(str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'dried')))# %>%
  
  #select(-FoodEx2) %>% unique()

#Create a 'shellfish' ingredient in SHARP, using the mean of all the shellfish ingredients and add to SHARP
various$shellfish <- SHARP %>%
  filter(Ingredients %in% c('clams', 'lobster', 'oyster', 'oyster, pacific cupped', 'mussels', 'scallops', 'shrimps/prawns')) %>%
  summarise(`GHGE of 1 kg food as consumed_kgCO2eq` = mean(`GHGE of 1 kg food as consumed_kgCO2eq`),
            `Land use of 1 kg food as consumed_m2/yr` = mean(`Land use of 1 kg food as consumed_m2/yr`)) %>%
  mutate(Ingredients = 'shellfish',
         L1 = 'Fish, seafood, amphibians, reptiles and invertebrates')
SHARP <- full_join(SHARP, various$shellfish)
#Add composite ingredients
various$composite_ingredients_sharp <- readRDS('./Data/output/composite_ingredients_sustainability_markers.Rds')

#Add to SHARP
SHARP <- full_join(SHARP, various$composite_ingredients_sharp)
#Create a half-and-half row, half milk and half cream
SHARP <- SHARP %>% 
  add_row(Ingredients = 'half-and-half',
          L1 = 'Milk and dairy products',
          `GHGE of 1 kg food as consumed_kgCO2eq` = (10.6822437+1.5858702)/2,
          `Land use of 1 kg food as consumed_m2/yr` = (10.4333868+1.4606742)/2) %>%
  
  #Split ingredients that share a row, and remove duplicates created
  separate_rows(Ingredients, sep = '/') %>%
  unique() %>%
  
  #Create unique ID for each item
  mutate(ID = seq_along(Ingredients)) %>%
  #Add a FoodEx2 code for the composite ingredients not in SHARP
  replace_na(list(FoodEx2 = 'Composite ingredient not in SHARP'))
#Save
saveRDS(SHARP %>%
          select(-Ingredients), './Data/output/sharp_db.Rds')

#Create a df with reference names to query ingredient lists----
sharp_ref <- SHARP %>%
  select(ID, Ingredients) %>%
  
  #Remove plural s
  mutate(Ingredients = case_when(
    Ingredients %in% c('apples', 'pineapples', 'avocados', 'apricots',
                       'beetroots', 'breadcrumbs', 'carrots', 'cauliflowers',
                       'buns', 'chards', 'clams', 'coconuts', 'courgettes', 'crabs',
                       'cucumbers', 'figs', 'grapefruits', 'groupers', 'hazelnuts',
                       'herrings', 'kales', 'lemons', 'limes', 'lettuces',
                       'mandarins', 'melons', 'mussels', 'nectarines', 'olives',
                       'onions', 'oranges', 'oysters', 'papayas', 'peanuts', 'pears',
                       'pistachios', 'plums', 'pumpkins', 'rhubarbs', 'salmons',
                       'shrimps', 'syrups', 'trouts', 'hazelnuts', 'walnuts',
                       'scallops', 'sardines', 'blackcurrants', 'redcurrants',
                       'leeks', 'shallots', 'shrimps', 'prawns', 'almonds', 'olives',
                       'grapes', 'sausages', 'lobsters'
    ) ~ str_replace(Ingredients, 's\\b', ''),
    str_detect(Ingredients, 'nuts') ~ str_replace(Ingredients, 'nuts', 'nut'),
    str_detect(Ingredients, 'seeds') ~ str_replace(Ingredients, 'seeds', 'seed'),
    str_detect(Ingredients, 'peppers') ~ str_replace(Ingredients, 'peppers', 'pepper'),
    str_detect(Ingredients, 'lentils') ~ str_replace(Ingredients, 'lentils', 'lentil'),
    str_detect(Ingredients, 'lettuces') ~ str_replace(Ingredients, 'lettuces', 'lettuce'),
    str_detect(Ingredients, 'brussels sprouts') ~ 'brussel sprout',
    str_detect(Ingredients, 'sardine') ~ 'sardine',
    
    TRUE ~ Ingredients)) %>%
  
  #First two words contain the most important information to identify the ingredients
  mutate(Ingredients = str_replace(Ingredients, '_', ' ')) %>% #Composite ingredients has _ between words
  mutate(first_word = str_extract(Ingredients, '^\\w+'),
         temp = str_extract(Ingredients, '^\\w+\\s\\w+')) %>%
  mutate(second_word = str_remove(temp, '^\\w+\\s')) %>%
  select(-temp) %>% unique() %>%
  replace_na(list(second_word = '\\')) %>%
  
  #Clean up some of them that's not right, always have the most generic name in the first column, then a specification in second column if present
  mutate(
    first_word = case_when(
      Ingredients == 'wheat bread and rolls' ~ 'wheat bread and rolls',
      Ingredients == 'wheat bread and rolls, brown or wholemeal' ~ 'wheat bread and rolls',
      Ingredients == 'boiled egg' ~ 'egg',
      Ingredients == 'chickpeas dry' ~ 'chick pea',
      Ingredients == 'chickpea flour' ~ 'chick pea',
      Ingredients %in% c('mung bean sprouts', 'soy bean', 'beans dry and similar-',
                         'borlotti or other common beans dry', 'kidney bean dry seeds',
                         'navy beans dry seeds', 'beans canned', 'french beans with pods') ~ 'bean',
      Ingredients == 'bean with pods' ~ 'bean with pods',
      Ingredients == 'processed cheese and spreads' ~ 'processed cheese and spreads',
      Ingredients == 'cheese, feta' ~ 'feta',
      Ingredients == 'cheese, chevre frais' ~ 'chevre frais',
      Ingredients == 'soft-ripened cheese' ~ 'soft-ripened cheese',
      Ingredients == 'hard cheese' ~ 'hard cheese',
      Ingredients == 'hard to semi-hard cheese' ~ 'hard to semi-hard cheese',
      Ingredients == 'garden peas dry' ~ 'pea',
      Ingredients == 'sugar snap pea' ~ 'pea',
      Ingredients == 'mangoes' ~ 'mango',
      Ingredients == 'peaches' ~ 'peach',
      Ingredients == 'potatoes' ~ 'potato',
      Ingredients == 'romaines' ~ 'lettuce',
      Ingredients == 'spinaches' ~ 'spinach',
      Ingredients == 'sun-dried tomatoes' ~ 'tomato',
      Ingredients == 'tomatoes' ~ 'tomato',
      Ingredients == 'wheat flour' ~ 'wheat flour',
      Ingredients == 'wheat wholemeal flour' ~ 'wheat flour',
      Ingredients == 'beef minced meat' ~ 'beef',
      Ingredients == 'italian-style sausage' ~ 'sausage',
      Ingredients == 'broad beans without pods' ~ 'broad bean',
      Ingredients == 'broad beans dry' ~ 'bean',
      Ingredients == 'vegetable fats and oil' ~ 'oil',
      Ingredients == 'breadcrumb' ~ 'bread',
      Ingredients == 'fortified and liqueur wines' ~ 'fortified wine',
      Ingredients == 'condensed cream of celery soup' ~ 'condensed cream of celery soup',
      Ingredients == 'condensed cream of mushroom soup' ~ 'condensed cream of mushroom soup',
      Ingredients == 'condensed cream of chicken soup' ~ 'condensed cream of chicken soup',
      Ingredients == 'refrigerated buttermilk biscuit dough' ~ 'refrigerated buttermilk biscuit dough',
      Ingredients == 'fish cakes coarse' ~ 'fish cakes coarse',
      Ingredients == 'vegetables and vegetable products' ~ 'vegetables and vegetable products',
      Ingredients == 'shrimp salad' ~ 'shrimp salad',
      Ingredients == 'watercresses' ~ 'cress',
      Ingredients == 'yellow cake mix' ~ 'yellow cake',
      Ingredients == 'chocolate pudding mix' ~ 'chocolate pudding',
      Ingredients == 'german chocolate cake mix' ~ 'german chocolate cake',
      Ingredients == 'milk powder nonfat' ~ 'milk powder',
      Ingredients == 'milk powder' ~ 'milk powder',
      Ingredients == 'marrow bone beef' ~ 'marrow bone',
      Ingredients == 'sweet green pickle relish' ~ 'sweet green pickle relish',
      Ingredients == 'ice cream' ~ 'ice cream',
      Ingredients == 'wheat flour rye' ~ 'wheat flour',
      Ingredients == 'wheat flour rye wholemeal' ~ 'wheat flour',
      Ingredients == 'wheat flour semolina' ~ 'wheat flour',
      Ingredients == 'white bread mix' ~ 'white bread',
      Ingredients == 'coffee ground, roasted' ~ 'coffee ground',
      TRUE ~ first_word),
    
    second_word = case_when(
      Ingredients == 'wheat bread and rolls' ~ 'white',
      Ingredients == 'wheat bread and rolls, brown or wholemeal' ~ 'brown',
      Ingredients == 'boiled egg' ~ 'boiled',
      Ingredients == 'chickpeas dry' ~ '\\',
      Ingredients == 'chickpea flour' ~ 'flour',
      Ingredients == 'mung bean sprouts' ~ 'sprout',
      Ingredients == 'soy bean' ~ 'soy',
      Ingredients == 'beans dry and similar-' ~ '\\',
      Ingredients == 'borlotti or other common beans dry' ~ 'borlotti',
      Ingredients == 'kidney bean dry seeds' ~ 'kidney',
      Ingredients == 'navy beans dry seeds' ~ 'navy',
      Ingredients == 'beans canned' ~ 'canned',
      Ingredients == 'french beans with pods' ~ 'french',
      Ingredients == 'beans with pods' ~ '\\',
      Ingredients == 'processed cheese and spreads' ~ '\\',
      Ingredients == 'cheese, feta' ~ 'cheese',
      Ingredients == 'cheese, chevre frais' ~ '\\',
      Ingredients == 'soft-ripened cheese' ~ '\\',
      Ingredients == 'hard cheese' ~ '\\',
      Ingredients == 'hard to semi-hard cheese' ~ '\\',
      Ingredients == 'garden peas dry' ~ 'garden',
      Ingredients == 'sugar snap pea' ~ 'sugar snap',
      Ingredients == 'romaines' ~ 'romaine',
      Ingredients == 'sun-dried tomatoes' ~ 'sun-dried',
      Ingredients == 'cherry tomatoes' ~ 'tomato',
      Ingredients == 'wheat flour' ~ '\\',
      Ingredients == 'wheat wholemeal flour' ~ 'wholemeal',
      Ingredients == 'beef minced meat' ~ '\\', #Use as standard
      Ingredients == 'hard cheese' ~ '\\',
      Ingredients == 'italian-style sausage' ~ 'italian',
      Ingredients == 'rice grain, brown' ~ 'brown',
      Ingredients == 'sweet peppers' ~ 'pepper',
      Ingredients == 'vegetable juices' ~ 'juice',
      Ingredients == 'broad beans without pods' ~ 'without pods',
      Ingredients == 'broad beans dry' ~ 'broad',
      Ingredients == 'sesame seeds' ~ 'seed',
      Ingredients == 'vegetable fats and oil' ~ 'vegetable',
      Ingredients == 'breadcrumb' ~ 'crumb',
      Ingredients == 'jerusalem artichokes' ~ 'artichoke',
      Ingredients == 'fortified and liqueur wines' ~ '\\',
      Ingredients == 'condensed cream of celery soup' ~ '\\',
      Ingredients == 'condensed cream of mushroom soup' ~ '\\',
      Ingredients == 'condensed cream of chicken soup' ~ '\\',
      Ingredients == 'refrigerated buttermilk biscuit dough' ~ '\\',
      Ingredients == 'fish cakes coarse' ~ '\\',
      Ingredients == 'vegetables and vegetable products' ~ '\\',
      Ingredients == 'sunflower seed oil' ~ 'oil',
      Ingredients == 'vegetables, pickled' ~ 'pickled',
      Ingredients == 'cranberry sauce' ~ '\\',
      Ingredients == 'vinegar, wine' ~ 'wine',
      Ingredients == 'chili sauce sweet' ~ 'sweet',
      Ingredients == 'celery stalk' ~ '\\',
      Ingredients == 'hard cheese' ~ '\\',
      Ingredients == 'kiwi fruits green red yellow' ~ '\\',
      Ingredients == 'parnip roots' ~ '\\',
      Ingredients == 'watercresses' ~ 'water',
      Ingredients == 'rice grain' ~ '\\', #Standard,
      Ingredients == 'yellow cake mix' ~ 'mix',
      Ingredients == 'chocolate pudding mix' ~ 'mix',
      Ingredients == 'german chocolate cake mix' ~ 'mix',
      Ingredients == 'milk powder' ~ '\\',
      Ingredients == 'milk powder nonfat' ~ 'nonfat',
      Ingredients == 'marrow bone beef' ~ 'beef',
      Ingredients == 'sweet green pickle relish' ~ '\\',
      Ingredients == 'ice cream' ~ '\\',
      Ingredients == 'wheat flour rye' ~ 'rye',
      Ingredients == 'wheat flour rye wholemeal' ~ 'rye',
      Ingredients == 'wheat flour semolina' ~ 'semolina',
      Ingredients == 'white bread mix' ~ 'mix',
      Ingredients == 'coffee ground, roasted' ~ '\\',
      TRUE ~ second_word
    )
  ) %>%
  
  filter(!first_word %in% c('juice', 'fresh')) %>% #These may otherwise cause issues when searching
  distinct(., first_word, second_word, .keep_all = TRUE) %>% #Remove duplicates. Due to splitting up rows with '/', certain ingredients occur multiple times
  #Remove unnecessary column
  select(-Ingredients) %>% arrange(first_word, second_word)
#Is the maize flour in SHARP the same as corn starch?

#Save
saveRDS(sharp_ref, './Data/output/sharp_ref.Rds')



