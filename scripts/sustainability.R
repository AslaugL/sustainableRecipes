library(tidyverse)
library(stringi)
library(ggbeeswarm)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data')

#List to keep various objects and not clutter environment
various <- list()

#Recipes with ingredients----
recipes <- list(
  'Norway' = readRDS('./oppskrifter/NOR_clean.Rds') %>%
    unite('original_amounts', c(Amounts, unit_enhet), sep = ' ') %>%
    #Use 'ref' as Ingredient names, more cleaner
    mutate(org_ingredients = Ingredients) %>%
    select(-Ingredients) %>%
    rename(Ingredients = ref) %>%
    mutate(Ingredients = Ingredients %>% str_replace(',', '')),
  'UK' = readRDS('./oppskrifter/UK_clean.Rds')%>%
    unite('original_amounts', c(Amounts, unit_enhet), sep = ' ') %>%
    #Use 'ref' as Ingredient names, more cleaner
    mutate(org_ingredients = Ingredients) %>%
    select(-Ingredients) %>%
    rename(Ingredients = ref) %>%
    mutate(Ingredients = Ingredients %>% str_replace(',', '')),
  'US' = readRDS('./oppskrifter/US_clean.Rds') %>%
    #Rename
    rename(original_amounts = ingredient_amount)
)

#Change some names in recipes to better fit with the SHARP index database
recipes$US <- recipes$US %>%

  #Rename ingredients to fit with SHARP indicators
  mutate(Ingredients = Ingredients %>%
           str_replace('chile pepper', 'chili peppers') %>%
           str_replace('bell pepper|paprika', 'sweet pepper') %>%
           str_replace('all-purpose flour|self-rising flour','Wheat flour') %>%
           str_replace('wild rice', 'rice brown') %>% #Closest match
           str_replace('vermicelli|spaghetti', 'pasta') %>% #SHARP does not distinguish between different types of pasta
           str_replace('limes, zested and juiced', 'lime') %>%
           str_replace('naval oranges, zested and juiced', 'orange') %>%
           str_replace('frozen corn|corn kernels|whole kernel corn, undrained', 'sweet corn canned') %>% #Canned as it is measured in cups and kernel corns are usually bought in cans
           str_replace('Mung bean sprouts', 'bean sprouts') %>% #Common beans to sprout
           str_replace('bone-in ham|ham steaks \\(1/4 inch thick\\)|ham steak', 'pork steak') %>%
           str_replace('cooked lobster meat, diced', 'lobster') %>%
           str_replace('standing rib roast, bone in|Korean-style short ribs \\(beef chuck flanken, cut 1/3 to 1/2 inch thick across bones\\)', 'beef') %>%
           str_replace('whiskey', 'whisky') %>%
           str_replace('\\bale\\b', 'beer') %>%
           str_replace('ketchup', 'tomato ketchup') %>%
           str_replace('lemon, juiced', 'lemon') %>%
           str_replace('capers, with liquid|capers, drained and rinsed', 'capers') %>%
           str_replace('rotisserie chicken, skinned and boned, meat pulled into large chunks', 'cooked chicken') %>%
           str_replace('top round steak, cut into 1/4 inch strips', 'beef top round steak') %>%
           str_replace('bottom round roast', 'beef bottom round roast') %>%
           str_replace('corn tortilla chips', 'nachos')
  ) %>%

  #Conditionals us recipes
  mutate(Ingredients = case_when(
    ingredient_id == '4292' ~ 'celery stalk',
    str_detect(original_amounts, 'can') ~ paste0(Ingredients, ' canned'),
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ 'vinegar, wine',
    str_detect(Ingredients, 'vinegar') & !str_detect(Ingredients, 'wine') ~ 'vinegar',
    str_detect(Ingredients, 'cooked') & str_detect(Ingredients, 'chicken') ~ 'cooked chicken',
    str_detect(Ingredients, 'cooked') & str_detect(Ingredients, 'turkey') ~ 'cooked turkey',
    str_detect(Ingredients, 'lasagna noodle|lasagna plate|linguine pasta|fettuccine') ~ 'pasta',
    str_detect(Ingredients, 'hard-cooked|hard cooked') & str_detect(Ingredients, 'egg') ~ 'boiled egg',
    str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked\\b') ~ 'cooked rice',
    str_detect(Ingredients, 'pasta') & str_detect(Ingredients, '\\bcooked\\b') ~ 'cooked pasta',

    #Cheeses
    str_detect(tolower(Ingredients), 'cheddar|romano|parmigiano-reggiano|parmesan|parmigiano-reggiano') & str_detect(tolower(Ingredients), 'cheese') ~ 'Hard cheese',
    str_detect(tolower(Ingredients), 'havarti|swiss||monterey jack|pepperjack|asiago|mozzarella') & str_detect(tolower(Ingredients), 'cheese') ~ 'hard to semi-hard cheese',
    str_detect(tolower(Ingredients), 'ricotta') & str_detect(tolower(Ingredients), 'cheese') ~ 'Soft-ripened cheese',
    str_detect(tolower(Ingredients), 'american cheese') ~ 'Processed cheese and spreads', #It is a processed cheese product

    TRUE ~ Ingredients
  ))

recipes$Norway <- recipes$Norway %>%

  mutate(Ingredients = Ingredients %>%
           str_replace('meat dough', 'ground meat') %>%
           str_replace('uncooked', '') %>%
           str_replace('loff', 'bread white') %>%
           str_replace('sjalottløk', 'shallot') #Fix in original
           ) %>%

  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(tolower(org_ingredients), 'goat cheese') ~ 'chevre frais', #One is actually brown goat cheese
    str_detect(tolower(org_ingredients), 'pollock') ~ 'pollock',
    (str_detect(tolower(Ingredients), 'paragus|break') & str_detect(tolower(Ingredients), 'bean')) |
      org_ingredients == 'brewed beans' ~ 'bean with pods',
    org_ingredients == 'canned corn' ~ 'sweet corn canned',
    Ingredients %in% c('good and crispy corn', 'corn') ~ 'sweet corn',
    str_detect(tolower(org_ingredients), 'beer') ~ 'beer',
    tolower(org_ingredients) %in% c('manchego, grated', 'halloumi') ~ 'hard to semi-hard cheese', #Although manchego is based on sheep's milk not cow
    org_ingredients == 'strawberry basket' ~ 'strawberries',
    org_ingredients == 'agar' ~ 'agar',
    org_ingredients == 'tørr hvitvin' ~ 'wine', #SHARP does not distinguish between red/white/rose wine
    org_ingredients == 'Vialone Nano' ~ 'rice',
    org_ingredients == 'cognac' ~ 'cognac',
    org_ingredients == 'aroma mushrooms' ~ 'mushroom',
    org_ingredients == 'vineddik eller tamarindpasta' ~ 'vinegar',
    org_ingredients == 'shellfish mix' ~ 'shellfish',
    org_ingredients == 'ruccula' ~ 'lettuces and similar',
    org_ingredients == 'omelet' ~ 'omelet',
    org_ingredients == 'Sunniva® Pressed Orange Juice with Fruit Meat' ~ 'orange juice',
    org_ingredients == 'of tomato mackerel,  box or tube fatty fish' ~ 'mackerel canned', #SHARP does not have tomato mackerel
    str_detect(org_ingredients, 'shavsrøy') ~ 'Salvelinus alpinus', #It is in the salmon family, use values from salmon or trout?
    str_detect(org_ingredients, 'anglerfish') ~ 'anglerfish',
    org_ingredients == 'purified fish' ~ 'cod', #Lutefisk, maybe use som prepared fish type instead?
    org_ingredients == 'fish soup base' ~ '',
    org_ingredients == 'fish on request' ~ 'salmon', #It's sushi after all,
    org_ingredients == 'cold wolf fish' ~ 'Anarhichas lupus', #Steinbit, wolffish, catfish, etc
    org_ingredients %in% c('oksekjøtt, bog eller lår') ~ 'beef shoulder',
    is.na(Ingredients) | Ingredients == '' ~ org_ingredients,
    TRUE ~ Ingredients
  )) %>%

  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'spaghetti|lasagna plate') ~ 'pasta',
    org_ingredients == 'salmon fillet with leather, without legs' ~ 'salmon fillet',
    Ingredients == 'neck chops' ~ 'pork neck chops',
    Ingredients == 'bread' ~ 'wheat bread and rolls, brown',
    TRUE ~ Ingredients
  ))

recipes$UK <- recipes$UK %>%

  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(tolower(org_ingredients), 'goat cheese') ~ 'chevre frais', #One is actually brown goat cheese
    str_detect(tolower(org_ingredients), 'cheddar|romano|parmesan|parmigiano-reggiano') &
      !str_detect(org_ingredients, 'goat') &
      str_detect(tolower(org_ingredients), 'cheese') ~ 'Hard cheese',
    str_detect(tolower(org_ingredients), 'parmesan') ~ 'Hard cheese',
    str_detect(tolower(org_ingredients), 'havarti|mozzarella|jarlsberg') &
      str_detect(tolower(org_ingredients), 'cheese') ~ 'hard to semi-hard cheese',
    tolower(org_ingredients) %in% c('manchego, grated', 'halloumi') ~ 'hard to semi-hard cheese', #Although manchego is based on sheep's milk not cow
    str_detect(tolower(org_ingredients), 'blue|paneer|camembert') &
      str_detect(tolower(org_ingredients), 'cheese') ~ 'Soft-ripened cheese',
    str_detect(org_ingredients, 'baby squid') ~ 'baby squid', #Fix in UK dataset
    is.na(Ingredients) | Ingredients == '' ~ org_ingredients,
    Ingredients == 'bread' ~ 'wheat bread and rolls, brown',
    Ingredients == 'tomat' ~ 'tomato',
    TRUE ~ Ingredients))

#Find ingredients in recipes that are part of compound ingredients in SHARP
various$white_flour_products <- c('tortilla, fin', 'rolls', 'dinner rolls', 'french bread',
                                  'pocket bread round, cut in half', 'Mission jalapeno cheddar wraps',
                                  'jalapeno cheddar wraps', 'burrito-size flour tortillas, warmed',
                                  'flour tortillas, or more if needed', 'flour tortillas, warmed', 'flour tortillas',
                                  'bread white', 'white bread', '\\btortilla\\b', 'naan bread', 'breadsticks', 'soft bread cubes',
                                  'loff', 'ciabatta')
various$whole_flour_products <- c('whole wheat flour tortillas', 'EARTH GRAINSÂ® Extra Fiber Whole Wheat bread',
                                  'earth grainsâ® extra fiber whole wheat bread', 'crisp bread', 'bread crumb', 'flatbread')

temp <- bind_rows(recipes, .id = 'Country') %>%

  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%

  #Fix up some ingredient names
  mutate(Ingredients = Ingredients %>%
           str_replace(paste0(various$white_flour_products, collapse = '|'),
                       'wheat bread and rolls, white') %>%
           str_replace(paste0(various$whole_flour_products, collapse = '|'),
                       'wheat bread and rolls, brown') %>%
           str_replace('prepared dijon-style mustard', 'mustard') %>%
           str_replace('yogurt', 'yoghurt') %>%
           str_replace('canola oil', 'rapeseed oil') %>%
           str_replace('spaghetti|lasagna plate|elbow macaroni', 'pasta') %>%
           str_replace('\\btomat\\b', 'tomato') %>%
           str_replace('anchovies canned', 'anchovy canned') %>%
           str_replace('celeriac', 'celariac root') %>%
           str_replace('diced pimento peppers, drained', 'sweet peppers') %>%
           str_replace('ortegaâ® diced green chiles canned', 'chili pepper') %>%
           str_replace('white vine', 'white wine') %>%
           str_replace('garlic salt|celery salt', 'salt') %>% #To avoid them being classified as "garlic" or "celery"
           str_replace('large shrimp, peeled and deveined and butterflied', 'shrimp') %>%
           str_replace('ruccola', 'salad rocket') %>% #What ruccola is called in sharp db
           str_replace('sea salt', 'salt')
         ) %>%
  mutate(
    Ingredients = case_when(
      str_detect(Ingredients, 'chicken stock|chicken broth|vegetable broth|beef broth') ~ 'stock/broth',
      str_detect(Ingredients, 'chicken bouillon') ~ 'broth cube',
      str_detect(Ingredients, 'peas') & !str_detect(tolower(Ingredients), 'chick|sugar') ~ 'garden peas',
      str_detect(Ingredients, 'green bean|bean green') & !str_detect(original_amounts, 'can') ~ 'french bean',
      (str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|cannellini')) & !str_detect(Ingredients, 'kidney') ~ 'navy beans', #the white beans found in SHARP
      str_detect(Ingredients, 'mushroom|champignon') & !str_detect(Ingredients, 'cream|canned|dried') ~ 'common mushroom',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'dried') ~ 'dried mushroom',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'drained') ~ 'canned mushroom',
      Ingredients == 'squash' | str_detect(Ingredients, 'zucchini') ~ 'summer squash',
      Ingredients == 'bread' ~ 'wheat bread and rolls, brown',
      Ingredients == 'flank steak' ~ 'beef flank steak',
      org_ingredients == 'fish cakes, coarse' ~ 'fish cakes coarse',
      str_detect(Ingredients, 'green onion') ~ 'scallion',
      str_detect(Ingredients, 'sweet onion') ~ 'onion',
      org_ingredients == 'sun-dried tomatoes' ~ 'sun-dried tomatoes',

      #Alcohol
      str_detect(Ingredients, 'sherry|madeira') ~ 'fortified and liqueur wines', #These are fortified wines
      str_detect(Ingredients, 'marsala') ~ 'wine',
      str_detect(Ingredients, 'cognac') ~ 'brandy', #Cognac is a type of brandy
      TRUE ~ Ingredients),

    #Change cooked rice/pasta/bacon too their dry weight, using weight change ratio from "Weights, measures and portion sizes for foods" by Helsedirektoratet
    Amounts_kg = case_when(
      Ingredients == 'cooked pasta' ~ Amounts_kg/2.63,
      Ingredients == 'cooked rice' ~ Amounts_kg/2.94,
      str_detect(Ingredients, 'bacon') & str_detect(Ingredients, 'cooked') ~ Amounts_kg/0.31,
      TRUE ~ Amounts_kg)
    )

#Read sustainability data and change to fit recipes----
SHARP <- read_csv('./sustainability/SHARP/original/SHARPversion2018.csv') %>%

  #Rename to fit with the other datasets
  rename(Ingredients = `Food item`) %>%
  #Remove graphical characters
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  mutate(Ingredients = str_replace_all(Ingredients, '\\(|\\)', ''))

#Ingredients to remove, either because they're duplicates or just not needed
various$sharp_to_remove <- SHARP %>%

  #Whole groups of ingredients
  filter(L1 %in% c('Coffee, cocoa, tea and infusions', 'Composite dishes',
                   'Food products for young population', 'Sugar and similar, confectionery and water-based sweet desserts',
                   'Eggs and egg products', 'Grains and grain-based products', 'Alcoholic beverages', 'Seasoning, sauces and condiments',
                   'Milk and dairy products', 'Water and water-based beverages') |
           str_detect(Ingredients, 'rice|other wine-like fruit drinks|soft')) %>%
  #Remove ingredients in these categories that should be kept
  filter(!Ingredients %in% tolower(c('White sugar', 'Honey', 'Syrups',

                             #Cheeses
                             'Cream cheese', 'Cottage cheese', 'extra hard cheese parmesan, grana type',
                             'Firm/semi-hard cheese gouda and edam type', 'Hard cheese cheddar, emmental type',
                             'Cheese, feta', 'Soft - ripened cheese', 'Cheese, chevre frais',  'processed cheese and spreads',
                             'ricotta', 'mozzarella',
                             #Milk products
                             'buttermilk', 'coconut milk cocos nucifera liquid', 'cow milk', 'yoghurt, cow milk',
                             'evaporated milk liquid, unsweetened', 'cream, plain', 'cr�me fraiche and other mild variants of sour cream',
                             'sour cream, plain',

                             #Egg products
                             'hen eggs', 'boiled eggs', 'hen egg white', 'hen egg yolk',
                             #Grain/grain replacements products
                             'rice flour', 'rice drink', 'rice grain', 'rice grain, brown', 'noodle, rice', 'couscous',
                             'puff pastry', 'biscuits', 'maize flour', 'crisp bread, rye wholemeal', 'wheat bread and rolls',
                             'breadcrumbs', 'buns', 'wheat bread and rolls, brown or wholemeal', 'wheat flour',
                             'wheat wholemeal flour', 'dried pasta', 'pasta wholemeal', 'oat grain', 'bulgur', 'barley grain, pearled',
                             #Alcoholic beverages
                             'beer', 'wine', 'whisky', 'fortified and liqueur wines', 'cider', 'brandy',
                             #Conditments, sauces and spices
                             'soy sauce', 'salt', 'mixed herbs and spices', 'curry powder',
                             'stock cubes or granulate bouillon base', 'mustard and related sauces', 'vinegar', 'vinegar, wine',
                             'tomato ketchup and related sauces', 'barbecue or steak sauces', 'mayonnaise sauce',
                             #Grains
                             'short pastry doughs pate brisee',
                             
                             #Water
                             'tap water'))) %>% #All sugars have the same CO2 and land-use footprint, only need one, same with types of cheeses
  select(FoodEx2)

SHARP <- SHARP %>%
  #Filter ingredients not needed
  filter(!FoodEx2 %in% c(various$sharp_to_remove$FoodEx2,

                         #Potato products
                         "A011L", "A011S", "A03VG", "A0BYS", "A0BYV", "A00ZX", "A011D",
                         "A011E", "A011P", "A011R", "A0BYT", "A005A", "A00ZV",
                         #Pasta products
                         "A007G", "A007M", "A007J", "A007V", "A007P",
                         #Sprouts
                         "A00SF", "A00SY",

                         #Pork products
                         "A026R",
                         #Pigeon
                         "A01TA",
                         #Corned beef
                         "A0B9G"

                         )) %>%
  filter(!Ingredients %in% c('bovine and pig, minced meat', 'onion bulbs for fresh consumption',
                             'blue mussel', 'butter and margarine/oil blends', 'margarines and similar',
                             'shrimps, common', 'brown trout', 'atlantic salmon', 'herring, atlantic', 'mackerel, atlantic',
                             'carps', 'halibut, greenland', 'jam of fruit / vegetables', 'compote of fruit / vegetables',
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

           #Meat
           str_replace('beef tallow including processed suet', 'tallow') %>%
           str_replace('bovine, minced meat', 'ground meat') %>% #Used in the norwegian recipes
           str_replace('pork lard', 'lard') %>%

           #Poultry
           str_replace('hen ', '') %>%
           str_replace('eggs', 'egg') %>%
           str_replace('turkey fresh meat', 'turkey') %>%
           str_replace('cooked other poultry meat', 'chicken cooked/turkey cooked') %>%

           #Seafood
           str_replace('anglerfish and monkfish', 'anglerfish/monkfish') %>% #Easier to separate
           str_replace('cods, hakes, haddocks', 'cod/hake/haddock/pollock') %>% #Pollock is in the same fish family
           str_replace('canned tunas and similar', 'tuna canned') %>%
           str_replace('canned anchovies', 'anchovies canned') %>%
           str_replace('canned mackerel', 'mackerel canned') %>%
           str_replace('shrimps and prawns', 'shrimps/prawns') %>%
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
           str_replace('tap water', 'water')

         ) %>%

  #Conditional
  mutate(Ingredients = case_when(

    #All beers have the same CO2 and landuse
    str_detect(Ingredients, 'beer') ~ 'beer',
    str_detect(tolower(Ingredients), 'pork') ~ 'ham', #All are ham products
    str_detect(tolower(Ingredients), 'pig') ~ 'pork', #Pork meat is used in all the recipes, not pig meat. All pig products have the same values
    str_detect(tolower(Ingredients), 'bovine|beef') ~ 'beef', #Beef is used in the recipes, all have the same values
    str_detect(Ingredients, 'sausage') ~ 'sausage', #All have the same values
    str_detect(Ingredients, 'chicken') ~ 'chicken',
    str_detect(Ingredients, 'sheep') ~ 'sheep',
    str_detect(Ingredients, 'margarine') ~ 'margarine',
    str_detect(Ingredients, 'lobster') ~ 'lobster',
    str_detect(Ingredients, 'salami') ~ 'salami',
    str_detect(Ingredients, 'cherries') ~ 'cherries',
    TRUE ~ Ingredients
  )) %>%

  #Filter away other unnecessary ingredients
  filter(!(str_detect(Ingredients, 'canned ') |
           str_detect(Ingredients, 'biscuit'))) %>%
  filter(!(str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'dried'))) %>%

  select(-FoodEx2) %>% unique()

#Create a 'shellfish' ingredient in SHARP, using the mean of all the shellfish ingredients and add to SHARP
various$shellfish <- SHARP %>%
  filter(Ingredients %in% c('clams', 'lobster', 'oyster', 'oyster, pacific cupped', 'mussels', 'scallops', 'shrimps/prawns')) %>%
  summarise(`GHGE of 1 kg food as consumed_kgCO2eq` = mean(`GHGE of 1 kg food as consumed_kgCO2eq`),
            `Land use of 1 kg food as consumed_m2/yr` = mean(`Land use of 1 kg food as consumed_m2/yr`)) %>%
  mutate(Ingredients = 'shellfish',
         L1 = 'Fish, seafood, amphibians, reptiles and invertebrates')
SHARP <- full_join(SHARP, various$shellfish)
#Add composite ingredients
various$composite_ingredients <- readRDS('./oppskrifter/composite_ingredients.Rds')

#Add to SHARP
SHARP <- full_join(SHARP, various$composite_ingredients) %>%
  #Create a half-and-half row, half milk and half cream
  add_row(Ingredients = 'half-and-half',
          L1 = 'Milk and dairy products',
          `GHGE of 1 kg food as consumed_kgCO2eq` = (10.6822437+1.5858702)/2,
          `Land use of 1 kg food as consumed_m2/yr` = (10.4333868+1.4606742)/2) %>%

  #Create unique ID for each item
  mutate(ID = seq_along(Ingredients))
#Save
saveRDS(SHARP, 'sharp_db.Rds')

#Create a df with reference names to query ingredient lists----
sharp_ref <- SHARP %>%
  select(ID, Ingredients) %>%
  separate_rows(Ingredients, sep = '/') %>%

  #Remove plural s
  mutate(Ingredients = case_when(
         Ingredients %in% c('apples', 'pineapples', 'avocados', 'apricots',
                            'beetroots', 'breadcrumbs', 'carrots', 'cauliflowers',
                            'buns', 'chards', 'clams', 'coconuts', 'courgettes', 'crabs',
                            'cucumbers', 'figs', 'grapefruits', 'groupers', 'hazelnuts',
                            'herrings', 'kales', 'lemons', 'lentils', 'limes', 'lettuces',
                            'mandarins', 'melons', 'mussels', 'nectarines', 'olives',
                            'onions', 'oranges', 'oysters', 'papayas', 'peanuts', 'pears',
                            'pistachios', 'plums', 'pumpkins', 'rhubarbs', 'salmons',
                            'shrimps', 'syrups', 'trouts', 'hazelnuts', 'walnuts',
                            'scallops', 'sardines', 'blackcurrants', 'redcurrants',
                            'leeks', 'shallots', 'shrimps', 'prawns'
                            ) ~ str_replace(Ingredients, 's\\b', ''),
         TRUE ~ Ingredients)) %>%

  #First two words contain the most important information to identify the ingredients
  mutate(first_word = str_extract(Ingredients, '^\\w+'),
         temp = str_extract(Ingredients, '^\\w+\\s\\w+')) %>%
  mutate(second_word = str_remove(temp, '^\\w+\\s')) %>%
  select(-temp) %>% unique() %>%
  replace_na(list(second_word = 'nothing')) %>%

  #Clean up some of them that's not right, always have the most generic name in the first column, then a specification in second column if present
  mutate(
    first_word = case_when(
      Ingredients == 'lettuces and similar-' ~ 'lettuce',
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
      Ingredients == 'ground meat' ~ 'meat',
      Ingredients == 'italian-style sausage' ~ 'sausage',
      Ingredients == 'broad beans without pods' ~ 'broad bean',
      Ingredients == 'broad beans dry' ~ 'bean',
      Ingredients == 'vegetable fats and oil' ~ 'oil',
      Ingredients == 'breadcrumb' ~ 'bread',
      Ingredients == 'fortified and liqueur wines' ~ 'fortified and liqueur wines',
      Ingredients == 'condensed cream of celery soup' ~ 'condensed cream of celery soup',
      Ingredients == 'condensed cream of mushroom soup' ~ 'condensed cream of mushroom soup',
      Ingredients == 'condensed cream of chicken soup' ~ 'condensed cream of chicken soup',
      Ingredients == 'refrigerated buttermilk biscuit dough' ~ 'refrigerated buttermilk biscuit dough',
      Ingredients == 'fish cakes coarse' ~ 'fish cakes coarse',
      Ingredients == 'vegetables and vegetable products' ~ 'vegetables and vegetable products',
      TRUE ~ first_word),

    second_word = case_when(
      Ingredients == 'lettuces and similar-' ~ 'similar',
      Ingredients == 'wheat bread and rolls' ~ 'white',
      Ingredients == 'wheat bread and rolls, brown or wholemeal' ~ 'brown',
      Ingredients == 'boiled egg' ~ 'boiled',
      Ingredients == 'chickpeas dry' ~ 'nothing',
      Ingredients == 'chickpea flour' ~ 'flour',
      Ingredients == 'mung bean sprouts' ~ 'sprout',
      Ingredients == 'soy bean' ~ 'soy',
      Ingredients == 'beans dry and similar-' ~ 'nothing',
      Ingredients == 'borlotti or other common beans dry' ~ 'borlotti',
      Ingredients == 'kidney bean dry seeds' ~ 'kidney',
      Ingredients == 'navy beans dry seeds' ~ 'navy',
      Ingredients == 'beans canned' ~ 'canned',
      Ingredients == 'french beans with pods' ~ 'french',
      Ingredients == 'beans with pods' ~ 'nothing',
      Ingredients == 'processed cheese and spreads' ~ 'nothing',
      Ingredients == 'cheese, feta' ~ 'cheese',
      Ingredients == 'cheese, chevre frais' ~ 'nothing',
      Ingredients == 'soft-ripened cheese' ~ 'nothing',
      Ingredients == 'hard cheese' ~ 'hard cheese',
      Ingredients == 'hard to semi-hard cheese' ~ 'nothing',
      Ingredients == 'garden peas dry' ~ 'garden',
      Ingredients == 'sugar snap pea' ~ 'sugar snap',
      Ingredients == 'romaines' ~ 'romaine',
      Ingredients == 'sun-dried tomatoes' ~ 'sun-dried',
      Ingredients == 'cherry tomatoes' ~ 'tomato',
      Ingredients == 'wheat flour' ~ 'nothing',
      Ingredients == 'wheat wholemeal flour' ~ 'wholemeal',
      Ingredients == 'ground meat' ~ 'ground',
      Ingredients == 'hard cheese' ~ 'nothing',
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
      Ingredients == 'fortified and liqueur wines' ~ 'nothing',
      Ingredients == 'condensed cream of celery soup' ~ 'nothing',
      Ingredients == 'condensed cream of mushroom soup' ~ 'nothing',
      Ingredients == 'condensed cream of chicken soup' ~ 'nothing',
      Ingredients == 'refrigerated buttermilk biscuit dough' ~ 'nothing',
      Ingredients == 'fish cakes coarse' ~ 'nothing',
      Ingredients == 'vegetables and vegetable products' ~ 'nothing',
      Ingredients == 'sunflower seed oil' ~ 'oil',
      Ingredients == 'vegetables, pickled' ~ 'pickled',
      TRUE ~ second_word
    )
  ) %>%

  filter(!first_word %in% c('juice', 'fresh')) %>% #These may otherwise cause issues when searching

  select(-Ingredients)
#Is the maize flour in SHARP the same as corn starch?

#Turn all canned beans in different recipes to 'canned bean'

saveRDS(sharp_ref, 'sharp_ref.Rds')

#Calculate CO2 and land use----
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

#Run funcion
test <- checkRefList(temp %>% drop_na(Ingredients))

#Save
saveRDS(test, 'sharpe_checkref.Rds')

test <- readRDS('sharpe_checkref.Rds')

#Turn into one df, fix errors and do calculations
test2 <- bind_rows(test) %>%

  #Add back all the ingredients not found
  right_join(temp) %>% unique() %>%

  #Fix mistakes
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%

  #Clean up some mistakes
  mutate(ref = case_when(

    #Ingredients not in SHARP
    str_detect(Ingredients, 'olive paste tapenade|potato soft flatbread|coriander seed|mustard seed|fennel seed|caraway seed|shrimp paste|stock/broth|canned tomato sauce|tomato sauce canned|oyster sauce|pasta sauce|dry onion soup mix|onion powder|mango chutney|corn tortillas|sausage flavored pasta sauce|alfredo-style pasta sauce|tomato beans|curry paste|egg noodle|poultry seasoning|fish sauce|ginger garlic paste|lemongrass|bean dip|duck sauce|liquid smoke flavoring|caper') ~ '',
    str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'powder|sauce|flake|paste') ~ '',

    #Mistakes
    Ingredients == 'cherry' ~ 'cherries',
    Ingredients %in% c('diced tomatoes with green chili pepperss, drained canned', 'italian-style diced tomatoes, drained canned',
                       'italian-style diced tomatoes, undrained canned') ~ 'tomato',
    Ingredients == 'eggplant' ~ 'eggplant',
    Ingredients == 'seedless grapes, halved' ~ 'table grapes', #Change this?
    Ingredients == 'garlic, peeled and chopped' ~ 'garlic',
    Ingredients == 'hard to semi-hard cheese' ~ 'hard to semi-hard cheese',
    Ingredients == 'white kidney beans, drained and rinsed' ~ 'bean, canned',
    str_detect(Ingredients, 'turkey') ~ 'turkey',
    str_detect(Ingredients, 'butternut squash|acorn squash') ~ 'pumpkin', #These are pumpkins/autumn squash
    Ingredients == 'oil' ~ 'vegetable oil', #It would typically be some type of vegetable oil
    Ingredients == 'corn starch' ~ 'corn starch',
    Ingredients == 'fortified and liqueur wines' ~ 'fortified and liqueur wines',
    Ingredients == 'mirin (japanese sweet wine)' ~ 'wine',
    Ingredients == 'pitted prunes, halved' ~ 'prune',
    Ingredients == 'condensed cream of mushroom soup canned' ~ 'condensed cream of mushroom soup',
    Ingredients == 'condensed cream of chicken soup canned' ~ 'condensed cream of chicken soup',
    Ingredients == 'condensed cream of celery soup canned' ~ 'condensed cream of celery soup',
    Ingredients == 'refrigerated buttermilk biscuit dough' ~ 'refrigerated buttermilk biscuit dough',
    Ingredients %in% c('asian (toasted) sesame oil', 'sesame oil') ~ 'seed oil',
    Ingredients == 'buttermilk' ~ 'buttermilk',
    Ingredients == 'bean black' ~ 'beans canned',
    Ingredients == 'common mushroom' ~ 'mushroom',
    Ingredients == 'sheep head' ~ 'sheep',
    str_detect(Ingredients, 'juiced') & str_detect(Ingredients, 'lime') ~ 'lime',
    Ingredients == 'fish cakes coarse' ~ 'fish cakes coarse',
    Ingredients %in% c('frozen mixed vegetables', 'rema frozen vegetables') ~ 'vegetables and vegetable products',
    Ingredients == 'pistachio nuts' ~ 'pistachio',
    Ingredients == 'olives' ~ 'olives fresh',
    str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'green') & !str_detect(Ingredients, 'pickle') ~ 'olives fresh',
    Ingredients %in% c('olives, green, pickled', 'onion - pickled', 'pickled onion',
                       'sweet green pickle relish') ~ 'vegetables pickled',
    Ingredients %in% c('chick pea', 'chick pea canned') ~ 'chick pea',
    Ingredients == 'ham smoked' ~ 'ham',
    Ingredients == 'salmon roe' ~ 'fish roe',
    Ingredients %in% c('rice noodles', 'dried rice pasta') ~ 'rice noodle',
    Ingredients == 'smoked pollock' ~ 'smoked fish',
    Ingredients == 'onion pearl' ~ 'onion',
    str_detect(Ingredients, 'carrot') & !str_detect(Ingredients, 'juice') ~ 'carrot',
    TRUE ~ ref,
  )) %>%
  mutate(ID = case_when(
    str_detect(Ingredients, 'eggplant') ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'eggplant'),1]),
    Ingredients == 'Cherry' ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'cherries'),1]),
    Ingredients %in% c('diced tomatoes with green chili pepperss, drained canned', 'italian-style diced tomatoes, drained canned',
                       'italian-style diced tomatoes, undrained canned') ~ sharp_ref %>% filter(first_word == 'tomato' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'seedless grapes, halved' ~ sharp_ref %>% filter(first_word == 'table' & second_word == 'grapes') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'garlic, peeled and chopped' ~ sharp_ref %>% filter(first_word == 'garlic' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'hard to semi-hard cheese' ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'hard to semi-hard cheese'),1]),
    Ingredients == 'white kidney beans, drained and rinsed' ~ sharp_ref %>% filter(first_word == 'bean' & second_word == 'canned') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'turkey') ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'turkey'),1]),
    Ingredients == 'butternut squash' ~ sharp_ref %>% filter(first_word == 'pumpkin' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'oil' ~ sharp_ref %>% filter(first_word == 'oil' & second_word == 'vegetable') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'corn starch' ~ 0,
    Ingredients == 'fortified and liqueur wines' ~ sharp_ref %>% filter(first_word == 'fortified and liqueur wines' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'mirin (japanese sweet wine)' ~ sharp_ref %>% filter(first_word == 'wine' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'pitted prunes, halved' ~ sharp_ref %>% filter(first_word == 'prune' & second_word == 'dried') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'condensed cream of mushroom soup canned' ~ sharp_ref %>% filter(first_word == 'condensed cream of mushroom soup' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'condensed cream of chicken soup canned' ~ sharp_ref %>% filter(first_word == 'condensed cream of chicken soup' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'condensed cream of celery soup canned' ~ sharp_ref %>% filter(first_word == 'condensed cream of celery soup' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'refrigerated buttermilk biscuit dough' ~ sharp_ref %>% filter(first_word == 'refrigerated buttermilk biscuit dough' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients %in% c('asian (toasted) sesame oil', 'sesame oil') ~ sharp_ref %>% filter(first_word == 'seed' & second_word == 'oil') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'buttermilk' ~ sharp_ref %>% filter(first_word == 'buttermilk' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'bean black' ~ sharp_ref %>% filter(first_word == 'bean' & second_word == 'canned') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'common mushroom' ~ sharp_ref %>% filter(first_word == 'mushroom' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'sheep head' ~ sharp_ref %>% filter(first_word == 'sheep' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'juiced') & str_detect(Ingredients, 'lime') ~ sharp_ref %>% filter(first_word == 'lime' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'fish cakes coarse' ~ sharp_ref %>% filter(first_word == 'fish cakes coarse' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients %in% c('frozen mixed vegetables', 'rema frozen vegetables') ~ sharp_ref %>% filter(first_word == 'vegetables and vegetable products' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'pistachio nuts' ~ sharp_ref %>% filter(first_word == 'pistachio' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'olives' ~ sharp_ref %>% filter(first_word == 'olives' & second_word == 'fresh') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'green') & !str_detect(Ingredients, 'pickle') ~ sharp_ref %>% filter(first_word == 'olives' & second_word == 'fresh') %>% select(ID) %>% as.numeric(.),
    Ingredients %in% c('olives, green, pickled', 'onion - pickled',
                       'pickled onion', 'sweet green pickle relish') ~ sharp_ref %>% filter(first_word == 'vegetables' & second_word == 'pickled') %>% select(ID) %>% as.numeric(.),
    Ingredients %in% c('chick pea', 'chick pea canned') ~ sharp_ref %>% filter(first_word == 'chick pea' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'ham smoked' ~ sharp_ref %>% filter(first_word == 'ham' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'salmon roe' ~ sharp_ref %>% filter(first_word == 'fish' & second_word == 'roe') %>% select(ID) %>% as.numeric(.),
    Ingredients %in% c('rice noodles', 'dried rice pasta') ~ sharp_ref %>% filter(first_word == 'noodle' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'smoked pollock' ~ sharp_ref %>% filter(first_word == 'smoked' & second_word == 'fish') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'onion pearl' ~ sharp_ref %>% filter(first_word == 'onion' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    str_detect(Ingredients, 'carrot') & !str_detect(Ingredients, 'juice') ~ sharp_ref %>% filter(first_word == 'carrot' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    ref == '' ~ 0,
    TRUE ~ ID
  ))

#Ingredients not found in the reference
various$not_in_ref <- test2 %>%
  filter(is.na(ref) | ID == 0)

#Translate the norwegian ingredien names to english and run checkref again
various$english_translation <- readRDS('translate_ingredients.Rds')

various$norwegian_ingredients <- various$not_in_ref %>%
  #Get the norwegian named ingredients
  inner_join(temp %>% filter(Country == 'Norway')) %>% select(`Selected Meals`, Ingredients, weight_ID, Amounts_kg) %>%
  #Translate
  inner_join(various$english_translation, by = 'weight_ID') %>%

  #Rename columns
  rename(Ingredients = Ingredients.y,
         org_ingredients = Ingredients.x)

#Run checkRef on translated ingredients
various$translated_norwegian <- checkRefList(various$norwegian_ingredients)

#Add to test2
temp2 <- bind_rows(various$translated_norwegian) %>%
  
  inner_join(various$norwegian_ingredients) %>% unique() %>%
  #Clean up some mistakes
  mutate(ref = case_when(
    
    #Ingredients not in SHARP
    str_detect(Ingredients, 'nudler, med egg, tørr|potato soft flatbread|coriander seed|mustard seed|fennel seed|caraway seed|shrimp paste|stock/broth|canned tomato sauce|tomato sauce canned|oyster sauce|pasta sauce|dry onion soup mix|onion powder|mango chutney|corn tortillas|sausage flavored pasta sauce|alfredo-style pasta sauce|tomato beans|curry paste|egg noodle|poultry seasoning|fish sauce|ginger garlic paste|lemongrass|bean dip|duck sauce|liquid smoke flavoring|caper') ~ '',
    str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'powder|sauce|flake|paste') ~ '',

    #Mistakes
    Ingredients == 'cherry' ~ 'cherries',
    Ingredients %in% c('diced tomatoes with green chili pepperss, drained canned', 'italian-style diced tomatoes, drained canned',
                       'italian-style diced tomatoes, undrained canned') ~ 'tomato',
    Ingredients == 'oil' ~ 'vegetable oil', #It would typically be some type of vegetable oil
    Ingredients == 'corn starch' ~ '',
    Ingredients == 'coriander seed' ~ '',
    Ingredients == 'fish sauce' ~ '',
    Ingredients == 'fatty fish, raw' ~ 'salvelinus alpinus',
    org_ingredients == 'margarin' ~ 'margarine',
    org_ingredients == 'smør' ~ 'butter',
    TRUE ~ ref,
  )) %>%
  mutate(ID = case_when(
    str_detect(Ingredients, 'eggplant') ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'eggplant'),1]),
    Ingredients == 'Cherry' ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'cherries'),1]),
    Ingredients %in% c('diced tomatoes with green chili pepperss, drained canned', 'italian-style diced tomatoes, drained canned',
                       'italian-style diced tomatoes, undrained canned') ~ sharp_ref %>% filter(first_word == 'tomato' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'hard to semi-hard cheese' ~ as.numeric(sharp_ref[str_which(tolower(sharp_ref$first_word), 'hard to semi-hard cheese'),1]),
    Ingredients == 'oil' ~ sharp_ref %>% filter(first_word == 'oil' & second_word == 'vegetable') %>% select(ID) %>% as.numeric(.),
    Ingredients == 'corn starch' ~ 0,
    Ingredients == 'coriander seed' ~ 0,
    Ingredients == 'fish sauce' ~ 0,
    Ingredients == 'fatty fish, raw' ~ 0,
    org_ingredients == 'margarin' ~ sharp_ref %>% filter(first_word == 'margarine' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    org_ingredients == 'smør' ~ sharp_ref %>% filter(first_word == 'butter' & second_word == 'nothing') %>% select(ID) %>% as.numeric(.),
    ref == '' ~ 0,
    TRUE ~ ID
  )) %>%
  mutate(Ingredients = case_when(
    org_ingredients == 'margarin' ~ 'margarine',
    org_ingredients == 'smør' ~ 'butter',
    TRUE ~ Ingredients
  )) %>%
  #Add back a country identifier
  mutate(Country = 'Norway') %>%

  select(-c('sharp_ID', 'weight_ID')) %>% unique() #Some duplicates as butter/margarine has been doubled (they have the same weight_ID and was found twice in sharp_ref)
  
#Add back to test2, first remove the old
test2 <- anti_join(test2, 
                   temp2 %>% select(`Selected Meals`, org_ingredients) %>% rename(Ingredients = org_ingredients))

test2 <- full_join(test2, temp2)

#Update 'not in ref'
#Ingredients not found in the reference
various$not_in_ref <- test2 %>%
  filter(is.na(ref) | ID == 0)

#Save various
  various$white_flour_products <- NULL
  various$whole_flour_products <- NULL
  various$sharp_to_remove <- NULL
  various$shellfish <- NULL
  various$english_translation <- NULL
  various$norwegian_ingredients <- NULL
  various$not_in_ref <- NULL

  #Update not_in_ref, now that the translated ingredients are there
  various$not_in_ref <- test2 %>%
    filter(is.na(ref) | Ingredients == '')

  saveRDS(various, 'various_sustainability.Rds')

#NOR

#Check the green bean in Sushi hand rolls with salmon sake temak

calc <- test2 %>%
  #filter(!(is.na(ref) | Ingredients == '')) %>%
  select(ID, ref, Ingredients, Amounts_kg, Country, `Selected Meals`) %>%
  left_join(SHARP, by = 'ID') %>%

  mutate(CO2_footprint = `GHGE of 1 kg food as consumed_kgCO2eq`*Amounts_kg,
         Land_use = `Land use of 1 kg food as consumed_m2/yr`*Amounts_kg)

plot <- calc %>%
  select(`Selected Meals`, Ingredients.x, L1, Country, CO2_footprint, Land_use, Amounts_kg) %>%
  
  #Get the aggreagte values for each meal, calculate CO/Landuse pr 100g
  group_by(`Selected Meals`, Country) %>%
  summarise(meal_CO2 = sum(CO2_footprint, na.rm = TRUE),
            meal_land_use = sum(Land_use, na.rm = TRUE),
            meal_kg = sum(Amounts_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(meal_CO2_100 = meal_CO2/meal_kg/10,
         meal_land_use_100 = meal_land_use/meal_kg/10) %>%
  
  #Add back the individual values
  inner_join(calc %>% select(`Selected Meals`, Ingredients.x, CO2_footprint, Land_use, Amounts_kg, L1))


#Save
saveRDS(plot, 'data_to_analyze.Rds')

#Look over the ingredients and see if the mapping to the SHARP db looks ok
check <- test2 %>% select(Ingredients, ref) %>% unique()
