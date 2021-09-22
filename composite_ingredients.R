library(tidyverse)
library(stringi)
roxygen2::roxygenise()

#Run twice to get the nutrient amounts/sustainability indicators for mango chutney in the worcestershire sauce recipe

#Different databases to search through to find amounts in kilos, nutrient content and sustainability measurements
references <- list(
  'volume_weight' = readRDS('./Data/output/food_weight_ref.Rds') %>% filter(language == 'english'),
  'sustainability' = readRDS('./Data/output/sharp_ref.Rds'),
  'nutrients' = readRDS('./Data/output/nutrient_reference.Rds')
)
databases <- list(
  'volume_weight' = readRDS('./Data/output/all_weights.Rds'),
  'nutrients' = readRDS('./Data/output/nutrients_df.Rds'),
  'sustainability' = readRDS('./Data/output/sharp_db.Rds')
)

#Composite ingredient ingredients----
Name = c('condensed cream of mushroom soup', 'condensed cream of chicken soup',
         'condensed cream of celery soup', 'refrigerated buttermilk biscuit dough',
         'fish cakes_coarse', 'worcestershire_sauce', 'fish_sauce', 'taco_sauce',
         'oyster_sauce', 'hot pepper sauce', 'hoisin_sauce', 'pesto', 'pizza_sauce',
         'chunky_salsa', 'mango_chutney', 'guacamole', 'cranberry_sauce', 'tomato_sauce',
         'potato_flatbread', 'duck_sauce', 'shrimp_paste', 'chili sauce_sweet',
         'chili_sauce', 'shrimp_salad', 'barbeque_sauce', 'omelet', 'adobo_seasoning',
         'chinese five spice', 'italian seasoning', 'steak seasoning', 'puff pastry',
         'mint_sauce', 'taco spice mix', 'tandoori spice mix', 'shortcrust pastry',
         'horseradish_sauce', 'olive paste tapenade', 'beef gravy', 'tortilla_corn',
         'miso_paste', 'potetlefse', 'meatball', 'garam masala', 'sauce piri-piri',
         'sauce pad thai', 'sauce tikka masala', 'fajita spice mix')
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
0.33 cup chicken breast, diced
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
  #The mango chutney is actually tamarind paste, but that is in no database
  '0.25 cup raisins
0.25 cup boiling water
0.5 cup molasses
0.25 cup mango chutney
2 ounce canned anchovy
1 stk onion, coarsely chopped
2.5 cm of fresh ginger
6 clove garlic, crushed
2 cup white vinegar, divided
2 stk cardamom pods
2 tbsp salt
2 tbsp brown sugar
1 tbsp chili flakes
1 tbsp dry mustard
1 tsp whole cloves
1 tsp black pepper whole
0.5 tsp ground cinnamon',
  
  #Fish sauce
  #From https://nourishingjoy.com/homemade-fish-sauce/
  '6 clove garlic
3 tbsp  salt
6 stk bay leaf
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
125 ml broth vegetable
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
0.125 tsp black pepper',
  
  #Pesto
  #From https://www.simplyrecipes.com/recipes/fresh_basil_pesto/
  #Exchanging half of the basil for baby spinach as per options to have greens found in SHARP-ID
  '1 cup fresh basil
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
3 tbsp tomato puree
1 stk bay leaf
2 tbsp dried oregano
2 tsp brown sugar
1 bunch fresh basil',
  
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
1 cup vinegar
1 cup brown sugar
1 cup white granulated sugar
0.5 cup lemon juice
0.25 cup fresh ginger
1 stk onion (sliced thin)
1 stk red chili
2 clove garlic
4 tsp salt
1 tbsp toasted mustard seeds
1 stk cinnamon bar',
  
  #Shrimp paste
  #From https://www.saveur.com/article/Recipes/Shrimp-Paste/
  '0.5 pound unsalted butter
1 pound shrimp
0.5 tsp salt
0.5 tsp ground black pepper
0.25 cup sherry fortified wine 15 vol-% alcohol
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
0.5 dl basil fresh
2.5 dl vinegar
1 tsp salt',
  
  #Shrimp salad
  #From https://mills.no/oppskrift/mills/rekesalat/
  '0.5 dl mayonnaise sauce
0.5 dl sour cream
1 tbsp dill
0.5 tsp lemon juice
0.25 tsp pepper
500 g shrimp',
  
  #Barbeque sauce
  #From https://addapinch.com/homemade-bbq-sauce-recipe/
  '2 cup tomato ketchup
0.5 cup apple cider vinegar
0.25 cup packed brown sugar
2 tbsp honey
1 tbsp Worcestershire sauce
1 tbsp lemon juice',
  
  #Omelet
  #From https://www.aperitif.no/oppskrifter/oppskrift/grunnoppskrift-pa-omelett,69571
  '2 stk egg
2 tbsp water',
  
  #Adobo seasoning
  #From https://www.food.com/recipe/adobo-seasoning-442098
  '2 tbsp salt
1 tbsp paprika
2 tsp ground black pepper
1.5 tsp onion powder
1.5 tsp dried oregano
1.5 tsp ground cumin
1 tsp garlic powder
1 tsp chili powder',
  
  #Chinese five spice
  #From https://www.feastingathome.com/chinese-five-spice/
  '6 stk star anise pods
1.25 tsp cloves ground
2 tbsp cinnamon ground
2 tbsp fennel seeds
3 tsp black pepper',
  
  #Italian seasoning
  #From https://www.gimmesomeoven.com/italian-seasoning-recipe/
  '1.5 tsp dried oregano
1 tsp dried marjoram
1 tsp dried thyme
0.5 tsp dried basil
0.5 tsp dried rosemary
0.5 tsp dried sage',
  
  #Steak seasoning
  #From https://www.aspicyperspective.com/best-homemade-steak-seasoning/
  '1 tbsp sea salt
1 tbsp brown sugar, packed
2 tsp smoked paprika powder
2 tsp chili powder
1 tsp dried thyme
1 tsp ground black pepper
0.5 tsp garlic powder
0.5 tsp onion powder
0.5 tsp mustard powder
0.5 tsp cumin',
  
  #Puff pastry
  #From https://preppykitchen.com/puff-pastry/
  '227 g unsalted butter
240 g wheat flour (240g)
12 g sugar
2 g salt
0.14 dl water',
  
  #Mint sauce
  #From https://www.kitchensanctuary.com/homemade-mint-sauce/
  '30 g fresh mint
2.4 dl boiling water
2 tsp sugar
2 tbsp malt vinegar
1 pinch of salt',
  
  #Taco spice mix
  #From https://gimmedelicious.com/the-best-homemade-taco-seasoning/
  '4 tbsp chili powder
2 tbsp cumin
1 tbsp paprika
1 tbsp salt
1 tsp garlic powder
1 tsp dried onion
1 tsp oregano
1 tsp black pepper',
  
  #Tandoori spice mix
  #From https://www.epicurious.com/recipes/food/views/tandoori-spice-blend-363234
  '1 tsp dried ginger
1 tsp ground cumin
1 tsp dried coriander
1 tsp paprika powder
1 tsp turmeric
1 tsp salt
1 tsp cayenne pepper',
  
  #Shortcrust pastry
  #From https://www.bbcgoodfood.com/recipes/basic-shortcrust-pastry
  '225 g wheat flour
100 g butter, diced
1 pinch salt',
  
  #Horseradish sauce
  #From https://natashaskitchen.com/horseradish-sauce-recipe/
  '0.5 cup sour cream
2 tbsp prepared horseradish drained
2 tbsp mayonnaise
1 tsp apple cider vinegar
0.25 tsp salt
0.125 tsp black pepper
1 tbsp chives finely chopped',
  

# Olive paste tapenade
#From https://www.culinaryhill.com/olive-tapenade/
'1.5 cup olive green
2 stk anchovy fillets
3 tbsp capers rinsed
1.5 tbsp parsley coarsely chopped
3 clove garlic roasted if desired (see notes)
3 tbsp lemon juice
Salt and freshly ground black pepper
0.25 cup olive oil',

#(Homemade) beef gravy
#From https://www.recipetineats.com/gravy/
'1 stk broth cube chicken
1 stk broth cube beef
565 ml boiling water
60 g unsalted butter
4 tbsp wheat flour
0.25 tsp finely ground black pepper',

#Corn tortillas, flour should be the masa harina type
#From https://www.matprat.no/oppskrifter/kos/maistortilla/
'300 g corn flour
1 tsp salt
5 dl water',

#Miso paste
#From https://food52.com/blog/13601-make-miso-from-scratch-get-funky
'1 kg dried soybeans
2 tsp miso
1 kg brown or white rice koji
0.4 kg sea salt',

#potetlefse
'5 dl wheat flour
2000 g potato
2 tsp salt',

#Meatballs
#From https://www.tine.no/oppskrifter/middag-og-hovedretter/kjott/hjemmelagde-kj%C3%B8ttkaker
'1 stk onion
500 g minced meat
1 stk egg
2 tbsp potato starch
2 dl low fat milk
0.5 tsp salt
0.25 tsp pepper
2 tbsp butter',

#Garam Masala
#From https://www.seriouseats.com/garam-masala-recipe
'7 stk green cardamom pods
20 g whole coriander seed
10 g whole cumin seed
12 g whole black peppercorns
4 g whole cloves
4 g fennel seed
6 g cinnamon
1 g star anise
2 g ground nutmeg',

#Piri piri sauce
#From https://www.chilipeppermadness.com/chili-pepper-recipes/sauces/peri-peri-sauce/
'1 pound red chilies
4 clove garlic chopped
1 tsp chili powders
0.5 cup chopped cilantro
0.25 cup chopped basil
0.5 cup olive oil
3 tbsp lemon juice
Salt',

#Sauce pad thai
#From https://www.thespruceeats.com/chicken-pad-thai-without-tamarind-3217100
'0.33333333 cup chicken stock
3 tbsp rice vinegar
1 tbsp lime juice
3.5 tbsp brown sugar
2 tbsp fish sauce
1 tbsp soy sauce
0.125 tsp white pepper',

#Sauce tikka masala
#From https://www.simplyrecipes.com/recipes/chicken_tikka_masala/
'2 tbsp canola oil
5 ounce onion
2 tsp grated ginger
4 clove garlic
1 tbsp ground coriander seed
2 tsp paprika powder
1 tsp garam masala
0.5 tsp turmeric
0.5 tsp freshly ground black pepper
14 ounce canned tomatoes
6 tbsp yogurt
0.33 tsp cayenne pepper
0.5 tsp salt',

#Fajita spice
#From https://www.spendwithpennies.com/fajita-seasoning/
'1 tbsp chili powder
2 tsp ground cumin
2 tsp smoked paprika
2 tsp garlic powder
1 tsp onion powder
1 tsp granulated sugar
0.5 tsp black pepper
0.5 tsp kosher salt to taste
0.25 tsp cayenne pepper'
)

composite_ingredients <- tibble(Name = Name, Ingredients = Ingredients)

#Split amounts and ingredient name into separate columns, turn amounts into grams
various <- list(
  
  #Units to keep in the recipes
  #Recipes from Kolonialen has translated 'pk/pakke/stykk' to 'hp'
  #What is hp?
  'units' = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf', 'can',
              'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice', '\\bclove\\b',
              'neve', 'ml', 'cl', 'bunch', 'pack', 'plate', 'drop', 'twig', 'pound', 'ounce', 'stalk') %>%
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
  
  #Calculate weight of the ingredients----
#Turn volume units to dl
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

#Standardize names
composite_ingredients <- composite_ingredients %>%
  #Add columns needed in standardiseIngredients function
  mutate(Country = 'not_specified',
         `Selected Meals` = 'not_specified') %>%
  standardiseIngredients() %>%
  #Remove columns again as they are not needed here
  select(-c(Country, `Selected Meals`, Ingredients)) %>%
  rename(Ingredients = Ingredients_standardised)

#Split broth into water and broth cubes (1 cube pr 5 dl/0.5kg broth/water)
temp <- composite_ingredients %>%
  #Find broth ingredients
  filter(str_detect(Ingredients, 'water broth')) %>%
  #Add '/' to separate rows into water and broth cubes, and find number of broth cubes for the amount of water
  mutate(Ingredients = str_replace(Ingredients, 'broth', 'water/broth cube')) %>%
  separate_rows(Ingredients, sep = '/') %>%
  mutate(
    Amounts = case_when(
      str_detect(Ingredients, 'broth cube') ~ Amounts/0.5,
      TRUE ~ Amounts),
    unit_enhet = case_when(
      str_detect(Ingredients, 'broth cube') ~ 'stk',
      TRUE ~ unit_enhet)
  )

#Add back to composite ingredients df
composite_ingredients <- composite_ingredients %>%
  filter(!str_detect(Ingredients, 'water broth')) %>%
  bind_rows(., temp)

#Get ID from weight/volume database
temp <- checkRef(reference = references$volume_weight, composite_ingredients)

#Fix errors
temp2 <- temp %>%
  full_join(composite_ingredients) %>%
  mutate(
    ID = case_when(
      Ingredients == 'parmesan cheese' ~ fixRefID(references$volume_weight, 'parmesan'),
      Ingredients %in% c('chili peppers', 'chili', 'strong chili') ~ fixRefID(references$volume_weight, 'chili', 'red'),
      Ingredients == 'corn flour' ~ fixRefID(references$volume_weight, 'cornmeal', 'polenta'),
      TRUE ~ ID
    )) %>% unique()

#Calculate the weight of each ingredient
various$weights <- readRDS('./Data/output/all_weights.Rds') %>%
  filter(language == 'english')
#Ingredients to turn from tbsp to dl
various$to_dl <- c('pepper', 'peanøttsmør', 'olje', 'oil', 'smør', 'butter', 'margarin',
                   'ghee', 'garlic', 'baking powder')

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

various$ingredients_weight <- right_join(weights, temp2, by = c('ID', 'unit_enhet')) %>% #Doing this gives final three more rows than temp...?
  
  #Turn weights into kilo
  mutate(Amounts_kg = case_when(
    unit_enhet == 'kg' ~ Amounts,
    !unit_enhet == 'kg' ~ Amounts*g/1000
  )) %>%
  
  #Cleanup
  select(Name, Ingredients.y, Amounts_kg, ref, Amounts, unit_enhet) %>%
  unique() %>% #Got some values twice 
  rename(Ingredients = Ingredients.y) %>% select(-ref) %>%
  
  #Change some names to fit the SHARP database
  mutate(Ingredients = Ingredients %>%
           str_replace('can anchovies', 'anchovy canned') %>%
           str_replace('cranberry', 'cranberries'))

#Total weight of the recipes
various$weight_of_recipes <- various$ingredients_weight %>%
  group_by(Name) %>%
  summarise(Weight_kg = sum(Amounts_kg, na.rm = TRUE))

#Calculate nutrient content pr 100 g----
#Map to nutrient database
temp <- checkRef(various$ingredients_weight %>% select(Ingredients) %>% unique(), references$nutrients)

#Add amounts and fix some errors
temp2 <- temp %>%
  full_join(., composite_ingredients %>% select(Ingredients)) %>% unique() %>%
  mutate(ID = case_when(
    str_detect(Ingredients, 'vinegar') ~ fixRefID(references$nutrients, 'vinegar'),
    Ingredients == 'parmesan cheese' ~ fixRefID(references$nutrients, 'parmesan'),
    Ingredients == 'chili flake dried' ~ fixRefID(references$nutrients, 'chili', 'powder'),
    Ingredients %in% c('red chili', 'strong chili', 'chili peppers') ~ fixRefID(references$nutrients, 'chili pepper', 'red'),
    Ingredients == 'mushroom' ~ fixRefID(references$nutrients, 'mushroom'),
    Ingredients == 'sesame seed oil' ~ fixRefID(references$nutrients, 'sesame', 'oil'),
    Ingredients == 'dry mustard' ~ fixRefID(references$nutrients, 'mustard'),
    Ingredients == 'mayonnaise sauce' ~ fixRefID(references$nutrients, 'mayonnaise'),
    Ingredients == 'corn flour' ~ fixRefID(references$nutrients, 'corn flour', 'polenta'),
    Ingredients == 'dried soybeans' ~ fixRefID(references$nutrients, 'bean', 'soya'),
    
    TRUE ~ ID
  )) %>% unique() %>%
  
  inner_join(various$ingredients_weight) 

#Calculate the nutrient content
various$with_nutrients <- temp2 %>%
  select(Ingredients, ID, Name, Amounts_kg) %>%
  #Get nutrient values for the ingredients
  inner_join(., databases$nutrients, by = 'ID') %>% select(-ID) %>%
  #Turn long to do the calculations
  pivot_longer(.,
               cols = -c(Name, Ingredients, Amounts_kg),
               names_to = 'feature',
               values_to = 'nutrient_value') %>%
  #Calc
  mutate(value = Amounts_kg*10*nutrient_value) %>% #Nutrient value for each ingredients
  select(-nutrient_value) %>%
  #Per recipe
  group_by(Name, feature) %>%
  summarise(temp = sum(value, na.rm = TRUE)) %>% ungroup() %>%
  
  #Calculate value per 100g
  #First add total weight of recipe
  inner_join(., various$weight_of_recipes) %>%
  mutate(value = temp/(Weight_kg*10)) %>%
  #Remove columns
  select(-c(Weight_kg, temp)) %>%
  #Turn wide again
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  #Rename
  rename(Ingredients = Name)

saveRDS(various$with_nutrients, './Data/output/composite_ingredients_nutrient_content.Rds')

#Calculate CO2 and landuse per kg----
temp <- checkRef(composite_ingredients %>% select(Ingredients) %>% unique(), references$sustainability)

#Fix errors
temp2 <- temp %>%
  full_join(., composite_ingredients) %>% unique() %>%
  
  mutate(ID = case_when(
    Ingredients %in% c('red chili', 'strong chili', 'chili peppers') ~ fixRefID(references$sustainability, 'chili', 'pepper'),
    str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ fixRefID(references$sustainability, 'vinegar', 'wine'),
    str_detect(Ingredients, 'vinegar') ~ fixRefID(references$sustainability, 'vinegar'),
    Ingredients == 'fresh cranberry' ~ fixRefID(references$sustainability, 'cranberries'),
    Ingredients == 'mango chutney' ~ fixRefID(references$sustainability, 'mango chutney'),
    Ingredients == 'olive green' ~ fixRefID(references$sustainability, 'olives', 'canned'),
    str_detect(Ingredients, 'broth cube') ~ fixRefID(references$sustainability, 'stock', 'cubes'),
    Ingredients == 'dried soybeans' ~ fixRefID(references$sustainability, 'bean', 'soy'),
    
    #Not in ref
    Ingredients %in% c('chili flake dried', 'chili powder', 'corn starch',
                       'smoked paprika powder', 'fennel seed', 'onion powder',
                       'garlic powder', 'coriander seed') ~ 0,
    
    TRUE ~ ID
  ))

#Final df with co2/landuse pr kg
final <- full_join(temp2, various$ingredients_weight) %>% left_join(., databases$sustainability, by ='ID') %>%
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
    str_detect(Ingredients, 'salsa|soup|sauce|chutney|guacamole|pesto|paste|spice|seasoning') ~ 'Seasoning, sauces and condiments',
    str_detect(Ingredients, 'fish') ~ 'Fish, seafood, amphibians, reptiles and invertebrates',
    str_detect(Ingredients, 'dough|bread|pastry') ~ 'Grains and grain-based products',
    Ingredients == 'shrimp_salad|omelet' ~ 'Composite dishes'
  ))

#Save
saveRDS(final, './Data/output/composite_ingredients_sustainability_markers.Rds')
