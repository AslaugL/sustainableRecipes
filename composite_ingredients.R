devtools::load_all(path = '.')

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

#Empty list to fill with various df's
various <- list()

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
         'sauce pad thai', 'sauce tikka masala', 'fajita spice mix', 'kimchi', 'sauce_teriyaki',
         'mire_poix', 'graham_cracker', 'amaretti cookie', 'brownie_mix', 'yellow cake_mix',
         'chocolate pudding_mix', 'german chocolate cake_mix', 'marshmallow', 'biscuit_digestive',
         'caramel', 'sweet green pickle relish', 'saltine_cracker')
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
0.25 tsp cayenne pepper',

#Kimchi
#From https://www.tine.no/oppskrifter/lunsj-og-smaretter/forretter-snacks-og-tapas/kimchi
'0.5 stk chinese cabbage
6 stk carrot
15 stk radish
1 tbsp salt
2 tbsp fish sauce
4 cm fresh ginger
1 stk red chili pepper
6 clove garlic',

#Teriyaki sauce
#From https://www.daringgourmet.com/homemade-teriyaki-sauce/
'0.5 cup soy sauce
0.25 cup brown sugar
1.5 tsp fresh ginger ,minced
1 clove garlic
1 tbsp honey
1 tsp sesame oil
3 tbsp sherry
1 pinch sugar
0.25 cup water
3 tsp cornstarch',

#Mire poix
#From https://www.masterclass.com/articles/complete-guide-to-mirepoix-the-aromatic-vegetable-base#5-common-mirepoix-variations
'100 g carrot
100 g celery
200 g onion',

#Graham cracker
#From https://www.biggerbolderbaking.com/homemade-graham-crackers/
'284 g whole wheat flour
170 g light brown sugar
1 tsp cinnamon
1 tsp baking soda
0.75 tsp salt
100 g butter, room temperature
3 tbsp whole milk
85 g honey
2 tsp vanilla extract',

#Amaretti cookie
#From https://www.shelovesbiscotti.com/soft-amaretti-cookies/
'250 g almonds
200 g sugar
1 lemon the zest
3 stk egg whites
1 tsp almond extract
0.33 cup sugar',

#Homemade brownie mix
#From https://iambaker.net/homemade-brownie-mix/
'200 g granulated sugar
40 g unsweetened cocoa powder sifted
64 g all-purpose flour
0.25 tsp kosher salt
0.25 tsp baking powder',

#Yellow cake mix
#From https://www.food.com/recipe/homemade-yellow-cake-mix-subtitute-18-25oz-betty-crocker-box-472383
'2 cup all-purpose flour
1.5 cup sugar
1 tbsp baking powder
0.5 cup non-fat powdered milk',

#Chocolate pudding mix
#From https://www.countrycleaver.com/2014/04/diy-instant-chocolate-pudding-mix.html
'1.25 cup sugar
1 cup cornstarch
1 cup milk powder
0.15 cup cocoa powder
1 pinch of salt',

#German chocolate cake mix
#Dry ingredients from https://www.cookingclassy.com/german-chocolate-cake/
'248 g all-purpose flour
400 g granulated sugar
68 g unsweetened cocoa powder
2 tsp baking powder
1.5 tsp baking soda
1 tsp salt',

#Marshmallows
#From https://www.foodiewithfamily.com/homemade-marshmallows-foodie-christmas-gift-4/
'0.75 ounce unflavored gelatin
0.5 cup cold water
2 cup granulated sugar
0.66 cup light corn syrup
0.25 cup water
0.25 tsp salt
1 tbsp pure vanilla extract',

#Digestive biscuit
#From https://www.biggerbolderbaking.com/homemade-digestive-biscuits/
'236 g whole wheat flour
1 tsp baking powder
0.5 tsp salt
85 g powdered sugar
115 g butter 
57 ml milk',

#Caramel
#From https://tastesbetterfromscratch.com/homemade-caramels/
'1 cup butter
4 cup granulated sugar
2 cup light corn syrup
24 ounce evaporated milk
1 tsp vanilla extract',

#Sweet green pickle relish
#From https://www.healthycanning.com/sweet-green-relish
'3.5 kg pickling cucumbers 
175 g pickling salt
1 litre white vinegar
450 g sugar 
1 tablespoon celery seed
1 tablespoon mustard seed
325 g onion',

#Saltine cracker
#From https://www.restlesschipotle.com/saltines/
'4 cup all-purpose flour
1 tbsp baking powder
0.25 cup unsalted butter
1.33 cup milk
1 stk egg white
1 tbsp water'

)

composite_ingredients <- tibble(Name = Name, Ingredients = Ingredients)

composite_ingredients <- composite_ingredients %>%
  #Separate ingredients into separate rows
  separate_rows(., Ingredients, sep = '\n') %>%
  
  #Turn ingredients to lowercase
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  
  #Add columns needed by standardiserecipes
  mutate(Source = '') %>%
  
  #Standardise ingredients and units
  standardiseRecipes()

#Get ID from weight/volume database
temp <- checkRef(reference = references$volume_weight, composite_ingredients)

#Ingredients to turn from tbsp to dl
various$to_dl <- c('pepper', 'peanøttsmør', 'olje', 'oil', 'smør', 'butter', 'margarin',
                   'ghee', 'garlic', 'baking powder')

weights <- databases$volume_weight %>%
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
  filter(unit_enhet %in% composite_ingredients$unit) %>%
  rename(unit = unit_enhet)

various$ingredients_weight <- right_join(weights, temp, by = c('ID', 'unit')) %>% #Doing this gives final three more rows than temp...?
  
  #Turn weights into kilo
  mutate(Amounts_kg = case_when(
    unit == 'kg' ~ Amounts,
    !unit == 'kg' ~ Amounts*g/1000
  )) %>%
  
  #Cleanup
  select(Name, Ingredients.y, Amounts_kg, ref, Amounts, unit) %>%
  unique() %>% #Got some values twice 
  rename(Ingredients = Ingredients.y) %>% select(-ref)

#Total weight of the recipes
various$weight_of_recipes <- various$ingredients_weight %>%
  group_by(Name) %>%
  summarise(Weight_kg = sum(Amounts_kg, na.rm = TRUE))

#Calculate nutrient content pr 100 g----
#Map to nutrient database
temp <- checkRef(various$ingredients_weight, references$nutrients) %>%
  #Join with the ingredient weight df
  inner_join(various$ingredients_weight) 

#Calculate the nutrient content
various$with_nutrients <- temp %>%
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
temp <- checkRef(various$ingredients_weight, references$sustainability)

#Final df with co2/landuse pr kg
final <- full_join(temp, various$ingredients_weight) %>% left_join(., databases$sustainability, by ='ID') %>%
  select(Name, Ingredients, Amounts_kg, `GHGE of 1 kg food as consumed_kgCO2eq`, `Land use of 1 kg food as consumed_m2/yr`) %>%
  
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
    str_detect(Ingredients, 'salsa|soup|sauce|chutney|guacamole|pesto|paste|spice|seasoning|gravy|garam') ~ 'Seasoning, sauces and condiments',
    str_detect(Ingredients, 'fish') ~ 'Fish, seafood, amphibians, reptiles and invertebrates',
    str_detect(Ingredients, 'meatball') ~ 'Meat and meat products',
    str_detect(Ingredients, 'kimchi|mire_poix|sweet green pickle relish') ~ 'Vegetables and vegetable products',
    str_detect(Ingredients, 'dough|bread|pastry|lefse|tortilla|graham|saltine|biscuit_digestive') ~ 'Grains and grain-based products',
    Ingredients %in% c('shrimp_salad', 'omelet') ~ 'Composite dishes',
    Ingredients %in% c('amaretti cookie', 'marshmallow','brownie_mix', 'yellow cake_mix',
                       'chocolate pudding_mix', 'german chocolate cake_mix', 'caramel')  ~ 'Sugar and similar, confectionery and water-based sweet desserts',
  ))

#Save
saveRDS(final, './Data/output/composite_ingredients_sustainability_markers.Rds')
