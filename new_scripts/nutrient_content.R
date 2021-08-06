library(tidyverse)
library(readxl)
library(stringi)

#Working directory
setwd('C:/Users/aslau/Desktop/UiBV21/Master/Data')

#Nutrient content database----
#List to fill with various items to keep environment clean
various <- list()

#Load raw data helsedir
raw_data <- read_xlsx('./porsjoner_vekt_næringsinnhold/matvaretabellen2020.xlsx')

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
  
  #Whole foodgroups
  filter(!(str_detect(Foodgroup,
                      'dessert|other meat products, prepared|egg, prepared|cookies|cod liver oil|homemade|chocolate|instant|cake|breakfast cereals|porridge|pizza') &
             (!str_detect(Foodgroup, 'sausage') | !str_detect(food_item, 'alkaline cured|tofu') ))) %>%
  
  #Remove unnecessary dairy items
  #filter(!(Foodgroup %in% c('milk and milk based beverages','yoghurt') &
   #          !str_detect(food_item, 'milk, cultured, plain|kefir|kulturmelk|unspecified|cultured milk, plain, skyr|yoghurt, plain, biola|yoghurt, whole milk, plain'))) %>%

  #Remove unnecessary bread items
  #filter(!(Foodgroup %in% c('snacks', 'bread, rolls, industry made') &
   #          !(str_detect(food_item, 'salted sticks|tortilla chips|baguette, white|industrially made|bun|tortilla') &
    #             !str_detect(food_item, 'maarud|santa maria|old el paso') ))) %>%
  #Div
  #filter(!(str_detect(Foodgroup, 'sandwhich meats|sandwhich fish|cheese, reduced fat|miscellaneous ingredients') &
             #Keep certain food items
   #          !(str_detect(Foodgroup, 'sausage') | str_detect(food_item, 'tuna|salmon, smoked|mackerel fillet, in tomato sauce, 60 % mackerel, canned|cocoa powder|cod roe, compressed, canned|anchovies, canned|anchovy fillets, canned|herring, pickled, cured, marinated, drained|ham, boiled|ham, cured|ham, smoke-cured|sausage, cured, morr|cottage|ricotta|tofu|tahini|curry paste|coconut milk, canned|baking|bakers|mustard|taco spice|salt|vinegar|puff pastry') ))) %>%
  
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
    
    #lamb----
    food_item == 'lamb, for stewing, raw' ~ 'lamb_stew meat',
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
    str_detect(food_item, 'grapes') ~ str_replace(food_item, 'grapes', 'grape'),
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
    food_item == 'grape, unspecified, raw' ~ 'grape',
    
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
    food_item == 'flatbread, hard' ~ 'bread flat hard',
    food_item == 'bulgur, uncooked' ~ 'bulgur_wheat',
    food_item == 'cashew nuts, salted' ~ 'cashew nut salt',
    food_item == 'peanuts, raw' ~ 'peanut',
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
    food_item == 'rolls, white, industrially made' ~ 'rolls white',
    food_item == 'sesame seeds, without shell' ~ 'sesame_seed',
    food_item == 'tortilla chips' ~ 'nacho', 
    food_item == 'wheat flour, 80 % extraction' ~ 'wheat flour',
    food_item == 'wheat flour, wholemeal' ~ 'wheat flour_wholemeal',
    food_item == 'peanuts, raw' ~ 'peanut',
    food_item %in% c('hamburger bun', 'peanut butter', 'potato starch') ~ str_replace(food_item, ' ', '_'),
  
    str_detect(food_item, 'bread, semi-coarse') & str_detect(food_item, '25-50') & str_detect(food_item, 'industrially made') ~ 'bread',
    str_detect(food_item, 'bread, white') & str_detect(food_item, '0-25') & str_detect(food_item, 'industrially made') & !str_detect(food_item, 'spiral|square') ~ 'bread_white',
    str_detect(food_item, 'bread, coarse') & str_detect(food_item, '50-75') & str_detect(food_item, 'industrially made') ~ 'bread_coarse',
    food_item == 'rolls, white, industrially made' ~ 'roll_white',
    food_item == 'tortilla, corn flour, soft' ~ 'tortilla_corn',
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
    
  #Keep the unspecified ingredients
  str_detect(food_item, ', unspecified, raw') ~ str_replace(food_item, ', unspecified, raw', ''),
  
  #Remove 'raw' from certain ingredients----
  food_item %in% c('pineapple, raw', 'asparagus, raw', 'avocado, raw', 'banana, raw',
                   'blueberries, raw', 'catfish, raw', 'char, raw',
                   'cherries, raw', 'chives, raw', 'clementine, raw',
                   'coconut, raw', 'cranberries, raw',
                   'egg, raw', 'fennel, raw', 'garlic, raw', 'grapefruit, raw', 'haddock, raw',
                   'hare, raw', 'kale, raw', 'jerusalem artichoke, raw', 'cranberries, raw',
                   'egg, raw', 'lemon, raw', 'lime, raw', 'lychee, raw', 'orange, raw',
                   'parsnip, raw', 'pomegranate, raw', 'sweet potato, raw', 'rabbit, raw',
                   'raspberries, raw', 'redfish, raw', 'rhubarb, raw', 'scallop, raw',
                   'shallot, raw', 'strawberries, raw', 'spinach, raw', 'squid, raw',
                   'tuna, raw') ~ str_replace(food_item, ', raw', ''),
  
  #Some single food items to keep
  food_item %in% c('ghee') ~ food_item
  
    #TRUE ~ food_item----
    
  )) %>%
  
  mutate(Ingredients = case_when(
    #Turn seeds and nuts into singular form
    str_detect(food_item, 'nuts') ~ str_replace(food_item, 'nuts', 'nut'),
    #str_detect(food_item, 'seeds') ~ str_replace(food_item, 'seeds', 'seed'),
    
    #Some other fruits and vegetable
    Foodgroup %in% c('vegetables, raw and frozen', 'fruits and berries, raw') & str_detect(food_item, ', raw') & is.na(Ingredients) ~ str_replace(food_item, ', raw', ''),
    Foodgroup %in% c('vegetables, raw and frozen', 'fruits and berries, raw') & str_detect(food_item, ', ') & is.na(Ingredients) ~ str_replace(food_item, ', ', '_'),
    
    TRUE ~ Ingredients
  )) %>%
  
  mutate(Ingredients = str_replace(Ingredients, 'alfalfa seed, sprouted, raw', 'alfalfa_sprout')) %>%
    #Separate rows with multiple food items
    separate_rows(., Ingredients, sep = '/') %>%
  
  #Remove some resulting errors
  filter(!(food_item %in% c('sweet pepper, yellow/orange, raw') & Ingredients %in% c('orange'))) %>%
  
  rename(ID = FoodID) %>%
  mutate(ID = as.numeric(ID))

#Add missing food items from FoodData Central----
#Nutrients
fromFoodDataCentral_nutrients <- read_csv('./porsjoner_vekt_næringsinnhold/FoodData_Central_sr_legacy_food/food_nutrient.csv')
fromFoodDataCentral_nutrient_names <- read_csv('./porsjoner_vekt_næringsinnhold/FoodData_Central_sr_legacy_food/nutrient_incoming_name.csv')
#Food items
fromFoodDataCentral_foods <- read_csv('./porsjoner_vekt_næringsinnhold/FoodData_Central_sr_legacy_food/food.csv') %>%
  
  #Select missing foods
  filter(description %in% c(
    
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
    'Seaweed, agar, dried', 'Miso', 'Soup, onion, dry, mix', 'Alcoholic beverage, rice (sake)',
    'Shortening, vegetable, household, composite', 'Pickle relish, sweet', 'Syrups, maple',
    'Sauce, ready-to-serve, pepper, TABASCO', 'Tapioca, pearl, dry', 'Molasses', 'Vital wheat gluten')) %>%
  
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
           str_replace('Miso', 'miso') %>%
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
           str_replace('Vital wheat gluten', 'gluten')
         #'Tamarind nectar, canned'
         ) %>%
  
  #Get nutrient content from db
  #First get nutrient id's
  inner_join(., fromFoodDataCentral_nutrients, by = 'fdc_id') %>%
  #Select columns
  select(description, nutrient_id, amount) %>%
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
         Salt = Sodium*2.5
           ) %>%
  #Remove the columns
  select(-c(glucose, galactose, dextrose, maltose, sucrose, lactose, `alpha-tocopherol`, `gamma-tocopherol`, `beta-tocopherol`, `delta-tocopherol`)) %>%
  #Add zero for NA
  replace(is.na(.), 0) %>%
  
  #Create new ID column
  group_by(Ingredients) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(ID = ID + 100)

#Add to clean_nutrients df
clean_nutrients <- bind_rows(clean_nutrients, fromFoodDataCentral_foods %>% select(ID, Ingredients))

#Add composite ingredients----
various$component_ingredients_nutrients <- readRDS('composite_ingredients_nutrient_content.Rds') %>%
  #Create an ID column
  group_by(Ingredients) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(ID = ID + 200)

clean_nutrients <- bind_rows(clean_nutrients, various$component_ingredients_nutrients %>% select(ID, Ingredients)) %>%
  #Add a shellfish row
  add_row(ID = 10000, Ingredients = 'shellfish')

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
              values_from = value)
  
#Add the foodDataCentral and composite ingredients
nutrients_to_use <- nutrients_to_use %>%
  bind_rows(., fromFoodDataCentral_foods) %>%
  bind_rows(., various$component_ingredients_nutrients) %>%
  bind_rows(., various$shellfish)

saveRDS(nutrients_to_use, 'nutrients_df.Rds')
  
  
#Create a search reference----
  reference <- clean_nutrients %>%
    select(ID, Ingredients) %>%
    drop_na(Ingredients) %>%
    #Separate rows with /
    separate_rows(Ingredients, sep = '/') %>%
    
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
    arrange(first_word, second_word)
  
  
  saveRDS(reference, 'nutrient_reference.Rds')

#Sustainability indicators from SHARP----
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
                                       'hen eggs', 'boiled eggs', 'hen egg white', 'hen egg yolk', 'fried eggs',
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
                           "A0B9G",
                           #Div
                           "A026M"
                           
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
             str_replace('courgettes', 'zucchini') %>% #Name used in recipes
             
             #Meat
             str_replace('beef tallow including processed suet', 'tallow') %>%
             str_replace('bovine, minced meat', 'ground meat') %>% #Used in the norwegian recipes
             str_replace('pork lard', 'lard') %>%
             
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
             str_replace('tap water', 'water') %>%
             str_replace('pasta sauce', 'tomato sauce') #PAsta sauces are tomato sauces
           
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
    
    #Change 'and similar' to 'other'
    mutate(Ingredients = Ingredients %>%
             str_replace('and similar-', 'other') %>%
             str_replace('and similar', 'other')) %>%
    
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
  various$composite_ingredients_sharp <- readRDS('./composite_ingredients_sustainability_markers.Rds')
  
  #Add to SHARP
  SHARP <- full_join(SHARP, various$composite_ingredients_sharp)
    #Create a half-and-half row, half milk and half cream
  SHARP <- SHARP %>% 
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
                         'herrings', 'kales', 'lemons', 'limes', 'lettuces',
                         'mandarins', 'melons', 'mussels', 'nectarines', 'olives',
                         'onions', 'oranges', 'oysters', 'papayas', 'peanuts', 'pears',
                         'pistachios', 'plums', 'pumpkins', 'rhubarbs', 'salmons',
                         'shrimps', 'syrups', 'trouts', 'hazelnuts', 'walnuts',
                         'scallops', 'sardines', 'blackcurrants', 'redcurrants',
                         'leeks', 'shallots', 'shrimps', 'prawns', 'almonds', 'olives',
                         'grapes'
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
        Ingredients == 'ground meat' ~ 'meat',
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
        Ingredients == 'hard cheese' ~ 'hard cheese',
        Ingredients == 'hard to semi-hard cheese' ~ '\\',
        Ingredients == 'garden peas dry' ~ 'garden',
        Ingredients == 'sugar snap pea' ~ 'sugar snap',
        Ingredients == 'romaines' ~ 'romaine',
        Ingredients == 'sun-dried tomatoes' ~ 'sun-dried',
        Ingredients == 'cherry tomatoes' ~ 'tomato',
        Ingredients == 'wheat flour' ~ '\\',
        Ingredients == 'wheat wholemeal flour' ~ 'wholemeal',
        Ingredients == 'ground meat' ~ 'ground',
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
        TRUE ~ second_word
      )
    ) %>%
    
    filter(!first_word %in% c('juice', 'fresh')) %>% #These may otherwise cause issues when searching
    distinct(., first_word, second_word, .keep_all = TRUE) %>% #Remove duplicates. Due to splitting up rows with '/', certain ingredients occur multiple times
    #Remove unnecessary column
    select(-Ingredients) %>% arrange(first_word, second_word)
  #Is the maize flour in SHARP the same as corn starch?

  #Save
  saveRDS(sharp_ref, 'sharp_ref.Rds')

  