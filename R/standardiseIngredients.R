#' Standardising ingredients names in a recipe
#' @title standardiseIngredients
#'
#' @description Standardise ingredient names and adds them in a new column called "Ingredients_standardised".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows.
#'
#' @return The dataframe with a new column "Ingredients_standardised".
#'
#' @export
standardiseIngredients <- function(df){
  df  %>%
    
    #Turn ingredients to lowercase
    mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      
      #Vegetables/fruit----
      str_detect(Ingredients, 'acorn squash') ~ 'winter squash acorn',
      str_detect(Ingredients, 'apple') & !str_detect(Ingredients, 'juice|vinegar|butter|pine|wasabi|sauce|syrup|muesli') ~ 'apple',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'sauce') ~ 'apple sauce',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'pine') ~ 'apple juice',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'syrup') ~ 'syrup apple',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'dried') ~ 'apricot dried',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'jam') ~ 'apricot jam',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'nectar') ~ 'apricot nectar',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'preserve') ~ 'apricot preserve',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'drained') ~ 'apricot canned',
      str_detect(Ingredients, 'apricot')  ~ 'apricot',
      str_detect(Ingredients, 'artichoke') & str_detect(Ingredients, 'heart') & str_detect(Ingredients, 'drain|can') ~ 'artichoke heart canned',
      str_detect(Ingredients, 'earth shocks') ~ 'artichoke',
      str_detect(Ingredients, 'asparagus') & str_detect(Ingredients, 'white') ~ 'asparagus white',
      str_detect(Ingredients, 'asparagus') & str_detect(Ingredients, 'bean') ~ 'bean green asparagus',
      str_detect(Ingredients, 'asparagus') ~ 'asparagus',
      str_detect(Ingredients, 'avocado') & !str_detect(Ingredients, 'wok') ~ 'avocado',
      
      str_detect(Ingredients, 'banan') & !str_detect(Ingredients, 'shallot') ~ 'banana',
      str_detect(Ingredients, 'beet|better') & str_detect(Ingredients, 'yellow') ~ 'beetroot yellow',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'cooked|boiled') ~ 'beetroot cooked',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'root') & !str_detect(Ingredients, 'pickle') ~ 'beetroot',
      str_detect(Ingredients, 'curran') & str_detect(Ingredients, 'jam') ~ 'jam currant',
      str_detect(Ingredients, 'black curran') & !str_detect(Ingredients, 'juice') ~ 'black currant',
      str_detect(Ingredients, 'currant') & !str_detect(Ingredients, 'juice') ~ 'black currant', #Use as default
      str_detect(Ingredients, 'blueberr') & str_detect(Ingredients, 'pie fill') ~ 'blueberries pie filling',
      str_detect(Ingredients, 'blueberr') & str_detect(Ingredients, 'jam') ~ 'jam blueberries',
      str_detect(Ingredients, 'blueberr') ~ 'blueberries',
      str_detect(Ingredients, 'broccolini') ~ 'broccolini',
      str_detect(Ingredients, 'broccoli') ~ 'broccoli', #The 'broccoli peas' are broccoli florets according to the recipe site
      str_detect(Ingredients, 'brussel') & str_detect(Ingredients, 'sprout') ~ 'brussel sprout',
      str_detect(Ingredients, 'butternut') ~ 'winter squash butternut',
      
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'napa') ~ 'cabbage napa',
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'red') & !str_detect(Ingredients, 'shred') ~ 'cabbage red',
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'china|chinese') ~ 'cabbage china',
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'savoy') ~ 'cabbage savoy',
      str_detect(Ingredients, 'cabbage') & !str_detect(Ingredients, 'meat|root') ~ 'cabbage',
      str_detect(Ingredients, 'bok choi|bok choy') ~ 'cabbage bok choi',
      str_detect(Ingredients, 'carrot|raw yellow') & !str_detect(Ingredients, 'paste|wok|mire') ~ 'carrot',
      str_detect(Ingredients, 'cauliflower') & !str_detect(Ingredients, 'butter') ~ 'cauliflower',
      str_detect(Ingredients, 'celery|cellery') & !str_detect(Ingredients, 'salt|soup|seed') ~ 'celery', #Use celery for stangselleri
      str_detect(Ingredients, 'celeriac') & !str_detect(Ingredients, 'mire') ~ 'celariac root',
      str_detect(Ingredients, 'chard') & !str_detect(Ingredients, 'wine') ~ 'mangold',
      str_detect(Ingredients, 'cherry tomato') & str_detect(Ingredients, 'can') ~ 'cherry tomato canned',
      str_detect(Ingredients, 'cherry tomato') ~ 'cherry tomato',
      str_detect(Ingredients, 'cherry|cherries') & str_detect(Ingredients, 'can|in syrup') & !str_detect(Ingredients, 'tomato') ~ 'cherries canned', #Name used in SHARP and Matvaretabellen
      str_detect(Ingredients, 'cherry|cherries') & !str_detect(Ingredients, 'tomato') ~ 'cherries', #Name used in SHARP and Matvaretabellen
      str_detect(Ingredients, 'chicory') & str_detect(Ingredients, 'white') ~ 'chicory white',
      str_detect(Ingredients, 'chicory') & str_detect(Ingredients, 'red') ~ 'chicory red',
      str_detect(Ingredients, 'chicory') ~ 'chicory',
      str_detect(Ingredients, 'jalap') ~ 'chili pepper jalapeno',
      str_detect(Ingredients, 'chili|chile') & str_detect(Ingredients, 'green') ~ 'chili pepper green',
      ((str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'chili')) | (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'red|rød'))) & !str_detect(Ingredients, 'powder') |
        str_detect(Ingredients, 'mild chili') & !str_detect(Ingredients, 'sauce') | str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'chop') |
        str_detect(Ingredients, 'chili') & str_detect(Amounts, 'stk') |
        str_detect(Ingredients, 'red') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'hot') & str_detect(Ingredients, 'slice') |
        str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'seed') ~ 'chili pepper red',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'dried') & !str_detect(Ingredients, 'flake') ~ 'chili pepper dried',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'pickle') ~ 'chili pepper pickled',
      str_detect(Ingredients, 'clemen') ~ 'clementine',
      str_detect(Ingredients, 'coconut') & !str_detect(Ingredients, 'milk|cream|oil') ~ 'coconut',
      str_detect(Ingredients, 'minima|baby corn') ~ 'corn baby',
      str_detect(Ingredients, 'corn') & str_detect(Amounts, 'g') & str_detect(Ingredients, 'can') |
        str_detect(Ingredients, 'kernel') & str_detect(Ingredients, 'drained') |
        str_detect(Ingredients, 'sweetcorn') & str_detect(Ingredients, 'canned') ~ 'sweet corn kernels canned',
      str_detect(Ingredients, 'corn') & str_detect(Amounts, 'g') & !str_detect(Ingredients, 'starch|tortilla|oil') | str_detect(Ingredients, 'corn kernel') ~ 'sweet corn kernels',
      str_detect(Ingredients, 'corn') & str_detect(Amounts, 'stk') & !str_detect(Ingredients, 'pepper') ~ 'corn cob',
      str_detect(Ingredients, 'cranberr') & str_detect(Ingredients, 'jam') ~ 'cranberries jam',
      str_detect(Ingredients, 'cranberr') & !str_detect(Ingredients, 'sauce') ~ 'cranberries',
      str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'snake') ~ 'cucumber snake',
      str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'jam|pickle') ~ 'cucumber pickled',
      str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'sandwichspread') ~ 'sandwichspread cucumber',
      str_detect(Ingredients, 'cucumber') ~ 'cucumber',
      
      str_detect(Ingredients, 'eggplant|aubergine') ~ 'eggplant',
      
      str_detect(Ingredients, 'fennel') & !str_detect(Ingredients, 'seed') ~ 'fennel',
      str_detect(Ingredients, 'fig') ~ 'fig',
      
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'chinese') ~ 'garlic chinese',
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'wild') ~ 'garlic wild',
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'powder|granule') ~ 'garlic powder',
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'whole') & !str_detect(Ingredients, 'salt|powder') ~ 'whole garlic',
      str_detect(Ingredients, 'garlic') & !str_detect(Ingredients, 'pickle|sauce|paste|oil|baguette|cheese|salt') ~ 'garlic',
      str_detect(Ingredients, 'grape') & str_detect(Ingredients, 'juice') ~ 'grape juice',
      str_detect(Ingredients, 'grape') ~ 'grape',
      
      str_detect(Ingredients, 'horseradish|horse raddish') & !str_detect(Ingredients, 'sauce') ~ 'horseradish',
      
      str_detect(Ingredients, 'jerusalem artichoke') ~ 'jerusalem artichoke',
      
      str_detect(Ingredients, 'kale') ~ 'kale',
      str_detect(Ingredients, 'kimchi') ~ 'kimchi',
      str_detect(Ingredients, 'kiwi') ~ 'kiwi',
      
      str_detect(Ingredients, 'leek') & !str_detect(Ingredients, 'mire|onion|tortilla') ~ 'leek',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'grass') ~ 'lemongrass',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'peel|zest') ~ 'lime, the juice and zest',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'peel|sheel|zest') & !str_detect(Ingredients, 'juice') ~ 'lime, the zest',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice|pressed') & !str_detect(Ingredients, 'peel') ~ 'lime, the juice',
      str_detect(Ingredients, 'lime') & str_detect(Amounts, 'tsp|tbsp|dl') ~ 'lime, the juice',
      str_detect(Ingredients, 'lime') & !str_detect(Ingredients, 'sheet|lemon|leaf|beverage') ~ 'lime',
      str_detect(Ingredients, 'lingonberr') & str_detect(Ingredients, 'jam') ~ 'lingonberry jam',
      str_detect(Ingredients, 'lingonberr') ~ 'lingonberry',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'curd') ~ 'curd lemon',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'peel|zest|shell') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice and zest',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice',
      str_detect(Ingredients, 'lemon') & (str_detect(Amounts, 'tsp|tbsp|dl') | str_detect(Ingredients, 'drop')) & !str_detect(Ingredients, 'peel|shell|zest') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'shell|peel|zest') & !str_detect(Ingredients, 'pepper') | str_detect(Ingredients, 'sitronskall') ~ 'lemon, the zest',
      str_detect(Ingredients, 'of lime or lemon') ~ 'lemon', #Recipe has lemon in the name
      str_detect(Ingredients, 'lemon') & !str_detect(Ingredients, 'lime|balm|pepper|beverage') ~ 'lemon',
      str_detect(Ingredients, 'lychee') ~ 'lychee',
      
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'lettuce') ~ 'salad lamb lettuce',
      
      str_detect(Ingredients, 'mango') & !str_detect(Ingredients, 'chutney') ~ 'mango',
      
      str_detect(Ingredients, 'tamarind juice') ~ 'tamarind juice',
      str_detect(Ingredients, 'tomat') & (str_detect(Amounts, 'can|box|hp') | str_detect(Ingredients, 'can|box|drain')) & !str_detect(Ingredients, 'water|mackerel|beans|sauce|soup') ~ 'tomato canned',
      str_detect(Ingredients, 'tomat') & !str_detect(Ingredients, 'canned|alsa|sauce|ketchup|canned|cherry|can|box|purée|puree|paste|mackerel|tube|vegetable|sun|beans|bunch') ~ 'tomato',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'pur') ~ 'tomato puree',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'bunch') ~ 'tomato bunch',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'sun') ~ 'tomato sun dried',
      str_detect(Ingredients, 'ketchup') ~ 'tomato ketchup',
      str_detect(Ingredients, 'turnip') ~ 'turnip',
      
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'black|kalamata') ~ 'olive black',
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'green') ~ 'olive green',
      str_detect(Ingredients, 'olive') & !str_detect(Ingredients, 'oil|tapenade') ~ 'olive green',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'pickle') ~ 'onion pickled',
      str_detect(Ingredients, 'onion') & !str_detect(Ingredients, 'pickle|spring|green|pearl|leek|mire|garlic|powder|soup|bread|seed') ~ 'onion',
      str_detect(Ingredients, 'pearl onion') & str_detect(Ingredients, 'pickle') ~ 'pearl onion pickled',
      str_detect(Ingredients, 'pearl onion') ~ 'pearl onion',
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'zest|peel|shell') ~ 'orange, the juice and zest',
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'shell|zest|peel') ~ 'orange, the zest',
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') ~ 'orange juice',
      str_detect(Ingredients, 'orange') ~ 'orange',
      
      str_detect(Ingredients, 'parsley') & str_detect(Ingredients, 'root') ~ 'parsley root',
      str_detect(Ingredients, 'parsnip') ~ 'parsnip',
      str_detect(Ingredients, 'pea') & !str_detect(Ingredients, 'chick|broccoli|nut|sugar|asparagus|onion|pearl|horse|peach|dill|pear') ~ 'peas green',
      str_detect(Ingredients, 'peach') & str_detect(Ingredients, 'can') ~ 'peach canned',
      str_detect(Ingredients, 'peach') ~ 'peach',
      str_detect(Ingredients, 'pear') & str_detect(Ingredients, 'sirup|syrup') & !str_detect(Ingredients, 'onion|barley') ~ 'syrup pear',
      str_detect(Ingredients, 'pear') & !str_detect(Ingredients, 'onion|barley') ~ 'pear',
      str_detect(Ingredients, 'pineapple') & str_detect(Ingredients, 'can') ~ 'pineapple canned',
      str_detect(Ingredients, 'pineapple') & str_detect(Ingredients, 'juice') ~ 'pineapple juice',
      str_detect(Ingredients, 'pineapple') ~ 'pineapple',
      str_detect(Ingredients, 'plantain') ~ 'plantain',
      str_detect(Ingredients, 'pomegranat') & str_detect(Ingredients, 'kernel') ~ 'pomegranate kernel',
      str_detect(Ingredients, 'pomegranat') ~ 'pomegranate',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'starch') ~ 'potato starch',
      str_detect(Ingredients, 'potato') & !str_detect(Ingredients, 'rice|bread|sweet|mash|flour|mash') ~ 'potato',
      str_detect(Ingredients, 'buttery') & str_detect(Ingredients, 'mash') ~ 'potato mash buttery',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'mash') & !str_detect(Ingredients, 'cook|boil') ~ 'potato mash',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'cook|boil') ~ 'potato boiled',
      str_detect(Ingredients, 'pumpkin') & !str_detect(Ingredients, 'seed|butternut') ~ 'winter squash pumpkin',
      str_detect(Ingredients, 'prune') ~ 'prune',
      
      str_detect(Ingredients, 'radish') & str_detect(Ingredients, 'daikon') ~ 'radish daikon',
      str_detect(Ingredients, 'radish') & !str_detect(Ingredients, 'horse') ~ 'radish',
      str_detect(Ingredients, 'raisin') & !str_detect(Ingredients, 'brais|flour') ~ 'raisin',
      str_detect(Ingredients, 'raspberry') & str_detect(Ingredients, 'jam') ~ 'raspberries jam',
      str_detect(Ingredients, 'raspbe') ~ 'raspberries',
      str_detect(Ingredients, 'rhubarb') & str_detect(Ingredients, 'juice') ~ 'rhubarb juice',
      str_detect(Ingredients, 'rhubarb') ~ 'rhubarb',
      
      str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'heart') ~ 'salad heart',
      str_detect(Ingredients, 'ruccula|rocket salad|rocket|arugula|ruccola|peppery salad') ~ 'salad rocket',
      str_detect(Ingredients, 'lettuce') & !str_detect(Ingredients, 'lamb')  ~ 'salad lettuce',
      str_detect(Ingredients, 'salad') &str_detect(Ingredients, 'crispi') ~ 'salad crispi',
      str_detect(Ingredients, 'lollo rosso') ~ 'salad lollo rosso',
      str_detect(Ingredients, 'salad') & !str_detect(Ingredients, 'shrimp|oil') ~ 'salad',
      str_detect(Ingredients, 'sauerkraut') ~ 'sauerkraut',
      str_detect(Ingredients, 'scallion|green onion|spring onion') ~ 'scallion',
      str_detect(Ingredients, 'shallot') ~ 'shallot',
      str_detect(Ingredients, 'sorrel') ~ 'sorrel',
      str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'baby') ~ 'spinach baby',
      str_detect(Ingredients, 'spinach') ~ 'spinach',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'alfalfa') ~ 'sprouts alfalfa',
      str_detect(Ingredients, 'strawberr') & str_detect(Ingredients, 'jam') ~ 'jam strawberry',
      str_detect(Ingredients, 'strawberr') ~ 'strawberries',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'pea') ~ 'sugar snap pea',
      str_detect(Ingredients, 'swede') | (str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'root')) | str_detect(Ingredients, 'rutabaga') ~ 'swede',
      str_detect(Ingredients, 'pickled red pepper') ~ 'sweet pepper pickled',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') & str_detect(Ingredients, 'green') ~ 'sweet pepper green grilled',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') & str_detect(Ingredients, 'can') ~ 'sweet pepper grilled canned',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') ~ 'sweet pepper grilled',
      str_detect(Ingredients, 'sweet pepper') & str_detect(Ingredients, 'can|drain') ~ 'sweet pepper canned',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'green') | str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'green') & str_detect(Ingredients, 'slice|deseed|strips') ~ 'sweet pepper green',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'yellow') | str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'yellow') & str_detect(Ingredients, 'slice|deseed') ~ 'sweet pepper yellow',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & !str_detect(Ingredients, 'grilled|pickled|powder|spice|smoked') | str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'red') & str_detect(Ingredients, 'slice|deseed|chopped') & !str_detect(Ingredients, 'hot') ~ 'sweet pepper red',
      str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'potato') ~ 'sweet potato',
      str_detect(Ingredients, 'squash|zucchini|courgette') & !str_detect(Ingredients, 'butter|acorn') ~ 'summer squash zucchini', #Standard
      
      str_detect(Ingredients, 'watermelon') ~ 'watermelon',
      str_detect(Ingredients, 'water chestnut') ~ 'water chestnut',
      
      #Dairy / substitutes----
      str_detect(Ingredients, 'ghee|clarified butter') ~ 'butter clarified ghee',
      str_detect(Ingredients, 'buttercream') ~ 'buttercream',
      str_detect(Ingredients, 'spice') & str_detect(Ingredients, 'butter') ~ 'spice butter',
      str_detect(Ingredients, 'butter|smør') & str_detect(Ingredients, 'unsalted|usalted') ~ 'unsalted butter',
      str_detect(Ingredients, 'butter|smør') & !str_detect(Ingredients, 'frying|dough|unsalted|browning|brushing|pepper|sour cream|roasting|butternut|pastry|greasing|milk|beans|peanut|vanilla aroma') ~ 'butter',
      str_detect(Ingredients, 'butter') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'butter for cooking',
      str_detect(Ingredients, 'buttermilk') & !str_detect(Ingredients, 'dough') ~ 'buttermilk',
      
      str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'norman') ~ 'cheese blue normanna', #Tine cheese
      str_detect(Ingredients, 'norzola') ~ 'cheese blue norzola',
      str_detect(Ingredients, 'gorgonzola') ~ 'cheese gorgonzola',
      str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cotija') ~ 'cheese cotjia',
      str_detect(Ingredients, 'stilton') & str_detect(Ingredients, 'cheese') ~ 'cheese blue stilton',
      str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'blue') ~ 'cheese blue',
      str_detect(Ingredients, 'brie') ~ 'cheese brie',
      str_detect(Ingredients, 'camembert') ~ 'cheese camembert',
      str_detect(Ingredients, 'real goat cheese') ~ 'goat brown cheese',
      str_detect(Ingredients, 'cheddar') ~ 'cheese cheddar',
      str_detect(Ingredients, 'garlic cheese') ~ 'cheese garlic',
      str_detect(Ingredients, 'gruyère|gruyere') ~ 'cheese gruyere',
      str_detect(Ingredients, 'chevre') ~ 'cheese goat chevre white',
      str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'cheese') & !str_detect(Ingredients, 'hard') ~ 'cheese goat',
      str_detect(Ingredients, 'feta|fat cheese in cubes|semi-solid cheese in cubes') & str_detect(Ingredients, 'cheese') | str_detect(Ingredients, 'feta') & str_detect(Ingredients, 'crumbled') ~ 'cheese feta', #Fat cheese is a translation error from Norwegian
      str_detect(Ingredients, 'halloumi') ~ 'cheese halloumi',
      str_detect(Ingredients, 'jarlsberg') ~ 'cheese jarlsberg',
      str_detect(Ingredients, 'manchego') ~ 'cheese manchego',
      str_detect(Ingredients, 'mascarpone') ~ 'cheese mascarpone',
      str_detect(Ingredients, 'mozzarella') ~ 'cheese mozzarella',
      str_detect(Ingredients, 'norvegia') ~ 'cheese norvegia',
      str_detect(Ingredients, 'port salut') ~ 'cheese port salut',
      str_detect(Ingredients, 'ricotta') ~ 'cheese ricotta salata',
      str_detect(Ingredients, 'romano') & str_detect(Ingredients, 'cheese') ~ 'cheese romano',
      str_detect(Ingredients, 'american cheese') ~ 'cheese american',
      str_detect(Ingredients, 'swiss') & str_detect(Ingredients, 'cheese') ~ 'cheese swiss',
      str_detect(Ingredients, 'provolone') & str_detect(Ingredients, 'cheese') ~ 'cheese provolone',
      str_detect(Ingredients, 'monterey jack|pepperjack') & str_detect(Ingredients, 'cheese') ~ 'cheese monterey jack',
      str_detect(Ingredients, 'neufchatel') & str_detect(Ingredients, 'cheese') ~ 'cheese neufchatel',
      str_detect(Ingredients, 'asiago') & str_detect(Ingredients, 'cheese') ~ 'cheese asiago',
      str_detect(Ingredients, 'pecorino') & str_detect(Ingredients, 'cheese') ~ 'cheese pecorino',
      str_detect(Ingredients, 'emmentaler') & str_detect(Ingredients, 'cheese') ~ 'cheese emmentaler',
      str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'hard') ~ 'cheese hard goat',
      str_detect(Ingredients, 'snøfrisk|snow fresh') ~ 'cheese cream goat snøfrisk',
      str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cream') | str_detect(Ingredients, 'kremgo') ~ 'cheese cream',
      (str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'skinny|low fat|lean|mager')) | str_detect(Ingredients, 'paneer cheese') ~ 'cheese cottage low fat', #Paneer is a cheese like low fat cc
      str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'cheese') ~ 'cheese cottage',
      str_detect(Ingredients, 'parmesan') ~ 'parmesan cheese',
      str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'soft') ~ 'cheese soft',
      str_detect(Ingredients, 'cheese') & !str_detect(Ingredients, 'yogurt|yoghurt') ~ 'cheese semi-hard',
      
      str_detect(Ingredients, 'whip') & str_detect(Ingredients, 'it|stabilizer') ~ 'whip it stabilizer',
      str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'double') ~ 'cream double 48 %',
      str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'whip|heavy') | str_detect(Ingredients, 'whipped topping') ~ 'cream whipped 37 %',
      str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'ice') & str_detect(Ingredients, 'vanilla') ~ 'ice cream vanilla',
      str_detect(Ingredients, 'cream') & (str_detect(Ingredients, 'food') | !str_detect(Ingredients, 'cheese|sour|cracker|sauce|coconut|light|condensed|ice')) ~ 'cream household 18 %', #Standard
      str_detect(Ingredients, 'crème fraîche 18 %') ~ 'crème fraîche 18 %',
      str_detect(Ingredients, 'crème fraîche 10 %') ~ 'crème fraîche 10 %',
      str_detect(Ingredients, 'crème fraîche|creme fraiche') ~ 'crème fraîche 35 %', #The original
      
      str_detect(Ingredients, 'kefir') ~ 'kefir',
      str_detect(Ingredients, 'kesam|tine light 2%') & str_detect(Ingredients, 'low fat|1 %') ~ 'quark, 1 %',
      str_detect(Ingredients, 'kesam') ~ 'quark, 7 %',
      
      str_detect(Ingredients, 'margarin') & str_detect(Ingredients, 'frying') ~ 'margarine for cooking',
      str_detect(Ingredients, 'margarin') ~ 'margarine',
      str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'milk') ~ 'dairy imitate oatmilk',
      str_detect(Ingredients, 'milk|tinemelk') & !str_detect(Ingredients, 'whole|full-fat|coconut|butter|extra|almond|soy|evaporated|powder|condensed|chocolate') ~ 'milk 1 %', #Standard
      str_detect(Ingredients, 'milk|melk') & str_detect(Ingredients, 'whole|full-fat') ~ 'whole milk 3.5 %',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'extra light|skim milk') ~ 'milk 0.1 %',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'evaporated|condensed') ~ 'milk evaporated',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'coconut') ~ 'milk coconut',
      str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'coconut') ~ 'milk coconut cream full fat',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'powder') & str_detect(Ingredients, 'nonfat') ~ 'milk powder nonfat',
      
      str_detect(Ingredients, 'lightroom 10 %|light flow 10 %') | str_detect(Ingredients, 'sour') & str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'light') ~ 'sour cream 10 %',
      str_detect(Ingredients, 'lightroom 18 %|light stream|light flow 18%|lightroom 18%|light flow 18 %|light rømme') | (str_detect(Ingredients, 'sour cream|soured cream') & !str_detect(Ingredients, '%')) | (str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'light') & !str_detect(Ingredients, 'alternativ')) ~ 'sour cream 18 %', #Standard
      str_detect(Ingredients, 'seatroom|seat cream') | str_detect(Ingredients, 'sour') & str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'full fat') ~ 'sour cream 35 %',
      
      str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'greek') ~ 'yoghurt greek',
      str_detect(Ingredients, 'yogurt|yoghurt') ~ 'yoghurt',
      
      #Herbs and spices----
      str_detect(Ingredients, 'adobo seasoning') ~ 'adobo seasoning',
      str_detect(Ingredients, 'allspice|of all kinds') ~ 'allspice',
      str_detect(Ingredients, 'anise') &str_detect(Ingredients, 'extract') ~ 'anise extract',
      str_detect(Ingredients, 'anis') & !str_detect(Ingredients, 'star') ~ 'anise ground',
      
      str_detect(Ingredients, 'basil') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') |
                                            str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'basil fresh herbs',
      str_detect(Ingredients, 'thaibasil') ~ 'basil fresh herbs',
      str_detect(Ingredients, 'basil') & !str_detect(Ingredients, 'pesto') ~ 'basil dried', #Standard
      str_detect(Ingredients, 'bay leaf') ~ 'bay leaf',
      str_detect(Ingredients, 'burrito spice') ~ 'burrito spice mix',
      
      str_detect(Ingredients, 'cajun') & str_detect(Ingredients, 'spice') ~ 'cajun spice',
      str_detect(Ingredients, 'caraway') ~ 'caraway seed',
      str_detect(Ingredients, 'cardamom') & str_detect(Ingredients, 'fruit|pod|capsule') ~ 'cardamom pod',
      str_detect(Ingredients, 'cardamom') ~ 'cardamom',
      str_detect(Ingredients, 'celery') & str_detect(Ingredients, 'seed') ~ 'celery seed',
      str_detect(Ingredients, 'chervil') & !str_detect(Ingredients, 'parsley|rosemary|thyme|mint|basil|oregano|dill|coriander|tarragon') ~ 'chervil fresh herb', #All chervils are fresh
      str_detect(Ingredients, 'chives') ~ 'chives fresh herb', #All chives are fresh
      (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'dried') & str_detect(Ingredients, 'flake')) | str_detect(Ingredients, 'red pepper flate|red pepper flake') | str_detect(Ingredients, 'chili flake') ~ 'chili flake dried',
      str_detect(Ingredients, 'chinese') & str_detect(Ingredients, 'spice') ~ 'chinese five spice',
      str_detect(Ingredients, 'cinnamon') & str_detect(Ingredients, 'bar|rod|stick') ~ 'cinnamon bar',
      str_detect(Ingredients, 'cinnamon') & !str_detect(Ingredients, 'muesli') ~ 'cinnamon',
      str_detect(Ingredients, 'cloves|carnation') & !str_detect(Ingredients, 'garlic') ~ 'cloves',
      str_detect(Ingredients, 'coriander') & str_detect(Ingredients, 'seed') ~ 'coriander seed',
      str_detect(Ingredients, 'coriander|cilantro') & !str_detect(Ingredients, 'seed') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf|malt') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'coriander fresh herbs',
      str_detect(Ingredients, 'coriander|cilantro') ~ 'coriander dried', #Standard
      str_detect(Ingredients, 'cress') ~ 'cress fresh herbs',
      str_detect(Ingredients, 'cumin') ~ 'cumin',
      str_detect(Ingredients, 'curry') & !str_detect(Ingredients, 'paste') ~ 'curry powder',
      
      str_detect(Ingredients, 'dill') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'dill fresh herbs',
      str_detect(Ingredients, 'dill') ~ 'dill dried', #Standard
      
      str_detect(Ingredients, 'fajita') & str_detect(Ingredients, 'spice') ~ 'fajita spice mix',
      str_detect(Ingredients, 'fennel') & str_detect(Ingredients, 'seed') ~ 'fennel seed',
      str_detect(Ingredients, 'fenugreek leaf') & str_detect(Ingredients, 'dried') ~ 'fenugreek leaf dried',
      str_detect(Ingredients, 'fenugreek seed') ~ 'fenugreek seed',
      
      str_detect(Ingredients, 'garam') ~ 'garam masala',
      str_detect(Ingredients, 'pav bhaji masala') ~ 'pav bhaji masala',
      Ingredients == 'italian seasoning' ~ 'italian seasoning',
      str_detect(Ingredients, 'ginger') & (str_detect(Ingredients, 'fresh|grated|chopped') | str_detect(Amounts, 'cm')) ~ 'fresh herbs ginger',
      str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'pickle') ~ 'ginger pickled',
      str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'paste') ~ 'paste ginger',
      str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'syrup') ~ 'syrup ginger',
      str_detect(Ingredients, 'ginger') & !str_detect(Ingredients, 'bread') ~ 'dried ginger',
      str_detect(Ingredients, 'zedoari') ~ 'ginger zedoari', #In the same family
      str_detect(Ingredients, 'guacamole spice') ~ 'guacamole spice mix',
      
      str_detect(Ingredients, 'juniper') ~ 'juniper berry',
      
      str_detect(Ingredients, 'lemon balm') ~ 'lemon balm',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'leaf|leaves|sheet') ~ 'lime leaf',
      
      str_detect(Ingredients, 'marjoram') & str_detect(Ingredients, 'twig|chopped') ~ 'marjoram fresh herbs',
      str_detect(Ingredients, 'mint') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'mint fresh herbs',
      str_detect(Ingredients, 'mint') ~ 'mint dried', #Standard
      
      str_detect(Ingredients, 'nutmeg') ~ 'nutmeg',
      
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'powder') ~ 'onion powder',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'seed') ~ 'onion seed',
      str_detect(Ingredients, 'oregano') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'oregano fresh herbs',
      str_detect(Ingredients, 'oregano') & str_detect(Ingredients, 'dried|spice') ~ 'oregano dried',
      str_detect(Ingredients, 'oregano') ~ 'oregano dried', #Standard
      
      str_detect(Ingredients, 'paprika|pepper') & str_detect(Ingredients, 'powder|spice') & !str_detect(Ingredients, 'spice seasoning pepper') ~ 'paprika powder',
      str_detect(Ingredients, 'paprika') & str_detect(Ingredients, 'smoked') ~ 'paprika powder smoked',
      str_detect(Ingredients, 'parsley|mug') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'parsley fresh herbs',
      str_detect(Ingredients, 'parsley') ~ 'parsley dried', #Standard
      
      str_detect(Ingredients, 'rosemary') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'rosemary fresh herbs',
      str_detect(Ingredients, 'rosemary') ~ 'rosemary dried', #Standard
      
      str_detect(Ingredients, 'saffron') ~ 'saffron',
      str_detect(Ingredients, 'sage') & !str_detect(Ingredients, 'sau') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'sage fresh herbs',
      str_detect(Ingredients, 'sage') & !str_detect(Ingredients, 'sau') ~ 'sage dried', #Standard
      str_detect(Ingredients, 'salvie') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'salvie fresh herbs',
      str_detect(Ingredients, 'salvie') ~ 'salvie dried', #Standard
      str_detect(Ingredients, 'sazon seasoning') ~ 'sazon seasoning',
      str_detect(Ingredients, 'star anis') ~ 'star anise',
      str_detect(Ingredients, 'summer savory') ~ 'summer savory fresh herbs',
      
      str_detect(Ingredients, 'taco') & str_detect(Ingredients, 'spice|season') ~ 'taco spice mix',
      str_detect(Ingredients, 'tandoori') & !str_detect(Ingredients, 'paste') ~ 'tandoori spice mix',
      str_detect(Ingredients, 'tarragon') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'tarragon fresh herbs',
      str_detect(Ingredients, 'tarragon') & str_detect(Ingredients, 'dried') ~ 'tarragon dried',
      str_detect(Ingredients, 'tarragon') & !str_detect(Ingredients, 'parsley|rosemary|thyme|chervil|mint|basil|chives|dill|coriander') ~ 'tarragon dried', #Standard
      str_detect(Ingredients, 'thyme') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'thyme fresh herbs',
      str_detect(Ingredients, 'thyme') ~ 'thyme dried', #Standard
      str_detect(Ingredients, 'turmeric') ~ 'turmeric',
      
      #Mushrooms----
      str_detect(Ingredients, 'aroma') & str_detect(Ingredients, 'champignon|mushroom|soup') ~ 'mushroom aroma champignon',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'porcini') & str_detect(Ingredients, 'dried') ~ 'mushroom porcini dried',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'dry|dried') ~ 'mushroom dried',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'shiitake|shitake') ~ 'mushroom shiitake',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'shimeji') ~ 'mushroom shimeji',
      str_detect(Ingredients, 'portebello') ~ 'mushroom portebello',
      str_detect(Ingredients, 'chanterelle') ~ 'mushroom chanterelle',
      str_detect(Ingredients, 'champig') ~ 'mushroom champignon',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'chestnut') ~ 'mushroom chestnut',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'drained') ~ 'mushroom canned',
      str_detect(Ingredients, 'mushroom') & !str_detect(Ingredients, 'rice|condensed') ~ 'mushroom',
      
      #Flours, grains/nuts/legumes----
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'flour') ~ 'almond flour',
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'extract') ~ 'almond extract',
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'milk') ~ 'almond milk',
      str_detect(Ingredients, 'almond') & !str_detect(Ingredients, 'potato') ~ 'almond',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'cinnamon') & str_detect(Ingredients, 'muesli') ~ 'muesli apple-cinnamon',
      
      str_detect(Ingredients, 'bagel') ~ 'rolls white bagel',
      str_detect(Ingredients, 'rolls|poppyseed hot dog bun') ~ 'rolls white',
      str_detect(Ingredients, 'baguette') & str_detect(Ingredients, 'garlic') ~ 'rolls white baguette garlic',
      str_detect(Ingredients, 'baguette') ~ 'rolls white baguette',
      str_detect(Ingredients, 'canned chickpeas or mixed beans') | str_detect(Ingredients, 'chickpea|chick pea|garbanzo bean') & str_detect(Ingredients, 'drain') ~ 'chick pea canned', #Chickpeas are in the name of the recipe
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'sprout') ~ 'bean sprout',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'ferment') & str_detect(Ingredients, 'black') & str_detect(Ingredients, 'rinse|drain') ~ 'bean black canned fermented',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'black') &
        (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean black canned',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|navy|cannellini|butter') &
        (str_detect(Ingredients, 'can|box|carton|drained|boiled') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean white canned',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'kidney|red') &
        (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean kidney canned',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'black') ~ 'bean black',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|navy|cannellini|butter') ~ 'bean white',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'kidney|red') ~ 'bean kidney',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'green|french|break|snap') ~ 'bean green',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'horse|broad|fava|brew') ~ 'bean broad',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'tomat') ~ 'bean white tomato',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'can') ~ "beans'n'pork canned",
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'can') ~ 'bean canned',
      
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'digestive') | Ingredients %in% c('wheat biscuits', 'tea biscuit') ~ 'biscuit digestive', #Digestive biscuits can be used as "tea biscuits" as a the bottom layer
      str_detect(Ingredients, 'speculaas|speculoos') & str_detect(Ingredients, 'spread') ~ 'spread speculaas',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'speculaas|speculoos') ~ 'biscuit speculaas',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'plain') ~ 'biscuit plain',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'ginger') ~ 'biscuit gingerbread',
      str_detect(Ingredients, 'kruidnoten') ~ 'biscuit kruidnoten',
      
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'rye') & str_detect(Ingredients, 'crumb') ~ 'bread crumb rye',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'rye') ~ 'bread rye',
      str_detect(Ingredients, 'ontbijtkoek') ~ 'bread ontbijtkoek',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'pumpernickel') ~ 'bread pumpernickel',
      (str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crumb|grate') & str_detect(Ingredients, 'white')) | str_detect(Ingredients, 'grilling flour') & str_detect(Ingredients, 'white') ~ 'bread crumb white',
      (str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crumb|grate')) | str_detect(Ingredients, 'grilling flour') ~ 'bread crumb',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'stick') ~ 'breadstick',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crisp') & str_detect(Ingredients, 'coarse') ~ 'crisp bread coarse',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crisp') ~ 'crisp bread',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'coarse|whole wheat|whole-wheat|whole grain|whole-grain|wholegrain|brown') ~ 'bread coarse',
      str_detect(Ingredients, 'chapati') ~ 'bread brown chapati',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'white') & str_detect(Ingredients, 'mix') ~ 'white bread mix',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'white') | str_detect(Ingredients, 'loff') ~ 'bread white',
      str_detect(Ingredients, 'flatbread') ~ 'bread flat hard',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'nan|naan') ~ 'bread naan',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'nan|naan') ~ 'bread crisp',
      str_detect(Ingredients, 'pita') & str_detect(Ingredients, 'whole-wheat|whole wheat') ~ 'bread coarse pita',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'pocket|pita') ~ 'bread white pita',
      str_detect(Ingredients, 'bread') & !str_detect(Ingredients, 'flat|hamburger|rolls|pita|italian|olive oil|flour') ~ 'bread',
      str_detect(Ingredients, 'bulgur|bulgar') ~ 'bulgur wheat',
      
      str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'nut') ~ 'cashew nut salt',
      str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'roast|toast') ~ 'cashew nut roasted',
      str_detect(Ingredients, 'cashew') ~ 'cashew nut',
      str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'chick') &
        (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'chick pea canned',
      str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'chick|gram') ~ 'chick pea flour',
      str_detect(Ingredients, 'chickpea|chick pea') & !str_detect(Ingredients, 'lentil') ~ 'chick pea',
      str_detect(Ingredients, 'ciabatta|italian bread') ~ 'ciabatta',
      (str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'starch')) | str_detect(Ingredients, 'maizena') | str_detect(Ingredients, 'corn') & !str_detect(Ingredients, 'oil|pepper|crispy|cob|minim|coat|water to the|flour') & str_detect(Amounts, 'tbsp|tsp') ~ 'corn starch',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'flour') ~ 'corn flour',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'meal') & str_detect(Ingredients, 'mix') ~ 'corn meal mix',
      str_detect(Ingredients, 'polenta') ~ 'corn flour polenta',
      str_detect(Ingredients, 'cookie') & str_detect(Ingredients, 'amarettini') ~ 'cookies amarettini',
      str_detect(Ingredients, 'cous') ~ 'couscous',
      str_detect(Ingredients, 'cream cracker') ~ 'cracker cream',
      str_detect(Ingredients, 'saltine cracker') ~ 'cracker saltine',
      str_detect(Ingredients, 'graham cracker') & str_detect(Ingredients, 'crust') ~ 'graham cracker crust',
      str_detect(Ingredients, 'graham cracker') ~ 'graham cracker',
      
      str_detect(Ingredients, 'flax') & str_detect(Ingredients, 'meal') ~ 'flaxseed meal',
      str_detect(Ingredients, 'flax') & str_detect(Ingredients, 'seed') ~ 'seed flax',
      
      str_detect(Ingredients, 'hamburger') & str_detect(Ingredients, 'bread|bun') ~ 'hamburger bun',
      str_detect(Ingredients, 'hazelnut') & !str_detect(Ingredients, 'oil') ~ 'hazelnut',
      str_detect(Ingredients, 'hemp') & str_detect(Ingredients, 'seed') ~ 'seed hemp',
      
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'red') ~ 'lentil red',
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'green') ~ 'lentil green',
      str_detect(Ingredients, 'lentil') & str_detect(Amounts, 'box|can') ~ 'lentil canned',
      str_detect(Ingredients, 'lentil') ~ 'lentil',
      
      str_detect(Ingredients, 'nacho|tortilla chip') ~ 'nacho',
      
      str_detect(Ingredients, 'oatmeal|oat flour') ~ 'oatmeal',
      str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'flake|rolled') ~ 'oat rolled',
      str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'quick|porridge') ~ 'oat quick',
      
      str_detect(Ingredients, 'pancake') & str_detect(Ingredients, 'mix') ~ 'pancake dry mix',
      str_detect(Ingredients, 'pancake') & !str_detect(Ingredients, 'syrup') ~ 'pancake',
      str_detect(Ingredients, 'pasta|paste|spagetti|spaghetti') & str_detect(Ingredients, 'whole') ~ 'pasta whole grain',
      str_detect(Ingredients, 'lasagna|lasagne') & str_detect(Ingredients, 'plate|sheet') ~ 'lasagna plate pasta',
      str_detect(Ingredients, 'pasta|spagetti|spaghetti|tagli|pens|macaroni') & !str_detect(Ingredients, 'lasagna') & str_detect(Ingredients, '\\bcooked') ~ 'pasta cooked',
      str_detect(Ingredients, 'pasta|spagetti|spaghetti|tagli|pens|macaroni|tortellini') & !str_detect(Ingredients, 'lasagna') & !str_detect(Ingredients, 'sauce') ~ 'pasta',
      str_detect(Ingredients, 'lasagna noodles') ~ 'pasta',
      str_detect(Ingredients, 'barley') ~ 'pearl barley',
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'butter') ~ 'peanut butter',
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'salt') ~ 'peanut salt',
      str_detect(Ingredients, 'peanut') & !str_detect(Ingredients, 'oil') ~ 'peanut',
      str_detect(Ingredients, 'pecans') ~ 'pecan',
      str_detect(Ingredients, 'pie dough') ~ 'pie dough',
      str_detect(Ingredients, 'pine') & str_detect(Ingredients, 'nut|seed|kernel') & !str_detect(Ingredients, 'apple') ~ 'pine nut',
      str_detect(Ingredients, 'pistachio') ~ 'pistachio nut',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'flour') ~ 'potato starch',
      str_detect(Ingredients, 'lompe') ~ 'potato flatbread lompe',
      str_detect(Ingredients, 'puff pastry|butter dough') ~ 'puff pastry',
      str_detect(Ingredients, 'pumpkin seed') ~ 'pumpkin seed',
      
      str_detect(Ingredients, 'quinoa') ~ 'quinoa',
      
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked') ~ 'rice cooked',
      str_detect(Ingredients, 'rice|ris') & str_detect(Ingredients, 'basmati') ~ 'rice basmati',
      str_detect(Ingredients, 'rice|ris') & str_detect(Ingredients, 'risotto|arbori|paella') | str_detect(Ingredients, 'vialone nano') ~ 'rice risotto',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'jasmin') ~ 'rice jasmin',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'parboiled|pre boiled') ~ 'rice parboiled',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked') & str_detect(Ingredients, 'wild') ~ 'rice wild cooked',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'wild') ~ 'rice wild',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked') ~ 'rice cooked',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'whole|brown') ~ 'rice brown long grain',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'sushi') ~ 'rice sushi',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'noodle') ~ 'rice noodle',
      str_detect(Ingredients, 'rice') & !str_detect(Ingredients, 'beef|potato|vinegar|wine') ~ 'rice white long grain',
      
      str_detect(Ingredients, 'sesame') & str_detect(Ingredients, 'seed') ~ 'sesame seed',
      str_detect(Ingredients, 'semolina') ~ 'wheat flour semolina',
      str_detect(Ingredients, 'shortcrust pastry') ~ 'shop-bought shortcrust pastry',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'low fat|low-fat') ~ 'soy milk low fat',
      str_detect(Ingredients, 'sunflower') & str_detect(Ingredients, 'seed') & !str_detect(Ingredients, 'oil') ~ 'seed sunflower',
      
      str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'whole|coarse') ~ 'tortilla coarse',
      str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'corn') ~ 'tortilla corn',
      str_detect(Ingredients, 'tortilla|wraps') & !str_detect(Ingredients, 'pita|chip') ~ 'tortilla',
      
      str_detect(Ingredients, 'walnut') & !str_detect(Ingredients, 'oil') ~ 'walnut',
      str_detect(Ingredients, 'spelt|farro') & str_detect(Ingredients, 'flour') | str_detect(Ingredients, 'farro') ~ 'wheat flour spelt',
      str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'wholemeal') | str_detect(Ingredients, 'wheat') & str_detect(Ingredients, 'whole') & str_detect(Ingredients, 'flour') ~ 'wheat flour wholemeal',
      str_detect(Ingredients, 'rye') & str_detect(Ingredients, 'flour') ~ 'wheat flour rye',
      str_detect(Ingredients, 'rye') & str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'whole') ~ 'wheat flour rye wholemeal',
      str_detect(Ingredients, 'wheat flour|all-purpose flour|plain flour|flour|durum wheat|self-raising|bread') & !str_detect(Ingredients, 'whole|gram|tortilla|potato|corn|spelt') ~ 'wheat flour',
      
      
      #Poultry----
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'breast|fillet|filet') &
        str_detect(Ingredients, 'without|skinless|no skin') &
        str_detect(Ingredients, 'cooked') & !str_detect(Amounts, 'stk') ~ 'chicken breast without skin cooked',
      str_detect(Ingredients, 'chicken') & 
        str_detect(Ingredients, 'breast|fillet|filet') &
        str_detect(Ingredients, 'without|skinless|no skin') ~ 'chicken breast without skin',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'breast|fillet|filet') &
        str_detect(Ingredients, 'cooked') & !str_detect(Amounts, 'stk') ~ 'chicken breast cooked',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'breast|fillet|filet') ~ 'chicken breast',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'thigh|leg') &
        str_detect(Ingredients, 'without|skinless|no skin') ~ 'chicken thigh without skin',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'thigh|leg') &
        str_detect(Ingredients, 'grilled|cooked') & !str_detect(Amounts, 'stk') ~ 'chicken thigh cooked', #This is 
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'thigh|leg') ~ 'chicken thigh',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'grilled|cooked') & !str_detect(Amounts, 'stk') ~ 'chicken whole cooked',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'drum|club') ~ 'chicken drumstick',
      str_detect(Ingredients, 'chicken') & !str_detect(Ingredients, 'power|condensed|broth|stock') ~ 'chicken whole',
      
      str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'breast') ~ 'duck breast',
      str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'leg') ~ 'duck leg',
      
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'boil|hard cook|hard-cook|hard boiled') & !str_detect(Amounts, 'stk') ~ 'egg boiled',
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'noodle') ~ 'egg noodle',
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'white') ~ 'egg white',
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'yolk') ~ 'egg yolk',
      str_detect(Ingredients, 'egg') & !str_detect(Ingredients, 'plant') ~ 'egg',
      
      str_detect(Ingredients, 'grouse') ~ 'hen breast fillet grouse',
      
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'ground|dough') ~ 'turkey minced meat',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'breast|fillet') ~ 'turkey breast',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'club') ~ 'turkey drumstick chicken', #Add chicken to use to calculate nutrition values
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'ham') ~ 'turkey ham canned',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'grill') ~ 'sausage turkey chicken', #Prior turkey chicken grill sausage
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'cooked') ~ 'turkey meat cooked',
      str_detect(Ingredients, 'turkey') & !str_detect(Ingredients, 'broth|stock|fund|escalope') ~ 'whole turkey',
      
      #Meat----
      str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') & str_detect(Ingredients, 'cooked') ~ 'bacon cooked',
      str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') ~ 'bacon',
      str_detect(Ingredients, 'lard') ~ 'lard pork fat',
      str_detect(Ingredients, 'bankekjøtt|beef round roast|bottom round roast|knocked meat') | str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'round steak') ~ 'beef bottom round',
      str_detect(Ingredients, 'roast beef') ~ 'beef bottom round roast beef',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'sirloin|tip') ~ 'beef sirloin', #Tips often come from the sirloin
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'comb|entre') |
        str_detect(Ingredients, 'entrecote|cote de boef|scotch or black welsh beef|standing rib roast, bone in|rib-eye steak|rib eye steak|ribeye steak') ~ 'beef rib-eye steak', #Also used for the steak in Steak with potato salad
      str_detect(Ingredients, 'beef|cattle') & str_detect(Ingredients, 'tenderloin|fillet') & !str_detect(Ingredients, 'outer') ~ 'beef tenderloin',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'rump') | str_detect(Ingredients, 'top round steak') ~ 'beef roast of knuckle',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shank') & !str_detect(Ingredients, 'calf') ~ 'beef shank',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'high back|high-roast|stew meat|pureed') | str_detect(Ingredients, 'chuck steak|cubed beef') | Ingredients == 'of beef' ~ 'beef chuck roll',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shoulder') | str_detect(Ingredients, 'shin of beef|beef leg') ~ 'beef shoulder', #Shoulder and shin are similar
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'brisket|short rib') ~ 'beef brisket',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'tongue') ~ 'beef tongue',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'bone') & !str_detect(Ingredients, 'steak') ~ 'beef bones',
      str_detect(Ingredients, 'ox') & str_detect(Ingredients, 'tail') ~ 'beef oxtail',
      str_detect(Ingredients, 'hanger steak|flank steak') ~ 'beef flank steak', #Cut from the same are of the animal
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'flat') & !str_detect(Ingredients, 'elk|deer') ~ 'beef topside', #Also some veal meat
      str_detect(Ingredients, 'beef|angus|cattle') & str_detect(Ingredients, 'outer') ~ 'beef striploin',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince') & str_detect(Ingredients, 'lean') ~ 'beef minced meat 6 %',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince|all-beef hot dog') | str_detect(Ingredients, 'chop dough') ~ 'beef minced meat',
      str_detect(Ingredients, 'minced meat') & !str_detect(Ingredients, 'pork|deer|tofu') ~ 'beef minced meat', #Standard
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shredded|steak strip|sirloin butt') ~ 'beef sirloin butt', #Also used in Beef quesedillas and Steak Strips On Spaghetti With Parsley Pesto
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince') | str_detect(Ingredients, 'ground') & str_detect(Ingredients, 'meat') & !str_detect(Ingredients, 'pork|turkey|deer') | str_detect(Ingredients, 'meat dough') ~ 'beef minced meat',
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'liver') ~ 'beef calf liver',
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'leg') ~ 'beef calf shoulder', #Actually hind leg but not in database,
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'steak') ~ 'beef veal for roast',
      str_detect(Ingredients, 'beef|cattle') & !str_detect(Ingredients, 'fund|broth|stock|bouilljon|bouillion|bouillon|consomme|gravy') ~ 'beef chuck roll',
      str_detect(Ingredients, 'bone marrow') ~ 'marrow bone',
      
      str_detect(Ingredients, 'deer') & str_detect(Ingredients, 'ground') & !str_detect(Ingredients, 'rein|rain') ~ 'roe deer minced meat',
      
      str_detect(Ingredients, 'elk') & str_detect(Ingredients, 'flat') ~ 'elk moose inside round',
      str_detect(Ingredients, 'elk') & str_detect(Ingredients, 'thigh') ~ 'elk shoulder',
      
      str_detect(Ingredients, 'game meat with bone') ~ 'game beef elk shoulder',
      str_detect(Ingredients, 'wild meat with bones') ~ 'game beef venison shoulder',
      
      str_detect(Ingredients, 'ham') & str_detect(Ingredients, 'cured') | str_detect(Ingredients, 'pancetta|prosciutto') ~ 'ham cured',
      str_detect(Ingredients, 'ham') & str_detect(Ingredients, 'smoked') ~ 'ham smoked',
      str_detect(Ingredients, 'ham') & !str_detect(Ingredients, 'bacon|tenderloin|hamburger|champignon|pork from|turkey|steak|bone') ~ 'ham',
      
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'shoulder|in slice|neck|with bone') ~ 'lamb shoulder', #Shoulder and neck meat can be interchanged
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'breast and skirt') ~ 'lamb breast skirt',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'ground|minced') ~ 'lamb minced meat',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'stick meat') ~ 'lamb cured rib',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') & str_detect(Ingredients, 'smoke') ~ 'lamb leg smoked',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') ~ 'lamb leg roast',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'stew|pot') ~ 'lamb stew meat',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'shank') ~ 'lamb shank',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'carree') ~ 'lamb hind saddle',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'chop|rib') ~ 'lamb chop',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'cooked') ~ 'lamb shoulder cooked', #Default
      str_detect(Ingredients, 'lamb') & !str_detect(Ingredients, 'power|broth|bouillon|stock') ~ 'lamb shoulder', #Default
      str_detect(Ingredients, 'sheep cabbage meat') ~ 'lamb sheep cabbage stew meat',
      str_detect(Ingredients, 'sheep head') ~ 'lamb sheep head',
      
      str_detect(Ingredients, 'meat') & str_detect(Ingredients, 'ball') ~ 'meatball',
      
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'butt') ~ 'pork shoulder',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ground|mince') ~ 'pork minced meat',
      str_detect(Ingredients, 'neck') & str_detect(Ingredients, 'chop') ~ 'pork neck chop',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'chop') ~ 'pork chop',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'neck') ~ 'pork neck',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'tenderloin|fillet|escalope') | Ingredients == 'thinly sliced pork loin' ~ 'pork tenderloin',
      Ingredients == 'cold roast pork loin' ~ 'pork tenderloin cooked',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'shred') | Ingredients == 'pork steak, cut into strips' ~ 'pork inside round', #What is used to create shredded pork in Norway, also why it says inside round in Satay of pigs with honey and ginger
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'hock') ~ 'pork hock',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'stew|shoulder') ~ 'pork shoulder', 
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'pulled') ~ 'pork shoulder cooked', 
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'salt') ~ 'pork shoulder salt', 
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'kidney') ~ 'pork kidney', 
      str_detect(Ingredients, 'pig') & str_detect(Ingredients, 'liver') ~ 'pork liver',
      str_detect(Ingredients, 'pork belly') ~ 'pork belly',
      str_detect(Ingredients, 'pork rib roast') ~ 'pork rib roast',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ham') | str_detect(Ingredients, 'ham steak') ~ 'pork ham roast',
      str_detect(Ingredients, 'bone-in ham') ~ 'pork ham roast bone in',
      str_detect(Ingredients, 'spare rib') & !str_detect(Ingredients, 'beef') | str_detect(Ingredients, 'pork sparerib') ~ 'pork spare rib',
      str_detect(Ingredients, 'pork') & !str_detect(Ingredients, 'sausage|bratwurst') ~ 'pork shoulder', #Default
      
      str_detect(Ingredients, 'rabbit') ~ 'rabbit',
      str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'fillet') ~ 'reindeer tenderloin',
      str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'pot meat') ~ 'reindeer chuck roll',
      str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'flat') ~ 'reindeer inside round',
      str_detect(Ingredients, 'reindeer') ~ 'reindeer',
      
      str_detect(Ingredients, 'salami') ~ 'salami',
      str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'chorizo') | str_detect(Ingredients, 'chorizo') ~ 'sausage chorizo',
      str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'vossa') ~ 'sausage vossa',
      str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'chipolata') ~ 'sausage chipolata',
      str_detect(Ingredients, 'sausage|bratwurst') & str_detect(Ingredients, 'pork') ~ 'sausage pork',
      str_detect(Ingredients, 'sausage') & !str_detect(Ingredients, 'mustard|sauce') ~ 'sausage',
      
      #Seafood----
      str_detect(Ingredients, 'anchovy fillet|sardines or anchovies') ~ 'anchovy fillet',
      str_detect(Ingredients, 'anchovies') ~ 'anchovy canned',
      str_detect(Ingredients, 'angler fish|anglerfish') ~ 'anglerfish',
      
      str_detect(Ingredients, 'catfish|wolf fish|wolffish') ~ 'catfish',
      str_detect(Ingredients, 'ishavsrøye') ~ 'char ishavsrøye fatty fish',
      str_detect(Ingredients, 'clam') ~ 'clam',
      str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'lutefisk') ~ 'cod lutefisk',
      str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'clip') | str_detect(Ingredients, 'clipfish') ~ 'cod clipfish',
      str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'cooked') ~ 'cod cooked',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'dried|dry') ~ 'cod dried',
      str_detect(Ingredients, 'fish fillet|firm white fish|different fillets of white fish|optional fish without skin and bone') & !str_detect(Ingredients, 'angler|cat|cod|pollock') ~ 'cod',
      str_detect(Ingredients, 'cod') ~ 'cod fillet',
      str_detect(Ingredients, 'crab') & str_detect(Ingredients, 'shell') ~ 'crab shell',
      str_detect(Ingredients, 'crab') & str_detect(Ingredients, 'claw') ~ 'crab claw',
      str_detect(Ingredients, 'crab') ~ 'crab',
      
      str_detect(Ingredients, 'fish cake') & str_detect(Ingredients, 'coarse') ~ 'fish cakes coarse',
      str_detect(Ingredients, 'fish, head, back bone') | Ingredients == 'fish cut' ~ 'fish scraps for broth',
      
      str_detect(Ingredients, 'grouper') ~ 'grouper',
      
      str_detect(Ingredients, 'haddock') ~ 'haddock',
      str_detect(Ingredients, 'halibut') ~ 'halibut',
      str_detect(Ingredients, 'herring') & str_detect(Ingredients, 'smoked') ~ 'herring smoked',
      str_detect(Ingredients, 'herring') ~ 'herring',
      
      str_detect(Ingredients, 'lobster') & str_detect(Ingredients, '\\bcooked') ~ 'lobster cooked',
      str_detect(Ingredients, 'lobster') & !str_detect(Ingredients, 'shell') ~ 'lobster',
      
      str_detect(Ingredients, 'mackerel') & str_detect(Ingredients, 'tomato') ~ 'mackerel tomato canned',
      str_detect(Ingredients, 'mackerel') ~ 'mackerel',
      str_detect(Ingredients, 'mussels') & !str_detect(Ingredients, 'power') ~ 'mussels',
      str_detect(Ingredients, 'sand shell') ~ 'scallop', #Similar in nutrition value
      
      str_detect(Ingredients, 'oyster') & !str_detect(Ingredients, 'sauce') ~ 'oyster',
      
      str_detect(Ingredients, 'pollock') & str_detect(Ingredients, 'smoked') ~ 'pollock smoked',
      str_detect(Ingredients, 'pollock|pollack|chop fillet, without skins and bones|saithe') ~ 'pollock',
      str_detect(Ingredients, 'prawn') ~ 'prawn',
      
      str_detect(Ingredients, 'redfish') ~ 'redfish',
      
      str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'smoked') ~ 'salmon smoked',
      str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'roe') ~ 'salmon roe',
      str_detect(Ingredients, 'salmon') ~ 'salmon',
      str_detect(Ingredients, 'sandshell') ~ 'sandshell',
      str_detect(Ingredients, 'sardine') ~ 'sardine',
      str_detect(Ingredients, 'scallop') ~ 'scallop',
      str_detect(Ingredients, 'scampi') ~ 'scampi',
      str_detect(Ingredients, 'sea bass') ~ 'sea bass',
      str_detect(Ingredients, 'sea urchin') ~ 'sea urchin',
      str_detect(Ingredients, 'shellfish') & !str_detect(Ingredients, 'borth|stock') ~ 'shellfish',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, '\\bcooked') ~ 'shrimp cooked',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, 'lake') ~ 'shrimp in brine',
      str_detect(Ingredients, 'shrimp') & !str_detect(Ingredients, 'paste|salad|shellfish') ~ 'shrimp',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, 'salad') ~ 'shrimp salad',
      str_detect(Ingredients, 'squid') & !str_detect(Ingredients, 'honey') ~ 'squid',
      
      str_detect(Ingredients, 'trout') & str_detect(Ingredients, 'cured') ~ 'cured trout',
      str_detect(Ingredients, 'trout') & str_detect(Ingredients, 'caviar') ~ 'trout caviar',
      str_detect(Ingredients, 'trout') ~ 'trout',
      str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'oil') ~ 'tuna in oil canned',
      str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'water|drained') ~ 'tuna in water canned',
      str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'can') ~ 'tuna in oil canned', #Default
      str_detect(Ingredients, 'tuna') ~ 'tuna',
      
      #Oils----
      #If multiple oils are listed as alternatives, use first in alphabetical order
      str_detect(Ingredients, 'canola|rapeseed') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'rapeseed oil for deep frying',
      str_detect(Ingredients, 'canola|rapeseed') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'rapeseed oil for cooking',
      str_detect(Ingredients, 'canola|rapeseed') & str_detect(Ingredients, 'oil')  ~ 'rapeseed oil',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'sichuan') ~ 'oil chili sichuan',
      str_detect(Ingredients, 'coconut oil') & str_detect(Ingredients, 'deep fry') ~ 'coconut oil for deep frying',
      str_detect(Ingredients, 'coconut oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'coconut oil for cooking',
      str_detect(Ingredients, 'coconut oil') ~ 'coconut oil',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'oil corn for deep frying',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'oil') ~ 'oil corn',
      
      str_detect(Ingredients, 'garlic oil') ~ 'garlic oil',
      
      str_detect(Ingredients, 'hazelnut oil') ~ 'hazelnut oil',
      
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'olive oil for deep frying',
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'olive oil for cooking',
      str_detect(Ingredients, 'olive oil|olivenolje|extra-virgin olive|olive oil') ~ 'olive oil',
      
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'peanut oil for deep frying',
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'peanut oil for cooking',
      str_detect(Ingredients, 'peanut|groundnut') & str_detect(Ingredients, 'oil') ~ 'peanut oil',
      
      str_detect(Ingredients, 'sesame oil') ~ 'sesame oil',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'soybean oil for deep frying',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|waking|browning|roasting|greasing|brushing') ~ 'soybean oil for deep frying',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') ~ 'soybean oil',
      str_detect(Ingredients, 'sunflower oil') & str_detect(Ingredients, 'deep fry') ~ 'sunflower oil for deep frying',
      str_detect(Ingredients, 'sunflower oil') & str_detect(Ingredients, 'frying|waking|browning|roasting|greasing|brushing') ~ 'sunflower oil for cooking',
      str_detect(Ingredients, 'sunflower oil') ~ 'sunflower oil',
      
      str_detect(Ingredients, 'walnut') & str_detect(Ingredients, 'oil') ~ 'walnut oil',
      
      str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'truffle') ~ 'oil truffle',
      
      str_detect(Ingredients, 'oil for deep frying') | Ingredients == 'frying oil' ~ 'vegetable oil for deep frying',
      Ingredients %in% c('oil for frying', 'oil for brushing', 'lubricating and brushing oil') ~ 'vegetable oil for cooking',
      str_detect(Ingredients, 'vegetable oil|salad oil|oil, neutral|vegetabie oil') | Ingredients %in% c('oil', 'of oil', 'oil neutral')  ~ 'vegetable oil',
      
      str_detect(Ingredients, 'mayo') & str_detect(Ingredients, 'vegan') ~ 'mayonnaise vegan',
      str_detect(Ingredients, 'mayo') ~ 'mayonnaise',
      
      #Div----
      str_detect(Ingredients, 'agar') ~ 'agar',
      str_detect(Ingredients, 'agave') & str_detect(Ingredients, 'nectar') ~ 'agave nectar',
      str_detect(Ingredients, 'agave') & str_detect(Ingredients, 'syrup') ~ 'agave syrup',
      str_detect(Ingredients, 'asafoetida powder') ~ 'asafoetida powder',
      
      str_detect(Ingredients, 'baking powder') ~ 'baking powder',
      str_detect(Ingredients, 'baking soda|bicarbonate of soda') ~ 'baking soda',
      str_detect(Ingredients, 'beer') & str_detect(Ingredients, 'dark|amber|christmas') ~ 'beer dark',
      str_detect(Ingredients, 'beer|ale') ~ 'beer',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'pickle') ~ 'beetroot pickled',
      Ingredients == 'black truffle or 2 tbsp s truffle oil' ~ 'black truffle',
      str_detect(Ingredients, 'brandy') ~ 'spirits 40 vol-% alcohol brandy',
      str_detect(Ingredients, 'brine') & !str_detect(Ingredients, 'shrimp') ~ 'water brine',
      str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'base') ~ 'bolognese base',
      str_detect(Ingredients, 'bearnaise') & str_detect(Ingredients, 'base') ~ 'bearnaise base',
      str_detect(Ingredients, 'beverage') & str_detect(Ingredients, 'carbonated') & str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'lime') ~ 'carbonated beverage lemon-lime',
      str_detect(Ingredients, 'browning') & str_detect(Ingredients, 'sauce') ~ 'sauce browning',
      str_detect(Ingredients, 'brown') & str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'mix') | str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'powder') ~ 'gravy brown mix',
      
      str_detect(Ingredients, 'caper|hijack') ~ 'caper',
      str_detect(Ingredients, 'caramel') & str_detect(Ingredients, 'sauce') ~ ' sauce caramel',
      str_detect(Ingredients, 'caramel') ~ 'caramel',
      (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'powder|spice')) | str_detect(Ingredients, 'ground red pepper') ~ 'chili powder',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'sweet') ~ 'sweet chili sauce',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sauce') ~ 'chili sauce',
      str_detect(Ingredients, 'german') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') & str_detect(Ingredients, 'chocolate') ~ 'german chocolate cake mix',
      str_detect(Ingredients, 'instant chocolate pudding mix') ~ 'chocolate pudding mix',
      str_detect(Ingredients, 'package brownie mix') ~ 'brownie mix',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'milk') & !str_detect(Ingredients, 'candy') ~ 'chocolate milk',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'semi|dark') ~ 'chocolate semi-sweet',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'unsweetened') ~ 'chocolate unsweetened',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'white') ~ 'chocolate white',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'vermicelli') ~ 'chocolate semi-sweet vermicelli',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'candy') & str_detect(Ingredients, 'bar') | str_detect(Ingredients, 'chocolatebar') ~ 'chocolate candy bar',
      str_detect(Ingredients, 'chocolate') & !str_detect(Ingredients, 'syrup|icing|kruidnoten') ~ 'chocolate semi-sweet', #Default
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'icing') ~ 'icing chocolate',
      str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mango') ~ 'chutney mango',
      str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mint') ~ 'chutney mint',
      str_detect(Ingredients, 'cider') & !str_detect(Ingredients, 'vinegar') ~ 'cider',
      str_detect(Ingredients, 'cocoa') | str_detect(Ingredients, 'cacao') & str_detect(Ingredients, 'powder') ~ 'cocoa powder',
      str_detect(Ingredients, 'cognac') ~ 'spirits 40 vol-% alcohol cognac',
      str_detect(Ingredients, 'color') & str_detect(Ingredients, 'sprinkles') ~ 'sprinkles colored',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'celery') ~ 'condensed cream of celery soup',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'chicken') ~ 'condensed cream of chicken soup',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'mushroom') ~ 'condensed cream of mushroom soup',
      str_detect(Ingredients, 'condensed') & str_detect(Ingredients, 'tomato soup') ~ 'condensed tomato soup',
      str_detect(Ingredients, 'cooking') & str_detect(Ingredients, 'spray') ~ 'cooking spray',
      str_detect(Ingredients, 'cream sauce') & str_detect(Ingredients, 'base') ~ 'cream sauce base',
      str_detect(Ingredients, 'crisps') ~ 'crisps',
      str_detect(Ingredients, 'custard') & str_detect(Ingredients, 'hot') ~ 'custard hot',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'custard') ~ 'custard vanilla',
      
      str_detect(Ingredients, 'dark rum') | Ingredients == 'rum' ~ 'spirits 40 vol-% alcohol dark rum',
      str_detect(Ingredients, 'dip mix') ~ 'dip mix',
      
      str_detect(Ingredients, 'espresso') & str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'ground') ~ 'espresso bean coffee ground',
      
      str_detect(Ingredients, 'fish soup') & str_detect(Ingredients, 'base') ~ 'fish soup base',
      
      str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'beef') ~ 'beef gravy',
      str_detect(Ingredients, 'gravy') ~ 'beef gravy', #Use as default
      str_detect(Ingredients, 'guacamole') ~ 'guacamole',
      
      str_detect(Ingredients, 'harissa') & str_detect(Ingredients, 'mild') ~ 'harissa mild',
      str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'base') ~ 'hollandaise base',
      str_detect(Ingredients, 'honey') & !str_detect(Ingredients, 'mustard') ~ 'honey',
      
      str_detect(Ingredients, 'kirsch') ~ 'spirits 40 vol-% alcohol kirsch',
      str_detect(Ingredients, 'kombu') & str_detect(Ingredients, 'dashi') ~ 'dashi kombu dried kelp',
      
      str_detect(Ingredients, 'madeira') ~ 'madeira fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'marsala') ~ 'marsala fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'marshmallow') & str_detect(Ingredients, 'cream') ~ 'marshmallow cream',
      str_detect(Ingredients, 'marshmallow') ~ 'marshmallow',
      str_detect(Ingredients, 'mire poix') ~ 'mire poix',
      str_detect(Ingredients, 'miso') & str_detect(Ingredients, 'white') & str_detect(Ingredients, 'paste') ~ 'miso paste white',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'seed') ~ 'mustard seed',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'powder') ~ 'mustard powder',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'whole|grain|coarse') ~ 'mustard whole grain',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'dijon') ~ 'mustard dijon',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'honey') ~ 'mustard honey',
      str_detect(Ingredients, 'mustard') & !str_detect(Ingredients, 'cheese') ~ 'mustard',
      
      str_detect(Ingredients, 'noodle') ~ 'rice noodle', #Default
      str_detect(Ingredients, 'nori') & str_detect(Ingredients, 'flak|seaweed') ~ 'nori seaweed',
      
      str_detect(Ingredients, 'olive paste tapenade') ~ 'olive paste tapenade',
      str_detect(Ingredients, 'dry onion soup mix') ~ 'onion soup mix',
      
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'sichuan') & str_detect(Ingredients, 'bean') ~ 'chili bean paste sichuan',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'chili') | str_detect(Ingredients, 'sambal|rose harissa') ~ 'paste chili',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'carrot') ~ 'paste carrot',
      str_detect(Ingredients, 'paste|pasta') & str_detect(Ingredients, 'curry') ~ 'paste curry',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'garlic') ~ 'paste garlic',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'shrimp') ~ 'paste shrimp',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'sun') ~ 'paste tomato sun-dried',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'bean') ~ 'paste vanilla bean',
      str_detect(Ingredients, 'pesto') & str_detect(Ingredients, 'vegan') ~ 'pesto vegan',
      str_detect(Ingredients, 'pesto') ~ 'pesto',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'white') ~ 'white pepper',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'cayenne') ~ 'cayenne pepper',
      str_detect(Ingredients, 'pepper|Pepper') & !str_detect(Ingredients, 'chili|white|sweet|cayenne|spice|bell|salad|sauce') & !str_detect(Ingredients, 'salt') ~ 'black pepper',
      str_detect(Ingredients, 'sweet green pickle relish') ~ 'sweet green pickle relish',
      str_detect(Ingredients, 'pickle') & !(str_detect(Ingredients, 'cucumber|ginger|sweet pepper|onion|beet')) | str_detect(Ingredients, 'gherkin') ~ 'cucumber pickled', #Standard
      str_detect(Ingredients, 'pizza filling') ~ 'pizza filling',
      
      str_detect(Ingredients, 'sake') ~ 'sake',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'oil') ~ 'salt and pepper and oil',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'butter') ~ 'salt and pepper and butter',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') ~ 'salt and pepper',
      str_detect(Ingredients, 'salt') & !str_detect(Ingredients, 'peanut|lamb|pork|anchovy|soy|flesk|cashew') ~ 'salt',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'barbeque|barbecue') ~ 'sauce barbeque',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'bearnaise') ~ 'sauce bearnaise',
      str_detect(Ingredients, 'bearnaise') ~ 'sauce bearnaise',
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot pepper') ~ 'sauce hot pepper',
      str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'chunky') ~ 'salsa chunky',
      str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'tomato') ~ 'salsa tomato',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'cheese') ~ 'sauce cheese',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sweet') ~ 'sauce sweet chili',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'hot') ~ 'sauce hot chili',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sour') ~ 'sauce sour chili',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'salt') ~ 'sauce salt chili',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'mild') ~ 'sauce mild chili',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'garlic') ~ 'sauce chili garlic',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') ~ 'sauce chili',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'cream') ~ 'sauce cream',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'fish|fisk') ~ 'sauce fish',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'hoisin') ~ 'sauce hoisin',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'horseradish') ~ 'sauce horseradish',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'mint') ~ 'sauce mint',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'teriyaki') ~ 'sauce teriyaki',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'tomat') ~ 'sauce tomato',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'oyster') ~ 'sauce oyster',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'pad thai') ~ 'sauce pad thai',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'piri-piri') ~ 'sauce piri-piri',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'sweet') | str_detect(Ingredients, 'ketjap medja') ~ 'sauce sweet soy',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'soy') ~ 'sauce soy',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'taco') ~ 'sauce taco',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'tikka masala') ~ 'sauce tikka masala',
      str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'white') ~ 'sauce white',
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'cranberr') ~ 'sauce cranberry',
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'worcestershire') ~ 'sauce worcestershire',
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pasta|spagetti|spaghetti') ~ 'sauce pasta',
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') ~ 'sauce pizza',
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot') ~ 'sauce hot',
      str_detect(Ingredients, 'sherry') & !str_detect(Ingredients, 'vinegar') ~ 'sherry fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'shortening') & str_detect(Ingredients, 'vegetable') ~ 'shortening vegetable',
      str_detect(Ingredients, 'shortening') ~ 'shortening',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'brown|castor') ~ 'sugar brown',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'confect|icing|powder') ~ 'sugar confectioners',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'vanilla') ~ 'sugar vanilla',
      str_detect(Ingredients, 'sugar') & !str_detect(Ingredients, 'asparagus|pea') ~ 'sugar',
      str_detect(Ingredients, 'syrup|sirup') & str_detect(Ingredients, 'maple') ~ 'syrup maple',
      str_detect(Ingredients, 'syrup') & str_detect(Ingredients, 'chocolate') ~ 'syrup chocolate',
      str_detect(Ingredients, 'syrup') ~ 'syrup',
      
      str_detect(Ingredients, 'tabasco') ~ 'tabasco',
      str_detect(Ingredients, 'toenjang') ~ 'toenjang soybean paste',
      str_detect(Ingredients, 'tofu') ~ 'tofu',
      str_detect(Ingredients, 'toro jegergryte') ~ 'toro jegegryte',
      str_detect(Ingredients, 'toro moussaka') ~ 'toro moussaka',
      
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'extract') ~ 'vanilla extract',
      str_detect(Ingredients, 'vermouth') ~ 'vermouth fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'apple|cider') ~ 'vinegar apple cider',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'balsamic') ~ 'vinegar balsamic',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'red wine') ~ 'vinegar red wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'rice') ~ 'vinegar rice',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'sherry') ~ 'vinegar sherry',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'white wine') ~ 'vinegar white wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ 'vinegar wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'brown') ~ 'vinegar brown',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'clear|light|5%') ~ 'vinegar',
      str_detect(Ingredients, 'vinegar') ~ 'vinegar',
      str_detect(Ingredients, 'vodka') ~ 'spirits 40 vol-% alcohol vodka',
      
      str_detect(Ingredients, 'water') & !str_detect(Ingredients, 'corn|beef|tuna|coffee|chili|cream|cress|chestnut|melon') ~ 'water',
      str_detect(Ingredients, 'water to the corn') ~ 'water',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'vegetable') ~ 'broth cube vegetable',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'fish') ~ 'broth cube fish',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'beef|meat') ~ 'broth cube beef',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'chicken') ~ 'broth cube chicken',
      str_detect(Ingredients, 'stock|broth|bouillon|borth') & str_detect(Ingredients, 'cube|dice') ~ 'broth cube',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'chicken') ~ 'water broth chicken',
      str_detect(Ingredients, 'stock|broth|bouillon|bouilljon|consomme') & str_detect(Ingredients, 'beef|meat') ~ 'water broth beef',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'shellfish') ~ 'water broth shellfish',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'game|wild') ~ 'water broth game',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'turkey') ~ 'water broth turkey',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'vegetable') ~ 'water broth vegetable',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'lamb') ~ 'water broth lamb',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'fish') ~ 'water broth fish',
      str_detect(Ingredients, 'stock|broth|bouillon|frying pan|power') ~ 'water broth',
      str_detect(Ingredients, 'wasabi') ~ 'wasabi',
      str_detect(Ingredients, 'whisky|whiskey') ~ 'whisky spirits 40 vol-% alcohol',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'rice') ~ 'wine rice',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'white') & !str_detect(Ingredients, 'vinegar') ~ 'wine white',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'red|merlot') & !str_detect(Ingredients, 'vinegar') ~ 'wine red',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'port') ~ 'wine port fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'mirin japanese sweet wine') ~ 'wine mirin',
      
      str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'dry|dried') ~ 'yeast dry',
      str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'nutritional') ~ 'yeast nutritional',
      str_detect(Ingredients, 'yeast') ~ 'yeast',
      str_detect(Ingredients, 'yellow') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') ~ 'yellow cake mix',
      
      TRUE ~ Ingredients
    )) %>%
    
    #Rename column for later
    rename(org_ingredients = Ingredients,
           Ingredients = Ingredients_standardised)
}