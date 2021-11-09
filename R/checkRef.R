#' Check if an ingredient can be found in a reference database.
#' @title checkRef
#'
#' @description Checks if an ingredient is present in a reference database
#'
#' @param df A dataframe with an Ingredients column to be checked against the reference database. To fix common errors, the df also needs a "unit" column.
#' @param reference The reference database.
#' @param fix_errors Fix known errors automatically? For fixes of volume/weight database, df must have a unit column.
#'
#' @return A dataframe with the ingredient name and the reference ID of the first hit in the database. See references for how the databse must be built.
#'
#' @export

#Look through a list of ingredients
checkRef <- function(df, reference, fix_errors = TRUE){
  
  #Helper function to look for one ingredient only
  temp <- function(ingredient, reference){
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
                   reference$second_word[i] == '\\') {
          
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
  
  #Run on all ingredients
  #results <- df %>% select(Ingredients) %>% unique() %>% lapply(., temp, reference = reference) %>% bind_rows()
  
  results <- lapply(df$Ingredients %>% unique(), temp, reference = reference) %>% bind_rows()
  
  #Fix some known errors
  if(isTRUE(fix_errors)){
    
    #Fix volume weight hits
    if(deparse(substitute(reference)) == 'references$volume_weight'){
      
      #Add ingredient metadata, what meal it comes from, unit etc
      temp <- results %>%
        full_join(., df) %>%
        unique() %>%
        
        #Fix errors
        mutate(ID = case_when(
          Ingredients == 'butter clarified ghee' ~ fixRefID(reference = reference, 'ghee'),
          Ingredients %in% c('cheese brie', 'cheese mascarpone') ~ fixRefID(reference = reference, 'soft ripened cheese'),
          Ingredients == 'eggplant' ~ fixRefID(reference = reference, 'eggplant'),
          Ingredients == 'sugar' ~ fixRefID(reference = reference, 'sugar', 'white'),
          Ingredients == 'bread flat hard' ~ fixRefID(reference = reference, 'flatbread', 'hard'),
          Ingredients == 'caper' ~ fixRefID(reference = reference, 'capers'),
          str_detect(Ingredients, 'cheddar|jarlsberg|norvegia|semi-hard') ~ fixRefID(reference = reference, 'hard to semi-hard cheese'),
          Ingredients == 'cheese mozzarella' ~ fixRefID(reference = reference, 'mozzarella'),
          Ingredients == 'parmesan cheese' ~ fixRefID(reference = reference, 'parmesan'),
          Ingredients %in% c('chili pepper green', 'chili pepper jalapeno') ~ fixRefID(reference = reference, 'chili', 'red'), #Same in volume
          Ingredients == 'mackerel tomato canned' ~ fixRefID(reference = reference, 'mackerel', 'fillet'),
          Ingredients == 'peas green' ~ fixRefID(reference = reference, 'pea', 'frozen'),
          Ingredients == 'pork neck chop' ~ fixRefID(reference = reference, 'pork', 'neck'),
          Ingredients == 'sweet pepper grilled' & unit == 'stk' ~ fixRefID(reference = reference, 'sweet', 'pepper'),
          Ingredients == 'sweet pepper grilled' ~ fixRefID(reference = reference, 'sweet pepper', 'grilled'),
          Ingredients == 'turkey chicken drumstick' ~ fixRefID(reference = reference, 'turkey', 'drumstick'),
          Ingredients == 'lemongrass' ~ fixRefID(reference = reference, 'lemongrass'),
          Ingredients == 'fig' ~ fixRefID(reference = reference, 'fig'),
          Ingredients == 'shrimp' & unit == 'dl' ~ fixRefID(reference = reference, 'shrimps', 'in'),
          Ingredients == 'bean white canned' ~ fixRefID(reference = reference, 'bean white', 'canned'),
          Ingredients == 'bean kidney canned' ~ fixRefID(reference = reference, 'bean kidney', 'canned'),
          Ingredients == 'bean black canned' ~ fixRefID(reference = reference, 'bean black', 'canned'),
          Ingredients == 'chick pea canned' ~ fixRefID(reference = reference, 'chick pea', 'canned'),
          Ingredients == 'mustard powder' ~ fixRefID(reference = reference, 'mustard', 'powder'),
          str_detect(Ingredients, 'salad') & unit == 'stk' ~ fixRefID(reference = reference, 'heart', 'salad'),
          Ingredients %in% c('lettuce', 'salad lettuce') & unit == 'dl' ~ fixRefID(reference = reference, 'iceberg', 'lettuce'),
          Ingredients == 'chopped parsley or generous sprinkling dill fronds, or mixture optional' ~ fixRefID(reference = reference, 'parsley', 'fresh'),
          Ingredients == 'basil' & unit == 'twig' ~ fixRefID(reference = reference, 'basil', 'fresh'),
          Ingredients == 'coriander' & unit == 'twig' ~ fixRefID(reference = reference, 'coriander', 'fresh'),
          Ingredients %in% c('bean canned', 'bean black') ~ fixRefID(reference = reference, 'bean black', 'canned'),
          Ingredients == 'rice brown long grain' ~ fixRefID(reference = reference, 'rice'),
          str_detect(Ingredients, 'polenta') ~ fixRefID(reference = reference, 'cornmeal', 'polenta'),
          Ingredients == 'corn' & unit == 'dl' ~ fixRefID(reference = reference, 'corn', 'kernel'),
          Ingredients == 'cranberries jam' ~ fixRefID(reference = reference, 'jam', 'marmalade'),
          Ingredients == 'hamburger bun' ~ fixRefID(reference = reference, 'hamburger', 'bread'),
          Ingredients == 'mustard honey' ~ fixRefID(reference = reference, 'mustard'),
          Ingredients %in% c('salad lollo rosso', 'salad heart') & unit == 'leaf' ~ fixRefID(reference = reference, 'lettuce'),
          Ingredients == 'bread crumb' & unit == 'slice' ~ fixRefID(reference = reference, 'bread'),
          str_detect(Ingredients, 'spice mix') & unit == 'pack' ~ fixRefID(reference = reference, 'taco', 'spice'),
          Ingredients == 'cheese goat chevre white' & unit == 'stk' ~ fixRefID(reference = reference, 'soft ripened cheese'),
          Ingredients == 'watermelon' ~ fixRefID(reference = reference, 'melon', 'water'),
          Ingredients == 'broccolini' ~ fixRefID(reference = reference, 'broccolini'),
          Ingredients == 'scampi' & unit == 'stk' ~ fixRefID(reference = reference, 'prawn'),
          str_detect(Ingredients, 'salsa') & unit == 'dl' ~ fixRefID(reference = reference, 'salsa'),
          Ingredients == 'sausage turkey chicken' ~ fixRefID(reference = reference, 'sausage turkey chicken'),
          Ingredients %in% c('chili peppers', 'chili', 'strong chili') ~ fixRefID(reference = reference, 'chili', 'red'),
          Ingredients == 'corn flour' ~ fixRefID(reference = reference, 'cornmeal', 'polenta'),
          Ingredients == 'chicken breast' & unit == 'dl' ~ fixRefID(reference = reference, 'chicken', 'diced'),
          Ingredients == 'salad rocket' ~ fixRefID(reference = reference, 'ruccola'),
          Ingredients == 'lime lead' ~ fixRefID(reference = reference, 'bay', 'leaf'), #Assume similar
          Ingredients == 'oat quick' ~ fixRefID(reference = reference, 'rolled', 'oats'),
          Ingredients == 'agave nectar' ~ fixRefID(reference = reference, 'honey'),
          Ingredients %in% c('shortening', 'shortening vegetable') ~ fixRefID(reference = reference, 'margarine'),
          Ingredients == 'onion seed' ~ fixRefID(reference = reference, 'poppy', 'seed'), #Both are small seeds
          #Ingredients with no references
          Ingredients %in% c('mustard powder', 'chinese five spice', 'of dip mix', 'asafoetida powder',
                             'sauce browning') ~ 0,
          
          TRUE ~ ID
        )) 
      
      #Add the new reference words
      results <- temp %>%
        full_join(., reference %>% filter(ID %in% temp$ID)) %>%
        mutate(ref = paste0(first_word, ' ', second_word)) %>%
        select(-c(language, first_word, second_word, loop))
        
    } else if(deparse(substitute(reference)) == 'references$nutrients'){
      
      #Add ingredient metadata, like what meal it comes from, source etc
      temp <- results %>%
        full_join(., df) %>%
        unique() %>%
        
        #Fix errors
        mutate(ID = case_when(
          
          #Fruit and veg
          Ingredients == 'eggplant' ~ fixRefID(reference = reference, 'eggplant'),
          Ingredients == 'peach' ~ fixRefID(reference = reference, 'peach'),
          Ingredients %in% c('red chili', 'strong chili', 'chili peppers') ~ fixRefID(reference = reference, 'chili pepper', 'red'),
          Ingredients == 'sweet corn kernels' ~ fixRefID(reference = reference, 'sweet corn', 'canned'),
          Ingredients == 'sweet potato' ~ fixRefID(reference = reference, 'sweet potato'),
          Ingredients == 'chili pepper dried' ~ fixRefID(reference = reference, 'chili pepper', 'red'),
          Ingredients %in% c('potato', 'potato boiled') ~ fixRefID(reference = reference, 'potato'), 
          Ingredients == 'jerusalem artichoke' ~ fixRefID(reference = reference, 'jerusalem artichoke'),
          Ingredients == 'mangold' ~ fixRefID(reference = reference, 'mangold'),
          str_detect(Ingredients, 'butternut') ~ fixRefID(reference = reference, 'winter squash', 'pumpkin'),
          Ingredients == 'sweet pepper grilled' ~ fixRefID(reference = reference, 'sweet pepper', 'red'), #Standard
          Ingredients == 'watermelon' ~ fixRefID(reference = reference, 'watermelon'), 
          Ingredients == 'salsa' ~ fixRefID(reference = reference, 'chunky', 'salsa'),
          
          #Dairy
          Ingredients == 'parmesan cheese' ~ fixRefID(reference = reference, 'parmesan'),
          Ingredients == 'butter clarified ghee' ~ fixRefID(reference = reference, 'ghee'),
          Ingredients == 'cheese cottage low fat' ~ fixRefID(reference = reference, 'cottage cheese', 'low fat'),
          Ingredients == 'cheese cottage' ~ fixRefID(reference = reference, 'cottage cheese'),
          Ingredients %in% c('cheese asiago', 'cheese cotija') ~  fixRefID(reference = reference, 'parmesan'), #Can be substituted for eachother in recipes
          Ingredients == 'cheese blue' ~ fixRefID(reference = reference, 'gorgonzola', 'blue cheese'), #Use as standard for time being
          Ingredients == 'cheese goat chevre white' ~ fixRefID(reference = reference, 'chevre'),
          Ingredients == 'cheese cream' ~ fixRefID(reference = reference, 'cream cheese'),
          Ingredients == 'cheese hard goat' ~ fixRefID(reference = reference, 'hard goat cheese', 'kvitlin'), #Use as standard for time being
          Ingredients == 'cheese jarlsberg' ~ fixRefID(reference = reference, 'jarlsberg'), 
          Ingredients == 'cheese manchego' ~ fixRefID(reference = reference, 'cheddar'), #Can be substituted in recipes
          Ingredients == 'cheese mozzarella' ~ fixRefID(reference = reference, 'mozzarella'),
          Ingredients == 'cheese norvegia' ~ fixRefID(reference = reference, 'norvegia'),
          Ingredients == 'cheese ricotta salata' ~ fixRefID(reference = reference, 'ricotta salata'),
          Ingredients == 'cheese port salut' ~ fixRefID(reference = reference, 'port salut'),
          Ingredients == 'cheese semi-hard' ~ fixRefID(reference = reference, 'norvegia'), #Use as standard for time being
          Ingredients == 'goat brown cheese' ~ fixRefID(reference = reference, 'goat cheese brown'),
          Ingredients == 'cheese blue normanna' ~ fixRefID(reference = reference, 'normanna', 'blue cheese'),
          Ingredients == 'cheese blue norzola' ~ fixRefID(reference = reference, 'norzola', 'blue cheese'),
          Ingredients == 'cheese blue normanna' ~ fixRefID(reference = reference, 'normanna', 'blue cheese'),
          Ingredients == 'cheese cream goat snøfrisk' ~ fixRefID(reference = reference, 'snøfrisk', 'goat cream cheese'),
          Ingredients == 'cheese garlic' ~ fixRefID(reference = reference, 'norvegia'), #Standard
          Ingredients == 'cheese mascarpone' ~ fixRefID(reference = reference, 'mascarpone'),
          Ingredients == 'tine light 2 % a good alternative to sour cream' ~ fixRefID(reference = reference, 'quark', '1'), #Closest in nutritional value
          
          #Div
          Ingredients == 'mushroom' ~ fixRefID(reference = reference, 'mushroom'),
          Ingredients == 'sesame seed oil' ~ fixRefID(reference = reference, 'sesame', 'oil'),
          Ingredients == 'sugar' ~  fixRefID(reference = reference, 'sugar'),
          str_detect(Ingredients, 'water broth') ~ fixRefID(reference = reference, 'water'),
          ref == 'mushroom' & !str_detect(Ingredients, 'condensed cream of mushroom soup') ~ fixRefID(reference = reference, 'mushroom'),
          Ingredients == 'condensed cream of celery soup' ~ fixRefID(reference = reference, 'condensed cream of celery soup'),
          Ingredients == 'condensed cream of chicken soup' ~ fixRefID(reference = reference, 'condensed cream of chicken soup'),
          Ingredients == 'oil corn' ~ fixRefID(reference = reference, 'vegetable', 'oil'),
          Ingredients == 'onion soup mix' ~ fixRefID(reference = reference, 'onion soup mix'),
          Ingredients == 'sauce hot pepper' ~ fixRefID(reference = reference, 'hot pepper sauce'),
          Ingredients == 'sauce pasta' ~ fixRefID(reference = reference, 'tomato', 'sauce'), #Use as substitute for time being
          Ingredients == 'sauce hot' ~ fixRefID(reference = reference, 'hot pepper sauce'),
          Ingredients == 'olive paste tapenade' ~ fixRefID(reference = reference, 'olive paste tapenade'),
          Ingredients == 'homemade beef gravy' ~ fixRefID(reference = reference, 'beef gravy'),
          Ingredients == 'sweet chili sauce' ~ fixRefID(reference = reference, 'chili sauce', 'sweet'),
          Ingredients == 'refrigerated buttermilk biscuit dough' ~ fixRefID(reference = reference, 'refrigerated buttermilk biscuit dough'),
          
          #Grains, seeds nuts
          Ingredients == 'chick pea' ~ fixRefID(reference = reference, 'chick pea'),
          Ingredients == 'rice white long grain' ~ fixRefID(reference = reference, 'rice white long grain'),
          Ingredients == 'corn flour' ~ fixRefID(reference = reference, 'corn flour', 'polenta'),
          Ingredients == 'dried soybeans' ~ fixRefID(reference = reference, 'bean', 'soya'),
          Ingredients %in% c('cashew nut salt', 'cashew nut roasted') ~ fixRefID(reference = reference, 'cashew', 'salt'),
          Ingredients %in% c('bread crumb', 'bread', 'bread naan', 'breadstick') ~ fixRefID(reference = reference, 'bread'),
          Ingredients %in% c('crisp bread', 'crisp bread coarse') ~ fixRefID(reference = reference, 'crisp bread', 'coarse'),
          Ingredients == 'rolls white baguette garlic' ~ fixRefID(reference = reference, 'bread', 'white'),
          Ingredients == 'lentil' ~ fixRefID(reference = reference, 'lentil', 'green'), #Use as standard
          Ingredients %in% c('bread brown chapati', 'tortilla coarse') ~ fixRefID(reference = reference, 'bread', 'coarse'),
          Ingredients == 'bean canned' ~ fixRefID(reference = reference, 'bean', 'kidney canned'), #Standard 
          Ingredients == 'pearl barley' ~ fixRefID(reference = reference, 'pearl barley'),
          Ingredients == 'peanut' ~ fixRefID(reference = reference, 'peanut'),
          Ingredients == 'peanut salt' ~ fixRefID(reference = reference, 'peanut', 'salt'),
          Ingredients == 'rice parboiled' ~ fixRefID(reference = reference, 'rice parboiled'),
          Ingredients == 'rice brown long grain' ~ fixRefID(reference = reference, 'rice brown long grain'),
          
          #Seafood
          Ingredients == 'cod lutefisk' ~ fixRefID(reference = reference, 'lutefisk'),
          Ingredients == 'mackerel tomato canned' ~ fixRefID(reference = reference, 'mackerel', 'tomato canned'),
          
          #Herbs spices and conditments
          Ingredients == 'parsley' ~ fixRefID(reference = reference, 'parsley', 'fresh'),
          Ingredients == 'dry mustard' ~ fixRefID(reference = reference, 'mustard'),
          Ingredients == 'mayonnaise sauce' ~ fixRefID(reference = reference, 'mayonnaise'),
          Ingredients == 'chili flake dried' ~ fixRefID(reference = reference, 'chili', 'powder'),
          str_detect(Ingredients, 'vinegar') & !str_detect(ref, 'vinegar') ~ fixRefID(reference = reference, 'vinegar'),
          Ingredients == 'mustard honey' ~ fixRefID(reference = reference, 'mustard'),
          Ingredients == 'lemongrass' ~ fixRefID(reference = reference, 'lemongrass'),
          Ingredients == 'taco spice mix' ~ fixRefID(reference = reference,'taco spice mix'),
          
          #Meat
          Ingredients == 'pork neck chop' ~ fixRefID(reference = reference, 'pork', 'neck chop'),
          Ingredients == 'sausage' ~  fixRefID(reference = reference, 'sausage'),
          Ingredients == 'chicken' ~ fixRefID(reference = reference, 'chicken', 'whole'),
          Ingredients == 'whole turkey' ~ fixRefID(reference = reference, 'turkey', 'meat'),
          Ingredients == 'pork neck' ~ fixRefID(reference = reference, 'pork', 'neck chop'),
          
          #Not in reference
          Ingredients %in% c('duck or goose fat for confit', 'lime leaf', 'cranberries jam',
                             'cooking spray', 'red food coloring', 'beef fund', 'fish scraps for broth',
                             'pack high quality charcoal briquettes', 'pomegranate kernel', 'yeast nutritional',
                             'salmon roe', 'spice seasoning pepper', 'toro greek moussaka', 'paste chili',
                             'fish soup base', 'guacamole spice mix', 'lamb sheep head', 'lemon balm',
                             'lingonberry jam', 'marrow bone', 'rhubarb juice', 'beef bones',
                             '20 pound pack high quality charcoal briquettes', 'wine rice'
                             
          ) ~ 0,
          
          #Substitutions or ingredients not found in Matvaretabellen
          Ingredients %in% c('garlic oil', 'oil truffle') ~ fixRefID(reference = reference, 'olive', 'oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
          Ingredients %in% c('frying oil', 'oil') ~ fixRefID(reference = reference, 'vegetable', 'oil'),
          Ingredients == 'hazelnut oil' ~ fixRefID(reference = reference, 'walnut', 'oil'), #Another nut oil
          Ingredients == 'bean canned' ~ fixRefID(reference = reference, 'bean black', 'canned'),
          Ingredients == 'scampi' ~ fixRefID(reference = reference, 'shrimp'),
          Ingredients == 'ciabatta' ~ fixRefID(reference = reference, 'bread', 'white'),
          Ingredients == 'elk shoulder' ~ fixRefID(reference = reference, 'elk moose'),
          Ingredients == 'lime, the zest' ~ fixRefID(reference = reference, 'lemon', 'zest'),
          Ingredients %in% c('salsa', 'salsa tomato') ~ fixRefID(reference = reference, 'chunky', 'salsa'),
          
          TRUE ~ ID
        ))
      
      #Add the new reference words
      results <- temp %>%
        inner_join(., reference, by = 'ID') %>%
        mutate(ref = paste0(first_word, ' ', second_word)) %>%
        select(-c(first_word, second_word, loop))
      
      
    } else if(deparse(substitute(reference)) == 'references$sustainability'){
      
      #Add ingredient metadata, what meal it comes from
      temp <- results %>%
        full_join(., df) %>%
        unique() %>%
        
        #Fix errors
        mutate(ID = case_when(
          
          #Grains, nuts, seeds, legumes
          Ingredients == 'almond' ~ fixRefID(reference = reference, 'almonds', 'sweet'),
          Ingredients == 'hazelnut' ~ fixRefID(reference = reference, 'nut', 'hazel'),
          Ingredients %in% c('lentil', 'lentil green') ~ fixRefID(reference = reference, 'lentil', 'dry'),
          Ingredients == 'peas green' ~ fixRefID(reference = reference, 'pea', 'garden'),
          Ingredients %in% c('bean green asparagus', 'bean green', 'bean broad') ~ fixRefID(reference = reference, 'bean with pods', 'with'),
          str_detect(Ingredients, 'bean|chick pea|lentil') & !str_detect(Ingredients, 'canned|sprout|oil') ~ fixRefID(reference = reference, 'beans', 'dry'),
          str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'canned') | Ingredients == 'bean white tomato' ~ fixRefID(reference = reference, 'bean', 'canned'),
          str_detect(Ingredients, 'noodle') ~ fixRefID(reference = reference, 'noodle'),
          Ingredients == 'pistachio nut' ~ fixRefID(reference = reference, 'pistachio'),
          Ingredients == 'dried soybeans' ~ fixRefID(reference = reference, 'bean', 'soy'),
          Ingredients == 'pecan' ~ fixRefID(reference = reference, 'tree', 'nut'),
          Ingredients == 'tahini' ~ fixRefID(reference = reference, 'sesame', 'seed'),
          Ingredients %in% c('seed flax', 'flaxseed meal') ~ fixRefID(reference = reference, 'linseed'),
          Ingredients == 'corn starch' ~ fixRefID(reference = reference, 'corn', 'flour'), #Use as substitute
          
          #Veggies and fruit
          str_detect(Ingredients, 'pickled') & str_detect(Ingredients, 'ginger|sweet pepper|cucumber') ~ fixRefID(reference = reference, 'vegetables', 'pickled'),
          str_detect(Ingredients, 'canned') & str_detect(Ingredients, 'sweet pepper|sweet corn|artichoke') ~ fixRefID(reference = reference, 'vegetables', 'canned'),
          str_detect(Ingredients, 'endive|chicory') ~ fixRefID(reference = reference, 'curly', 'endives'),
          Ingredients == 'peach' ~ fixRefID(reference = reference, 'peaches', 'other'),
          Ingredients == 'sorrel' ~ fixRefID(reference = reference, 'lettuce', 'other'),
          str_detect(Ingredients, 'winter squash') ~ fixRefID(reference = reference, 'pumpkin'),
          str_detect(Ingredients, 'eggplant') ~ fixRefID(reference = reference, 'eggplant'),
          Ingredients == 'garlic chinese' ~ fixRefID(reference = reference, 'garlic'),
          Ingredients %in% c('corn baby', 'corn cob') ~ fixRefID(reference = reference, 'sweet', 'corn'),
          Ingredients == 'mangold' ~ fixRefID(reference = reference, 'chard'),
          Ingredients == 'olive black' ~ fixRefID(reference = reference, 'olives', 'canned'),
          Ingredients %in% c('olive green', 'of olives') ~ fixRefID(reference = reference, 'olives', 'fresh'),
          Ingredients == 'fresh cranberry' ~ fixRefID(reference = reference, 'cranberries'),
          Ingredients == 'olive green' ~ fixRefID(reference = reference, 'olives', 'canned'),
          Ingredients %in% c('red chili', 'strong chili', 'chili peppers') ~ fixRefID(reference = reference, 'chili', 'pepper'),
          Ingredients == 'tomato bunch' ~ fixRefID(reference = reference, 'tomato'),
          Ingredients %in% c('salad', 'salad heart', 'salad lollo rosso') ~ fixRefID(reference = reference, 'head', 'lettuce'),
          Ingredients == 'salad crispi' ~ fixRefID(reference = reference, 'crisp', 'lettuce'),
          Ingredients == 'salsa tomato' ~ fixRefID(reference = reference, 'chunky', 'salsa'), #Standard
          Ingredients == 'tomato sun dried' ~ fixRefID(reference = reference, 'tomato', 'sun-dried'),
          str_detect(Ingredients, 'tomato canned') & !str_detect(Ingredients, 'mackerel') ~ fixRefID(reference = reference, 'preserved', 'tomato'),
          Ingredients == 'watermelon' ~ fixRefID(reference = reference, 'watermelons'), #Fix ref query to water melon
          str_detect(Ingredients, 'the zest') ~ fixRefID(reference = reference, 'citrus', 'fruit'), #Reference for citrus fruit peel
          Ingredients == 'clementine' ~ fixRefID(reference = reference, 'mandarin'),
          Ingredients == 'black currant' ~ fixRefID(reference = reference, 'blackcurrant'),
          Ingredients == 'tamarind juice' ~ fixRefID(reference = reference, 'fruit', 'juice'),
          Ingredients == 'salsa' ~ fixRefID(reference = reference, 'chunky', 'salsa'), 
          
          #Red meat
          str_detect(Ingredients, 'reindeer|elk shoulder') ~ fixRefID(reference = reference, 'mammals', 'meat'),
          str_detect(Ingredients, 'pork') & !str_detect(Ingredients, 'lard') ~ fixRefID(reference = reference, 'pork'),
          Ingredients == 'lamb sheep cabbage stew meat' ~ fixRefID(reference = reference, 'lamb', 'fresh'),
          
          #Poultry
          str_detect(Ingredients, 'turkey') & !str_detect(Ingredients, 'broth') ~ fixRefID(reference = reference, 'turkey'),
          Ingredients == 'hen breast fillet grouse' ~ fixRefID(reference = reference, 'poultry', 'fresh'), #All poultry meats have the same CO2 and landuse in the db
          
          #Seafood
          Ingredients == 'scampi' ~ fixRefID(reference = reference, 'prawn'),
          Ingredients == 'arctic char' ~ fixRefID(reference = reference, 'trout'), #Look up other alternatives
          Ingredients == 'catfish' ~ fixRefID(reference = reference, 'miscellaneous', 'demersal'), #Steinbit
          Ingredients == 'salmon roe' ~ fixRefID(reference = reference, 'fish', 'roe'),
          
          #Div
          str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ fixRefID(reference = reference, 'vinegar', 'wine'),
          str_detect(Ingredients, 'vinegar') ~ fixRefID(reference = reference, 'vinegar'),
          Ingredients == 'condensed cream of celery soup' ~ fixRefID(reference = reference, 'condensed cream of celery soup'),
          Ingredients == 'condensed cream of chicken soup' ~ fixRefID(reference = reference, 'condensed cream of chicken soup'),
          str_detect(Ingredients, 'mushroom') & !str_detect(Ingredients, 'dried|canned') ~ fixRefID(reference = reference, 'mushroom'),
          Ingredients %in% c('garlic oil', 'oil truffle') ~ fixRefID(reference = reference, 'olive', 'oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
          Ingredients == 'sauce hot' ~ fixRefID(reference = reference, 'hot', 'pepper'),
          Ingredients == 'sesame oil' ~ fixRefID(reference = reference, 'seed', 'oil'),
          Ingredients == 'hazelnut oil' ~ fixRefID(reference = reference, 'walnut', 'oil'),
          Ingredients == 'sauce pasta' ~ fixRefID(reference = reference, 'tomato', 'sauce'),
          Ingredients == 'sweet chili sauce' ~ fixRefID(reference = reference, 'chili', 'sweet'),
          str_detect(Ingredients, 'cognac|kirsch') ~ fixRefID(reference = reference, 'brandy'),
          str_detect(Ingredients, 'broth cube') ~ fixRefID(reference = reference, 'stock', 'cubes'),
          Ingredients == 'nacho' ~ fixRefID(reference = reference, 'tortilla', 'corn'), #Similar ingredients just with more salt
          str_detect(Ingredients, 'broth cube') ~ fixRefID(reference = reference, 'stock', 'cubes'),
          Ingredients == 'mango chutney' ~ fixRefID(reference = reference, 'mango chutney'),
          Ingredients == 'soybean oil' ~ fixRefID(reference = reference, 'soy', 'oil'),
          Ingredients == 'mustard honey' ~ fixRefID(reference = reference, 'mustard'),
          Ingredients == 'refrigerated buttermilk biscuit dough' ~ fixRefID(reference = reference, 'refrigerated buttermilk biscuit dough'),
          
          #Dairy
          str_detect(Ingredients, 'cheddar|romano|parmigiano-reggiano|parmesan|parmigiano-reggiano|cheese hard goat|cheese cotjia|gruyere') ~ fixRefID(reference = reference, 'hard cheese'),
          str_detect(Ingredients, 'halloumi|manchego|havarti|swiss|monterey jack|pepperjack|asiago|mozzarella|goat brown cheese|jarlsberg|cheese semi-hard|provolone|norvegia') ~ fixRefID(reference = reference, 'hard to semi-hard cheese'),
          str_detect(Ingredients, 'ricotta|cheese blue|camembert|chevre|neufchatel|port salut|brie|mascarpone|gorgonzola|cheese soft') ~ fixRefID(reference = reference, 'soft-ripened cheese'),
          Ingredients == 'cheese american' ~ fixRefID(reference = reference, 'processed cheese and spreads'),
          Ingredients == 'yogurt greek' | Ingredients == 'kefir' | str_detect(Ingredients, 'quark') ~ fixRefID(reference = reference, 'yoghurt'),
          
          
          #Bread and rolls
          Ingredients %in% c('bread', 'bread coarse', 'tortilla coarse', 'crisp bread coarse',
                             'bread crumb', 'bread brown chapati') ~ fixRefID(reference = reference, 'wheat bread and rolls', 'brown'),
          Ingredients %in% c('hamburger bun', 'bread white', 'tortilla', 'crisp bread', 'breadstick', 'ciabatta',
                             'rolls white', 'cracker cream', 'bread naan', 'bread flat hard') | str_detect(Ingredients, 'rolls white') ~ fixRefID(reference = reference, 'wheat bread and rolls', 'white'),
          
          #Herbs and spices
          str_detect(Ingredients, 'saffron|fenugreek seed|mint fresh herbs|mint dried|lemon balm|turmeric|anise|marjoram|sazon seasoning|ginger|caraway|lemongrass|basil|rosemary|thyme|tarragon|pepper|sage|masala|oregano|spice mix|nutmeg|cloves|coriander|cumin|dill|fenugreek leaf|juniper berry|cinnamon|chives|chervil|cardamom|caper|allspice|bay leaf|paprika powder|fennel seed') &
            !str_detect(Ingredients, 'sauce|paste|sweet|chili') |
            str_detect(Ingredients, 'chili') & !str_detect(Ingredients, 'pepper|paste|sauce') |
            Ingredients %in% c('herbs', 'different spices', 'spices') ~ fixRefID(reference = reference, 'mixed', 'herbs'),
          
          #Not in ref
          Ingredients %in% c('yeast nutritional', 'paste chili', 'agar', 'gluten',
                             'corn starch', 'nori seaweed','salmon roe',
                             'plantain', 'tabasco', 'tapioca', 'sake', 'wine rice', 'liquid smoke flavoring',
                             'pack high quality charcoal briquettes', 'cooking spray', 'quinoa', 'paste carrot',
                             'red food coloring', 'toro greek moussaka', 'banana', 'paste curry',
                             'fish scraps for broth', 'fish soup base', 'paste garlic',
                             'pomegranate kernel', 'sauce white', 'celery seed',
                             'sauce bearnaise', 'wine rice', 'onion soup mix') ~ 0,
          
          TRUE ~ ID
        ))
      
      #Add the new reference words
      results <- temp %>%
        inner_join(., reference) %>%
        mutate(ref = paste0(first_word, ' ', second_word)) %>%
        select(-c(first_word, second_word, loop))
      
    } else {
      stop("Sorry, there are no added fixes for the reference used.")
    }
    
  } else {
    results
  }
  
  results
  
}
