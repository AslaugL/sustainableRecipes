#' Standardise the ingredients in a recipe
#' @title standardiseRecipes
#'
#' @description Standardise all names of different ingredients, and extract to separate columns and standardise units of each ingredient, volume units to dl and weight units to kg Ingredients with similar density to water is turned into kg.
#'
#' @param df A dataframe with an Ingredient column with rowwise ingredients where amount + unit (i.e. 1 dl) is at the beginning of the row.
#'
#' @return The dataframe with all ingredient amounts and units in separate columns, with ingredient names and units  standardised.
#'
#' @export
standardiseRecipes <- function(df){
  
  #Different units to use
  units = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf', 'can',
            'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice', '\\bclove\\b',
            'neve', 'ml', 'cl', 'bunch', 'pack', 'plate', 'drop', 'twig', 'pound', 'ounce', 'stalk',
            'quart') %>%
    #Add whitespace on both sides to only match a unit in a string
    sapply(., function(x) {paste0('\\s', x, '\\s')}) 
  
  #Ingredients that are counted as individual pieces/stk
  stk = c('anchovy', 'anise', 'apple', 'apricot', 'avocado', 'artichoke', 'aubergine',
              
              'banana', 'bay leaf', 'bean', 'baguette', 'bok choi', 'broccoli', 'broth cube', 'basil',
              'bread',
              
              'cabbage', 'cardamom', 'carrot', 'cauliflower', 'Celariac root', 'celery', 'celery stalk',
              'champignons', 'chicken', 'chicory', 'chili', 'ciabatta', 'cinnamon', 'clementine', '\\bcloves\\b', 'cod',
              'cod fillet', 'crab', 'cracker cream', 'cucumber', 'coriander',
              
              'duck',
              
              'egg', 'entrecôtekam', 'eggplant',
              
              'fennel', 'fig',
              
              'garlic', 'grape',
              
              'hamburger bun', 'hen breast fillet grouse', 'herring smoked',
              
              'jerusalem artichoke', 'juniper berry',
          
              'kiwi',
              
              'lamb chop', 'leek', 'lemon', 'lemongrass', 'lettuce', 'lime',
              
              'mango', 'marshmallow', 'mushroom',
              
              'nori seaweed', 'nut',
              
              'olive', 'onion', 'orange',
              
              'paprika', 'pear', 'pepper', 'peppercorns', 'pineapple', 'pomegranate', 'plate', 'pork',
              'pork sausage', 'potato', 'prawn', 'prune',
              
              'radish', 'roll',
              
              'salad', 'salmon', 'scallion', 'scallop', 'scampi', 'shallot', 'sheep', 'sheet', 'shrimp',
              'squid', 'stock cube',
              
              'tenderloin', 'thyme', 'tomato', 'tortilla', 'trout', 'turkey')
  
  standardise = df %>%
  
  #Extract the amount and units of each ingredient into their own column
  mutate(Amounts = case_when(
    
    #Extract amounts with units
    str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) ~
      str_extract(Ingredients, '((\\d+\\.\\d+|\\d+-\\d+|\\d+)\\s\\w+)'),
    #Extract pure numbers
    !str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) &
      str_detect(Ingredients, '^\\d+') ~ str_extract(Ingredients, '^[^\\s]+')),
    
    #Remove this information from Ingredients columns
    Ingredients = case_when(
      str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) ~
        str_remove(Ingredients, '((\\d+\\.\\d+|\\d+-\\d+|\\d+)\\s\\w+)'),
      !str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) & str_detect(Ingredients, '^\\d+') ~
        str_remove_all(Ingredients, '^[^\\s]+'),
      TRUE ~ Ingredients)) %>%
    
    #Trim and squish
    mutate(Ingredients = Ingredients %>%
             str_trim(side = 'both') %>%
             str_squish()) %>%
    
    #Standardise ingredient names
    standardiseIngredients() %>%
    
    #Add stk to the amounts of all the items listed in 'stk'
    mutate(Amounts = case_when(
      !str_detect(Amounts, '[:alpha:]') &
        str_detect(Ingredients, regex(paste0(stk, collapse = '|'), ignore_case = TRUE)) ~ paste0(Amounts, ' stk'),
      TRUE ~ Amounts)) %>%
    
    #Split Amounts into an amounts and units column
    separate(., Amounts, c('Amounts', 'unit'), sep = ' ') %>%
    #Turn amounts into numeric
    mutate_at('Amounts', ~as.numeric(.)) %>%
    
    #Standardise weight units to grams and volume units to dl
    standardiseUnits()
}