#' Standardising the units in a recipe. Volume units to dl, weight to g.
#' @title standardiseUnits
#'
#' @description Standardise units, volume units to dl and weight units to grams.
#'
#' @param df A dataframe with a unit column indicating what type of unit is used, and a numerical Amount column listing the amount.
#'
#' @return The dataframe with all ingredient units and volume standardised.
#'
#' @export
 standardiseUnits <- function(df){
   
   #Turn everything into the same unit----
   standardised <- df %>% mutate(Amounts = case_when(
     str_detect(Ingredients, 'cider') & unit == 'glass' ~ 3.41,
     unit == 'cup' ~ Amounts * 2.45,
     unit == 'l' ~ Amounts * 10,
     unit == 'ml' ~ Amounts / 100,
     unit == 'cl' ~ Amounts / 10,
     unit == 'tbsp' ~ Amounts / 6.67,
     unit == 'tsp' ~ Amounts / 20,
     unit == 'krm' ~ Amounts / 100,
     unit == 'drop' ~ Amounts / 2000, #One drop is 0.05ml 
     unit == 'pinch' ~ Amounts / (20*16), #A pinch is usually defined as 1/16 of a tsp
     unit == 'ounce' ~ Amounts * 28.35,
     unit == 'pound' ~ Amounts * 453.59,
     TRUE ~ Amounts
   )) %>%
     mutate(unit = case_when(
       unit %in% c('cup', 'l', 'ml', 'cl', 'tsp', 'tbsp', 'krm', 'drop', 'pinch') | str_detect(Ingredients, 'cider') & unit == 'glass' ~ 'dl',
       unit %in% c('ounce', 'pound') ~ 'g',
       Ingredients %in% c('shrimp', 'salad rocket') ~ str_replace(unit, 'neve', 'dl'),
       str_detect(Ingredients, 'beef') & !str_detect(Ingredients, 'tongue|cube') ~ str_replace(unit, 'stk|slice', 'portion'),
       str_detect(Ingredients, 'cod fillet') ~ str_replace(unit, 'slice', 'portion'),
       str_detect(Ingredients, 'salad|caper|parsley') ~ str_replace(unit, 'neve', 'dl'),
       TRUE ~ unit
     )) %>%
      mutate(unit = case_when(
         str_detect(Ingredients, 'fresh herb') & Source == 'Kolonialen' ~ str_replace(unit, 'stk|twig', 'bunch'),
         
         TRUE ~ unit
      )) %>%
      
      #Turn juice, water, vinegar and other liquids with similar density to water from dl/l to grams as they are all about 100g/dl
      mutate(
         Amounts = case_when(
            (str_detect(Ingredients, 'water|kefir|spirits|beer|madeira|marsala|cognac|cider|juice|Juice|broth|kraft|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
                unit == 'dl') &
               !str_detect(Ingredients, 'sugar|cheese|flour') ~ Amounts * 100,
            TRUE ~ Amounts),
         unit = case_when(
            (str_detect(Ingredients, 'water|kefir|spirits|beer|madeira|marsala|cognac|cider|juice|Juice|broth|wine|vin|eddik|vinegar|fund|milk|stock|cream|Crème Fraîche|rømme|yogurt|yoghurt|sherry') &
                unit == 'dl') &
               !str_detect(Ingredients, 'sugar|cheese|flour') ~ 'g',
            TRUE ~ unit)) %>%
      
      #From grams into kilos 
      mutate(
         Amounts = case_when(
            unit == 'g' ~ Amounts/1000,
            TRUE ~ Amounts),
         unit = unit %>%
            str_replace('\\bg\\b', 'kg'))
   
   
   #Single types of ingredients that needs standardisation----
   #Change the amount of lemon/orange/lime juice/zest from whole pieces of fruit to dl when applicable
   temp <- standardised %>%
      filter(str_detect(Ingredients, 'the juice|the zest')) %>%
      
      #If it says 'juice and zest' treat as a whole fruit
      mutate(Ingredients = str_replace(Ingredients, ', the juice and zest', '')) %>%
      
      #Turn whole fruits into the equivalent amount of juice in kg and zest in dl
      mutate(
         Amounts = case_when(
            #Juice (info from https://www.webstaurantstore.com/blog/2760/juice-in-citrus-fruits.html)
            str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(unit, 'stk') ~ (Amounts*3)*0.015,
            str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(unit, 'stk') ~ (Amounts*2)*0.015,
            #Zest (info from https://bakingbites.com/2017/01/how-much-zest-does-citrus-lemon-yield/)
            str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ (Amounts*(1/3))*0.15,
            str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ (Amounts*1)*0.15,
            str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ (Amounts*1.5)*0.15,
            
            TRUE ~ Amounts),
         unit = case_when(
            str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'stk') ~ 'kg',
            str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'stk') ~ 'dl',
            
            TRUE ~ unit)
      )
   
   #Add back
   standardised <- standardised %>%
      #Remove old rows
      filter(!str_detect(Ingredients, 'the juice|the zest')) %>%
      #Add updated ones
      bind_rows(., temp)
   
   #Split rows with 'salt and pepper and oil' and similar
   standardised <- standardised %>%
      separate_rows(Ingredients, sep = ' and ') %>%
      #Remove "slice" of pepper/salt, mistaken translation from Norwegian
      mutate(unit = case_when(
         !(Ingredients %in% c('salt', 'pepper') & unit == 'slice') ~ unit
      ))
   
   #Split broth into water and broth cubes
   #Pull out broth ingredients
   temp <- standardised %>%
      filter(str_detect(Ingredients, 'water broth')) %>%
      #Add / to split the rows into two separate ingredients, and add "cube" to broth
      mutate(Ingredients = str_replace(Ingredients, 'water broth', 'water/broth cube')) %>%
      separate_rows(Ingredients, sep = '/') %>%
      #Turn the broth cube amounts into number of broth cubes, 1 cube per 5 dl/0.5 kg water
      mutate(
         Amounts = case_when(
            str_detect(Ingredients, 'broth cube') ~ Amounts/0.5,
            TRUE ~ Amounts),
         unit = case_when(
            str_detect(Ingredients, 'broth cube') ~ 'stk',
            TRUE ~ unit)
      )
   
   #Add back to recipes
   standardised <- standardised %>%
      #Remove broth wthout broth cubes
      filter(!str_detect(Ingredients, 'water broth')) %>%
      #Add back with broth cues
      bind_rows(., temp)
   
   #Turn cooked products into their raw equivalents, using the convertion factors from Helsedirekttortatet Mål Vekt og Porsjonsstørrelser----
   standardised <- standardised %>%
      mutate(
         Amounts = case_when(
            Ingredients == 'bacon cooked' ~ Amounts/0.31,
            Ingredients %in% c('chicken breast cooked', 'chicken breast without skin cooked') ~ Amounts/0.8,
            Ingredients %in% c('chicken cooked', 'turkey meat cooked') ~ Amounts/0.4,
            Ingredients == 'lamb shoulder cooked' ~ Amounts/0.56,
            Ingredients == 'pork tenderloin cooked' ~ Amounts/0.75,
            Ingredients == 'pasta cooked' ~ Amounts/2.63,
            Ingredients == 'rice cooked' ~ Amounts/2.94,
            Ingredients == 'beetroot cooked' ~ Amounts/0.95, #Use values for parsley root
            Ingredients %in% c('lobster cooked', 'shrimp cooked') ~ Amounts/0.77, #Use values for Lean fish, with skin, simmered
            
            TRUE ~ Amounts),
         #Remove cooked from ingredient name
         Ingredients = str_replace(Ingredients, ' cooked', '')
      )
 }