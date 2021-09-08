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
   
   standardised <- df %>% mutate(Amounts = case_when(
     Ingredients_standardised == 'cider' & unit == 'glass' ~ 3.41,
     unit == 'cup' ~ Amounts * 2.45,
     unit == 'l' ~ Amounts * 10,
     unit == 'ml' ~ Amounts / 100,
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
       unit %in% c('cup', 'l', 'ml', 'tsp', 'tbsp', 'krm', 'drop', 'pinch') | Ingredients_standardised == 'cider' & unit == 'glass' ~ 'dl',
       unit %in% c('ounce', 'pound') ~ 'g',
       Ingredients_standardised %in% c('shrimp', 'salad rocket') ~ str_replace(unit, 'neve', 'dl'),
       str_detect(Ingredients, 'beef') & !str_detect(Ingredients, 'tongue|cube') ~ str_replace(unit, 'stk', 'portion'),
       str_detect(Ingredients_standardised, 'fresh herb|coriander|basil|thyme') ~ str_replace(unit, 'stk', 'twig'),
       str_detect(Ingredients_standardised, 'salad|caper|parsley') ~ str_replace(unit, 'neve', 'dl'),
       TRUE ~ unit
     ))
 }