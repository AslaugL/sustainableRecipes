#' Find the amounts (percent) of fruit, vegetables, legumes, nuts and oils in recipes.
#' @title pctOfFruitVegLegumesNutsOils
#'
#' @description Find the amounts in percent of fruit, vegetables, legumes, nuts and oils in a recipe.
#'
#' @param df A tidy dataframe with a sample_id column for recipe names, an Ingredients column with ingredients in rows, a Foodgroup column and an Amounts column (with the amounts in weight).
#'
#' @return The dataframe with a a feature column with two features: one with the pct of fruit, vegetables, legumes, nuts and oils used to calculate Nutriscore, and one with fruit, vegetables, legumes and nuts to calculate Keyhole score.
#'
#' @export
pctOfFruitVegLegumesNutsOils <- function(df) {
  
  with_pct <- df %>%
    select(sample_id, Ingredients, Foodgroup, Amounts) %>% unique() %>%
    
    #Nutriscore and keyhole relevant foodgroups and ingredients
    mutate(
      nutriscore_foods = case_when(
        (Foodgroup %in% c('Vegetables and\nvegetable products', 'Fruit and\nfruit products',
                          'Fruit/vegetable juice\n and nectar', 'Legumes, nuts, seeds') &
           !str_detect(Ingredients, 'mushroom|coconut milk|sesame seed|pine')) |
          str_detect(Ingredients, 'olive oil|rapeseed oil|walnut oil') ~ 'nutriscore_fruit_veg_legumes_nuts_oils'),
      keyhole_foods = case_when(
        Foodgroup %in% c('Vegetables and\nvegetable products', 'Fruit and\nfruit products',
                         'Legumes, nuts, seeds') & !str_detect(Ingredients, 'peanut') ~ 'keyhole_fruit_veg_legumes')
    ) %>%
    
    #Keep relevant amounts, remove the rest
    mutate(
      nutriscore_amounts_pct = case_when(
        #Dried fruit/veg and ketchup/purees should be doubled
        !is.na(nutriscore_foods) & str_detect(Ingredients, 'paste tomato|ketchup|dried|raisin|prune') ~ Amounts*2,
        !is.na(nutriscore_foods) ~ Amounts),
      keyhole_amounts_pct = case_when(
        !is.na(keyhole_foods) ~ Amounts)
    ) %>%
    select(-c(Amounts, Foodgroup, nutriscore_foods, keyhole_foods)) %>%
    
    #Calculate total amounts for each recipe
    #Turn longer, since amounts are pr 100 g the % of vegetables etc is found by multiplying with 100
    pivot_longer(.,
                 cols = c(nutriscore_amounts_pct, keyhole_amounts_pct),
                 names_to = 'feature',
                 values_to = 'value') %>%
    group_by(sample_id, feature) %>%
    summarise(pct = sum(value, na.rm = TRUE)*100) %>%
    ungroup()
  
}
  
  