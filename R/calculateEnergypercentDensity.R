#' @title calculateEnergypercentDensity
#'
#' @description Calculate percent of total energy from macronutrients, and density per MJ energy.
#'
#' @param df A dataframe with of recipes with sample_id (recipe id), and names of the macronutrients like they are named in Matvaretabellen: Kilocalories, Kilojoules, Carbo, Sugar, Fat, SatFa, Protein, `Dietary fibre`.
#'
#' @return A tidy dataframe with a sample_id, densityMJ and energy_percent column, and a feature column explaining which macronutrient the densityMJ/energy_perecent refers to.
#'
#' @export
calculateEnergypercentDensity <- function(df){
  
  df %>%
    
    #Select macronutrient columns
    select(sample_id, Kilocalories, Kilojoules, Carbo, Sugar, Fat, SatFa, Protein, `Dietary fibre`) %>%
    
    #Turn tidy for calculations
    pivot_longer(
      cols = -c(sample_id, Kilocalories, Kilojoules),
      names_to = 'feature',
      values_to = 'grams') %>%
    #Create a MJ column for density pr MJ calculations
    mutate(
      MJ = Kilocalories*0.004184,
      
      #Calculate how many kcals from each macronutrient, and the density of fibre pr megajoule energy
      #Carbohydrates, sugar and protein is about 4kcal/g, fat 9kcal/g
      kcal_macro = case_when(
        str_detect(feature, 'Carbo|Sugar|Protein') ~ grams*4,
        str_detect(feature, 'Fa') ~ grams*9,
        str_detect(feature, 'fibre') ~ grams*2),
      densityMJ = grams/MJ,
      energy_percent = kcal_macro/Kilocalories*100
    ) %>%
    
    #Remove columns
    select(-c(MJ, kcal_macro, grams, Kilojoules, Kilocalories))
  
}