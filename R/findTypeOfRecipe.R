#' What type of recipe is it?
#' @title findTypeOfRecipe
#'
#' @description Decide type of recipe based on its name. Recipe type corresponds to Keyhole certification types of food.
#'
#' @param df A dataframe with an column of recipe names titled "sample_id".
#'
#' @return The dataframe with a new column "recipe_type"
#'
#' @export

findTypeOfRecipe <- function(df) {
  
  with_type <- df %>%
    #Type of recipe
    mutate(
      recipe_type = case_when(
        str_detect(sample_id, 'soup|stew|\\bpot\\b|casserole') ~ 'soups_and_stews',
        str_detect(sample_id, 'pizza|\\bpie\\b|\\bPies\\b|\\btart\\b') ~ 'pizza_and_pies',
        str_detect(sample_id, 'wraps') ~ 'wraps',
        
        #Use the ready meal with either a carbohydrate or a protein part, description 27 in Forskrift om frivillig merking med Nøkkelhullet
        TRUE ~ 'ready_meals_27' 
      )
    )
  
}
  
  