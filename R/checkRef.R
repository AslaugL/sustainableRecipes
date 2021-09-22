#' Check if an ingredient can be found in a reference database.
#' @title checkRef
#'
#' @description Checks if an ingredient is present in a reference database
#'
#' @param df A dataframe with an Ingredients column to be checked against the reference database.
#' @param reference The reference database.
#'
#' @return A dataframe with the ingredient name and the reference ID of the first hit in the database. See references for how the databse must be built.
#'
#' @export

#Look through a list of ingredients
checkRef <- function(df, reference){
  
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
  results <- lapply(df$Ingredients, temp, reference = reference) %>% bind_rows()
  
}
