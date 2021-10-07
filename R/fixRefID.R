#' Fix the ID number of an ingredient if checkRef made a mistake.
#' @title fixRefID
#'
#' @description Set a new ID number for an ingredient if checkRef made a mistake.
#'
#' @param reference Reference database to look up.
#' @param first_w The correct first word in reference database.
#' @param second_w The correct second word in reference database.
#'
#' @return The updated ID number.
#'
#' @export

fixRefID <- function(reference, first_w, second_w = '\\') {
  
  ID <- reference %>% filter(first_word == first_w & second_word == second_w) %>% select(ID) %>% as.numeric(.)
  
}
