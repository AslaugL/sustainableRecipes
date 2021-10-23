#' @title filterP
#'
#' @description Filter a p.adj column in a dataframe for values below a certain number, default 0.05.
#'
#' @param df Dataframe to be filtered.
#' @param filterp Filter p.adj? 'yes' or 'no'? Default is 'yes'.
#' @param num p.adj below this number is kept. Default is 0.05.
#'
#' @return A filtered dataframe.
#'
#' @export

filterP <- function(df, filterp = 'yes', num = 0.05){
  #Filter p.adj value
  if(filterp == 'yes'){
    df %>%
      dplyr::filter(p.adj < num)
  }else if(filterp == 'no'){
    df <- df
  }else {
    stop("filterp must be yes or no")
  }
}
