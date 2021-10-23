#' @title filterFeatures
#'
#' @description Filter a 'feature' column in a dataframe for feature(s) of interest.
#'
#' @param df Dataframe to be filtered, must have a 'feature' column.
#' @param features Feature(s) to filter the dataframe by.
#'
#' @return A filtered dataframe.
#'
#' @export

filterFeatures <- function(df, features){
  #Filter the feature column for any features provided
  if(any(df$feature %in% features)){
    df %>% filter(feature %in% features) #If any features are found in the feature column, filter the column by these
  }else if(!any(df$feature %in% features) & length(features) > 0){ #If none of the provided features are found, give notice
    print("None of the provided features were found in the data")
    df <- df
  }else if(is.null(feature)){ #If no features are provided, do nothing
    df <- df
  }
}
