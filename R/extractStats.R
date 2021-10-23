#' @title extractStats
#'
#' @description Get a string of stats for plots, from an rstatix-results df (t-test/wilcox + respective effect sizes)
#'
#' @param df Rstatix results dataframe after Welch's t-test or Wilcoxon test.
#' @param filterp Should the dataframe be filtered for their p.adj value? Default 'yes'.
#' @param num If filterp is 'yes', only features with p.adj values < num are kept. Default is 0.05.
#'
#' @return A dataframe with the feature column and a new 'string' column with stats that can be used as titles for plots.
#'
#' @export

extractStats <- function(df, filterp = 'yes', num = 0.05) {

  stats <- df %>%
    filterP(., filterp = filterp, num = num) %>% #Filter out significantly different features
    mutate_at(., 'p.adj', ~formatC(., format = 'e', digits = 2)) %>% #Format scientific notation
    mutate_at(., c('estimate', 'conf.low', 'conf.high', 'effsize'), ~round(., digits = 2)) %>% #Format digits

    #Write the string based on test type, t-test or wilcox
    dplyr::mutate(., string = case_when(
      method == "Welch's t-test" ~ sprintf('%s, Welch t test, FDR: %s\n Estimate: %s, ci: %s; %s, Effect size: %s', .$feature, .$p.adj, .$estimate, .$conf.low, .$conf.high, .$effsize),
      method == 'Wilcoxon' ~ sprintf('%s, Wilcox, FDR: %s\n Estimate: %s, ci: %s;  %s,  Effect size: %s', .$feature, .$p.adj, .$estimate, .$conf.low, .$conf.high, .$effsize))
    ) %>% select(feature, string)
}
