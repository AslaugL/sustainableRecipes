#' Helper function to get results from corr.test
#' @title collectCorr
#'
#' @description Create a df with group, adjusted p.value and symbols for showing the significance of the p value from a corr.test result.
#'
#' @param corr_test_results The results dataframe from running corr.test.
#'
#' @return A dataframe with the correlation, its p-value and the level of significance in *.
#'
#' @export

collectCorr <- function(corr_test_results) {
  
  correlations <- tibble(
    'estimate' = round(corr_test_results$r[1,2], 2),
    'pvalue' = corr_test_results$p.adj,
  ) %>%
    mutate(pvalue_star = as.character(symnum(pvalue, corr = FALSE, na = FALSE, 
                                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                             symbols = c("***",
                                                         "**",
                                                         "*",
                                                         "'", " "))))
                                             # With larger textsize depending on significance
                                             # symbols = c("<span style='font-size:18pt'>***</span>",
                                             #             "<span style='font-size:15pt'>**</span>",
                                             #             "<span style='font-size:12pt'>*</span>", "'", " "))))
}
