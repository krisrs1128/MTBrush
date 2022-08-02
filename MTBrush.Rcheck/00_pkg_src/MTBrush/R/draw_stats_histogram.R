#' draw histograms of statistics for each term
#'
#'This function generates histograms of statistics for all terms calculated by fit_statistics which also serve as base plots in the Shiny App interface to make interactions on
#'
#' @param stats_df  A data.frame contains statistics of tests across features; the output from fit_statistics function
#'
#' @return Histograms of statistics based on results from previous hypothesis testings across features
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @examples
#' fit_results <- example_data %>% split_dataset(feature) %>% fit_statistics(~ lm(value ~ X1 * X2, .), feature)
#' draw_stats_histogram(fit_results)

draw_stats_histogram <- function(stats_df){
  ggplot(stats_df %>% filter(term != "(Intercept)")) +
    geom_histogram(aes(x = statistic), bins = 100) +
    scale_y_continuous(expand = c(0, 0, .1, .1)) +
    facet_wrap(~ term, scales = "free")
}
