#' draw histograms of statistics for each term
#'
#'This is a part of the shiny app to draw several histograms of statistics for each term
#'
#' @param stats_df the output table from fit_statistics function
#'
#' @return several histograms of all subsets based on terms in the model
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%

draw_stats_histogram <- function(stats_df){
  ggplot(stats_df %>% filter(term != "(Intercept)")) +
    geom_histogram(aes(x = statistic), bins = 100) +
    scale_y_continuous(expand = c(0, 0, .1, .1)) +
    facet_wrap(~ term, scales = "free")
}
