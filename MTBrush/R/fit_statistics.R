#' fit_statistics
#'
#' This method helps to fit a model for groups of subset
#'
#' @param subsets the groups of subset from split_dataset function
#' @param lm_func the linear model chosen by user to fit the data
#' @param group the name of the group column
#'
#' @return a table with statistics of each term for all subsets
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom purrr map map_dfr

fit_statistics <- function(subsets, lm_func, group) {
  group <-enquo(group)
  statistics <- subsets %>%
    map(lm_func) %>%
    map_dfr(tidy, .id = as_label(group)) %>%
    select(!!group, everything())
  return(statistics)
}
