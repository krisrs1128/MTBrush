#' Calculate test statistics across all features
#'
#' This method maps overall all groups of subsets and returns a set of test statistics associated with each.
#' The testing function can be specified by the user.
#'
#' @param subsets List of data.frame subsets as output by split_dataset
#' @param lm_func A function that takes one subset of samples and returns a vector of test statistics associated with it
#' @param group The name of the column used when initially splitting measurements across features
#'
#' @return A single data.frame with statistics from the collection of tests across features
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#' @importFrom purrr map map_dfr
#'
#' @examples
#'
#' sub <- split_dataset(example_data, feature)
#' fit_statistics(sub, ~ lm(value ~ X1 * X2, .), feature)
fit_statistics <- function(subsets, lm_func, group) {
  group <- enquo(group)
  subsets %>%
    map(lm_func) %>%
    map_dfr(tidy, .id = as_label(group)) %>%
    select(!!group, everything())
}
