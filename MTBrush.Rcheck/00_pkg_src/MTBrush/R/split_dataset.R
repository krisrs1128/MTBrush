#' Split the data frame into many subsets
#'
#' This function splits the given data set into many smaller subsets based on the feature of interest.
#' Each subset only contains measurements of a sample and all subsets take same measurements.
#'
#' @param df  A long data.frame with sample measurements across all features
#' @param group The column name of the feature over which to split the data.frame for downstream testing
#'
#' @return A list with one data.frame for each feature specified in group
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @examples
#' split_dataset(example_data, feature)

split_dataset <- function(df, group) {
  group <-enquo(group)
  subsets <- df %>%
    mutate(group = as.factor(!!group)) %>%
    split(list(.$group))
  return(subsets)
}
