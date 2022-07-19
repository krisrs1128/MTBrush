# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Split the data set
#'
#' This function splits the given data set into many smaller subsets based on the group
#'
#' @param df the given data frame
#' @param group the name of the group column
#'
#' @return groups of subsets
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%

split_dataset <- function(df, group) {
  group <-enquo(group)
  subsets <- df %>%
    mutate(group = as.factor(!!group)) %>%
    split(list(.$group))
  return(subsets)
}
