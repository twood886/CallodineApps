#' @title Named Group Split
#' @description Split data by group and rename list by group name
#' @import tidyverse
#' @export
named_group_split <- function (...) {
  data <- group_by(...)
  names <- group_keys(data) %>% map(as.character) %>% reduce(paste,
                                                             sep = "~~")
  group_split(data) %>% set_names(names)
}
