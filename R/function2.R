#' function2
#' groups data by \code{group}, followed by a specific summary statistic for a
#' selected variable \code{value} within the grouped data.
#' @param data is the dataframe name
#' @param group string name for variable to group by.
#' @param fun name of the function to be used. Example: mean, median, or std.
#' @param value string name for variable to use the summary statistic function on.
#'
#' @return a table with two columns 1) \code{group}, and 2) a column with the name \code{statistic} that shows the value of the statistic.
#' @export
#' @import dplyr
#'
#' @examples
#' function2(data = drg,
#'          group = DRG.Definition,
#'          fun = mean,
#'          value = Average.Medicare.Payments)
#'
#'
#'
function2 <- function(data, group, fun, value) {
  data %>% group_by({{group}}) %>%
    summarize(statistic = fun({{value}}))
}

