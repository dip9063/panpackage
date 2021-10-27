#' function1
#' Creates a boxplot of \code{varx} versus \code{vary} with ggplot.
#'
#' @param df dataframe name
#' @param varx string name for variable x in the dataframe
#' @param vary string name for variable y in the dataframe
#' @param title text data for title of the boxplot
#' @param xlab text data for the x-axis for the boxplot
#' @param ylab text data for the y-axis of the boxplot
#' @param legend text data for the legend title of the boxplot
#'
#' @return a boxplot of \code{varx} versus \code{vary}, with title
#' x and y labels, and legend title.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @import dplyr
#'
#' @examples
#' function1(df = drg,
#'          varx = "DRG.Definition",
#'          vary = "Average.Total.Payments",
#'          title = "title name",
#'          xlab = "x label",
#'          ylab = "y label",
#'          legend = "legend title")
#'
#'
function1 <- function(df, varx, vary, title, xlab, ylab, legend) {

  ggplot(df, aes(x = get(varx), y = get(vary))) +
    geom_boxplot() +
    labs(title = title,
         y = ylab,
         x = xlab) +
    guides(color = guide_legend(title = legend)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))

}
