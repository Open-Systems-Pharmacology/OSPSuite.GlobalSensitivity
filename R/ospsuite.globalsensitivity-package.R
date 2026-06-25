#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @importFrom dplyr %>%
#' @importFrom ospsuite pkAnalysesToDataFrame
#' @importFrom stats aggregate approx fft na.omit qnorm quantile runif sd
#' @importFrom utils head tail
## usethis namespace: end
NULL

# Declare variables that are used in non-standard evaluation (e.g. inside
# `ggplot2::aes()`, `subset()` and `facet_grid()`) so that R CMD check does not
# report them as undefined global variables.
utils::globalVariables(c(
  "Measure",
  "Parameter",
  "ParameterDisplayName",
  "ParameterRanks",
  "Value",
  "label",
  "mustar",
  "output",
  "pk",
  "signColumn",
  "stdv",
  "values",
  "x",
  "xLab",
  "y",
  "yLab",
  "ymax",
  "ymean",
  "ymin",
  "z"
))
