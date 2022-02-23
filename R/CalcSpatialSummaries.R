#' Calculate summaries of the posterior predictive distribution of proportion
#' of cells that exceed a target value.
#'
#' @description
#' `CalcSpatialSummaries()` computes summary statistics of the posterior
#' predictive distribution of the proportion of cells that exceed a
#' target value.
#'
#' @param cover An array of cover simulations from \code{\link{SimDAST}}.
#'
#' @param target The target value of percent cover.
#' @return A dataframe.
#'
#' @export
#' @import tidyr
#' @import dplyr
#' @import magrittr

CalcSpatialSummaries <- function(cover, target) {
  myfunc <- function(x) {
    x <- x[!is.na(x)]
    sum(x >= target) / length(x)
  }
  tmp <- apply(cover, c(1,4), myfunc)
  out <- as.data.frame(tmp) %>%
    mutate(Year = 0:(nrow(tmp) - 1)) %>%
    pivot_longer(cols = starts_with("V")) %>%
    mutate(name = as.numeric(gsub("V", "", name))) %>%
    rename("Ensemble" = name, "Proportion" = value)
  return(out)
}
