#' Calculate summaries of the posterior predictive distribution
#' of average cover.
#'
#' @description
#' `CalcTemporalSummaries()` computes summary statistics of the posterior
#' predictive distribution of average cover in each year. Cover is averaged
#' over space and then the summaries are computed over MCMC iterations.
#'
#' @param cover An array of cover simulations from \code{\link{SimDAST}}.
#'
#' @return A dataframe.
#' @export

CalcTemporalSummaries <- function(cover) {
  cover <- apply(cover, c(1, 4), mean, na.rm = TRUE)
  x <- apply(cover, 1, mean, na.rm = TRUE)
  xll95 <- apply(cover, 1, quantile, p = 0.025, na.rm = TRUE)
  xul95 <- apply(cover, 1, quantile, p = 0.975, na.rm = TRUE)
  xll68 <- apply(cover, 1, quantile, p = 0.16, na.rm = TRUE)
  xul68 <- apply(cover, 1, quantile, p = 0.84, na.rm = TRUE)

  out <- data.frame(Year = 0:(length(x)-1),
                    MeanCover = x,
                    LL95 = xll95,
                    UL95 = xul95,
                    LL68 = xll68,
                    UL68 = xul68)
  return(out)
}
