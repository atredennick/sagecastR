#' Performs sensitivity to climate analysis for fitted model with spatial
#'    component to test against targets.
#'
#' @param posteriors A k*p matrix, where k is the number of MCMC iterations
#'     and p is the number of parameters. Column names must be: \code{Beta[1]},
#'     \code{Beta[2]}, \code{Beta[3]}, \code{Beta[4]}, \code{sigma_a}, and
#'     \code{sigma_y}.
#' @param eta A k*c matrix, where k is the number of MCMC iterations and
#'     c is the number of grid cells in the spatial domain.
#' @param delta The deviation from mean climate. A named vector: one element for
#'     "precipitation" and one element for "temperature".
#' @param coreName TBD
#' @param target A percent cover target value.
#' @return A dataframe.
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @export

RunSensitivityTarget <- function(
  posteriors,
  eta,
  delta,
  coreName,
  target
) {
  posts <- posteriors
  drivers <- c("Baseline", "Precipitation", "Temperature", "Both")
  muPrime <- posts[ , "Beta[1]"] / (1 - posts[ , "Beta[2]"])

  pry <- matrix(data = NA, nrow = length(muPrime), ncol = length(drivers))
  colnames(pry) <- drivers

  for(drive in drivers) {
    X <- switch(
      drive,
      "Baseline" = c(0, 0),
      "Precipitation" = c(delta["precipitation"], 0),
      "Temperature" = c(0, delta["temperature"]),
      "Both" = c(delta["precipitation"], delta["temperature"])
    )
    for(i in 1:length(muPrime)) {
      tmp <- muPrime[i] + eta + posts[i, "Beta[3]"] * X[1] + posts[i, "Beta[4]"] * X[2]
      pry[i, drive] <- 1 - ecdf(exp(tmp))(target)
    }
  }

  res <- as.data.frame(pry) %>%
    mutate(CoreName = coreName,
           Rep = 1:n()) %>%
    pivot_longer(cols = c(Baseline, Precipitation, Temperature, Both)) %>%
    mutate(name = factor(name, levels = c(drivers))) %>%
    rename("ProportionGreaterThanTarget" = value,
           "Driver" = name) %>%
    dplyr::as_tibble()

  return(res)
}
