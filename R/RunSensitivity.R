#' Performs sensitivity to climate analysis for fitted model.
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
#' @return A dataframe.
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @export

RunSensitivity <- function(
  posteriors,
  delta,
  coreName
) {
  posts <- posteriors
  drivers <- c("Precipitation", "Temperature", "Both")

  muPrime <- posts[ , "Beta[1]"] / (1 - posts[ , "Beta[2]"])
  muPert <- matrix(data = NA, nrow = length(muPrime), ncol = length(drivers))
  colnames(muPert) <- drivers

  for(drive in drivers) {
    X <- switch(
      drive,
      "Precipitation" = c(delta["precipitation"], 0),
      "Temperature" = c(0, delta["temperature"]),
      "Both" = c(delta["precipitation"], delta["temperature"])
    )
    muPert[ , drive] <- muPrime + posts[ , "Beta[3]"] * X[1] + posts[ , "Beta[4]"] * X[2]
  }

  sensRes <- enframe(muPrime, name = NULL, value = "Baseline") %>%
    bind_cols(as_tibble(muPert)) %>%
    mutate(CoreName = coreName,
           Rep = 1:n()) %>%
    pivot_longer(cols = c(Precipitation, Temperature, Both)) %>%
    mutate(Sensitivity = log(exp(value) / exp(Baseline))) %>%
    dplyr::select(-Baseline) %>%
    mutate(name = factor(name, levels = c(drivers))) %>%
    rename("LogChange" = value,
           "Driver" = name)

  return(sensRes)
}
