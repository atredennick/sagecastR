#' Simulate the dynamic additive spatiotemporal (DAST) model.
#'
#' @description
#' `SimDAST()` simulates the dynamic process defined by the DAST model, which
#' is built upon the Gompertz population model. The function allows for
#' prospective analyses of cover change under climate change scenarios,
#' for example, by the user supplying a `climate` matrix for future time
#' steps. The function returns a, typically large, array with dimensions
#' of: time, y (latitude), x (longitude), MCMC iterations. Note that a
#' colonization process is added to DAST for simulation to avoid permanent
#' extinction in cells that go to zero percent cover.
#'
#' @param theta A matrix of DAST parameters. Must be at least one row. Must
#'    contain at least the following columns: `Beta[1]` (intercept),
#'    `Beta[2]` (density-dependence), `Beta[3]` (precip effect),
#'    `Beta[4]` (temperature effect), `sigma_y` (temporal standard deviation).
#'
#' @param initialCover A matrix of initial cover values for each cell.
#'
#' @param eta The empirical spatial offset for each cell. Must be same
#'    dimensions as `initialCover`.
#'
#' @param climate A matrix of future climate values. Rows are number of years
#'    and columns are the precipitation (column 1) and temperature (column 2)
#'    covariate values. Note that these typically need to be scaled
#'    appropriately if the coefficients were standardized during model fitting.
#'
#' @param years Number of years to simulate.
#'
#' @param colPr The fitted probability of colonization if cover in a cell is 0.
#'
#' @param colCover The average percent cover of colonized cells.
#'
#' @return A large array.
#'
#' @export

SimDAST <- function(theta,
                    initialCover,
                    eta,
                    climate,
                    years,
                    colPr,
                    colCover) {

  if(!is.matrix(theta)) {
    theta <- as.matrix(theta, nrow = 1)
  }

  edim <- nrow(theta)
  cover <- array(data = NA, dim = c(years, nrow(eta), ncol(eta), edim))
  cover[1 , , , ] <- initialCover
  for(t in 2:years) {
    for(e in 1:edim) {

      # growth process
      zcurr <- log(cover[t-1, , , e])
      mu <- exp(theta[e,"Beta[1]"] +
                  theta[e,"Beta[2]"]*zcurr +
                  theta[e, "Beta[3]"]*climate[t,1] +
                  theta[e, "Beta[4]"]*climate[t,2] +
                  rnorm(1, 0, theta[e, "sigma_y"]) +
                  eta)
      nas <- which(is.na(mu))
      mu[is.na(mu)] <- 1
      newcover <- matrix(rpois(length(mu), mu), ncol = ncol(mu))
      newcover[nas] <- NA

      # colonization Process
      zeros <- which(newcover == 0)
      colonizers <- rbinom(length(zeros), size = 1, colPr)
      colonizerCover <- colonizers * colCover
      newcover[zeros] <- colonizerCover
      cover[t, , , e] <- newcover
    }
  }
  return(cover)
}
