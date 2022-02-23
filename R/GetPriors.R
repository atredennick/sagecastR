#' Handy function to return priors from Tredennick et al. 2016
#'
#' Reads in MCMC results from Tredennick et al. 2016 (Ecosphere)
#'     and computes summary statistics for prior distributions
#'     of the intercept and density-dependence parameters.
#'
#' @return A data frame.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export

GetPriors <- function() {
  outs <- sageCastR:::tredPrior
  outSummary <- outs %>%
    group_by(Parameter) %>%
    summarise(Average = mean(value),
              StdDev = sd(value),
              .groups = "drop") %>%
    filter(Parameter %in% c("int_mu", "beta_mu")) %>%
    mutate(Parameter = ifelse(Parameter == "int_mu", "intercept", "dd_beta")) %>%
    as.data.frame()
  return(outSummary)
}

