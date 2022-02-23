#' Compute Bayesian p-values from posterior predictive checks
#'
#' @description
#' Compute test stastics from the posterior predictive distribution
#' and calculate Bayesian p-values for posterior predictive checks of
#' model lack of fit. A Chi-square test statistic is used here.
#'
#' @param df
#' @param theta
#' @param climate
#' @return A data frame.
#' @import tidyr
#' @import tibble
#' @import dplyr
#' @import magrittr
#' @export

ComputeBayesPValues <- function(df, theta) {

  ppcs <- tibble()

  for(i in 1:nrow(theta)) {
    thetai <- theta[i, ]
    gammas <- thetai[grep("gamma", names(thetai))] %>%
      as_tibble() %>%
      mutate(Year = 1:n()) %>%
      rename("gamma" = value)

    X <- df %>%
      left_join(gammas, by = "Year")

    Xprime <- X %>%
      mutate(mu = thetai["Beta[1]"] + LogLagCover*thetai["Beta[2]"] +
               PrecipScaled*thetai["Beta[3]"] + TempScaled*thetai["Beta[4]"] +
               gamma + offset) %>%
      mutate(yrep = rpois(nrow(X), exp(mu))) %>%
      # chi square test a la https://esajournals.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/ecs2.3058
      mutate(discrep = (yrep - exp(mu))^2 / exp(mu),
             discdata = (Cover - exp(mu))^2 / exp(mu))

    Xtime <- Xprime %>%
      group_by(CellId) %>%
      summarise(ssqrep = sum(discrep),
                ssqdata = sum(discdata)) %>%
      mutate(testssq = as.numeric(ssqrep >= ssqdata))

    # yrepm = mean(yrep),
    #         cover = mean(Cover),
    #         cvrep = sd(yrep) / mean(yrep),
    #         cvdata = sd(Cover) / mean(Cover),
    # testmean = as.numeric(yrepm >= cover),
    #      testcv = as.numeric(cvrep >= cvdata),



    Xspace <- Xprime %>%
      group_by(Year) %>%
      summarise(ssqrep = sum(discrep),
                ssqdata = sum(discdata)) %>%
      mutate(testssq = as.numeric(ssqrep >= ssqdata))

    # yrepm = mean(yrep),
    # cover = mean(Cover),
    # cvrep = sd(yrep) / mean(yrep),
    # cvdata = sd(Cover) / mean(Cover),
    # testmean = as.numeric(yrepm >= cover),
    # testcv = as.numeric(cvrep >= cvdata),

    tests <- data.frame(
      Iteration = i,
      SpatialDiscrep = sum(Xspace$testssq) / nrow(Xspace),
      TemporalDiscrep = sum(Xtime$testssq) / nrow(Xtime)
    )

    # TemporalMean = sum(Xtime$testmean) / nrow(Xtime),
    # TemporalCV = sum(Xtime$testcv, na.rm = T) / nrow(Xtime),
    # SpatialMean = sum(Xspace$testmean) / nrow(Xspace),
    # SpatialCV = sum(Xspace$testcv) / nrow(Xspace),

    ppcs <- bind_rows(ppcs, tests)
  }

  # Summarize ppcs
  bayesPvals <- enframe(colMeans(ppcs)) %>%
    filter(name != "Iteration")

  return(bayesPvals)
}
