#' Estimate spatial autocorrelation
#'
#' Uses a variogram to estimate the distance at which spatial
#'    autocorrelation goes away.
#'
#' @param dataList A list of the data, including counts and the
#'     list of spatially gridded covariates.
#' @param model The poisson regression model to fit.
#' @return A list.
#' @import sf
#' @import raster
#' @import stars
#' @import rgeos
#' @import gstat
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export

GetSpatialDepdendence <- function(dataList, model) {
  # Format data
  y <- dataList$Count.data
  years <- unique(y$Year)
  X <- list()  # empty object for storage

  for(i in 1:length(years)) {
    x <- dataList$Grid[[i]]
    locs <- sp::coordinates(x)
    covars <- x@data
    covars$lon <- locs[ , 1]
    covars$lat <- locs[ , 2]
    covars$LagYear <- covars$Year
    covars$Year <- NULL
    X <- rbind(X, covars)
  }

  # Merge the y and X data for modeling data frame
  df <- y %>%
    left_join(X, by = c("Cell", "LagYear")) %>%
    filter(!is.na(LogLagCount))

  # Fit a poisson model with no random effects structure
  out <- glm(formula = model, family = "poisson", data = df)
  df$resids <- resid(out)  # extract residuals

  # Summarize residuals by cell
  resids <- df %>%
    group_by(Cell, lat, lon) %>%
    summarise(ResidualError = mean(resids), .groups = "drop")

  # Convert to spatial dataframe
  coordinates(resids) <- c("lon", "lat")
  vario <- variogram(ResidualError ~ 1, data = resids, width = 50)
  varioMod <- vgm(psill = 1, model = "Exp", range = 5000, nugget = 0.05)
  varioFit <- fit.variogram(vario, model = varioMod)
  range <- varioFit$range[2]
  rangeParameter <- range/3  # sigma in exponential decay model

  # Plot the results
  # outPlot <- plot(vario, varioFit)

  # Store output as a list
  outList <- list(
    range = range,
    rangeParameter = rangeParameter
    # variogramPlot = outPlot
  )
  return(outList)
}
