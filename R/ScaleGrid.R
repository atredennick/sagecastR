#' Center and scale covariate list
#'
#'
#' @param Grid A list of spatially gridded covariates.
#' @param vars A character vector naming the columns to scale.
#' @return A list.
#' @export

ScaleGrid <- function(Grid, vars = c("AvgSprSumPpt", "AvgSprSumTemp"), scale = TRUE) {
  t.steps <- length(Grid)
  X.pred.list <- vector("list", t.steps)
  for(it in 1:t.steps){
    X.pred.list[[it]] <- as.matrix(Grid[[it]]@data)
  }
  X <- stack_list(X.pred.list)
  if (scale) {
    Xscaled <- scale(X[ , vars], center = TRUE, scale = TRUE)
    means <- attributes(Xscaled)$`scaled:center`
    sds <- attributes(Xscaled)$`scaled:scale`
  } else {
    Xscaled <- X[ , vars]
    means <- NA
    sds <- NA
  }

  Xscaled[is.nan(Xscaled)] <- 0
  Xscaled[is.na(Xscaled)] <- 0
  X[ , vars] <- Xscaled
  X <- as.data.frame(X)

  years <- unique(X$Year)
  for(it in 1:t.steps) {
    xtmp <- subset(X, Year == years[it])
    Grid[[it]]@data[ , vars] <- xtmp[ , vars]
  }

  outList <- list(Grid = Grid,
                  Means = means,
                  StdDevs = sds)
  return(outList)
}
