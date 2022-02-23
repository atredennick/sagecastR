#' Takes a spatially balanced random sample from the full grid of data
#' to generate a subset of cells to use for modeling fitting.
#'
#' @param cellIds A numeric vector of cell ids.
#' @param proportionToFit The proportion of total cells to sample.
#' @param spatialDf A spatial points polygon to be sampled.
#' @return A list.
#' @import sf
#' @import ggplot2
#' @import SDraw
#' @export

GetDataSample <- function(cellIds, proportionToFit, spatialDf) {
  numCells <- length(cellIds)
  sampleCells <- numCells * proportionToFit
  sampledPoly <- bas.polygon(x = spatialDf, n = sampleCells)
  subCells <- sampledPoly$Cell

  sfAll <- st_as_sf(spatialDf)
  sfSub <- sfAll %>%
    filter(Cell %in% subCells)

  outplot <- ggplot() +
    geom_sf(data = sfAll, aes(fill = exp(LogLagCount)), color = NA, alpha = 0.5) +
    geom_sf(data = sfSub, fill = "red", color = NA, alpha = 1) +
    scale_fill_viridis_c() +
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  out <- list(
    cellsToFit = subCells,
    samplePlot = outplot
  )

  return(out)
}

