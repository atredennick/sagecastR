#' Generate knots
#'
#' Creates a K matrix and knot placements for the spatial
#'    autoregressive model.
#'
#' @param sfObj A prototypical \code{sf} object for the spatial domain.
#' @param spatialRange A list of results returned by
#'     \code{GetSpatialDependence}.
#' @return A large list.
#' @import sf
#' @import raster
#' @import stars
#' @import rgeos
#' @import gstat
#' @import ggplot2
#' @export

GetKnots <- function(sfObj, spatialRange) {
  # r <- suppressWarnings(as_Spatial(sfObj))
  r <- sfObj
  # Extract and order coordinates
  xy <- suppressWarnings(st_coordinates(st_centroid(r)))
  r <- r[order(-xy[,"Y"], xy[,"X"]),]
  r <- as_Spatial(r)

  # Re-extract ordered coordinates and define extent
  #Coords <- suppressWarnings(st_coordinates(st_centroid(r)))
  Coords <- coordinates(r)
  xMin <- min(Coords[,1])
  xMax <- max(Coords[,1])
  yMin <- min(Coords[,2])
  yMax <- max(Coords[,2])

  # If x and y extents differ, match before making knots
  # to ensure evenly spaced knots. Excess knots beyond extent
  # of data are dropped after the fact.
  diffY <- yMax - yMin
  diffX <- xMax - xMin

  if(diffY > diffX) {
    # add in x dimension
    diff <- (diffY - diffX) / 2
    xMin <- min(Coords[,1]) - diff
    xMax <- max(Coords[,1]) + diff
    yMin <- min(Coords[,2])
    yMax <- max(Coords[,2])
  } else if(diffX > diffY) {
    # add in y dimension
    diff <- (diffX - diffY) / 2
    xMin <- min(Coords[,1])
    xMax <- max(Coords[,1])
    yMin <- min(Coords[,2]) - diff
    yMax <- max(Coords[,2]) + diff
  }


  splits.x <- round((xMax-xMin)/ceiling(spatialRange$range))
  splits.y <- round((yMax-yMin)/ceiling(spatialRange$range))
  X <- xMin+(xMax-xMin)/splits.x*c(0:splits.x)
  Y <- yMin+(yMax-yMin)/splits.y*c(splits.y:0)
  XY <- expand.grid(x=X,y=Y)

  crsString <- suppressWarnings(CRS(as.character(unlist(st_crs(r)[1]))))
  Knots <- SpatialPoints(coords=XY, proj4string=crsString)
  # obsPts <- SpatialPoints(coords=Coords, proj4string=crsString)
  Distances <- gDistance(Knots, r[!is.na(r@data$sage_percent_cover),], byid=TRUE)
  Distances <- apply(Distances, 2, 'min')
  my.buffer <- 100
  Which.include <- which(Distances<my.buffer)
  Knot.cell.distances <- gDistance(Knots[Which.include,], r, byid=TRUE)
  diff.x <- (xMax-xMin)/splits.x
  diff.y <- (yMax-yMin)/splits.y

  # Knot.distances=gDistance(Knots[Which.include,],Knots[Which.include,],byid=TRUE)
  # m <- reshape2::melt(Knot.distances)
  # ggplot(data=m, aes(x=Var1, y=Var2))+
  #   geom_raster(aes(z=value, fill=value))

  # Convert to sf object for plotting
  knotsSf <- st_as_sf(Knots[Which.include])
  knotPlot <- ggplot() +
    geom_sf(data = sfObj, aes(fill = sage_percent_cover), color = NA) +
    geom_sf(data = knotsSf, col = "red") +
    scale_fill_viridis_c()

  Knot.Adj <- rect_adj(splits.x+1,splits.y+1)
  Knot.Adj <- Knot.Adj[Which.include, Which.include]
  Q.knot <- -Knot.Adj
  diag(Q.knot) <- apply(Knot.Adj, 2, "sum")
  Q.knot <- Matrix(Q.knot)

  sigma <- spatialRange$rangeParameter
  w <- exp(-Knot.cell.distances/sigma)  # exponential covariance structure

  K <- w/apply(w,1,'sum')
  K.data=list(K=K,Q.knot=Q.knot,plot = knotPlot, knotsSp = Knots[Which.include])

  return(K.data)
}
