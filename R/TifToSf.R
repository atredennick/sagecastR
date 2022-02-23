#' Converts tif to sf object
#'
#' @param coverPath Character string of the path to cover data of interest.
#' @param filename A character string of the filename to read.
#' @param resFactor An integer for the factor by which to decrease
#'     the resolution of the spatial data. Default is \code{NULL}, implying
#'     no change to the native resolution.
#' @return An sf object
#' @import sf
#' @import raster
#' @import stars
#' @import rgeos
#' @import gstat
#' @export

TifToSf <- function(coverPath, filename, resFactor = NULL) {
  f <- filename
  shp <- suppressWarnings(raster(paste0(coverPath, f)))

  if(!is.null(resFactor)) {
    shp <- raster::aggregate(shp, fact = resFactor, fun = mean)
  }

  r <- st_as_sf(raster::rasterToPolygons(shp, na.rm = FALSE))
  colnames(r) <- c("sage_percent_cover", "geometry")
  st_crs(r) <- st_crs(shp)
  xy <- suppressWarnings(st_coordinates(st_centroid(r)))
  r <- r[order(-xy[, "X"], xy[, "Y"]), ]

  return(r)
}
