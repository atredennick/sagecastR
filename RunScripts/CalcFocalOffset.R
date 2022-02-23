#' Calculate the spatial offset using a focal mean
#'
#' Calculates a temporal average of all input rasters, then a focal mean using
#' circular moving window with a given radius. The spatial offset is calculated
#' as the difference of the log(focal mean) - log(equilibrium cover).
#'
#'
#' @param covStack a RasterStack object with each layer containing annual
#'     sagebrush percent cover data
#' @param r radius to use for the circular focal mean.  Must be in same
#'     linear units as the covStack CRS
#' @param spatGrid a SpatialPolygonsDataFrame to which values for the spatial
#'     offset will be appended as new columns
#' @return a list object with the overall equilibrium cover and the output
#'     SpatialPolygonsDataFrame
#' @import raster
#' @import sp
#' @importFrom sf st_crs
#' @export
#' @examples
#' # Make a cover raster stack
#' library(raster)
#' rasterDir <- file.path("L:/102 - USGS/102 - 88 - SageCastWY/Data/BackInTime/Sagebrush/WYGrouseCoreAreas/North Laramie")
#' covRasts <- list.files(rasterDir, "*.tif", full.names = T, ignore.case = T)
#' covStack <- raster::stack(covRasts)
#' spatGrid <- readRDS("L:/102 - USGS/102 - 88 - SageCastWY/Code/GridExample.RDS")
#' # Calc focal offset
#' focalOffset <- CalcFocalOffset(covStack, r = 800, spatGrid = spatGrid) # takes about 5 minutes to run
#' focalOffset$equalibriumCover
#' summary(focalOffset$offsetData)

CalcFocalOffset <- function(covStack,
                r,
                spatGrid) {

  # Calc temporal mean for each cell
  temporalMean <- raster::calc(covStack, mean)
  

  # Calc equilibrium cover across cells and time
  equilCov <- mean(raster::values(temporalMean), na.rm = T)

  # Check that the raster crs units are in meters
  if(sf::st_crs(temporalMean)$units_gdal != "metre"){
   warning("The spatial units of the input raster stack are not meters!")
  }

  # Calculate a constant weight matrix for circular moving window
  cnstWeights <- raster::focalWeight(temporalMean, r, type='circle')
  cnstWeights[cnstWeights > 0] <- 1
  
  # calculate distance to center on the weights matrix
  dimVal <- dim(cnstWeights)[1]
  resVal <- raster::res(temporalMean)[1]
  centCell <- median(1:(dimVal))
  xMat <- matrix(rep(1:dimVal, each = dimVal), nrow = dimVal)
  yMat <- matrix(rep(rev(1:dimVal), dimVal), nrow = dimVal)
  distMat <- (sqrt((centCell - xMat)^2 + (centCell - yMat)^2))*resVal # euclidian distance

  # Apply an exponential decay function to weights based on distance
  sigma <- max(r) / 3
  focalWeights <- exp(-distMat / sigma)
  focalWeights[cnstWeights == 0] <- 0
  focalWeights <- focalWeights/sum(focalWeights) # scale to sum to 1
  # image(focalWeights)
  # sum(focalWeights)
  
  # Grow raster extent by radius
  growCells <- ceiling(r/resVal)
  growRast <- raster::extend(temporalMean, growCells)
  
  # Prior to running moving window with exponential decay function,
  #     fill missing values in using focal mean with constant weights
  focalFill <- raster::focal(growRast,
                             w = cnstWeights,
                             fun = mean,
                             na.rm = TRUE,
                             pad = TRUE,
                             NAonly = TRUE)
  

  # Calc focal mean using decay function with all NAs pre-filled
  focalMean <- raster::focal(focalFill,
                             w = focalWeights,
                             fun = sum,
                             na.rm = F)
  focalMean <- raster::crop(focalMean, temporalMean) # crop back to original extent
  
  # set output cell values to NA if input was NA
  raster::values(focalMean)[is.na(raster::values(temporalMean))] <- NA

  # calc offset value
  offset <- log(focalMean) - log(equilCov)

  # convert spatGrid to points
  spatCoords <- sp::coordinates(spatGrid)
  spatPnts <- sp::SpatialPointsDataFrame(spatCoords,
                                     data = spatGrid@data,
                                     proj4string = spatGrid@proj4string)

  # Stack outputs for extraction
  outStack <- raster::stack(temporalMean,
                    focalMean,
                    offset)

  # Check if output fields already exist
  if(any(c("temporalMean", "focalMean", "offset") %in% names(spatGrid))){
    warning("One or more of the following fields already exist \n in the input spatGrid: (temporalMean, focalMean, offset), \n these will be overwritten)")
    spatGrid@data <- spatGrid@data[, names(spatGrid)[!names(spatGrid) %in% c("temporalMean", "focalMean", "offset")]]
  }

  # Extract values at the spatial grid point locations
  exVals <- raster::extract(outStack, spatPnts, sp = T)
  names(exVals)[match(c("layer.1", "layer.2", "layer.3"), names(exVals))] <-
    c("temporalMean", "focalMean", "offset")

  # Add extracted data to original spatial Grid
  spatGridOut <- merge(spatGrid, exVals@data[,c("Cell", "temporalMean", "focalMean", "offset")],
                    by.all = "Cell", all.x = T)

  # make output list
  output <- list(equalibriumCover = equilCov,
                 offsetData = spatGridOut)

  output
}

















