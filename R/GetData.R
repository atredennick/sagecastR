#' Read in and format data
#'
#' Creates a list of geospatial data for modeling
#'
#' @param coverPath A character string specifying the path to the cover data
#'     of interest. Assumes tif files from the USGS BIT data.
#' @param weatherPath A character string specifying the path to the weather
#'     data of interest. Assumes tif files from PRISM.
#' @param resFactor An integer for the factor by which to decrease
#'     the resolution of the spatial data. Default is \code{NULL}, implying
#'     no change to the native resolution.
#' @param totalToRead An integer for the number of files (years) to read.
#'     Default is \code{NULL}, meaning that all years are read in and
#'     collated.
#' @return A large list.
#' @import sf
#' @import raster
#' @import stringr
#' @import stars
#' @import rgeos
#' @import gstat
#' @export

GetData <- function(coverPath,
                    weatherPath,
                    resFactor = NULL,
                    totalToRead = NULL) {
  dpath <- coverPath
  cpath <- weatherPath
  fnames <- list.files(dpath, pattern = ".tif$")
  cfnames <- sort(list.files(cpath))

  Grid <- list()
  Count.data <- list()
  if(is.null(totalToRead)) {
    n <- length(fnames)
  } else {
    n <- totalToRead
  }

  for(i in 1:n) {
    f <- fnames[i]
    doYear <- as.numeric(str_split(f, pattern = "_", simplify = TRUE)[1, 3])

    # Cover data for doYear
    shp <- suppressWarnings(raster(paste0(dpath, f)))
    if(!is.null(resFactor)) {
      shp <- raster::aggregate(shp, fact = resFactor, fun = mean)
    }
    r <- raster::rasterToPolygons(shp, na.rm = FALSE)
    xy <- coordinates(gCentroid(r, byid = TRUE))
    rSort <- r[order(-xy[,"y"], xy[,"x"]),]
    colnames(rSort@data) <- c("Count")
    rSort$Count <- ceiling(rSort$Count)
    rSort$Cell <- 1:nrow(rSort)

    if(i > 1) {
      # Only store cover data starting in year 2 (1986)
      tmpCount <- rSort@data[ , c("Cell", "Count")]
      tmpCount$Year <- doYear
      tmpCount$LagYear <- doYear - 1
      if(doYear == 2013) {
        tmpCount$LagYear <- 2011
      }
      tmpCount$Time <- i - 1  # match this years ID with last years covariates
      tmpCount$AreaSurveyed <- 1
      Count.data <- bind_rows(Count.data, tmpCount)
    }

    # rSort[is.na(rSort$Count), "Count"] <- median(rSort@data[!is.na(rSort@data$Count), "Count"])
    rSort$LogLagCount <- log(rSort$Count + 0.00001)
    rSort$Count <- NULL

    # Read in covariates
    yearCovarFiles <- cfnames[grep(as.character(doYear), cfnames)]
    pptFile <- yearCovarFiles[grep("ppt", yearCovarFiles)]
    tmeanFile <- yearCovarFiles[grep("tmean", yearCovarFiles)]

    pptShp <- suppressWarnings(raster::raster(paste0(cpath, pptFile)))
    if(!is.null(resFactor)) {
      pptShp <- raster::aggregate(pptShp, fact = resFactor, fun = mean)
    }
    r <- raster::rasterToPolygons(pptShp, na.rm = FALSE)
    xy <- coordinates(gCentroid(r, byid = TRUE))
    rPpt <- r[order(-xy[,"y"], xy[,"x"]),]

    tmeanShp <- suppressWarnings(raster::raster(paste0(cpath, tmeanFile)))
    if(!is.null(resFactor)) {
      tmeanShp <- raster::aggregate(tmeanShp, fact = resFactor, fun = mean)
    }
    r <- raster::rasterToPolygons(tmeanShp, na.rm = FALSE)
    xy <- coordinates(gCentroid(r, byid = TRUE))
    rTmean <- r[order(-xy[,"y"], xy[,"x"]),]

    rSort$AvgSprSumPpt <- rPpt@data[,1]
    rSort$AvgSprSumTemp <- rTmean@data[,1]
    rSort$Year <- doYear
    rSort$Time <- i
    Grid[[i]] <- rSort

    cat(paste("Done processing year", doYear, "\n"))
  }
  return(list("Grid" = Grid, "Count.data" = Count.data))
}
