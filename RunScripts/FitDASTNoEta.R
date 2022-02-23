# This script fits the dynamic additive spatio-temporal (DAST)
# abundance model to remotely sensed estimates of sagebrush cover
# over time.


# Load libraries ----------------------------------------------------------

# data wrangling
library(tidyverse)
library(ggmcmc)
library(stringr)
library(here)

# spatial packages
library(sf)
library(stars)
library(raster)
library(rgeos)
library(gstat)

# model fitting in Stan
library(rstan)

# in-house package for workhorse functions
library(sageCastR)

# parallel processing for stan
library(doParallel)
library(foreach)


# Global settings ---------------------------------------------------------


serverDir <- "\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/"
bitPath <- paste0(serverDir, "Data/BackInTime/Sagebrush/WYGrouseCoreAreas/")
prismPath <- paste0(serverDir, "Data/PRISM/WYGrouseCoreAreas/")
allAreas <- list.files(bitPath)

resFactor <- NULL  # NULL = 100-m resolution
numberOfYears <- NULL  # NULL = all years
proportionToFit <- 0.05

subs <- c("Powder")
allAreas <- allAreas[allAreas %in% subs]

for(areaId in 1:length(allAreas)) {
  coreArea <- allAreas[areaId]
  coreId <- gsub(pattern = " ", replacement = "", coreArea)

  # set paths dynamically based on coreArea
  coverPath <- paste0(bitPath, coreArea, "/")
  weatherPath <- paste0(prismPath, coreArea, "/")
  outPath <- paste0("./Output/MCMC/", coreId, "/")
  suppressWarnings(dir.create(outPath))  # don't need to see warning if it exists


  # Read in data ------------------------------------------------------------

  dat <- GetData(
    coverPath = coverPath,
    weatherPath = weatherPath,
    resFactor = resFactor,
    totalToRead = numberOfYears
  )
  allGrids <- dat$Grid
  countData <- dat$Count.data
  rm(dat)  # remove this big object once everything is extracted

  # gridExample <- allGrids[[1]]
  # saveRDS(object = gridExample, "./Code/GridExample.RDS")

  # Extract cover starting points for simulation ----------------------------

  # initialConditions <- GetInitialConditions(df = countData)



  # Remove NAs from count data ----------------------------------------------

  if(is.null(resFactor)) {
    # remove NAs, when no resampling
    countData <- countData[!is.na(countData$Count), ]
  } else {
    # remove NaNs, which are the NAs returned after resampling
    countData <- countData[!is.nan(countData$Count),]
  }



  # Scale the weather covariates --------------------------------------------

  scaledGridList <- ScaleGrid(allGrids, scale = TRUE)
  allGrids <- scaledGridList$Grid
  scalers <- list(means = scaledGridList$Means,
                  sds = scaledGridList$StdDevs)
  rm(scaledGridList)

  # store all the data in a list
  dataList <- list(Grid = allGrids, Count.data = countData)
  rm(allGrids)


  # Estimate spatial autocorrelation range ----------------------------------

  spatialRange <- GetSpatialDepdendence(
    dataList,
    model = Count ~ LogLagCount + AvgSprSumPpt + AvgSprSumTemp
  )

  # get the range of autocorrelation
  radiusOfSpatialDependence <- spatialRange$range



  # Calculate spatial offset ------------------------------------------------

  # create raster stack
  rasts <- list.files(coverPath, "*.tif", full.names = TRUE, ignore.case = TRUE)
  stack <- raster::stack(rasts)
  if(!is.null(resFactor)) {
    stack <- raster::aggregate(stack, fact = resFactor, fun = mean)
  }
  gridToSample <- dataList$Grid[[1]]

  spatialField <- CalcFocalOffset(covStack = stack,
                                  r = radiusOfSpatialDependence,
                                  spatGrid = gridToSample)
  rm(stack)


  Eta <- spatialField$offsetData
  etaDf <- Eta@data %>%
    dplyr::select(Cell, offset) %>%
    rename("Eta" = offset)
  # spplot(Eta, "offset", at = seq(-2, 2, 0.1))

  # Extract subset of data for fitting --------------------------------------

  gridToSample <- dataList$Grid[[1]]
  gridToSample <- gridToSample[!is.na(gridToSample$LogLagCount), ]

  # Set up design matrix for all data to be subsetted
  X <- list()  # empty object for storage

  for(i in 1:length(dataList$Grid)) {
    x <- dataList$Grid[[i]]
    locs <- sp::coordinates(x)
    covars <- x@data
    covars$lon <- locs[ , 1]
    covars$lat <- locs[ , 2]
    covars$LagYear <- covars$Year
    covars$Year <- NULL
    X <- rbind(X, covars)
  }
  rm(x)
  rm(locs)
  rm(dataList)
  # gc()

  cellIds <- gridToSample$Cell
  sampleList <- GetDataSample(cellIds = cellIds,
                              proportionToFit = proportionToFit,
                              spatialDf = gridToSample)
  subCellsForFitting <- sampleList$cellsToFit
  dataToFit <- countData[countData$Cell %in% subCellsForFitting, ]



  # Get priors from Tredennick et al. 2016
  betaPriors <- GetPriors()
  betaMeans <- c(betaPriors$Average, 0, 0)  # add zeros for the climate effects
  betaSds <- c(0.5, 0.5, 2, 2)  # add sd = 2 for climate effects

  # Fit Stan model
  y <- dataToFit
  years <- unique(y$Year)

  # Merge the y and X data for modeling data frame
  df <- y %>%
    left_join(X, by = c("Cell", "LagYear")) %>%
    left_join(etaDf, by = "Cell")

  y <- df$Count
  X <- cbind(rep(1, nrow(df)),
             df[ , c("LogLagCount", "AvgSprSumPpt", "AvgSprSumTemp")])
  X <- as.matrix(X)
  colnames(X) <- NULL


  # Set up data
  n <- length(y)
  ny <- length(unique(df$Year))
  p <- ncol(X)
  iy <- df$Time.x
  Bm <- betaMeans
  Bs <- betaSds
  Eta <- df$Eta

  datalist <- list(n = n,
                   ny = ny,
                   p = p,
                   iy = iy,
                   Bm = Bm,
                   Bs = Bs,
                   y = y,
                   X = X,
                   Eta = Eta)

  rm(Eta)
  rm(etaDf)
  rm(df)

  pars <- c("Beta", "gamma", "sigma_y")

  # Compile the model
  mcmcConfig <- stan(file = "./Runscripts/PoissonDAST-NoK.stan",
                     data = datalist,
                     pars = pars,
                     chains = 0)

  mcmc1 <- stan(fit = mcmcConfig,
                data = datalist,
                pars = pars,
                chains = 3,
                iter = 2000,
                warmup = 1000,
                cores = 3)

  # save outputs
  outMat <- as.matrix(mcmc1)
  mcmcFile <- paste0(outPath, "MCMC2.RDS")
  saveRDS(outMat, mcmcFile)

  etaFile <- paste0(outPath, "EtaInfo2.RDS")
  saveRDS(spatialField, etaFile)

  scalerFile <- paste0(outPath, "CovariateScalers2.RDS")
  saveRDS(scalers, scalerFile)

  dataFile <- paste0(outPath, "DataList2.RDS")
  saveRDS(datalist, dataFile)

  rm(list= ls()[!(ls() %in% c('allAreas', "areaId", "serverPath", "bitPath",
                              "prismPath", "resFactor", "numberOfYears",
                              "proportionToFit"))])
  gc()
}

