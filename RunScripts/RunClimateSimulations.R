# Run climate simulations

# Author: Andrew Tredennick



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
# library(sageCastR)
source("R/SimDAST.R")
source("R/WriteNetCDF.R")
source("R/CalcTemporalSummaries.R")
source("R/CalcSpatialSummaries.R")
library(raster)
library(EML)
library(ncdf4)
library(emld)
library(lubridate)
library(foreach)
library(doParallel)



# helper function for rotating matrices
rotate <- function(x) t(apply(x, 2, rev))



# Load data ---------------------------------------------------------------

sspvec <- c("ssp126", "ssp245", "ssp585")
mcmciters <- 50
ncdir <- paste0("\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/",
                "Output/Forecasts/")

climateFutures <- read.csv(here("AuxData", "CMIP6_future_by_core.csv"))
nameMapping <- read.csv(here("AuxData", "CoreAreaNamesMapping.csv"))
gcmBias <- readRDS(here("Output", "GCMBiasCorrections.RDS"))
gcmFile <- here("Output", "gcmErrorSummary.xlsx")
gcmDat <- readxl::read_excel(gcmFile, sheet = 1) %>%
  as.data.frame() %>%
  drop_na() %>%
  dplyr::select(GCM, relErrBiasCorr) %>%
  rename("source_id" = GCM,
         "relErr" = relErrBiasCorr)

colonization <- readRDS(here("Output", "ColonizationModelResults.RDS"))

# Large rasters are held outside of the repo on WEST server
# these are just the downloaded BIT products from USGS
rasterDir <- paste0("\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/",
                    "Data/BackInTime/Sagebrush/WYGrouseCoreAreas/")
allAreas <- list.files(rasterDir)



# Run projections in parallel ---------------------------------------------

# cl <- makeCluster(10)

# foreach(a = 1:length(allAreas),
#         .packages = c("raster",
#                       "lubridate",
#                       "emld",
#                       "ncdf4",
#                       "tidyverse",
#                       "sageCastR")) %do% {

for(a in 1:length(allAreas)) {
  coreArea <- allAreas[a]
  targets <- nameMapping %>%
    filter(Name == coreArea) %>%
    dplyr::select(NestingTarget, SummerTarget)

  # get initial conditions
  coreNoSep <- gsub(" ", "", coreArea)

  tmpDir <- paste0(rasterDir, coreArea)
  tmpFiles <- list.files(tmpDir)
  tmpFile <- tmpFiles[length(tmpFiles)]
  ras <- raster(paste0(tmpDir, "/", tmpFile))
  initMat <- rotate(as.matrix(ras))

  # get eta
  etaDir <- "./Output/MCMC/"
  etaFile <-  paste0(etaDir, gsub(" ", "", coreArea), "/EtaInfo.RDS")
  eta <- readRDS(etaFile)$offsetData
  etaMat <- matrix(data = eta@data$offset,
                   ncol = ncol(initMat),
                   nrow = nrow(initMat))
  etaMat <- rotate(t(etaMat))

  # subset colonization parameters
  col <- colonization %>%
    filter(CoreArea == coreArea)

  coords <- coordinates(eta)
  lons <- unique(coords[ , 1])
  lats <- unique(coords[ , 2])

  test <- all.equal(dim(etaMat), dim(initMat))
  if(!test) {
    stop("Dimensions of spatial offset and initial conditions do not match.")
  }

  # get sample of posteriors
  pfile <- paste0("Output/MCMC/", coreNoSep, "/MCMC.RDS")
  posts <- readRDS(pfile)
  rids <- sample(1:nrow(posts), size = mcmciters, replace = FALSE)
  theta <- posts[rids, ]

  # forecast metadata
  ensembles <- 1:nrow(theta)  # this is MCMC iterations for param uncertainty
  forecast_start_date <- ymd("2018-07-01")  # start date of our forecast


  for(ssp in sspvec) {
    # get climate
    allClim <- climateFutures %>%
      filter(experiment_id == ssp) %>%
      filter(coreName == coreArea)

    relBias <- gcmBias %>%
      filter(coreName == coreArea) %>%
      dplyr::select(-application) %>%
      pivot_wider(names_from = "name", values_from = "meanbias") %>%
      rename("prBias" = precipitation, "tasBias" = temperature)

    allClim <- allClim %>%
      left_join(relBias, by = c("coreName", "source_id")) %>%
      drop_na() %>%
      mutate(pr = pr*prBias,
             tas = tas+tasBias) %>%
      dplyr::select(-prBias, -tasBias)

    clim <- allClim %>%
      left_join(gcmDat, by = "source_id") %>%
      drop_na() %>%
      group_by(year) %>%
      mutate(w = relErr/sum(relErr)) %>%
      ungroup() %>%
      filter(year >= 2018) %>%
      group_by(coreName, year) %>%
      summarise(pr = sum(pr*w),
                tas = sum(tas*w),
                .groups = "drop") %>%
      arrange(year) %>%
      dplyr::select(year, pr, tas)

    nyears <- nrow(clim)
    n_time <- nyears
    times <- year(seq(from=forecast_start_date, by="1 year", length=n_time))

    # set forecast flag
    forecast <- c(0, rep(1, n_time-1)) # this code indicates 1 hindcast for 2018

    # get climate scalers
    sclDir <- here("Output", "MCMC")
    sclFile <-  paste0(sclDir, "/", gsub(" ", "", coreArea), "/CovariateScalers.RDS")
    scl <- readRDS(sclFile)
    scldf <- data.frame(name = c("pr", "tas"),
                        mean = as.numeric(scl$means),
                        sdev = as.numeric(scl$sds))

    # scale the climate vectors
    climScaled <- clim %>%
      pivot_longer(cols = pr:tas) %>%
      left_join(scldf, by = "name") %>%
      mutate(ScaledValue = (value - mean)/sdev) %>%
      dplyr::select(year, name, ScaledValue) %>%
      pivot_wider(names_from = name, values_from = ScaledValue) %>%
      arrange(year) %>%
      dplyr::select(pr, tas) %>%
      as.matrix()

    cover <- SimDAST(theta,
                     initialCover = initMat,
                     eta = etaMat,
                     climate = climScaled,
                     years = n_time,
                     colPr = col$ProbabilityOfColonization,
                     colCover = col$MeanColonizationCover)

    # calculate summaries
    trends <- CalcTemporalSummaries(cover)
    propsNest <- CalcSpatialSummaries(cover, target = targets$NestingTarget)
    propsNest$TargetValue <- targets$NestingTarget
    propsNest$TargetType <- "nesting"
    propsSummer <- CalcSpatialSummaries(cover, target = targets$SummerTarget)
    propsSummer$TargetValue <- targets$SummerTarget
    propsSummer$TargetType <- "summer"

    props <- bind_rows(propsNest, propsSummer)

    ds <- here("Output", "ForecastSummaries")
    ft <- paste0(ds, "/", coreNoSep, "-", "dast-forecast-summaries", "-", ssp, ".csv")
    write.csv(trends, file = ft, row.names = FALSE)

    fp <- paste0(ds, "/", coreNoSep, "-", "dast-forecast-proportions", "-", ssp, ".csv")
    write.csv(props, file = fp, row.names = FALSE)

    # save as netCDF
    WriteNetCDF(coreId = coreNoSep,
                scenario = ssp,
                forecast_start_date = forecast_start_date,
                lons = lons,
                lats = lats,
                ensembles = ensembles,
                outDir = ncdir,
                forecast_flag = forecast,
                cover = cover)

    rm(cover)
    gc()

    cat(paste0("Done with ", coreArea, ". ssp ", ssp, "\n"))
  }  # end ssp loop
}  # end foreach loop

# parallel::stopCluster(cl)
