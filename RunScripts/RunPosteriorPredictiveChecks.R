# Fit colonization model for SageCast simulations. The colonization
# model is simple: estimate the probability of a zero cover pixel
# transitioning to a non-zero cover value from year t to t+1. The
# model has no covariates, no spatial structure, and no temporal structure.
# Each core area is fit independently. Note that the colonization model
# is only used for simulating the dynamic process in DAST. It is not
# necessary for fitting the dynamic cover model.
#
# Author: Andrew Tredennick
# Email: atredennick@west-inc.com
# Date created: 2021-04-06



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(raster)
library(here)
library(SDraw)
library(rgeos)
library(gstat)
library(sageCastR)

serverDir <- "\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/"



# Fit model for each core area --------------------------------------------

# read in rasters
mainDir <- paste0(serverDir, "Data/BackInTime/Sagebrush/WYGrouseCoreAreas/")
allAreas <- list.files(mainDir)

allAreas <- c("Greater South Pass 2", "Uinta")

allPbs <- tibble()
for(coreArea in allAreas) {
  coreNoSep <- gsub(" ", "", coreArea)
  rdir <- paste0(mainDir, coreArea)
  covRasts <- list.files(rdir, "*.tif", full.names = T, ignore.case = T)
  covStack <- suppressWarnings(raster::stack(covRasts))
  shp <- covStack[[1]]
  r <- raster::rasterToPolygons(shp, na.rm = FALSE)
  xy <- coordinates(gCentroid(r, byid = TRUE))
  rSort <- r[order(-xy[,"y"], xy[,"x"]),]
  colnames(rSort@data) <- c("Count")
  rSort$Count <- ceiling(rSort$Count)
  rSort$Cell <- 1:nrow(rSort)

  df <-  rSort[!is.na(rSort$Count), ]
  sampleSize <- nrow(df) * 0.05

  set.seed(287261)
  samp <- suppressWarnings(bas.polygon(df, sampleSize))
  dataCells <- samp$Cell

  dataMatrix <- covStack[sort(dataCells)]
  df <- as_tibble(dataMatrix) %>%
    mutate(CellId = sort(dataCells)) %>%
    pivot_longer(cols = starts_with("nlcd")) %>%
    separate(name, into = c("t1", "t2", "Year", "t3", "t4", "t5"), sep = "_") %>%
    dplyr::select(CellId, Year, value) %>%
    mutate(Year = as.numeric(Year)) %>%
    group_by(CellId) %>%
    arrange(Year) %>%
    mutate(lagvalue = lag(value)) %>%
    arrange(CellId, Year) %>%
    ungroup() %>%
    rename("Cover" = value,
           "LagCover" = lagvalue) %>%
    drop_na() %>%
    filter(Year != 1985)

  # Read in eta
  etadf <- readRDS(paste0("Output/MCMC/", coreNoSep, "/EtaInfo.RDS"))
  etadf <- etadf$offsetData@data %>%
    filter(Year == 1985) %>%
    dplyr::select(Cell, offset) %>%
    rename("CellId" = Cell) %>%
    filter(CellId %in% sort(dataCells))

  df <- df %>%
    left_join(etadf, by = c("CellId")) %>%
    drop_na()  # remove any NA offset cells

  # Read in climate
  cpath <- paste0(serverDir, "Data/PRISM/WYGrouseCoreAreas/", coreArea)
  pptRasts <- list.files(cpath, "ppt", full.names = T, ignore.case = T)
  pptStack <- suppressWarnings(raster::stack(pptRasts))
  pptMatrix <- pptStack[sort(dataCells)]
  pptdf <- as_tibble(pptMatrix) %>%
    mutate(CellId = sort(dataCells)) %>%
    pivot_longer(cols = starts_with("PRISM")) %>%
    separate(name, into = c("t1", "t2", "t3", "Year"), sep = "_") %>%
    dplyr::select(CellId, Year, value) %>%
    mutate(Year = as.numeric(Year)) %>%
    rename("Precipitation" = value)

  tempRasts <- list.files(cpath, "tmean", full.names = T, ignore.case = T)
  tempStack <- suppressWarnings(raster::stack(tempRasts))
  tempMatrix <- tempStack[sort(dataCells)]
  tempdf <- as_tibble(tempMatrix) %>%
    mutate(CellId = sort(dataCells)) %>%
    pivot_longer(cols = starts_with("PRISM")) %>%
    separate(name, into = c("t1", "t2", "t3", "Year"), sep = "_") %>%
    dplyr::select(CellId, Year, value) %>%
    mutate(Year = as.numeric(Year)) %>%
    rename("Temperature" = value)


  climate <- pptdf %>%
    left_join(tempdf, by = c("CellId", "Year"))

  # Scale the climate covariates
  scalers <- readRDS(paste0("Output/MCMC/", coreNoSep, "/CovariateScalers.RDS"))
  climateScaled <- climate %>%
    mutate(PrecipScaled = (Precipitation - scalers$means["AvgSprSumPpt"]) /  scalers$sds["AvgSprSumPpt"],
           TempScaled = (Temperature - scalers$means["AvgSprSumTemp"]) /  scalers$sds["AvgSprSumTemp"]) %>%
    dplyr::select(-Precipitation, -Temperature)


  df <- df %>%
    left_join(climateScaled, by = c("CellId", "Year"))

  # Convert some values
  Xdf <- df %>%
    mutate(Year = as.numeric(as.factor(Year)),
           LogLagCover = log(LagCover + 0.00001)) %>%
    dplyr::select(-LagCover)

  # Load posteriors
  posts <- readRDS(paste0("Output/MCMC/", coreNoSep, "/MCMC.RDS"))

  # use a 1,000 element sample of the posteriors
  set.seed(20210413)
  posts <- posts[sample(1:nrow(posts), size = 1000, replace = FALSE), ]
  pvals <- ComputeBayesPValues(df = Xdf, theta = posts)
  pvals <- pvals %>%
    mutate(CoreArea = coreArea)
  allPbs <- bind_rows(allPbs, pvals)
}

outfile <- "./Output/BayesianPValues-Redos.RDS"
saveRDS(allPbs, outfile)





# test <- Xprime %>%
#   filter(CellId == sort(dataCells)[1])
#
# colors <- c("Data" = "tan",
#             "Yrep" = "darkblue",
#             "Expected" = "dodgerblue")
#
# g1 <- ggplot(test, aes(x = Year+1985)) +
#   geom_line(aes(y = Cover, color = "Data"), size = 1) +
#   geom_line(aes(y = yrep, color = "Yrep")) +
#   geom_line(aes(y = exp(mu), color = "Expected")) +
#   theme_classic() +
#   labs(x = "Year", y = "Cover (%)") +
#   scale_color_manual(values = colors, name = NULL) +
#   ggtitle("A: temporal trend in one cell") +
#   guides(color = FALSE)
#
# test2 <- Xprime %>%
#   filter(Year == 1)
#
# g2 <- ggplot(test2) +
#   geom_histogram(aes(x = Cover, fill = "Data"), bins = 20) +
#   geom_histogram(aes(x = yrep, color = "Yrep"), bins = 20, fill = NA) +
#   geom_histogram(aes(x = exp(mu), color = "Expected"), bins = 20, fill = NA) +
#   theme_classic() +
#   labs(x = "Cover (%)", y = "Frequency") +
#   scale_fill_manual(values = colors, name = NULL) +
#   scale_color_manual(values = colors, name = NULL) +
#   ggtitle("B: spatial variation in one year")
#
# gout <- cowplot::plot_grid(g1, g2, nrow = 1)
# gout
