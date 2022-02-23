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



# Set work space ----------------------------------------------------------

rm(list = ls(all.names = TRUE))
setwd("\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/")



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(raster)


# Define model function ---------------------------------------------------

FitColonization <- function(df) {
  # From Tredennick et al. 2016, Ecosphere
  # https://github.com/atredennick/sageAbundance/blob/master/scripts/sim_equilibrium_forecasts.R
 
  colD <- df[which(df$LagCover == 0), ]
  colD$colonizes <- ifelse(colD$Cover==0, 0, 1)
  colFit <- glm(colonizes ~ 1, data=colD, family = "binomial")
  colIntercept <- as.numeric(coef(colFit))
  colProb <- exp(colIntercept) / (1 + exp(colIntercept))  # antilogit
  coverVals <- colD[which(colD$Cover > 0), ]$Cover
  colCover <- round(mean(coverVals), 0)
  out <- list(ProbColonization = colProb,
              ColonizationCover = colCover)
}



# Fit model for each core area --------------------------------------------

# read in rasters
mainDir <- "./Data/BackInTime/Sagebrush/WYGrouseCoreAreas/"
allAreas <- list.files(mainDir)

allColProbs <- tibble()

for(coreArea in allAreas) {
  rdir <- paste0(mainDir, coreArea)
  covRasts <- list.files(rdir, "*.tif", full.names = T, ignore.case = T)
  covStack <- raster::stack(covRasts)
  dims <- dim(covStack)
  arr <- array(data = covStack, dim = dim(covStack), 
               dimnames = list(Row = as.character(1:dims[1]),
                               Column = as.character(1:dims[2]),
                               Time = as.character(1:dims[3])))
  df <- as.data.frame.table(arr) %>%
    rename("Cover" = Freq) %>%
    group_by(Row, Column) %>%
    arrange(Time) %>%
    mutate(LagCover = lag(Cover)) %>%
    ungroup() %>%
    drop_na() %>%
    arrange(Row, Column, Time)
  
  colMod <- FitColonization(df)
  tmp <- data.frame(CoreArea = coreArea,
                    ProbabilityOfColonization = colMod$ProbColonization,
                    MeanColonizationCover = colMod$ColonizationCover)
  
  allColProbs <- bind_rows(allColProbs, tmp)
}



# Save the output ---------------------------------------------------------

fout <- "Output/ColonizationModelResults.RDS"
saveRDS(allColProbs, fout)

