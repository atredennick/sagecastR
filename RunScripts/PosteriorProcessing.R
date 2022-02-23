# PosteriorProcessing.R
#
# Script to post-process the chunk posteriors by combining with
# averaging over the chunks. The script also produces plots of
# the posterior distributions.
#
# Author: Andrew Tredennick

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rstan)
library(ggmcmc)
library(raster)
library(sageCastR)
library(sf)
library(raster)
library(rgeos)
library(gstat)
library(here)


# Set globals -------------------------------------------------------------

serverDir <- "\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/"
figPath <- "./Figures/"
filePath <- "./Output/MCMC/"
postPath <- "./Output/Posteriors/"
params <- c("Beta[1]", "Beta[2]", "Beta[3]",
            "Beta[4]", "sigma_y", "sigma_a")
paramLabs <- c("Intercept", "Dens. dep.", "Precip.",
               "Temp.", "Temporal var.", "Equilibrium cover")
paramLabs <- factor(paramLabs, levels = paramLabs)
climateFutures <- read.csv("./Data/CMIP6_future_by_core.csv")



# Calculate rHats ---------------------------------------------------------

CalcRHats <- function(m, itersPerChain = 1000) {
  l <- nrow(m)
  iters <- 1:l
  chains <- l / itersPerChain
  chunks <- split(iters, sort(iters%%chains))
  mcmclist <- list()
  for(i in 1:chains) {
    mcmclist[[i]] <- coda::as.mcmc(m[chunks[[i]],], chains = 1L)
  }
  mcmc <- coda::as.mcmc.list(mcmclist)
  rhats <- coda::gelman.diag(mcmc, multivariate = FALSE)
  return(rhats)
}

SweepPosts <- function(m, itersPerChain = 1000) {
  l <- nrow(m)
  iters <- 1:l
  chains <- l / itersPerChain
  chunks <- split(iters, sort(iters%%chains))
  for(ic in 1:length(chunks)) {
    mm <- m[chunks[[ic]], ]
    ids <- grep("gamma", colnames(mm))
    gammas <- mm[ , ids]
    meanGamma <- rowMeans(gammas)
    b0star <- mm[,"Beta[1]"] + meanGamma
    gammastar <- gammas
    gammastar[,] <- NA
    for(i in 1:ncol(gammas)) {
      gammastar[ , i] <- gammas[ , i] - meanGamma
    }
    m[chunks[[ic]],"Beta[1]"] <- b0star
    m[chunks[[ic]],ids] <- gammastar
  }
  return(m)
}

allIds <- list.files(filePath)

par(mfrow = c(1, 1))
rhats <- list()
for(coreId in allIds) {
  coreName <- coreId
  f <- paste0("./Output/MCMC/", coreId, "/MCMC.RDS")
  if(file.exists(f)) {
    posts <- readRDS(paste0("./Output/MCMC/", coreId, "/MCMC.RDS"))
    poststar <- SweepPosts(m = posts)
    rhats[[coreId]] <- CalcRHats(m = poststar)

    test <- colMeans(posts)
    gammas <- grep("gamma", names(test))
    gammas <- test[gammas]
    eq <- test["Beta[1]"] / (1 - test["Beta[2]"])
    plot(exp(eq + gammas), type = "l")

    # hist(posts[,2], main = coreId)
    # gammaIds <- grep("gamma", colnames(posts))
    # meanGamma <- rowMeans(posts[,gammaIds])
    # bStar <- posts[,1] + meanGamma
    # mm <- matrix(bStar, nrow = 1000)
    # matplot(mm, type = "l", main = coreId)
    # CalcRHats(m = as.matrix(bStar))
  }
}

saveRDS(rhats, "./Output/rhats.RDS")




# Make and save posteriors and plots --------------------------------------

# allIds <- list.files(filePath)
#
# for(coreId in allIds) {
#   coreName <- coreId
#   PlotPosteriors(
#     coreId = coreId,
#     coreName = coreName,
#     filePath = filePath,
#     figPath = figPath,
#     postPath = postPath,
#     params = params,
#     paramLabs = paramLabs
#   )
# }



# Compute equilibrium cover -----------------------------------------------

# needs to include Eta and then averaged over space.
# allpostdirs <- list.files("./Output/MCMC/")
# allposts <- tibble()
# for(d in allpostdirs) {
#   f <- paste0("./Output/MCMC/", d, "/MCMC.RDS")
#   if(file.exists(f)) {
#     tmp <- readRDS(f)
#     eta <- readRDS(paste0("./Output/MCMC/", d, "/EtaInfo.RDS"))
#     off <- eta$offsetData$offset
#     off <- off[!is.na(off)]
#     gammaIds <- grep("gamma", colnames(tmp))
#     meanGamma <- rowMeans(tmp[,gammaIds])
#     cover <-tmp[ , "Beta[1]"] / (1 - tmp[,"Beta[2]"])
#
#     # colMeans(tmp)
#     #
#     # year1 <- mean(tmp[ , "Beta[1]"]) + mean(tmp[,"Beta[2]"])*log(20) + mean(tmp[,"gamma[1]"]) +
#     #   mean(tmp[,"Beta[3]"])*-1 + mean(tmp[,"Beta[4]"])*-1
#     # c1 <- exp(year1 + off)
#     # hist(c1)
#     # hist(tmp[,"gamma[1]"])
#
#
#     full <- numeric(length(cover))
#     for(i in 1:length(cover)) {
#       full[i] <- mean(exp(cover[i] + off))
#     }
#     df <- data.frame(CoreArea = d,
#                      Cover = full)
#     allposts <- bind_rows(allposts, df)
#   }
# }



# Sensitivity analysis ----------------------------------------------------

sensDelta <- c("precipitation" = 1, "temperature" = 1)

# Log change sensitivity
for(coreId in allIds) {
  coreName <- coreId
  f <- paste0("./Output/MCMC/", coreId, "/MCMC.RDS")
  if(file.exists(f)) {
    posts <- readRDS(paste0("./Output/MCMC/", coreId, "/MCMC.RDS"))
    sens <- RunSensitivity(posteriors = posts,
                           delta = sensDelta,
                           coreName = coreName)
    outPath <- paste0("./Output/Sensitivities/", coreId, "/")
    suppressWarnings(dir.create(outPath))
    saveRDS(sens, file = paste0(outPath, "LogChangeSensitivity.RDS"))
  }
}

# Cover target sensitivity
# for(coreId in allIds) {
#   coreName <- coreId
#   f <- paste0("./Output/MCMC/", coreId, "/MCMC.RDS")
#   if(file.exists(f)) {
#     posts <- readRDS(paste0("./Output/MCMC/", coreId, "/MCMC.RDS"))
#     Eta <- readRDS(paste0("./Output/MCMC/", coreId, "/EtaInfo.RDS"))
#     w <- as.numeric(Eta$offsetData$offset)
#     w <- w[!is.na(w)]
#     targSens <- RunSensitivityTarget(posteriors = posts,
#                                      eta = w,
#                                      delta = sensDelta,
#                                      coreName = coreId,
#                                      target = 10)
#     outPath <- paste0("./Output/Simulations/Sensitivities/", coreId, "/")
#     suppressWarnings(dir.create(outPath))
#     saveRDS(targSens, file = paste0(outPath, "TargetSensitivity.RDS"))
#   }
# }






