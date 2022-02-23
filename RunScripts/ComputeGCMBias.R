# This script fits regression models relating PRISM data and CMIP6
# hindcasts. The regression models are then used to scale the CMIP6
# projections accordingly.


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(raster)
library(here)



# Set server directory ----------------------------------------------------

# This is needed because the PRISM rasters are stored on the
# WEST, Inc. server rather than in the repo, to save space.
serverDir <- "\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/"
prismDir <- paste0(serverDir, "Data/PRISM/WYGrouseCoreAreas/")



# Read data ---------------------------------------------------------------

gcm <- read_csv("Data/CMIP6_future_by_core.csv")

hindcasts <- gcm %>%
  filter(experiment_id == "historical") %>%
  drop_na()

# loop over core areas and take average values for each year
coreAreas <- list.files(prismDir)
prism <- tibble()
for(doArea in coreAreas) {
  rasDir <- paste0(prismDir, doArea)
  rasFiles <- list.files(rasDir)
  pfiles <- rasFiles[grep("ppt", rasFiles)]
  tfiles <- rasFiles[grep("tmean", rasFiles)]

  # create raster stacks and suppress warnings, warnings are about
  # the datum being updated based on ellipsoid CRS definition
  precip <- suppressWarnings(raster::stack(paste0(rasDir, "/", pfiles)))
  temp <- suppressWarnings(raster::stack(paste0(rasDir, "/", tfiles)))

  # compute means in each year across all cells
  precipMeans <- cellStats(precip, mean)
  tempMeans <- cellStats(temp, mean)

  # format into data frames and combine
  pptdf <- enframe(precipMeans) %>%
    separate(name, into = c("d1", "d2", "d3", "year"), sep = "_") %>%
    dplyr::select(year, value) %>%
    mutate(name = "precipitation")

  tempdf <- enframe(tempMeans) %>%
    separate(name, into = c("d1", "d2", "d3", "year"), sep = "_") %>%
    dplyr::select(year, value) %>%
    mutate(name = "temperature")

  df <- bind_rows(pptdf, tempdf) %>%
    mutate(coreName = doArea)
  prism <- bind_rows(prism, df)
}

prism <- prism %>%
  rename("prism_value" = value) %>%
  mutate(year = as.numeric(year))



# Combine the data sources ------------------------------------------------

hindcasts <- hindcasts %>%
  rename("precipitation" = pr,
         "temperature" = tas) %>%
  pivot_longer(cols = c("precipitation", "temperature"), values_to = "gcm_value")



# Compute relative bias ---------------------------------------------------

modeldf <- hindcasts %>%
  left_join(prism, by = c("coreName", "year", "name")) %>%
  dplyr::select(coreName, source_id, year, name, prism_value, gcm_value) %>%
  drop_na()

prBias <- modeldf %>%
  filter(name == "precipitation") %>%
  group_by(coreName, source_id) %>%
  mutate(obsmean = mean(prism_value)) %>%
  mutate(bias = obsmean / gcm_value) %>%
  mutate(meanbias = mean(bias)) %>%
  mutate(newval = gcm_value * meanbias) %>%
  ungroup() %>%
  mutate(application = "*")

tasBias <- modeldf %>%
  filter(name == "temperature") %>%
  group_by(coreName, source_id) %>%
  mutate(obsmean = mean(prism_value)) %>%
  mutate(bias = obsmean - gcm_value) %>%
  mutate(meanbias = mean(bias)) %>%
  mutate(newval = gcm_value + meanbias) %>%
  ungroup() %>%
  mutate(application = "+")

biasCorrection <- bind_rows(prBias, tasBias)

relBiasCorrections <- biasCorrection %>%
  dplyr::select(coreName, source_id, name, meanbias, application) %>%
  ungroup() %>%
  distinct() %>%
  arrange(coreName, source_id, name)

saveRDS(relBiasCorrections, "Output/GCMBiasCorrections.RDS")



# Time series plots -------------------------------------------------------

ggplot(biasCorrection %>% filter(name == "precipitation"),
       aes(x = year)) +
  geom_line(aes(y = gcm_value), color = "blue", linetype = 2) +
  geom_line(aes(y = newval), color = "red") +
  geom_line(aes(y = prism_value)) +
  facet_wrap(~source_id)

ggplot(biasCorrection %>% filter(name == "temperature"),
       aes(x = year)) +
  geom_line(aes(y = gcm_value), color = "blue", linetype = 2) +
  geom_line(aes(y = newval), color = "red") +
  geom_line(aes(y = prism_value)) +
  facet_wrap(~source_id)


