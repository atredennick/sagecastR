# Make abbreviation and naming map

library(tidyverse)
library(sf)

setwd("\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/")
coreNames <- list.files("Data/BackInTime/Sagebrush/WYGrouseCoreAreas/")

cores <- tibble(
  Name = coreNames,
  NameNoSep = gsub(" ", "", coreNames),
  Abbreviation = abbreviate(coreNames, named = FALSE)
)

# associate with wyoming regions
corePolys <- st_read("./Data/SageGrouseCoreAreasv4/corev4_072915_final.shp")
regionPolys <- st_read("./Data/WyomingRegions/Fedy_2014_region_poly.shp")
regionPolys <- st_transform(regionPolys, st_crs(corePolys))

# ggplot() +
#   geom_sf(data = regionPolys, aes(fill = Name)) +
#   geom_sf(data = corePolys)

coreRegions <- st_join(corePolys, regionPolys, largest = TRUE)

# ggplot() +
#   geom_sf(data = regionPolys, aes(color = Name)) +
#   geom_sf(data = coreRegions, aes(fill = Name))

regions <- coreRegions %>%
  dplyr::select(NAME, Name) %>%
  rename("Region" = Name,
         "Name" = NAME)
regions$geometry <- NULL

cores <- cores %>% left_join(regions, by = "Name")


# add cover targets
targs <- data.frame(Region = unique(cores$Region),
                    NestingTarget = c(15.43, 9.04, 13.32),
                    SummerTarget = c(16.71, 10.36, 12.29))
cores <- cores %>% left_join(targs, by = "Region")

write.csv(x = cores, "Data/CoreAreaNamesMapping.csv", row.names = FALSE)
