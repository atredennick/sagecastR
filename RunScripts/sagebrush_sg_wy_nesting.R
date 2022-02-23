library(terra)
library(tidyverse)
library(readr)
library(sf)
library(raster)

files <- list.files(path = "//igskbacbdbcdr1/odonnelled/active/SG_GISLibrary/Vegetation/USGS_TS_Shrubland_range_v1/DerivedData/GB_WY_4States/mosaic",
                    full.names = TRUE)[grepl(".tif", list.files(path = "//igskbacbdbcdr1/odonnelled/active/SG_GISLibrary/Vegetation/USGS_TS_Shrubland_range_v1/DerivedData/GB_WY_4States/mosaic"))]
sage_stack <- raster::stack(files[-34])
states <- read_sf("//IGSKBACBFS02/Home/amonroe/Documents/Restoration Analysis/State boundaries/tl_2017_us_state", "tl_2017_us_state")
wyoming <- dplyr::filter(states, NAME == "Wyoming") %>% st_transform(crs = proj4string(sage_stack))

sage_wy <- crop(sage_stack, wyoming)
sage_wy <- raster::mask(sage_wy, wyoming)
setwd("//igskbacbdbcdr1/ODonnelled/active/SG_GISLibrary/Vegetation/USGS_TS_Shrubland_range_v1/DerivedData")
writeRaster(sage_wy, file = "sagebrush_wyoming.tif", filetype = "GTiff", overwrite = TRUE)
sage_wy <- raster::stack("sagebrush_wyoming.tif")
sage_wy_rast <- rast(sage_wy)
ras100 <- rast(raster(extent(sage_wy), resolution = 100, crs = crs(sage_wy)))
sage_wy_100 <- resample(sage_wy_rast, ras100, method = "bilinear")

sage_0.95uci <- quantile(sage_wy_100, 0.95)

sg_habitat <- raster("//igskbacbfs02/home/amonroe/Documents/Sage-Grouse/wysg_lp_regional_nsw_bool/wysg_lp_regional_nsw_bool.tif")
wy_regions <- st_read(dsn = "//igskbacbfs02/home/amonroe/Documents/Sage-Grouse/Fedy_2014/Fedy_2014",
                      layer = "Fedy_2014_region_poly") %>% st_transform(crs = proj4string(sg_habitat))

sg_habitat_nesting <- rast(sg_habitat)

sg_habitat_nesting[sg_habitat_nesting == 11] <- 1
sg_habitat_nesting[sg_habitat_nesting == 10] <- NA
sg_habitat_nesting[sg_habitat_nesting == 101] <- 1
sg_habitat_nesting[sg_habitat_nesting == 111] <- 1
sg_habitat_nesting[sg_habitat_nesting == 100] <- NA
sg_habitat_nesting[sg_habitat_nesting == 110] <- NA
sg_habitat_nesting[sg_habitat_nesting == 0] <- NA

ras100 <- rast(ext(sage_0.95uci), resolution = 100, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
sg_habitat_nesting_100 <- resample(sg_habitat_nesting, ras100, method = "bilinear")

sage_0.95uci_sw <- crop(sage_0.95uci, vect(wy_regions[which(wy_regions$Name == "Southwest Region"), ]))
sage_0.95uci_sw <- mask(sage_0.95uci_sw, vect(wy_regions[which(wy_regions$Name == "Southwest Region"), ]))

sg_habitat_nesting_sw_100 <- crop(sg_habitat_nesting_100, vect(wy_regions[which(wy_regions$Name == "Southwest Region"), ]))
sg_habitat_nesting_sw_100 <- mask(sg_habitat_nesting_sw_100, vect(wy_regions[which(wy_regions$Name == "Southwest Region"), ]))

sage_95_uci_sg_sw <- sage_0.95uci_sw * sg_habitat_nesting_sw_100
median(as.numeric(values(sage_95_uci_sg_sw)), na.rm = T) # Nesting, southwest
# 15.43%

sage_0.95uci_ce <- crop(sage_0.95uci, vect(wy_regions[which(wy_regions$Name == "Central Region"), ]))
sage_0.95uci_ce <- mask(sage_0.95uci_ce, vect(wy_regions[which(wy_regions$Name == "Central Region"), ]))

sg_habitat_nesting_ce_100 <- crop(sg_habitat_nesting_100, vect(wy_regions[which(wy_regions$Name == "Central Region"), ]))
sg_habitat_nesting_ce_100 <- mask(sg_habitat_nesting_ce_100, vect(wy_regions[which(wy_regions$Name == "Central Region"), ]))

sage_95_uci_sg_ce <- sage_0.95uci_ce * sg_habitat_nesting_ce_100
median(as.numeric(values(sage_95_uci_sg_ce)), na.rm = T) # Nesting, central
# 13.32%

sage_0.95uci_ne <- crop(sage_0.95uci, vect(wy_regions[which(wy_regions$Name == "Northeast Region"), ]))
sage_0.95uci_ne <- mask(sage_0.95uci_ne, vect(wy_regions[which(wy_regions$Name == "Northeast Region"), ]))

sg_habitat_nesting_ne_100 <- crop(sg_habitat_nesting_100, vect(wy_regions[which(wy_regions$Name == "Northeast Region"), ]))
sg_habitat_nesting_ne_100 <- mask(sg_habitat_nesting_ne_100, vect(wy_regions[which(wy_regions$Name == "Northeast Region"), ]))

sage_95_uci_sg_ne <- sage_0.95uci_ne * sg_habitat_nesting_ne_100
median(as.numeric(values(sage_95_uci_sg_ne)), na.rm = T) # Nesting, northeast
# 9.04%

# Summer ------------------------------------------------------------------

sg_habitat_summer <- rast(sg_habitat)

sg_habitat_summer[sg_habitat_summer == 1] <- NA
sg_habitat_summer[sg_habitat_summer == 11] <- 1
sg_habitat_summer[sg_habitat_summer == 10] <- 1
sg_habitat_summer[sg_habitat_summer == 101] <- NA
sg_habitat_summer[sg_habitat_summer == 111] <- 1
sg_habitat_summer[sg_habitat_summer == 100] <- NA
sg_habitat_summer[sg_habitat_summer == 110] <- 1
sg_habitat_summer[sg_habitat_summer == 0] <- NA

ras100 <- rast(ext(sage_0.95uci), resolution = 100, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
sg_habitat_summer_100 <- resample(sg_habitat_summer, ras100, method = "bilinear")

sg_habitat_summer_sw_100 <- crop(sg_habitat_summer_100, vect(wy_regions[which(wy_regions$Name == "Southwest Region"), ]))
sg_habitat_summer_sw_100 <- mask(sg_habitat_summer_sw_100, vect(wy_regions[which(wy_regions$Name == "Southwest Region"), ]))

sage_95_uci_sg_sw <- sage_0.95uci_sw * sg_habitat_summer_sw_100
median(as.numeric(values(sage_95_uci_sg_sw)), na.rm = T) # Summer, southwest
# 16.71%

sg_habitat_summer_ce_100 <- crop(sg_habitat_summer_100, vect(wy_regions[which(wy_regions$Name == "Central Region"), ]))
sg_habitat_summer_ce_100 <- mask(sg_habitat_summer_ce_100, vect(wy_regions[which(wy_regions$Name == "Central Region"), ]))

sage_95_uci_sg_ce <- sage_0.95uci_ce * sg_habitat_summer_ce_100
median(as.numeric(values(sage_95_uci_sg_ce)), na.rm = T) # Summer, central
# 12.29%

sg_habitat_summer_ne_100 <- crop(sg_habitat_summer_100, vect(wy_regions[which(wy_regions$Name == "Northeast Region"), ]))
sg_habitat_summer_ne_100 <- mask(sg_habitat_summer_ne_100, vect(wy_regions[which(wy_regions$Name == "Northeast Region"), ]))

sage_95_uci_sg_ne <- sage_0.95uci_ne * sg_habitat_summer_ne_100
median(as.numeric(values(sage_95_uci_sg_ne)), na.rm = T) # Summer, northeast
# 10.36%

