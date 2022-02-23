

library(tidyverse)
library(ncdf4)
library(raster)
library(sageCastR)
library(RColorBrewer)
library(lattice)
library(gridExtra)

ncdfPath <- "L:\\102 - USGS\\102 - 88 - SageCastWY\\Output\\Forecasts\\"
ncNames <- c("SaltWells-dast-forecast-2018-2045-ssp585.nc",
             "SaltWells-dast-forecast-2046-2070-ssp585.nc",
             "SaltWells-dast-forecast-2071-2100-ssp585.nc")

ncfname <- paste0(ncdfPath, ncNames[1])
ncin <- nc_open(ncfname)


# get x's and y's
x <- ncvar_get(ncin,"lon")
xlname <- ncatt_get(ncin,"lon","long_name")
xunits <- ncatt_get(ncin,"lon","units")
nx <- dim(x)
head(x)

y <- ncvar_get(ncin,"lat")
ylname <- ncatt_get(ncin,"lat","long_name")
yunits <- ncatt_get(ncin,"lat", "units")
ny <- dim(y)
head(y)

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

# get percent cover
dname <- "cover"
cover_array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(cover_array)

lon <- ncvar_get(ncin, "lon")
dim(lon)

lat <- ncvar_get(ncin, "lat")
dim(lat)

nc_close(ncin)


m <- 1  # get one ensemble run
t <- 1  # get first year
cover_slice <- cover_array[t,,,m]
cover_slice[cover_slice > 100] <- 100
cols <- colorRampPalette(brewer.pal(9,"BrBG"))(25)
g1 <- levelplot(cover_slice, row.values = x, column.values = y, main = "2018 (observed)",
          xlab = "Longitude (meters)", ylab = "Latitude (meters)",
          col.regions = cols, at=seq(0, 100, length.out = 25),
          scales = list(tck = c(0,0), at = c(0), labels = c("")))

t <- 3
cover_slice <- cover_array[t,,,m]
cover_slice[cover_slice > 100] <- 100
cols <- colorRampPalette(brewer.pal(9,"BrBG"))(25)
g2 <- levelplot(cover_slice, row.values = x, column.values = y, main = "2020",
          xlab = "Longitude (meters)", ylab = "Latitude (meters)",
          col.regions = cols, at=seq(0, 100, length.out = 25),
          scales = list(tck = c(0,0), at = c(0), labels = c("")))

t <- 22
cover_slice <- cover_array[t,,,m]
cover_slice[cover_slice > 100] <- 100
cols <- colorRampPalette(brewer.pal(9,"BrBG"))(25)
g3 <- levelplot(cover_slice, row.values = x, column.values = y, main = "2040",
                xlab = "Longitude (meters)", ylab = "Latitude (meters)",
                col.regions = cols, at=seq(0, 100, length.out = 25),
                scales = list(tck = c(0,0), at = c(0), labels = c("")))



ncfname <- paste0(ncdfPath, ncNames[2])
ncin <- nc_open(ncfname)


# get x's and y's
x <- ncvar_get(ncin,"lon")
xlname <- ncatt_get(ncin,"lon","long_name")
xunits <- ncatt_get(ncin,"lon","units")
nx <- dim(x)
head(x)

y <- ncvar_get(ncin,"lat")
ylname <- ncatt_get(ncin,"lat","long_name")
yunits <- ncatt_get(ncin,"lat", "units")
ny <- dim(y)
head(y)

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

# get percent cover
dname <- "cover"
cover_array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(cover_array)

lon <- ncvar_get(ncin, "lon")
dim(lon)

lat <- ncvar_get(ncin, "lat")
dim(lat)

nc_close(ncin)


m <- 1  # get one ensemble run
t <- which(time == 42)
cover_slice <- cover_array[t,,,m]
cover_slice[cover_slice > 100] <- 100

cols <- colorRampPalette(brewer.pal(9,"BrBG"))(25)
g4 <- levelplot(cover_slice, row.values = x, column.values = y, main = "2060",
                xlab = "Longitude (meters)", ylab = "Latitude (meters)",
                col.regions = cols, at=seq(0, 100, length.out = 25),
                scales = list(tck = c(0,0), at = c(0), labels = c("")))



ncfname <- paste0(ncdfPath, ncNames[3])
ncin <- nc_open(ncfname)
# get x's and y's
x <- ncvar_get(ncin,"lon")
xlname <- ncatt_get(ncin,"lon","long_name")
xunits <- ncatt_get(ncin,"lon","units")
nx <- dim(x)
head(x)

y <- ncvar_get(ncin,"lat")
ylname <- ncatt_get(ncin,"lat","long_name")
yunits <- ncatt_get(ncin,"lat", "units")
ny <- dim(y)
head(y)

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

# get percent cover
dname <- "cover"
cover_array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(cover_array)

lon <- ncvar_get(ncin, "lon")
dim(lon)

lat <- ncvar_get(ncin, "lat")
dim(lat)

nc_close(ncin)

t <- which(time == 62) # get first year
cover_slice <- cover_array[t,,,m]
cover_slice[cover_slice > 100] <- 100

cols <- colorRampPalette(brewer.pal(9,"BrBG"))(25)
g5 <- levelplot(cover_slice, row.values = x, column.values = y, main = "2080",
                xlab = "Longitude (meters)", ylab = "Latitude (meters)",
                col.regions = cols, at=seq(0, 100, length.out = 25),
                scales = list(tck = c(0,0), at = c(0), labels = c("")))


m <- 1  # get one ensemble run
t <- which(time == 82) # get first year
cover_slice <- cover_array[t,,,m]
cover_slice[cover_slice > 100] <- 100

cols <- colorRampPalette(brewer.pal(9,"BrBG"))(25)
g6 <- levelplot(cover_slice, row.values = x, column.values = y, main = "2100",
                xlab = "Longitude (meters)", ylab = "Latitude (meters)",
                col.regions = cols, at=seq(0, 100, length.out = 25),
                scales = list(tck = c(0,0), at = c(0), labels = c("")))



saltWellsPlots <- grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 3, nrow = 2)
saveRDS(object = saltWellsPlots, file = "Figures/SaltWellsSpatialForecasts.RDS")
