library(raster)
# script to calculate sample size based on BIT rasters

wDir <- "L:\\102 - USGS\\102 - 88 - SageCastWY"

coreDir <- file.path(wDir, "Data\\BackInTime\\Sagebrush\\WYGrouseCoreAreas")


cores <- list.dirs(coreDir, full.names = TRUE)

sDat <- data.frame()

for(i in 1:length(cores)){
 cName <- basename(cores[i])
 if(cName != "WYGrouseCoreAreas"){
   trast <- raster(file.path(cores[i], "nlcd_sage_1985_mos_rec_v1.tif"))
   tvals <- getValues(trast)
   ncells = sum(!is.na(tvals))
   sDat <- rbind(sDat, list("coreName" = cName,
                            "totalCells" = ncells))
 }
}

sDat$sampleCells = ceiling(sDat$totalCells*0.05) #looks like sDraw uses ceiling for float inputs


write.csv(sDat, file.path(wDir, "Data", "SageGrouseCoreAreasv4", "sampleSizes.csv"))