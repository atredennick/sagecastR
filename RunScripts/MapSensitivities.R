library(raster)
library(ggplot2)
library(colorspace)
library(ggpubr)
library(here)

# Script to map the average sensitivities in each core area and
#     calculate the mean temperature, precip, and elevation.

#
wdir <- "\\\\lar-file-srv/Data/102 - USGS/102 - 88 - SageCastWY/"
# setwd(wdir)

nameMap <- read.csv("AuxData/CoreAreaNamesMapping.csv")

outFigDir <- here("Figures/SensitivityPlots/")
outDatDir <- here("Output/")

#-----Load the sensitivity files
sensDir <- here("Output/Sensitivities/")

allSensDirs <- basename(list.dirs(sensDir))
allSensDirs <- allSensDirs[!allSensDirs %in% c("Sensitivities",
                                               "NLar")]

sensDat <- data.frame()
for(i in 1:length(allSensDirs)) {
  sensFile <- readRDS(file.path(sensDir, allSensDirs[i],
                        "LogChangeSensitivity.RDS"))
  sensAg <- aggregate(Sensitivity ~ CoreName + Driver, data = sensFile,
                      FUN = mean)
  sensDat <- rbind(sensDat, sensAg)
}



#-------------------------------------------------------------------------------
#-----Get averages of environmental drivers in each coreArea
#-------------------------------------------------------------------------------


coreAreas <- sf::st_read(file.path(wdir,"Data\\SageGrouseCoreAreasv4\\corev4_072915_final.shp"))

ustates <- sf::st_read(paste(wdir,"Data", "US_states","tl_2019_us_state.shp",sep = "\\"))
wy <- ustates[ustates$NAME == 'Wyoming',]
wy <- sf::st_transform(wy, 26913) #UTMzone 13)
wyBfDist <- 4000 # distance in meters to buffer wyoming border
wybuff <- sf::st_buffer(wy,wyBfDist)


#-----Load environmental layers
# read in average precip
prismPr <- raster(file.path(wdir, "Data\\PRISM\\PRISM_normals",
                            "PRISM_ppt_30yr_normal_800mM2_annual_bil.bil"))

prismTmn <- raster(file.path(wdir, "Data\\PRISM\\PRISM_normals",
                            "PRISM_tmean_30yr_normal_800mM2_annual_bil.bil"))

dem <- raster(file.path(wdir, "Data\\US_elevation", "na_elevation.tif"))

#-----Process dem
cropDem <- crop(dem, sf::st_transform(wybuff, sf::st_crs(dem)))
elev <- projectRaster(cropDem, crs = sf::st_crs(coreAreas)$proj4string)

#-----Process climate
climStack <- stack(list(prismPr, prismTmn))
cropClim <- crop(climStack, sf::st_transform(wybuff, sf::st_crs(climStack)))
prjClim <- projectRaster(cropClim, crs = sf::st_crs(coreAreas)$proj4string)
rsClim <- resample(prjClim, elev)

# Combine layers
envStack <- stack(rsClim, elev)

#-----Convert core areas to raster
coreRast <- rasterize(x = coreAreas, y = envStack, field = 'ID', 'first')

#-----Extract data in coreAreas
coreStat <- as.data.frame(raster::zonal(envStack, coreRast, fun = 'mean'))
names(coreStat) <- c('ID', 'prMean', 'tMean', 'elev')
coreStat$NAME <- coreAreas$NAME[match(coreStat$ID, coreAreas$ID)]


#-------------------------------------------------------------------------------
#-----Combine sensitivity and driver dataframes
#-------------------------------------------------------------------------------

sensWide <- as.data.frame(tidyr::pivot_wider(sensDat,
                               id_cols = CoreName,
                               names_from = Driver,
                               values_from = Sensitivity,
                               names_prefix = "sens"))
names(sensWide) <- c("simpName", "sensPr", "sensTmean", "sensBoth")

sensWide$ID <- coreAreas$ID[match(sensWide$simpName,
                                  gsub(" ", "", coreAreas$NAME))]



combDat <- merge(coreStat, sensWide, by.all= "ID", all.x = T)

write.csv(combDat, file.path(outDatDir, "environ_sensitivity_summary.csv"))


#-------------------------------------------------------------------------------
#----- Maps of sensitivities
#-------------------------------------------------------------------------------

coreMerge <- merge(coreAreas, combDat, by.all = NAME, all.x = T)
coreMerge <- sf::st_transform(coreMerge, 26913)
wyProj <- sf::st_transform(wy, 26913)

pvars <- c("sensTmean", "sensPr", "sensBoth")
pNames <- c("Temperature", "Precipitation", "Combined")

pvars <- c("sensTmean", "sensPr")
pNames <- c("Temperature", "Precipitation")

coreMerge <- coreMerge %>%
  rename("Name" = NAME) %>%
  left_join(nameMap, by = "Name")

# remove the unreliable model estimates from the results
rms <- c("Powder", "Sage", "Seedskadee", "Uinta", "Elk Basin West")

coreMerge <- coreMerge %>%
  filter(!Name %in% rms)

pal <- RColorBrewer::brewer.pal(7, name = "BrBG")
pal[4] <- "grey80"
scale_color_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "color", ...) {
  binned_scale("color", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)
}

plotList <- list()
for(v in 1:length(pvars)){
  sPlot <- ggplot(data = wyProj) +
    geom_sf(fill = "white", color = "tan") +
    geom_sf(data = coreMerge, aes_string(fill=pvars[v]), color = NA) +
    geom_sf_label(data = coreMerge, aes(label = Abbreviation), size = 2,
                  fill = NA, label.size = NA, color = "grey35") +
    scale_x_continuous(breaks = c(seq(-111, -104, by = 2))) +
    # scale_color_fermenter_custom(pal, name = paste0(pNames[i],"\nsensitivity"),
    #                              breaks = seq(-0.2,0.2,by = 0.1),
    #                              limits = c(-1, 1)) +
    scale_fill_fermenter(palette = "BrBG",
                         direction = 1,
                         name = paste0(pNames[v],"\nsensitivity"),
                         breaks = seq(-0.25,0.25,by= 0.1),
                         limits = c(-1, 1)) +
    xlab("Longitude") +
    ylab("Latitude") +
    # ggtitle(paste0(pNames[v]," Sensitivity By Core Area")) +
    ggtitle(LETTERS[v]) +
    theme_classic(base_size = 10) +
    theme(strip.background = element_blank())+
    coord_sf()
    # theme(plot.title = element_text(hjust = 0.5))
  plotList[[pNames[v]]] <- sPlot
}

saveRDS(plotList, "./Output/sensitivityPlotList.RDS")

# Combine plots
# sensPlot <- ggarrange(plotList[[1]], plotList[[2]], plotList[[3]],
#                       labels = c("A", "B", "C"),
#                       ncol = 3, nrow = 1)
# sensPlot <- cowplot::plot_grid(plotlist = plotList, nrow = 1)

#
# # Plot and save
# pltDim <- c(5, 8.5)
# x11(width = pltDim[1], height = pltDim[2])
# sensPlot
# dev.print(jpeg, file = file.path(outFigDir, paste0("sensitivityPlot",".jpg")),
#           width = pltDim[1], height = pltDim[2], units = "in", res=300)
# dev.off()
#
# saveRDS(sensPlot, file.path(outFigDir, paste0("sensitivityPlot",".RDS")))












#
# for(v in 1:length(pvars)) {
#   sPlot <- ggplot(data = wyProj) +
#     geom_sf(aes(col = NAME)) +
#     scale_color_manual(name = "", values = c("Wyoming" = "black")) +
#     geom_sf(data = coreMerge, aes(fill = get(pvars[v]))) +
#     scale_fill_continuous_diverging(palette = 'Green-Brown',
#                                     name = "Sensitivity") +
#     # scale_fill_distiller(palette = "BrBG", name = "Sensitivity", direction = 1) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     # ggtitle(paste0(pNames[v]," Sensitivity By Core Area")) +
#     theme_bw() +
#     theme(text = element_text(size=11)) +
#     theme(plot.title = element_text(hjust = 0.5))
#
#   # Plot and save
#   pltDim <- c(6, 6)
#   x11(width = pltDim[1], height = pltDim[2])
#   print(sPlot)
#   dev.print(jpeg, file = file.path(outFigDir, paste0("sensitivityPlot_", pvars[v],".jpg")),
#             width = pltDim[1], height = pltDim[2], units = "in", res=300)
#   dev.off()
#
#   saveRDS(sPlot, file.path(outFigDir, paste0("sensitivityPlot_", pvars[v],".RDS")))
#
# }
#





