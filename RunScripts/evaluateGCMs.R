# library(raster)
library(ggplot2)
library(dplyr)
# library(sf)
# library(foreach)
# library(doSNOW)

wdir <- "L:\\102 - USGS\\102 - 88 - SageCastWY"
gitDir <- "C://data//git//sagecastr" 

  # # dates to use for analysis
  # begSpring <- "03-01"
  # endSummer <- "08-31"
  # 
  # rawdir <- paste(wdir,"GIS","CMIP", "raw", sep = "\\")
  # 
  # rawdat <- read.csv(file.path(wdir,"GIS","CMIP", "rawdat.csv"))


# JUST LOADING EXTRACTED results (commenting out extract code below)
    # #-------------------------------------------------------------------------------
    # #-----Extract climate variables for each GCM in each Sage Grouse core area
    # #-------------------------------------------------------------------------------
    # prismTpml <- raster(file.path(wdir, "GIS", "PRISM", "PRISM_WY", "PRISM_ppt_4km_1982.tif"))
    # 
    # ustates <- sf::st_read(paste(wdir,"GIS","tl_2019_us_state.shp",sep = "\\"))
    # wy <- ustates[ustates$NAME == 'Wyoming',]
    # wy <- sf::st_transform(wy, crs(prismTpml)@projargs)  
    # wyRast <- rasterize(x = wy, y = prismTpml, field = 1)
    # 
    # prjYears <- 1982:2018 # years to process
    # 
    # coreAreas <- sf::st_read("L:\\102 - USGS\\102 - 88 - SageCastWY\\Data\\SageGrouseCoreAreasv4\\corev4_072915_final.shp")
    # coreAreas <- sf::st_transform(coreAreas, crs(prismTpml)@projargs)
    # coreRast <- rasterize(x = coreAreas, y = prismTpml, field = 'ID', 'first')
    # 
    # 
    # # uSources <- sort(unique(rawdat$uSource))
    # # uDataSets <- sort(unique(rawdat$uDatID))
    # 
    # cmipDir <- file.path(wdir, "GIS", "CMIP", "CMIP_WY")
    # cmipPaths <- c(list.files(cmipDir,pattern = "\\.tif$",ignore.case = T, full.names = T))
    # cmipFiles <- c(list.files(cmipDir,pattern = "\\.tif$",ignore.case = T))
    # cmipDataSets <- unname(sapply(cmipFiles, function(x) substring(x, 1, (nchar(x)-9))))
    # 
    # udsets <- unique(cmipDataSets)
    # 
    # extractCoreCMIP <- function(dset) {
    #   rfiles <- cmipPaths[cmipDataSets == dset]
    #   dYears <- as.integer(sapply(rfiles, function(x) 
    #     substring(x, (nchar(x) - 7), (nchar(x)-4))))
    #   kYears <- dYears[dYears %in% prjYears]
    #   if(length(kYears) > 0){
    #     dStack <- raster::stack(rfiles[dYears %in% prjYears])
    #     rsStack <- raster::resample(dStack, prismTpml, method = 'ngb')
    #     coreStat <- raster::zonal(rsStack, coreRast, fun = 'mean')
    #     # coreStat <- raster::zonal(rsStack, wyRast, fun = 'mean')
    #     options(stringsAsFactors = FALSE)
    #     coreDF <- data.frame()
    #     for(i in 2:ncol(coreStat)){
    #       coreDF <- rbind(coreDF, list(coreID = coreStat[,1],
    #                                    val = coreStat[,i],
    #                                    year = rep(kYears[(i-1)], nrow(coreStat)),
    #                                    uDatID = rep(dset, nrow(coreStat))))
    #     }
    #   }
    #   else{
    #     coreDF <- data.frame(coreID = NA,
    #                          val = NA,
    #                          year = NA,
    #                          uDatID = NA)
    #   }
    #   return(coreDF)
    # }
    # 
    # start.time=Sys.time()
    # # cluster = makeCluster(as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))-1, type = "SOCK")
    # cluster = makeCluster(26, type = "SOCK")
    # registerDoSNOW(cluster)
    # coreDF <- foreach(i = udsets, .combine = rbind, .packages = c('raster')) %dopar% {
    #   extractCoreCMIP(i)
    # }
    # print(start.time - Sys.time())
    # print(Sys.time())
    # stopCluster(cluster)
    # 
    # summary(coreDF)
    # 
    # # ggplot(coreDF, aes(x = year, y = val)) +
    # #   geom_point() +
    # #   facet_wrap(~uDatID, scales = "free_y")
    # 
    # 
    # # Merge in gcm metadata
    # cmipDF <- merge(coreDF, 
    #                  rawdat[ c('uDatID', 'mip_era', 'institution_id',
    #                            'source_id', 'experiment_id',
    #                            'member_id', 'frequency',
    #                            'variable_id', 'variable_units')], 
    #                  by.all = 'uDatID', all.x = T)
    # 
    # # convert units to match PRISM
    # cmipDF$cval <- NA
    # # multiply precip by 86400 (number of seconds in day), to convert to mm/day
    # cmipDF[cmipDF$variable_units == 'kg m-2 s-1', 'cval'] <- 
    #   cmipDF[cmipDF$variable_units == 'kg m-2 s-1', 'val']*86400
    # 
    # #convert kelvin to celcius
    # cmipDF[cmipDF$variable_units == 'K', 'cval'] <- 
    #   cmipDF[cmipDF$variable_units == 'K', 'val']-273.15
    # 
    # 
    # #-----Extract PRISM Data
    # prismDir <- file.path(wdir, "GIS", "PRISM", "PRISM_WY")
    # pPaths <- c(list.files(prismDir,pattern = "\\.tif$",ignore.case = T, full.names = T))
    # pFiles <- c(list.files(prismDir,pattern = "\\.tif$",ignore.case = T))
    # pVars <- unname(sapply(pFiles, function(x)
    #   strsplit(x, "_")[[1]][2]))
    # 
    # prismDF <- data.frame()
    # for(v in unique(pVars)){
    #   vFiles <- pPaths[pVars == v]
    #   vYears <- as.integer(sapply(basename(vFiles), function(x) 
    #     substring(x, (nchar(x) - 7), (nchar(x)-4))))
    #   pStack <- raster::stack(vFiles)
    #   corePRISM <- raster::zonal(pStack, coreRast, fun = 'mean')
    #   # corePRISM <- raster::zonal(pStack, wyRast, fun = 'mean')
    #   options(stringsAsFactors = FALSE)
    #   for(i in 2:ncol(corePRISM)){
    #     prismDF <- rbind(prismDF, list(coreID = corePRISM[,1],
    #                                  cval = corePRISM[,i],
    #                                  year = rep(vYears[(i-1)], nrow(corePRISM)),
    #                                  variable_id = rep(ifelse(v == 'ppt', 'pr', 'tas'), nrow(corePRISM)),
    #                                  uDatID = rep("PRISM", nrow(corePRISM))))
    #   }
    # }
    # 
    # 
    # prismDF %>%
    #   group_by(variable_id) %>%
    #   summarize(mn = mean(cval),
    #             sd = sd(cval),
    #             max = max(cval),
    #             min = min(cval))
    # 
    # 
    # # Add cmip fields to prism DF for merge
    # cmipFields <- names(cmipDF)
    # for(i in cmipFields){
    #   if(!i %in% names(prismDF)){
    #     prismDF[,i] <- NA
    #   }
    # }
    # 
    # prismDF <- prismDF[,cmipFields]
    # prismDF$source_id <- 'PRISM'
    # 
    # climDF <- rbind(cmipDF, prismDF)
    # climDF$isPrism <- ifelse(climDF$uDatID == 'PRISM',1,0)
    # 
    # climDF %>%
    #   group_by(variable_id, isPrism) %>%
    #   summarize(mn = mean(cval),
    #             sd = sd(cval),
    #             max = max(cval),
    #             min = min(cval))
    # 
    # 
    # write.csv(climDF, file.path(wdir,"GIS","CMIP", "allGCM.csv"))

climDF <- read.csv(file.path(wdir, "Data", "CMIP6", "allGCM.csv"))

#-------------------------------------------------------------------------------
#-----Make plots to visualize the GCM and PRISM trends
#-------------------------------------------------------------------------------

#-----Only keep years 1985-2018 for evaluation
climDF <- climDF[climDF$year >= 1985 & climDF$year <= 2018,]


#-----Calc mean PRISM temperature across all sites
prismTasMn <- climDF %>%
  filter(variable_id == 'tas' & isPrism == 1) %>%
  group_by(year, source_id) %>%
  summarize(val_mn = mean(cval)) 

prismTasMn <- data.frame(year = prismTasMn$year,
                         val_mn = prismTasMn$val_mn,
                         source_id = rep(unique(climDF$source_id), each = nrow(prismTasMn)))

#-----Calc mean PRISM temperature across all sites
prismPrMn <- climDF %>%
  filter(variable_id == 'pr' & isPrism == 1) %>%
  group_by(year, source_id) %>%
  summarize(val_mn = mean(cval)) 

prismPrMn <- data.frame(year = prismPrMn$year,
                         val_mn = prismPrMn$val_mn,
                         source_id = rep(unique(climDF$source_id), each = nrow(prismPrMn)))

#-----Avg temperature across all core areas by GCM
climDF %>%
  filter(variable_id == 'tas' & isPrism == 0) %>%
  group_by(year, source_id) %>%
  summarize(val_mn = mean(cval)) %>%
  ggplot(aes(x = year, y = val_mn)) +
    geom_line(aes(col = 'GCM')) + 
    ylab("Average Temperature (Celcius)") +
    facet_wrap(~source_id) +
    geom_line(aes(x = year, y = val_mn, col = 'PRISM'), data = prismTasMn) +
    scale_color_manual(name = "", values = c("GCM" = "blue", "PRISM" = "black"))
    #annotate(geom='line', x = prismTasMn$year, y = prismTasMn$val_mn)
    #geom_line(aes(year, val_mn), data = prismTasMn, color = 'black', size = 1)
  


#-----Avg precip across all core areas by GCM
climDF %>%
  filter(variable_id == 'pr' & isPrism == 0) %>%
  group_by(year, source_id) %>%
  summarize(val_mn = mean(cval)) %>%
  ggplot(aes(x = year, y = val_mn)) +
  geom_line(aes(col = 'GCM')) + 
  ylab("Average Precipitation (mm/day)") +
  facet_wrap(~source_id) +
  geom_line(aes(x = year, y = val_mn, col = 'PRISM'), data = prismPrMn) +
  scale_color_manual(name = "", values = c("GCM" = "blue", "PRISM" = "black"))




#-------------------------------------------------------------------------------
#-----Scale climate variables relative to 1982 - 2014 mean for each GCM
#-------------------------------------------------------------------------------
centerData <- function(x){
  centered <- x - mean(x)
  centered
}

climScaled <- climDF %>%
  group_by(variable_id, year, source_id) %>%
  summarize(val_mn = mean(cval)) %>%
  group_by(variable_id, source_id) %>%
  mutate(scaled_mn = centerData(val_mn))


#-----Plot scaled precip
prismSub <- climScaled %>%
  filter(source_id == "PRISM" & variable_id == 'pr')

prismPrScaled <- data.frame(year = prismSub$year,
                            scaled_mn = prismSub$scaled_mn,
                            source_id = rep(unique(climScaled$source_id), 
                                            each = nrow(prismSub)))
climScaled %>%
  filter(variable_id == 'pr' & source_id != "PRISM") %>%
  group_by(year, source_id) %>%
  ggplot(aes(x = year, y = scaled_mn)) +
  geom_hline(yintercept = 0, col = 'grey') +
  geom_line(aes(col = 'GCM')) + 
  ylab("Average Precip Deviation From Mean (mm/day)") +
  facet_wrap(~source_id) +
  geom_line(aes(x = year, y = scaled_mn, col = 'PRISM'), data = prismPrScaled) +
  scale_color_manual(name = "", values = c("GCM" = "blue", "PRISM" = "black"))




#-----Plot scaled temperature
prismSub <- climScaled %>%
  filter(source_id == "PRISM" & variable_id == 'tas')

prismTasScaled <- data.frame(year = prismSub$year,
                            scaled_mn = prismSub$scaled_mn,
                            source_id = rep(unique(climScaled$source_id), 
                                            each = nrow(prismSub)))
climScaled %>%
  filter(variable_id == 'tas' & source_id != "PRISM") %>%
  group_by(year, source_id) %>%
  ggplot(aes(x = year, y = scaled_mn)) +
  geom_hline(yintercept = 0, col = 'grey') +
  geom_line(aes(col = 'GCM')) + 
  ylab("Average Temperature Deviation From Mean (Celcius)") +
  facet_wrap(~source_id) +
  geom_line(aes(x = year, y = scaled_mn, col = 'PRISM'), data = prismTasScaled) +
  scale_color_manual(name = "", values = c("GCM" = "blue", "PRISM" = "black"))





#-------------------------------------------------------------------------------
#-----Add "Null" model (PRISM Mean)
#-------------------------------------------------------------------------------
# nullDat <- data.frame(year = rep(1982:2014, 2),
#                       variable_id = rep(c('pr', 'tas'), each = length(1982:2014)),
#                       c())




#-------------------------------------------------------------------------------
#-----Load bias-corrections
#-------------------------------------------------------------------------------
biasCorr <- readRDS(file.path(gitDir, "Output", "GCMBiasCorrections.RDS"))

# load core areas (need ID field)
sfCores <- sf::st_read(file.path(wdir, "Data\\SageGrouseCoreAreasv4", 
                                 "corev4_072915_final.shp")) %>%
  sf::st_drop_geometry()

biasCorr <- left_join(biasCorr, sfCores[,c("NAME", "ID")],
                      by = c("coreName" = "NAME"))

# check no NAs
all(!is.na(biasCorr$ID))


#-----apply bias corrections
uSources <- sort(unique(climDF$source_id))
uGCM <- uSources[uSources != "PRISM" & uSources != "AWI-CM-1-1-MR"]
uVars <- sort(unique(climDF$variable_id))

climDF$valBC <- NA  # empty column to store bias corrections

for(g in uGCM){
  for(v in uVars){
    for(i in unique(climDF$coreID)){
      varName <- ifelse(v == 'tas', 'temperature', 'precipitation')
      tmeanbias <- biasCorr[biasCorr$source_id == g &
                              biasCorr$name == varName &
                              biasCorr$ID == i, ] %>%
        pull('meanbias')
      
      # error check
      if(length(tmeanbias) != 1) {stop("Check tmeanbias!")}
      
      climRows <- which(climDF$variable_id == v &
                         climDF$coreID == i &
                         climDF$source_id == g)
      
      if(v == 'tas') {
        climDF[climRows, 'valBC'] = climDF[climRows, 'cval'] + tmeanbias
      }
      else if(v=='pr') {
        climDF[climRows, 'valBC'] = climDF[climRows, 'cval'] * tmeanbias
      }
    }
  }
}

if(any(is.na(climDF[climDF$source_id %in% uGCM, 'valBC']))) {stop("MISSING bias-corrected values!")}


#-------------------------------------------------------------------------------
#-----Calculate Error Metric for each model
#-------------------------------------------------------------------------------
# Ej* = sum(Ei)
# Eij* = (Eij - min(Ej)) / max(Ej) - min(Ej)
# Eij = sum(abs(prismijk - gcmijk)) / K
# i = variable
# j = gcm
# k = yearly mean value
# K = total years


annClim <- climDF %>%
  group_by(year, source_id, variable_id) %>%
  summarize(val_mn = mean(cval),
            val_mnBC = mean(valBC)) %>% # bias corrected mean
  as.data.frame()

errDat <- data.frame()

for(g in uGCM){
  for(v in uVars){
    gDat <- annClim[annClim$source_id == g & annClim$variable_id == v,]
    if(nrow(gDat) > 0){
      gDat$scaledMn <- centerData(gDat$val_mn)
      pDat <- annClim %>%
        filter(source_id == "PRISM" & variable_id == v) %>%
        rename(prismMn = val_mn) %>%
        mutate(prismScaled = centerData(prismMn))
      gDat <- merge(gDat, pDat[, c('year', 'prismMn', 'prismScaled')],
                    by.all = year, all.x = T)
      mod1 <- lm(val_mn ~ prismMn, data = gDat)
      errDat <- rbind(errDat, list(GCM = g,
                                   variable = v,
                                   mae = sum(abs(gDat$prismMn - gDat$val_mn)),
                                   maeCent = sum(abs(gDat$prismScaled - gDat$scaledMn)),
                                   maeBiasCorr = sum(abs(gDat$prismMn - gDat$val_mnBC)),
                                 rsq = round(summary(mod1)$r.squared, 3)))
      rm(list = c("pDat", "mod1"))
    }
    rm(gDat)
  }
}

errDat <- errDat[order(errDat$variable, errDat$mae),]

#----calc relative error
# tst <- errDat %>%
#   group_by(GCM, variable) %>%
#   mutate(relErr = (mae - min(mae))/(max(mae) - min(mae)),
#          relErrCent = (maeCent - min(maeCent))/(max(maeCent) - min(maeCent))) %>%
#   as.data.frame()



#-------------------------------------------------------------------------------
#----- calc relative error
#-------------------------------------------------------------------------------
errDat2 <- data.frame()
for(v in uVars){
  vDat <- errDat[errDat$variable == v,]
  for(k in c('mae', 'maeCent', 'maeBiasCorr')) {
    kmae <- vDat[,k]
    minMae <- min(vDat[,k])
    maxMae <- max(vDat[,k])
    vDat[, paste0('relErr_',k)] <- (kmae - minMae)/(maxMae - minMae)
  }
  errDat2 <- rbind(errDat2, vDat)
}

 

#-------------------------------------------------------------------------------
#----- Convert to wide format and calc overall error
#-------------------------------------------------------------------------------
errDatWd <- errDat2[errDat2$variable == 'tas', ]
errDatWd <- errDatWd %>%
  rename(maeTas = mae,
         maeCentTas = maeCent,
         maeBiasCorrTas = maeBiasCorr,
         relErrTas = relErr_mae,
         relErrCentTas = relErr_maeCent,
         relErrBiasCorrTas = relErr_maeBiasCorr,
         rsqTas = rsq)

errDatWd <- errDat2 %>%
  filter(variable == 'pr') %>%
  rename(maePr = mae,
         maeCentPr = maeCent,
         maeBiasCorrPr = maeBiasCorr,
         relErrPr = relErr_mae,
         relErrCentPr = relErr_maeCent,
         relErrBiasCorrPr = relErr_maeBiasCorr,
         rsqPr = rsq) %>%
  select(-variable) %>%
  left_join(errDatWd, ., by = 'GCM') %>%
  mutate(relErr = relErrTas + relErrPr,
         relErrCent = relErrCentTas + relErrCentPr,
         relErrBiasCorr = relErrBiasCorrTas + relErrBiasCorrPr) %>%
  arrange(relErr) %>%
  select(GCM, relErr, relErrTas, relErrPr, maeTas, maePr, rsqTas, rsqPr,
         relErrCent, relErrCentTas, relErrCentPr,maeCentTas, maeCentPr,
         relErrBiasCorr, relErrBiasCorrTas, relErrBiasCorrPr, maeBiasCorrTas, maeBiasCorrPr)

xlsx::write.xlsx(errDatWd,
                 file.path(gitDir, "Output",
                           "gcmErrorSummary.xlsx"),
                 sheetName = "errorSumm",
                 row.names = FALSE)




#-------------------------------------------------------------------------------
#-----make plot comparing original and bias-corrected error stats
#-------------------------------------------------------------------------------
origOrd <- errDatWd$GCM[order(errDatWd$relErr)]

compPlot <- errDatWd %>%
  select(GCM, relErr, relErrBiasCorr) %>%
  tidyr::pivot_longer(.,
                      cols = starts_with("rel"),
                      names_to = 'Method',
                      values_to = 'error') %>%
  mutate(GCM = factor(GCM, levels = unique(origOrd)),
         Method = ifelse(Method == 'relErr', "Original", "Bias-Corrected")) %>%
  ggplot(aes(x = GCM, y = error, fill = Method)) +
  geom_bar(position="dodge", stat="identity") +
  ylab("Relative Error") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggsave(filename = file.path(gitDir, "Output",
                            "gcmErrorPlot.jpg"),
       plot = compPlot,
       width = 6.5,
       height = 4,
       units = 'in')













