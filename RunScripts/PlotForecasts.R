# Plot the forecast summaries

# Author: Andrew Tredennick



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(RColorBrewer)
library(ncdf4)

truncit <- function(x) {
  x[x > 100] <- 100
  return(x)
}

theme_set(theme_classic(base_size = 10) +
            theme(strip.background = element_blank()))

d <- here("Output", "ForecastSummaries")
allfiles <- list.files(d)
summaryFiles <- allfiles[grep("summaries", allfiles)]

forecasts <- tibble()
for(f in summaryFiles) {
  name <- unlist(strsplit(f, "-"))[1]
  scen <- unlist(strsplit(f, "-"))[5]
  scen <- unlist(strsplit(scen, "[.]"))[1]
  tmp <- read.csv(here("Output", "ForecastSummaries", f)) %>%
    # mutate_at(vars(-Year), truncit) %>%
    mutate(CoreArea = name,
           Scenario = scen)
  forecasts <- bind_rows(forecasts, tmp)
}

forecasts <- forecasts %>%
  mutate(CalYear = 2018+Year) %>%
  mutate() %>%
  filter(CalYear <= 2080)

timePoints <- forecasts %>%
  filter(CalYear %in% c(2020, 2040, 2060, 2080))

cols <- RColorBrewer::brewer.pal(7, "YlOrRd")[c(3,5,7)]

ggplot(forecasts, aes(x = CalYear, y = MeanCover,
                      color = Scenario, fill = Scenario)) +
  geom_ribbon(aes(ymin = LL95, ymax = UL95), alpha = 0.05, color = NA) +
  geom_ribbon(aes(ymin = LL68, ymax = UL68), alpha = 0.1, color = NA) +
  geom_line(size = 1, alpha = 0.3) +
  geom_errorbar(data = timePoints, aes(ymin = LL95, ymax = UL95),
                width = 0,
                position = position_dodge(width = 8)) +
  geom_errorbar(data = timePoints, aes(ymin = LL68, ymax = UL68),
                size = 2,
                width = 0,
                position = position_dodge(width = 8)) +
  scale_color_manual(values = cols, name = NULL) +
  scale_fill_manual(values = cols, name = NULL) +
  labs(x = "Year", y = "Sagebrush cover (%)") +
  guides(fill = FALSE) +
  facet_wrap(~CoreArea, scales = "free_y", ncol = 6) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text = element_text(size = 5),
        legend.position = "bottom") +
  labs(x = "Year", y = "Cover (%)") +
  scale_y_continuous(breaks= scales::pretty_breaks())


timePoints <- forecasts %>%
  filter(CalYear %in% c(2020, 2040, 2060, 2080))

ggplot(timePoints, aes(x = CalYear, color = Scenario)) +
  geom_errorbar(aes(ymin = LL95, ymax = UL95),
                width = 0,
                position = position_dodge(width = 8)) +
  geom_errorbar(aes(ymin = LL68, ymax = UL68),
                size = 2,
                width = 0,
                position = position_dodge(width = 8)) +
  facet_wrap(~CoreArea, scales = "free_y", ncol = 6) +
  scale_color_manual(values = cols, name = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text = element_text(size = 5),
        legend.position = "bottom") +
  labs(x = "Year", y = "Cover (%)") +
  scale_y_continuous(breaks= scales::pretty_breaks())



# Proportions above 10 ----------------------------------------------------

allfiles <- list.files(d)
summaryFiles <- allfiles[grep("summaries", allfiles, invert = TRUE)]

props <- tibble()
for(f in summaryFiles) {
  name <- unlist(strsplit(f, "-"))[1]
  scen <- unlist(strsplit(f, "-"))[5]
  scen <- unlist(strsplit(scen, "[.]"))[1]
  tmp <- read.csv(here("Output", "ForecastSummaries", f)) %>%
    mutate(CoreArea = name,
           Scenario = scen)
  props <- bind_rows(props, tmp)
}

props <- props %>%
  mutate(CalYear = 2018+Year) %>%
  group_by(CalYear, Scenario, CoreArea) %>%
  summarise(MeanProp = mean(Proportion),
            LL95 = quantile(Proportion, 0.025),
            UL95 = quantile(Proportion, 0.975),
            LL68 = quantile(Proportion, 0.16),
            UL68 = quantile(Proportion, 0.84),
            .groups = "drop")

ggplot(props, aes(x = CalYear, y = MeanProp,
                      color = Scenario, fill = Scenario)) +
  geom_hline(aes(yintercept = 0.25), color = "grey35", linetype = 2) +
  geom_ribbon(aes(ymin = LL95, ymax = UL95), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = LL68, ymax = UL68), alpha = 0.5, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~CoreArea, scales = "free_y") +
  scale_color_manual(values = cols, name = NULL) +
  scale_fill_manual(values = cols, name = NULL) +
  labs(x = "Year", y = "Proportion of cells > 10%") +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,1))




# Spatial plots -----------------------------------------------------------

ncdfPath <- "L:\\102 - USGS\\102 - 88 - SageCastWY\\Output\\Forecasts\\"
ncdfFile <- "NorthLaramie-dast-forecast-2046-2070-ssp585.nc"
ncdfName <- paste0(ncdfPath, ncdfFile)

ncin <- nc_open(ncdfName)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

# get temperature
tmp_array <- ncvar_get(ncin, "cover")
tmp_avg <- apply(tmp_array, 1:3, mean)
lattice::levelplot(tmp_avg[1:2,,])


dlname <- ncatt_get(ncin,"cover","long_name")
dunits <- ncatt_get(ncin,"cover","units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)
