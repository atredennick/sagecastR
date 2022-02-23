# This script runs a cluster analysis on the fitted regression
# coefficients for precip and temperature to identify groups of
# similar core areas.
#
# Author: Andrew Tredennick (atredennick@west-inc.com)


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(cluster)
library(factoextra)
library(here)
library(broom)



# Load and format data ----------------------------------------------------

mcmcDir <- "Output/MCMC/"
coreAreas <- list.files(mcmcDir)
estimates <- tibble()
for(doArea in coreAreas) {
  tmpEstimates <- readRDS(paste0(mcmcDir, doArea, "/MCMC.RDS"))
  meanEstimates <- colMeans(tmpEstimates)
  tmpStorage <- tibble(
    CoreArea = doArea,
    PrecipEffect = meanEstimates["Beta[3]"],
    TempEffect = meanEstimates["Beta[4]"]
  )
  estimates <- bind_rows(estimates, tmpStorage)
}

# remove outliers
estimates <- estimates %>%
  filter(! CoreArea %in% c("Powder", "Seedskadee"))

# update format for cluster
estimates <- as.data.frame(estimates)
rownames(estimates) <- estimates$CoreArea
estimates$CoreArea <- NULL

# store raw estimates and scale
rawEstimates <- estimates
estimates <- scale(estimates)




# Run cluster routines ----------------------------------------------------

set.seed(8372165)
gapStat <- clusGap(estimates,
                   FUN = kmeans,
                   nstart = 25,
                   K.max = 10,
                   B = 50)
fviz_gap_stat(gapStat)

fviz_nbclust(estimates, kmeans, method = "silhouette")
fviz_nbclust(estimates, kmeans, method = "wss")
k2 <- kmeans(estimates, centers = 2, nstart = 25)
fviz_cluster(k2, data = rawEstimates, stand = FALSE)



# Cluster analysis of environmental characteristics -----------------------

soil <- read.csv(here("Data", "CoreArea_POLARIS_Summaries.csv")) %>%
  dplyr::select(-X) %>%
  mutate(simpName = gsub(" ", "",CoreArea)) %>%
  dplyr::select(-CoreArea) %>%
  pivot_wider(names_from = "variable", values_from = "value")

sens <- read.csv("./Output/environ_sensitivity_summary.csv") %>%
  filter(! simpName %in% c("Powder", "Seedskadee")) %>%
  left_join(soil, by = c("simpName"))


sens <- read.csv("./Output/environ_sensitivity_summary.csv") %>%
  left_join(soil, by = c("simpName")) %>%
  dplyr::select(-X, -ID, -sensBoth, -simpName, -NAME) %>%
  dplyr::select(sensPr, sensTmean, everything()) %>%
  gather(evar, eval, prMean:sip95_5_15) %>%
  gather(svar, sval, sensPr:sensTmean) %>%
  group_by(evar, svar) %>%
  nest()

cor_fun <- function(df) cor.test(df$eval, df$sval, method = "spearman") %>% tidy()
corNest <- mutate(sens, model = map(data, cor_fun))
cors <- dplyr::select(corNest, -data) %>% unnest(model)
corTab <- cors %>%
  ungroup() %>%
  dplyr::select(evar, svar, estimate, p.value) %>%
  mutate(rho = ifelse(p.value <= 0.05, estimate, NA))

outCors <- corTab %>%
  left_join(sens) %>%
  unnest(cols = c(data)) %>%
  mutate(group = paste(svar, evar, sep = " ~ "))

ggplot(outCors, aes(x = eval, y = sval)) +
  geom_point() +
  facet_wrap(~group, scales = "free")


envChars <- sens %>%
  dplyr::select(-X, -ID, -NAME, -starts_with("sens"), -prMean, -tMean, -elev)

# update format for cluster
envChars <- as.data.frame(envChars)
rownames(envChars) <- envChars$simpName
envChars$simpName <- NULL

# store raw estimates and scale
rawEnvChars <- envChars
envChars <- scale(envChars)
fviz_nbclust(envChars, kmeans, method = "silhouette")
fviz_nbclust(envChars, kmeans, method = "wss")
k2e <- kmeans(envChars, centers = 10, nstart = 25)
fviz_cluster(k2e, data = envChars, stand = FALSE)


# compare cluster results
enframe(k2$cluster) %>%
  rename("SensCluster" = value) %>%
  left_join(
    enframe(k2e$cluster) %>%
      rename("EnvCluster" = value)
  ) %>%
  mutate(Diff = abs(SensCluster - EnvCluster)) %>%
  dplyr::select(name, Diff) %>%
  pivot_wider(names_from = Diff, values_from = name)


autoplot(prcomp(envChars), loadings = TRUE, loadings.label = TRUE)

df <- sens %>%
  mutate(Cat = case_when(
    sign(sensPr) == 1 & sign(sensTmean) == 1 ~ "A",
    sign(sensPr) == 1 & sign(sensTmean) == -1 ~ "TTT",
    sign(sensPr) == -1 & sign(sensTmean) == 1 ~ "B",
    sign(sensPr) == -1 & sign(sensTmean) == -1 ~ "C"
  )) %>%
  filter(! Cat == "TTT") %>%
  mutate(Cat = as.factor(Cat))

df$Cat <- relevel(df$Cat, ref = 1)
library(nnet)
model <- multinom(Cat ~ elev + tMean + prMean, data = df)
summary(model)

# Calculate z-values
zvalues <- summary(model)$coefficients / summary(model)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2
exp(coef(model))

df %>%
  ggplot(aes(x = Cat, y = tMean)) +
  geom_boxplot()


df <- sens %>%
  mutate(Cat = case_when(
    sign(sensTmean) == 1 ~ 1,
    TRUE ~ 0
  ))
model <- glm(Cat ~ tMean, data = df, family = "binomial")
summary(model)
coef(model)
(exp(coef(model))-1)*100

zvalues <- summary(model)$coefficients[,1] / summary(model)$coefficients[, 2]
pnorm(abs(zvalues), lower.tail=FALSE)*2

df %>%
  ggplot(aes(x = as.factor(Cat), y = tMean)) +
  geom_boxplot()
