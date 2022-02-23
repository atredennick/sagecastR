#' Plots and processes posterior distributions with prior overlaid.
#'
#' @param coreId A string for the core id.
#' @param coreName A string for the core name.
#' @param filePath A string specifying the file path with MCMC subdirs.
#' @param figPath A string specifying the file path with figure subdirs.
#' @param postPath A string specifying the file path with Posteriors subdirs.
#' @param params A character vector with parameter names to extract from
#'     the Stan MCMC results. Must match parameter names in the mcmc object.
#' @param paramLabs A factor vector specifying the order and names for each
#'     parameter to be plotted.
#' @return Nothing. Saves a list of ggplot object to specified directory.
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @export

PlotPosteriors <- function(coreId,
                           coreName,
                           filePath = "./Output/MCMC/",
                           figPath = "./Figures/",
                           postPath = "./Output/Posteriors/",
                           params = NULL,
                           paramLabs = NULL) {

  ## -- Define directories --
  filePath <- paste0(filePath, coreId, "/")
  figPath <- paste0(figPath, coreId, "/")
  postPath <- paste0(postPath, coreId, "/")
  suppressWarnings(dir.create(figPath))  # don't need warning if it exists
  suppressWarnings(dir.create(postPath))  # don't need warning if it exists


  ## -- Define null objects --
  if(is.null(params)) {
    params <- c("Beta[1]", "Beta[2]", "Beta[3]", "Beta[4]", "sigma_y")
  }
  if(is.null(paramLabs)) {
    paramLabs <- c("Intercept", "Dens. dep.", "Precip.", "Temp.", "Temporal var.", "Equilibrium cover")
    paramLabs <- factor(paramLabs, levels = paramLabs)
  }


  ## -- Read in mcmc results --
  mcmcFile <- list.files(filePath, pattern = "MCMC")
  mcmcMat <- readRDS(paste0(filePath, mcmcFile))
  cids <- which(colnames(mcmcMat) %in% params)
  posts <- mcmcMat[ , cids]

  ## -- Make plot of posterior distributions --
  postsDf <- as.data.frame(posts)
  colnames(postsDf) <- paramLabs[1:5]
  postsDf <- postsDf %>%
    mutate("Equilibrium cover" = exp(`Intercept` / (1 - `Dens. dep.`))) %>%
    mutate(iteration = 1:n()) %>%
    gather(key = "Parameter", value = "value", -iteration) %>%
    mutate(Parameter = factor(Parameter, levels = paramLabs))

  postsPlot <- ggplot(postsDf, aes(x = value, y=..density..)) +
    geom_histogram(color = "black", fill = "grey") +
    facet_wrap(~Parameter, nrow = 2, scales = "free") +
    ggthemes::theme_few(base_size = 12) +
    labs(x = "Parameter estimate", y = "Posterior density") +
    ggtitle(coreName)


  ## -- Save the output --
  saveRDS(postsPlot, file = paste0(figPath, "PosteriorPlots.RDS"))
}



# PlotPosteriors <- function(mcmc,
#                            paramName,
#                            priors,
#                            labels,
#                            plotTheme = theme_classic(),
#                            histbins = 20) {
#   outs <- ggmcmc::ggs(mcmc)
#   Beta <- outs[grep(paramName, outs$Parameter),]
#   betaMeans <- priors$mean
#   betaSds <- priors$sd
#   x <- seq(-10, 10, 0.001)
#
#   plotList <- list()
#   for(i in 1:length(unique(Beta$Parameter))) {
#     dens <- dnorm(x, betaMeans[i], betaSds[i])
#     prior <- data.frame(x = x,
#                         dens = dens,
#                         Parameter = unique(Beta$Parameter)[i])
#     tmp <- Beta %>%
#       filter(Parameter == unique(Beta$Parameter)[i])
#     maxy <- max(density(tmp$value)$y)
#     maxy <- maxy + maxy*0.1
#     p <- ggplot(tmp, aes(x = value, y = ..density..)) +
#       geom_histogram(fill = "grey65", color = "white", bins = histbins) +
#       geom_line(data = prior, aes(x = x, y = dens), col = "black") +
#       xlim(range(tmp$value)) +
#       ylim(c(0, maxy)) +
#       plotTheme +
#       labs(y = "Density", x = labels[i])
#     plotList[[i]] <- p
#   }
#   suppressWarnings(
#     out <- cowplot::plot_grid(
#       plotlist = plotList,
#       nrow = 1,
#       labels = "auto",
#       align = "h"
#     )
#   )
#
#   return(out)
# }
