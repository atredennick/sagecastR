#' Write the forecasts to compressed netCDF4 files.
#'
#' @return Nothing
#' @import ncdf4
#' @import EML
#' @import emld
#' @export


WriteNetCDF <- function(coreId,
                        scenario,
                        forecast_start_date,
                        lons,
                        lats,
                        ensembles,
                        outDir,
                        forecast_flag,
                        cover) {
  ids1 <- which(times >= 2018 & times <= 2045)
  ids2 <- which(times >= 2046 & times <= 2070)
  ids3 <- which(times >= 2071 & times <= 2100)
  ids <- list(ids1, ids2, ids3)
  batchname <- c("2018-2045", "2046-2070", "2071-2100")

  # metadata
  forecast_project_id <- "USGS-WEST::SageCast"
  forecast_site_id <- coreId
  forecast_model_id <- "SE-DAST"
  forecast_iteration_id <- format(Sys.time(), "%Y%m%d%H%M%S")
  forecast_scenario <- scenario

  for(i in 1:length(batchname)) {
    forecast_year_coverage <- batchname[i]

    # dimensions
    timedim <- ncdim_def("time",
                         units = paste("years since", year(forecast_start_date)),
                         vals = as.numeric(times[ids[[i]]] - year(forecast_start_date)),
                         longname = "time")

    londim <- ncdim_def("lon",
                        units = "meters",
                        vals = lons,
                        longname = "longitude")

    latdim <- ncdim_def("lat",
                        units = "meters",
                        vals = lats,
                        longname = "latitude")

    ensdim <- ncdim_def("ensemble",
                        units = "",
                        vals = ensembles,
                        longname = "ensemble member")

    ## quick check that units are valid
    udunits2::ud.is.parseable(timedim$units)
    udunits2::ud.is.parseable(londim$units)
    udunits2::ud.is.parseable(latdim$units)
    udunits2::ud.is.parseable(ensdim$units)

    # variable metadata
    def_list <- list()
    def_list[[1]] <- ncvar_def(name = "cover",
                               units = "percent",
                               dim = list(timedim, londim, latdim, ensdim),
                               missval = NA,
                               longname = "percent sagebrush cover",
                               prec = "double",
                               compression = 7)
    def_list[[2]] <- ncvar_def(name =  "forecast",
                               units = "integer",
                               dim = list(timedim),
                               longname = 'EFI standard forecast code. 0 = hindcast',
                               prec="integer")

    # create netCDF
    ncfname <- paste0(coreId, "-", "dast-forecast", "-", batchname[i], "-", scenario, ".nc")
    ncfpath <- paste0(outDir, ncfname)
    ncout <- nc_create(ncfpath, def_list, force_v4=T)

    ncvar_put(ncout, def_list[[1]], cover[ids[[i]], , , ])
    ncvar_put(ncout,def_list[[2]], forecast_flag[ids[[i]]])

    # Global attributes (metadata)
    ncatt_put(ncout,0,"forecast_project_id", as.character(forecast_project_id),
              prec =  "text")
    ncatt_put(ncout,0,"forecast_site_id", as.character(forecast_site_id),
              prec = "text")
    ncatt_put(ncout,0,"forecast_model_id", as.character(forecast_model_id),
              prec =  "text")
    ncatt_put(ncout,0,"forecast_iteration_id", as.character(forecast_iteration_id),
              prec =  "text")
    ncatt_put(ncout,0,"forecast_climate_scenario", as.character(forecast_scenario),
              prec = "text")
    ncatt_put(ncout,0,"forecast_year_coverage", as.character(forecast_year_coverage),
              prec = "text")
    nc_close(ncout)   # make sure to close the file
  }  # end ncdf loop
}
