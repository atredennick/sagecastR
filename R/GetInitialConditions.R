#' Extracts initial conditions for equilibrium simulations and projections.
#'
#' @param df A data frame of counts (cover) by Cell and Year.
#' @return A data frame.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @export

GetInitialConditions <- function(df) {
  lastObsCoverByCell <- df %>%
    filter(Year == max(Year))

  meanCoverByCell <- df %>%
    group_by(Cell) %>%
    summarise(MeanCover = round(mean(Count)),
              .groups = "drop")

  out <- list(
    meanCoverByCell = meanCoverByCell,
    lastObsCoverByCell = lastObsCoverByCell
  )

  return(out)
}

