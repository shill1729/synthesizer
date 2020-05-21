#' Return legal harmonies in first species
#'
#' @description {Return legal harmonies in first species}
#' @return vector
#' @export get_first_species_legal_harmonies
get_first_species_legal_harmonies <- function()
{
  # No unisons as they are forced into the exterior
  legal_harmonic_intervals <- c(3, 4, 7, 8, 9, 12, 15, 16)
  return(legal_harmonic_intervals)
}
