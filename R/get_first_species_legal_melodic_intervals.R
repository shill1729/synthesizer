#' Return the legal melodic intervals (independent of key) for first species
#'
#' @description {Return the legal melodic intervals (independent of key) for first species}
#' @return vector
#' @export get_first_species_legal_melodic_intervals
get_first_species_legal_melodic_intervals <- function()
{
  # Unison, M/m 2nd, M/m 3rd, p4, p5, m6 ascending only, p8, the same but descending except for m6
  legal_melodic_intervals <- union(union(get_steps(), get_skips()), get_leaps())

  return(c(legal_melodic_intervals, -legal_melodic_intervals[-c(1, 8)]))
}
