#' First species counterpoint
#'
#' @param cantus_firmus the given cantus firmus a vector in midi 0:127
#' @param root the root note of the cantus firmus mode
#' @param mode the mode in the key
#' @param voice compose a counterpoint "above" or "below"
#'
#' @description {First species counterpoint in the strict style.}
#' @return list
#' @export first_species
first_species <- function(cantus_firmus, root, mode, voice = "above")
{
  do.call(what = paste("counterpoint_", voice, sep = ""), args = list(cantus_firmus, mode, root))
}
