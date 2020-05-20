#' Produce analyses of each counterpoint choice
#'
#' @param i the index of the current note
#' @param cpt_targets the set of vectors of possible counterpoint targets
#' @param cpt the vector of counterpoint notes
#' @param cf the vector of cantus firmus notes
#'
#' @description {Produce analyses of each counterpoint choice}
#' @return data.frame
#' @export analyze_current
analyze_current <- function(i, cpt_targets, cpt, cf)
{
  # Analyze each target with up to the current note being written
  analyses <- list()
  for(j in 1:length(cpt_targets))
  {
    analyses[[j]] <- analyze_exercise_above(c(cpt[1:(i-1)], cpt_targets[j]), cf[1:i])
  }
  return(analyses)
}
