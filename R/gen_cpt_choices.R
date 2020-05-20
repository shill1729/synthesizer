#' Generate possible counterpoint choices
#'
#' @param prev_note previous counterpoint note
#' @param prev_harmony previous harmony interval
#' @param cf previous cantus firmus note
#' @param root the root of the exercise
#' @param mode the mode of the exercise
#'
#' @description {Generate the intersection of possible notes based off harmony formed with the current
#' cantus firmus and the melodic interval formed between the previous counterpoint note.}
#' @return vector
#' @export gen_cpt_choices
gen_cpt_choices <- function(prev_note, prev_harmony, cf, root, mode)
{
  # Legal melodic intervals; not diatonic
  legal_melodic_targets <- prev_note+get_first_species_legal_melodic_intervals()
  # Restricted to diatonic
  diatonic_melodic_targets <- intersect(legal_melodic_targets, get_mode_127(mode))
  # Repeat for harmonic intervals
  legal_harmonic_targets <- cf+get_first_species_legal_harmonies()
  diatonic_harmonic_targets <- intersect(legal_harmonic_targets, get_mode_127(mode))
  # Intersect the two
  cpt_targets <- intersect(diatonic_harmonic_targets, diatonic_melodic_targets)
  #  and remove those which exceed a 10th of the cantus firmus note
  cpt_targets <- cpt_targets[which(abs(cpt_targets-cf)<=16)]
  # Disallow voice crossing
  cpt_targets <- cpt_targets[which(cpt_targets>cf)]
  # and parallel fifths
  # if(prev_harmony == 7)
  # {
  #   cpt_targets <- cpt_targets[-which(cpt_targets-cf==7)]
  # }
  return(cpt_targets)
}
