#' Initialize a counterpoint above the cantus firmus
#'
#' @param cf the cantus firmus vector
#' @param mode the mode to compose in
#' @param root the root note in mod 127 the exercise is composed in
#'
#' @description {Initialize a counterpoint above the cantus firmus}
#' @return vector
#' @export initialize_above
initialize_above <- function(cf, mode, root)
{
  cpt <- matrix(0, nrow = n)
  # Get length of cantus firmus in number of notes
  n <- length(cf)
  # Initialize counterpoint
  cpt <- matrix(0, nrow = n)
  # First note is either unison, p5, or p8
  cpt[1] <- root+sample(c(0, 7, 12), size = 1)
  # Last note is p8
  cpt[n] <- root+12
  if (mode %in% c("phrygian", "locrian")) {
    penultimate_step <- 2
  } else if (mode %in% c("ionian", "dorian", "lydian", "mixolydian",
                         "aeolian")) {
    penultimate_step <- 1
  }
  cpt[n - 1] <- cpt[n] - penultimate_step
  return(cpt)
}
