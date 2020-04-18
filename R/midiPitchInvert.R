#' Invert pitches over an axis in midi units
#'
#' @param midi midi pitches to invert
#' @param axis the axis to invert over
#'
#' @return numeric or vector
#' @export midiPitchInvert
midiPitchInvert <- function(midi, axis = 60)
{
  (2*axis-midi)%%127
}
