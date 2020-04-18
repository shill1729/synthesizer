#' Convert from midi numbers to frequencies in hertz
#'
#' @param midi the midi number in 0-127
#'
#' @description {Formula for converting between frequencies and midi}
#' @return numeric
#' @export midiToFreq
midiToFreq <- function(midi)
{
  440*2^((midi-69)/12)
}
