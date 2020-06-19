#' Generate a chord progression given a sequence of chords and durations
#'
#' @param chords list of chords, each element a MIDI vector representation of a chord
#' @param durations vector of durations
#' @param harmonics vector or (not implemented yet) list of harmonics for each chord
#' @param n number of beats in a measure
#' @param bpm beats per minute
#' @param sampleRate sample rate
#'
#' @return vector
#' @export chord_prog
chord_prog <- function(chords, durations, harmonics = c(1, 0, 0.05), n = 4, bpm = 120, sampleRate = 48000)
{
  # Sound to return
  sound <- 0
  x <- list()
  N <- length(chords)
  for(i in 1:N)
  {
    x[[i]] <- tones(chords[[i]], durations[i], harmonics, type = "chord", n, bpm, sampleRate)
  }
  # Listen to a melody by binding by row into one long vector
  # chordprg <- do.call(rbind, x)
  chordprg <- unlist(x)
  sound <- chordprg
  return(sound)
}
