#' Synthesize a melody or chord given a sequence of midi notes and durations in measures
#'
#' @param midi vector of midi notes in 0-127
#' @param rhythms vector of rhythms in fractions of a measure, can be a constant rhythm
#' @param harmonics vector of harmonics or list of vectors of harmonics (of possibly different lengths)
#' @param type melody or chord
#' @param n number of beats in a measure
#' @param bpm the beats per minute
#' @param sampleRate the sample rate of the device
#'
#' @description {Synthesizes many sounds and strings them together for a melody or combine them for a chord}
#' @details {Harmonics may either be a single vector or a list of vectors of different lengths. The first entry must be unity and the rest must be in the unit interval.}
#' @return vector
#' @export tones
tones <- function(midi, rhythms, harmonics = c(1, 0.5, 0.25), type = "melody", n = 4, bpm = 120, sampleRate = 48000)
{
  # Sound to return
  sound <- 0
  # Get length of melody in terms of number of notes
  m <- length(midi)

  # Check rhythms for constant rhythms or variable
  r <- 1
  if(length(rhythms) == 1)
  {
    r <- rep(rhythms, m)
  } else if(length(rhythms) > 1)
  {
    r <- rhythms
  }
  if(length(r) != m)
  {
    stop(paste("Number of midi notes", m, "does not match number of durations", length(r)))
  }
  # Synthesize each pitch into a list
  x <- list()
  for(i in 1:m)
  {
    # If each tone has the same harmonics
    if(!is.list(harmonics))
    {
      x[[i]] <- tone(midi = midi[i], rhythm = r[i], harmonics = harmonics, n = n, bpm = bpm, sampleRate = sampleRate)
    } else if(is.list(harmonics))
    {
      # Otherwise, each tone has a different harmonic series
      x[[i]] <- tone(midi = midi[i], rhythm = r[i], harmonics = harmonics[[i]], n = n, bpm = bpm, sampleRate = sampleRate)
    }
  }

  # Combine into one sample:
  if(type == "melody")
  {
    # Listen to a melody by binding by row into one long vector
    melody <- do.call(rbind, x)
    sound <- melody
  } else if(type == "chord" && length(rhythms) == 1)
  {
    # Create data.frame of sounds as column vectors and sum by row to get chords
    chord <- apply(as.data.frame(do.call(cbind, x)), 1, sum)
    sound <- chord
  } else if(type == "chord" && length(rhythms) != 1)
  {
    stop("Chords must have equal length durations in each chord-tone")
  }
  return(sound)
}
