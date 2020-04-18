#' Produce a synthesized sound
#'
#' @param midi the midi note number for the tone to produce
#' @param rhythm the duration of the note in fractions of a measure
#' @param harmonics the amplitude of the harmonics
#' @param n number of beats in a measure
#' @param bpm the beats per minute
#' @param sampleRate the sample rate of the device typically windows is 48000 and mac osX is 44100
#'
#' @description {Wrapper to \code{seewave::synth()} function to produce a pure sine-wave tone. Additional parameters available for different wave forms, soon to be implemented.}
#' @details {The rhythms are converted to seconds via the number of beats per measure and bpm. Midi notes in 0-127 are converted to frequencies in Hertz.
#' The harmonics may either be a vector with leading component unity followed by amplitudes in the interval \eqn{(0,1)} or simply the number one.}
#' @return numeric vector
#' @export tone
tone <- function(midi, rhythm, harmonics = 1, n = 4, bpm = 120, sampleRate = 48000)
{
  if(length(harmonics)>1)
  {
    if(harmonics[1] != 1)
    {
      stop("Fundamental tone must have relative amplitude of 1")
    }
    for(i in 2:length(harmonics))
    {
      if(harmonics[i]>1)
      {
        stop("Overtones must have relative amplitudes less than 1")
      }
    }
  } else if(length(harmonics)==1)
  {
    if(harmonics != 1)
    {
      stop("Fundamental tone must have relative amplitude of 1")
    }
  }
  s <- seewave::synth(f = sampleRate, d = measureToSeconds(rhythm, n, bpm), cf = midiToFreq(midi), harmonics = harmonics)
  return(s)
}

