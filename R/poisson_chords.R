#' Generate a chord progression driven by a Poisson process and Markov chain
#'
#' @param t duration of total progression in measures
#' @param lambda mean number of chord changes per measure
#' @param chord_states state space for chords
#' @param P probability transition matrix for chord changes
#' @param initial_chord initial chord distribution
#' @param harmonics harmonics for the chords
#' @param n number of beats per measure
#' @param bpm the beats per minute
#' @param sampleRate sample rate
#'
#' @description {Simulate chord changes where the number of chord changes follows a Poisson process,
#' and the transitions of chords follow a given Markov jump chain.}
#' @return vector
#' @export poisson_chords
poisson_chords <- function(t, lambda, chord_states, P, initial_chord, harmonics = c(1, 0.5, 0.2, 0.1), n = 4, bpm = 120, sampleRate = 48000)
{

  # In measures and number of notes per measure
  durations <- poisson_durations(t, lambda)
  print(paste("Piece length in seconds =", sum(durations)*n/bpm))
  N <- length(durations)
  chord_chain <- markovChains::rdtmc(N, P, mu = initial_chord)
  chords <- chord_states[chord_chain$state]
  cprog <- chord_prog(chords, durations, harmonics, n, bpm, sampleRate)
  plot(cumsum(durations), 1:N, type = "s", xlab = "measure", ylab = "# chord changes")
  return(cprog)
}
