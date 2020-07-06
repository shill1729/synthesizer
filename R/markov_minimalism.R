#' Create a Q-matrix for an Entry exit process
#'
#' @param num_voices number of voices (positive integer) in piece
#' @param bd_rates the modified birth-death process rates, see details
#'
#' @description {Populate a \eqn{Q}-matrix, otherwise called infinitesimal generator matrix of a modified
#' birth and death process that models the entry-exits of voices in a musical piece.}
#' @details {The argument \code{bd_rates} must be a named \code{list} with elements
#' \itemize{
#' \item \code{rest} the rate of transitions to rests
#' \item \code{down} the rate of transitions down a voice
#' \item \code{up} the rate of transitions up a voice
#' \item \code{rest_any} the rate of transitions from a rest to any number of voices
#' \item \code{full_any} the rate of transitions from full-band to any number of voices}}
#' @return matrix
#' @export entryExitQ
entryExitQ <- function(num_voices, bd_rates)
{
  up <- bd_rates$up
  down <- bd_rates$down
  rest <- bd_rates$rest
  rest_any <- bd_rates$rest_any
  full_any <- bd_rates$full_any

  Q_ee <- markovChains::birth_death_Q(up, down, num_voices+1)
  diag(Q_ee) <- c(rep(-(up+down+rest), num_voices+1))
  Q_ee[2, 2] <- -(up+down)
  Q_ee[1,] <- c(-(num_voices)*rest_any, rep(rest_any, num_voices))
  Q_ee[-c(1:2), 1] <- rest
  Q_ee[num_voices+1, ] <- c(rest, rep(full_any, num_voices-1), -((num_voices-1)*full_any+rest))
  if(all.equal(sum(apply(Q_ee, 1, sum)), 0))
  {
    return(Q_ee)
  } else
  {
    print(Q_ee)
    stop("Rows do not sum to 1")
  }

}


#' Simulate a sample path of an entry-exit process
#'
#' @param tt time-length to simulate for (for musical applications, in measures)
#' @param Q the generator matrix of the Markov process
#'
#' @description {Simulate a modified birth-death process, we call an entry-exit process, as our intention
#' is to model the arrival and exiting of voices in a musical piece. Based off a BD process, the number of voices
#' can go up or down, and jump to rests and back.}
#' @return data.frame
#' @export entryExitProcess
entryExitProcess <- function(tt, Q)
{
  # Q has dimension m+1 x m+1 where m is number of voices
  # (we include 0 voices as rest, so 0:m has m+1 elements)
  num_voices <- dim(Q)[1]-1
  V <- markovChains::rctmc(tt, Q, mu = c(0, 1, rep(0, num_voices-1)), states = c(0:num_voices))
  return(V)
}


#' Given a sample path of an entry-exit process, sample which voices play
#'
#' @param num_voices the number of voices, a positive integer, in the piece
#' @param v the sample-path of the entry-exit process, produced from \code{entryExitProcess}
#'
#' @description {Returns a list, where each element is a vector of indexes, the indexes representing
#' which voice is present in the \eqn{i}-th section of the piece.}
#' @return list
#' @export arrange_voices
arrange_voices <- function(num_voices, v)
{
  voices <- 1:num_voices
  voice_entries <- list()
  voice_entries[[1]] <- sample_one(voices)
  for(i in 2:length(v$state))
  {
    # If previous section was a rest, sample randomly
    if(v$state[i-1] == 0)
    {
      voice_entries[[i]] <- sample(voices, size = v$state[i])
    } else if(v$state[i] == 0)
    {
      voice_entries[[i]] <- c(0)
    } else if(v$state[i] == num_voices)
    {
      voice_entries[[i]] <- voices
    } else if(v$state[i]-v$state[i-1] == 1)
    {
      voice_entries[[i]] <- union(voice_entries[[i-1]], sample_one(setdiff(voices, voice_entries[[i-1]])))
    } else if(v$state[i]-v$state[i-1] == -1)
    {

      voice_entries[[i]] <- setdiff(voice_entries[[i-1]], sample_one(voice_entries[[i-1]]))
    } else if(v$state[i-1] == num_voices)
    {
      voice_entries[[i]] <- sample(voices, size = v$state[i])
    }
  }
  voice_entries <- lapply(voice_entries, sort)
  # Check to see if number of arranged voices per section match Markov sample path
  # Since the voices arrangements count 0 voices, this will have length 1, so we adjust
  z <- sum(v$state==0)+sum(v$state-unlist(lapply(voice_entries, length)))
  if(all.equal(z, 0))
  {
    return(voice_entries)
  } else{
    stop("Number of arranged voices do not equal the generated state from the entry-exit process")
  }

}


#' Create a list of Probability transition matrices for the DTMC of repeated melodies
#'
#' @param num_voices number of voices, a positive integer, in the musical piece
#' @param rep_probs the vector of probabilities representing chances to repeat current melody
#' @param state_lengths the size of the state space per voice, i.e. the number of allotted melodies
#'
#' @description {Populate the probability-transition matrices for the DTMCs representing which melody
#' repeats and randomly switches from. The diagonals are constant, and the non-diagonal entries have equal weight.}
#' @details {See the paper for more details, but essential staying in the same state has high chance, while jumping to
#' any other melody has a lower but equal chance, as any other distinct melody.}
#' @return list of matrices
#' @export melodyTransP
melodyTransP <- function(num_voices, rep_probs, state_lengths)
{
  melody_Plist <- list()
  for(i in 1:num_voices)
  {
    P <- matrix((1-rep_probs[i])/(state_lengths[i]-1), state_lengths[i], state_lengths[i])
    diag(P) <- rep_probs[i]
    melody_Plist[[i]] <- P
  }
  return(melody_Plist)
}


#' Generate a list of random harmonics
#'
#' @param synthInput I have no idea. TODO: remember what this is
#' @param lambdaRange range of mean-number of harmonics per tone
#' @param betaRange range of beta parameters for amplitude distributions
#'
#' @description {Generate, for each tone, a random list of harmonics, where the number of harmonics is
#' Poisson distributed, with a randomly chosen mean rate, and the amplitudes of the harmonics
#' are IID Beta-distributed RVs, with randomly chosen shape/scale parameters.}
#' @return list
#' @export random_harmonics
random_harmonics <- function(synthInput, lambdaRange, betaRange)
{
  m <- length(synthInput[,1])
  # Creating the overtones
  lambdas <- sample(lambdaRange, size = m, replace = TRUE)
  shape1s <- sort(stats::runif(m, min = 0, max = betaRange))
  shape2s <- sort(stats::runif(m, min = 0, max = betaRange))
  harmonicsList <- list()
  for(i in 1:m)
  {

    harmonicsList[[i]] <- c(1, stats::rbeta(max(1, stats::rpois(1, lambdas[i])), shape1 = shape1s[i], shape2 = shape2s[i]))
  }
  return(harmonicsList)
}


#' Synthesize the sounds of a single voice in a given section
#'
#' @param initial_voice the index of the initial voice
#' @param P the probability transition matrix determining the melodic transitions
#' @param W the duration of the entire section
#' @param n the number of steps to generate the \eqn{DTMC(P)} sample-path (make it large)
#' @param nb the number of beats per measure
#' @param bpm the beats per minute
#' @param harmonicParams the parameters for the harmonics generation (see details)
#' @param state_lengths the size of the state-spaces for each voice i.e. the number of melodies per voice
#' @param melody_states the list of state-spaces containing the pitch-tones in midi numbering
#' @param rhythm_states the list of state-spaces containing the rhythms of each melody in fractions of measures
#'
#' @description {Synthesizes a single voice over a given section with model input}
#' @details {The algorithm generates a large DTMC and then uses the sample path up until the section duration is reached,
#' this allows for state-spaces of melodies of varying lengths.
#'
#' The argument \code{harmonicParams} must be a named list containing:
#' \itemize{
#' \item \code{lambdaRange} the range of mean number of harmonics
#' \item \code{betaRange} the range of the beta distribution parameters}
#' }
#' @return list containing the wave vector and the time-error between input section length and generated
#' @export minimal_voice
minimal_voice <- function(initial_voice, P, W, n, nb, bpm, harmonicParams, state_lengths, melody_states, rhythm_states)
{
  voiceRep <- markovChains::rdtmc(n, P, c(1, rep(0, state_lengths[initial_voice]-1)))
  pitches <- unlist(melody_states[[initial_voice]][voiceRep$state])
  rhythms <- unlist(rhythm_states[[initial_voice]][voiceRep$state])
  cutOffIndex <- which.max(cumsum(rhythms) > W)
  synthInput <- cbind(pitches, rhythms)
  synthInput <- utils::head(synthInput, cutOffIndex)
  harmonicsList <- random_harmonics(synthInput, harmonicParams$lambdaRange, harmonicParams$betaRange)
  current_wave <- tones(midi = synthInput[, 1], synthInput[, 2], harmonics = harmonicsList, type = "melody", n = nb, bpm = bpm)

  return(list(wave = current_wave, time_error = sum(synthInput[,2])-W))
}

#' Convert from seconds to measures
#'
#' @param seconds time value in seconds
#' @param nb number of beats per measure
#' @param bpm number of beats per minute
#'
#' @description {Convert frmo seconds to measures using the basic formula relating seconds, number of
#' beats per measure and the BPM}
#' @return numeric
#' @export secondsToMeasure
secondsToMeasure <- function(seconds, nb, bpm)
{
  seconds*bpm/(nb*60)
}

#' Generate a minimalist piece via Markov processes
#'
#' @param sample_path the sample path of the entry-exit process
#' @param comp_param the composition parameters, see details
#' @param arrangement the arrangement input-data, see details
#' @param harmonicsP the harmonics input data, see details
#' @param state_space_info the state-space input data, see details
#'
#' @details {The argument \code{comp_param} must be a named list containing,
#' \itemize{
#' \item \code{num_voices} the number of voices in the piece, a positive integer
#' \item \code{nb} the number of beats per measure
#' \item \code{bpm} the beats per minute}
#'
#' The argument \code{arrangement} must be a named list with elements
#' \itemize{
#' \item \code{voice_entries}, the list of vectors of indexes for voices played in a given section
#' \item \code{melody_Plist} the list of probability transition matrices for the DTMC of melodies per voice
#' \item \code{waiting_times} the duration of each section, in measures.}
#'
#' The argument \code{state_space_info} must be a named list with elements
#' \itemize{
#' \item \code{state_lengths}, number of melodies per voice
#' \item \code{melody_states}, the melody-state space for each voice
#' \item \code{rhythm_states}, the rhythm-state space for each voice
#' \item \code{lengthOfMelodies}, the duration of each melody per voice
#' }}
#' @return list
#' @export minimal_piece
minimal_piece <- function(sample_path, comp_param, arrangement, harmonicsP, state_space_info)
{
  time_errors <- list()
  v <- sample_path
  num_voices <- comp_param$num_voices
  nb <- comp_param$nb
  bpm <- comp_param$bpm
  voice_entries <- arrangement$voice_entries
  melody_Plist <- arrangement$melody_Plist
  waiting_times <- arrangement$waiting_times
  state_lengths <- state_space_info$state_lengths
  melody_states <- state_space_info$melody_states
  rhythm_states <- state_space_info$rhythm_states
  lengthOfMelodies <- state_space_info$lengthOfMelodies
  # The first section always starts with one voice
  # Sample the first voice from set of voices
  initial_voice <- sample_one(1:num_voices)
  # Grab its transition matrix for successive melodies
  P <- melody_Plist[[initial_voice]]
  W <- waiting_times[1]
  n <- ceiling(sum(W/lengthOfMelodies[[initial_voice]]))
  mv <- minimal_voice(initial_voice, P, W, n, nb, bpm, harmonicsP, state_lengths, melody_states, rhythm_states)
  current_wave <- mv$wave
  time_errors[[1]] <- mv$time_error
  piece_sections <- list()
  piece_sections[[1]] <- current_wave
  # waves <- current_wave
  for(i in 2:length(waiting_times))
  {
    waves <- 0
    print(paste("Section ", i, "Length = ", waiting_times[i]))
    print(paste("State = ", v$state[i]))
    print(paste("Voices = ", voice_entries[[i]]))
    # Rests
    if(v$state[i] == 0)
    {

      current_wave <- tone(0, rhythm = waiting_times[i], n = nb, bpm = bpm)
      waves <- current_wave
    } else if(v$state[i] != 0)
    {
      # If it's a solo voice
      if(length(voice_entries[[i]]) == 1)
      {
        # Pick the right solo voice from the directions
        initial_voice <- voice_entries[[i]]
        # Grab its transition matrix for successive melodies
        P <- melody_Plist[[initial_voice]]
        W <- waiting_times[i]
        n <- ceiling(sum(W/lengthOfMelodies[[initial_voice]]))
        mv <- minimal_voice(initial_voice, P, W, n, nb, bpm, harmonicsP, state_lengths, melody_states, rhythm_states)
        current_wave <- mv$wave
        time_errors[[i]] <- mv$time_error
        waves <- current_wave
      } else
      {

        for(j in 1:length(voice_entries[[i]]))
        {

          # Pick the right solo voice from the directions
          initial_voice <- voice_entries[[i]][j]
          # Grab its transition matrix for successive melodies
          P <- melody_Plist[[initial_voice]]
          W <- waiting_times[i]
          n <- ceiling(sum(W/lengthOfMelodies[[initial_voice]]))
          mv <- minimal_voice(initial_voice, P, W, n, nb, bpm, harmonicsP, state_lengths, melody_states, rhythm_states)
          # test <- synthSound(pitches, rhythms, f = f)
          current_wave <- mv$wave
          time_errors[[i]] <- mv$time_error
          length_wave <- length(current_wave)
          lss <- length(waves)
          dk <- abs(lss-length_wave)
          if(length_wave > lss)
          {
            if(dk%%2 == 0)
            {
              waves <- c(rep(0, dk/2), waves, rep(0, dk/2))+ current_wave
            } else
            {
              waves <- c(rep(0, dk/2), waves, rep(0, dk/2+1))+ current_wave
            }

          } else
          {
            if(dk%%2 == 0)
            {
              waves <- waves + c(rep(0, dk/2),current_wave, rep(0, dk/2))
            } else
            {
              waves <- waves + c(rep(0, dk/2),current_wave, rep(0, dk/2+1))
            }
          }
        }
      }
    }

    piece_sections[[i]] <- waves
  }
  piece <- unlist(piece_sections)
  print("Time discrepancy")
  print(sum(unlist(time_errors)))
  return(piece)
}

#' Generate an approximate minimalist piece via Markov processes
#'
#' @param num_voices number of voices in the piece, a positive integer
#' @param piece_length the duration of the piece, in measures
#' @param bd_rates the transition rates, see \code{entryExitQ} documentation
#' @param rep_probs the vector of repeat probabilities for each voice
#' @param harmonicsP parameters for harmonics, see \code{random_harmonics} documentation
#' @param state_space_info complete state-space input, see \code{minimal_piece} documentation
#' @param nb the number of beats per measure
#' @param bpm the beats per minute
#'
#' @description {Generate a minimalist composition, approximately, via Markov processes simulations}
#' @return list
#' @export markov_minimalism
markov_minimalism <- function(num_voices, piece_length, bd_rates, rep_probs, harmonicsP, state_space_info, nb = 4, bpm = 120)
{
  # Main
  Q_ee <- entryExitQ(num_voices, bd_rates)
  v <- entryExitProcess(piece_length, Q_ee)
  # Enforce time, end on rest
  if(utils::tail(v$time, 1) > piece_length)
  {
    v$time[length(v$time)] <- piece_length
    v$state[length(v$state)] <- 0
  }

  waiting_times <- c(diff(v$time))
  voice_entries <- arrange_voices(num_voices, v)
  # List of transition matrices for the melodies
  melody_Plist <- melodyTransP(num_voices, rep_probs, state_space_info$state_lengths)
  arrangement <- list(voice_entries = voice_entries, waiting_times = waiting_times,
                      melody_Plist = melody_Plist)
  comp_param <- list(num_voices = num_voices, nb = nb, bpm = bpm)
  piece <- minimal_piece(v, comp_param, arrangement, harmonicsP, state_space_info)

  output <- list(Q = Q_ee, sample_path = v, waiting_times = waiting_times, voice_entries = voice_entries,
                 melody_Plist = melody_Plist, piece = piece)

  graphics::par(mfrow = c(2, 1))
  graphics::plot(v, type = "s")
  graphics::plot(stats::density(waiting_times, from = 0))
  print(Q_ee)
  print(utils::head(v))
  print(utils::head(voice_entries))
  print(melody_Plist)

  return(output)
}
