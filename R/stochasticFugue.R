#' Generate a stochastic fugue based on Markov chains
#'
#' @param subject the fugue subject, a vector of midi numbers in 0-127
#' @param rhythms the rhythms, either constant or variable
#' @param N the number of statements to generate
#' @param registers the registers of each voice
#' @param transition_matrices a list of transition matrices, see details
#' @param subjectKeys the state space for keys of the subject
#' @param answerKeys the state space for the keys of the answers
#' @param keySig the key signature of the subject
#'
#' @description {Three Markov chains are generated to model an outline of a fugue, i.e. the path of the subject through different voices,
#' keys, and transformations. Then counterpoint is filled in on each statement for the other voices. Random harmonics are used.}
#' @details {transition_matrices must be a list of four probability transition matrices:
#' \itemize{
#' \item P.voice
#' \item P.transform
#' \item P.key
#' \item P.answer}}
#' @return vector
#' @export stochasticFugue
stochasticFugue <- function(subject, rhythms, N, registers, transition_matrices, subjectKeys, answerKeys, keySig = "major")
{
  # Extract probability transition matrices from list of transition matrices
  P.voice <- transition_matrices$P.voice
  P.transform <- transition_matrices$P.transform
  P.key <- transition_matrices$P.key
  P.answer <- transition_matrices$P.answer

  # Number of voices in fugue
  m <- dim(P.voice)[1]

  # Generate fugue outline chain
  fugue_chain <- fugueOutline(N, m, P.voice, P.transform, P.key, P.answer, keys = subjectKeys, answers = answerKeys)
  if(length(setdiff(c(1:m), fugue_chain$voice[1:m]) >= 1))
  {
    fugue_chain$voice[1:m] <- c(1, sample(x= 2:m, size = m-1))
  }

  # Boolean vector for whether voice has stated subject yet for book-keeping counterpoint entries
  hasStated <- rep(FALSE, m)

  # Lists for creating the fugue
  notations <- list()
  sounds <- list()
  # Loop over all voices
  for(i in 1:m)
  {
    print(paste("Working on voice", i))
    # For each voice, populate the voice's list of statements and counterpoints in order
    sample_path <- list()
    # Loop over all statements
    for(j in 1:N)
    {
      print(paste("On statement", j))
      # If the current voice is stating the fugue,
      if(fugue_chain$voice[j] == i)
      {
        print(paste("VOice", i, "is stating the fugue subject (again)"))
        hasStated[i] <- TRUE
        temp <- subject
        # If we are modulated to ii, iii, or vi, make sure to modulate major-minor notes
        if((fugue_chain$key[j] == 4 || fugue_chain$key[j] == 9 || fugue_chain$key[j] == 2) && keySig == "major")
        {
          for(l in length(temp))
          {
            if(temp[l] == 4 || temp[l] == 9)
            {
              temp[l] <- temp[l]-1
            }
          }
        }
        # Add the subject in the correct register with key-modulation
        sample_path[[j]] <- temp +registers[fugue_chain$voice[j]] +fugue_chain$key[j]
        # Check which transformation should be applied
        if(fugue_chain$transform[j] == "retrograde")
        {
          transformation <- rev
        } else if(fugue_chain$transform[j] == "inversion")
        {
          transformation <- function(x) midiPitchInvert(x, axis = registers[fugue_chain$voice[j]])
        } else if(fugue_chain$transform[j] == "retrograde-inversion")
        {
          transformation <- function(x) rev(midiPitchInvert(x, axis = registers[fugue_chain$voice[j]]))
        } else if(fugue_chain$transform[j] == "id")
        {
          transformation <- identity
        }
        # Transform the subject
        sample_path[[j]] <- transformation(sample_path[[j]])

      } else {
        # If the current voice not stating the subject, create counterpoint if it has already entered, otherwise do nothing
        if(hasStated[i] && i != fugue_chain$voice[j])
        {
          print(paste("voice", i, "has stated, creating counterpoint now"))
          if(fugue_chain$key[j] %in% c(2, 4, 9))
          {
            key <- getKey("minor")
          } else
          {
            key <- getKey("major")
          }
          if(i == 1)
          { # Bass voice should be -12
            sample_path[[j]] <- firstSpeciesAbove(temp, key)-12 +registers[fugue_chain$voice[i]]+fugue_chain$key[j]

          } else{
            sample_path[[j]] <- firstSpeciesAbove(temp, key) +registers[fugue_chain$voice[i]]+fugue_chain$key[j]
          }
        }
      }
    }
    # Put all statements/counterpoint into one long midi vector for each voice
    notations[[i]] <- unlist(sample_path)
  }
  # Find first statement
  first_statement <- fugue_chain$voice[1]
  for(i in 1:m)
  {
    print(paste("Synthesizing ", i, "-th voice", sep = ""))
    # Produce random harmonics
    K <- length(notations[[fugue_chain$voice[i]]])
    harmonics <- list
    for(j in 1:K)
    {
      harmonics[[j]] <- c(1, stats::rbeta(max(stats::rbeta(1, 20), 1), shape1 = 0.5, shape2 = 0.9))
    }
    sounds[[i]] <- tones(midi = notations[[fugue_chain$voice[i]]], rhythms = rhythms, harmonics = harmonics)
  }
  # Now for combining it all into one sound sample
  s <- sounds[[fugue_chain$voice[1]]]
  # Find lengths of sample for padding zeros
  pads <- sort(unlist(lapply(sounds, length)), decreasing = TRUE)
  for(i in 2:m)
  {
    print(paste("Joining voice", i))
    # Pad zeros
    num_zeros_pad <- pads[1]-pads[i]
    # Combine polyphonic melodies by adding
    print(paste("Voice", fugue_chain$voice[i], "being added"))
    print(paste("padding", num_zeros_pad, "zeros"))
    s <- s + c(rep(0, num_zeros_pad), sounds[[i]])
  }
  return(s)
}
