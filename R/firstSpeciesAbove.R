#' Compose first species counterpoint above a given cantus firmus
#'
#' @param cantusFirmus cantus firmus melody in 0-12 to write a counterpoint above
#' @param key the key being composed in
#'
#' @description {Compose first species counterpoint above a given cantus firmus in mod 12.}
#' @return list
#' @export firstSpeciesAbove
firstSpeciesAbove <- function(cantusFirmus, key)
{
  # List of note names,
  noteNames <- c("C", "C#", "Eb", "E", "F", "F#", "G", "G#", "A", "Bb", "B")
  noteNames <- append(paste(noteNames, "0", sep = ""), paste(noteNames[1:5], "1", sep = ""))
  # Z12 and pitch list
  Z12 <- 0:11 # One octave chromatics
  pitchList <- 0:(length(noteNames)-1) # Up to the tenth

  # Allowed harmonic intervals between voices: no unisons, seconds, fourths, tritones, 7ths, octaves or 9ths
  H <- setdiff(pitchList, c(0, 1, 2, 5, 6, 10, 11, 12, 13, 14))
  # Allowed melodic steps: no tritones or 7ths
  M <- setdiff(Z12, c(6, 10, 11))

  # Length of cantus firmus in number of notes
  n <- length(cantusFirmus)
  # The first, penultimate and ultimate notes are pre-determined
  counterpoint <- c(12, rep(0, n-3), 11, 12)

  # Loop in the interior without the indexes {1, n-1, n}
  for(i in 2:(n-2))
  {
    # Get previous counterpoint note
    y <- counterpoint[i-1]
    # Get current cantus firmus note
    x <- cantusFirmus[i]

    # Get legal melodic steps from previous note
    My <- intersect(union(y+M, y-M), key)
    # Get legal harmonic intervals from current cantus firmus note
    Hx <- intersect(x+H, key)
    # Intersect both to get legal counterpoint options
    noteSet <- intersect(My, Hx)
    # First check for options
    if(is.null(noteSet))
    {
      stop("No legal counterpoint notes")
    }

    # Checking rules are followed:
    # 1. Mind the tenth
    if(length(which(noteSet - x < 16)) > 0)
    {
      # Keep only notes that are within a 10th of the current cantus firmus note
      noteSet <- noteSet[which(noteSet-x < 16)]
    }
    # 2. Prevent voice crossing
    if(length(which(noteSet-x>0))>0)
    {
      noteSet <- noteSet[which(noteSet-x >0)]
    }
    # 3. No parallel 5ths or direct motion
    if(y-cantusFirmus[i-1]==7)
    {
      noteSet <- noteSet[-which(noteSet-x==7)]
    }

    # Finally compose the counterpoint note by note
    if(length(noteSet)> 1)
    {
      counterpoint[i] <- sample(noteSet, size = 1)
    } else if(length(noteSet) == 1)
    {
      countpoint[i] <- noteSet
    } else if(length(noteSet) == 0)
    {
      stop("No legal counterpoint notes")
    }
  }
  piece <- rbind(counterpoint = counterpoint,
                 cantusFirmus = cantusFirmus,
                 h12 = counterpoint - cantusFirmus,
                 yStep = c(0, diff(counterpoint)),
                 ySign = c(0, sign(diff(counterpoint))),
                 xStep = c(0, diff(cantusFirmus)),
                 xSign = c(0, sign(diff(cantusFirmus)))
                 )
  piece <- data.frame(t(piece), motion = ifelse(piece["xSign", ]==piece["ySign", ], "Similar", ifelse(piece["xSign", ]==-piece["ySign", ], "Contrary", "Oblique")))
  piece <- as.data.frame(t(piece))
  names(piece) <- 1:n

  # Plotting:
  graphics::plot(cantusFirmus, ylim = c(min(cantusFirmus, counterpoint), max(cantusFirmus, counterpoint)), type = "s", main = "First species counterpoint")
  graphics::lines(counterpoint, type = "s")

  output <- list(piece = piece, counterpoint = counterpoint)
  return(output)
}
