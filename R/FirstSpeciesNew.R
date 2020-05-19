
#' Get the mod 12 representation of a mode
#'
#' @param mode name mode
#'
#' @return vector
#' @export get_mode_12
get_mode_12 <- function(mode)
{
  out <- switch (mode,
                 ionian = c(0, 2, 4, 5, 7, 9, 11, 12),
                 dorian = c(0, 2, 3, 5, 7, 9, 10, 12),
                 phrygian = c(0, 1, 3, 5, 7, 8, 10, 12),
                 lydian = c(0, 2, 4, 6, 7, 9, 11, 12),
                 mixolydian = c(0, 2, 4, 5, 7, 9, 10, 12),
                 aeolian = c(0, 2, 3, 5, 7, 8, 10, 12),
                 locrian = c(0, 1, 3, 5, 6, 9, 10, 12)
  )
  return(out)
}

#' Get the midi mod 127 representation of a mode
#'
#' @param mode name mode
#'
#' @return vector
#' @export get_mode_127
get_mode_127 <- function(mode)
{
  mode127 <- list()
  for(i in 1:12)
  {
    mode127[[i]] <- get_mode_12(mode)+12*(i-1)
  }
  mode127 <- unlist(mode127)
  mode127 <- mode127[mode127 <= 127]
  mode127 <- unique(mode127)
  return(mode127)
}


#' Get the span of each interval among the mod 127 numbers of midi encoding
#'
#' @return list of vectors
#' @export interval_spans
interval_spans <- function()
{
  midi_range <- 0:127
  interval_equivalences <- list()
  for(i in 1:12)
  {
    interval_equivalences[[i]] <- midi_range[midi_range%%12 == i - 1]
  }
  names(interval_equivalences) <- c("tonic",
                                    "minor_second",
                                    "major_second",
                                    "minor_third",
                                    "major_third",
                                    "perfect_fourth",
                                    "tritone",
                                    "perfect_fifth",
                                    "minor_sixth",
                                    "major_sixth",
                                    "minor_seventh",
                                    "major_seventh"
  )
  return(interval_equivalences)
}

#' Return the legal melodic intervals (independent of key) for first species
#'
#' @return vector
#' @export get_first_species_legal_melodic_intervals
get_first_species_legal_melodic_intervals <- function()
{
  # Unison, M/m 2nd, M/m 3rd, p4, p5, m6 ascending only, p8, the same but descending except for m6
  legal_melodic_intervals <- union(union(get_steps(), get_skips()), get_leaps())

  return(c(legal_melodic_intervals, -legal_melodic_intervals[-c(1, 8)]))
}

#' Return the legal melodic steps (independent of key) for first species
#'
#' @return vector
#' @export get_steps
get_steps <- function()
{
  return(c(0, 1, 2))
}

#' Return the legal melodic skips (independent of key) for first species
#'
#' @return vector
#' @export get_skips
get_skips <- function()
{
  return(c(3, 4))
}


#' Return the legal melodic leaps (independent of key) for first species
#'
#' @return vector
#' @export get_leaps
get_leaps <- function()
{
  return(c(5, 7, 8, 12))
}

#' Return legal harmonies in first species
#'
#' @return vector
#' @export get_first_species_legal_harmonies
get_first_species_legal_harmonies <- function()
{
  # No unisons as they are forced into the exterior
  legal_harmonic_intervals <- c(3, 4, 7, 8, 9, 12, 15, 16)
  return(legal_harmonic_intervals)
}

#' Return imperfect consonances
#'
#' @return vector
#' @export get_imperfect_consonances
get_imperfect_consonances <- function()
{
  return(c(3, 4, 8, 9, 15, 16))
}

#' Return perfect consonances
#'
#' @return vector
#' @export get_perfect_consonances
get_perfect_consonances <- function()
{
  return(c(0, 5, 7, 12))
}

#' Return all consonances
#'
#' @return vector
#' @export get_consonances
get_consonances <- function()
{
  return(union(get_imperfect_consonances(), get_perfect_consonances()))
}

#' Return all dissonances
#'
#' @param exclude_fourth whether to exclude fourths as dissonant or not
#'
#' @return vector
#' @export get_dissonances
get_dissonances <- function(exclude_fourth = FALSE)
{
  if(exclude_fourth)
  {
    return(c(1, 2, 6, 10, 11, 13, 14))
  } else
  {
    return(c(1, 2, 5, 6, 10, 11, 13, 14))
  }

}


#' Generate the next note in a cantus firmus
#'
#' @param prev_note the previous note in the cf
#' @param root the root of the cf (absolute not mod 12)
#' @param mode the mode name
#'
#' @return numeric
#' @export gen_legal_melodic_sequence
gen_legal_melodic_sequence <- function(prev_note, root, mode)
{
  stop("TODO")
}


#' Generate a random cantus firmus given a root and mode
#'
#' @param n number of notes in cantus firmus
#' @param root root number of the cantus firmus (in aboslute midi numbers)
#' @param mode mode name
#'
#' @return vector
#' @export gen_cantus_firmus
gen_cantus_firmus <- function(n = 7, root = 60, mode = "ionian")
{
  cf <- matrix(0, nrow = n)
  cf[1] <- root
  cf[n] <- root
  # First species penultimate note is always the second
  if(mode %in% c("phrygian", "locrian"))
  {
    penultimate_step <- 1
  } else if (mode %in% c("ionian", "dorian", "lydian", "mixolydian", "aeolian"))
  {
    penultimate_step <- 2
  }
  cf[n-1] <- root+penultimate_step
  for(i in 2:(n-2))
  {
    cf[i] <- gen_legal_melodic_sequence(cf[i-1], root, mode)
  }
  return(as.data.frame(t(cf)))
}

