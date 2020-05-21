#' Get the mod 12 representation of a mode
#'
#' @param mode name mode
#'
#' @description {Return vector of the mode in mod 12, given the string name}
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
