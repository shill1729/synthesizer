#' Convert from Z12 to music theory pitches
#'
#' @param z numeric interval in Z12
#'
#' @description {Convenient function for converting between Z12 representation and
#' music theory pitch symbols.}
#' @return string
#' @export z12_to_intervals
z12_to_intervals <- function(z)
{
  if(z < 0 || z > 16)
  {
    stop("This function does not handle negative harmonies or harmonies greater than a 10th")
  }
  intervals <- c("unison", "b2nd", "2nd", "b3rd", "3rd", "4th", "tritone", "5th", "b6th", "6th", "b7th", "7th", "octave", "b9th","9th", "b10th", "10th")

  return(intervals[z+1])
}
