#' Convert time from measures to seconds
#'
#' @param rhythm the rhythm to convert to seconds, in a fraction of a measure, i.e. 1, 1/2, 1/4, 1/8, 1/16,
#' @param n number of beats in a measure, the lower number in a time signature N/n
#' @param bpm the beats per minute
#'
#' @return numeric
#' @export measureToSeconds
measureToSeconds <- function(rhythm, n = 4, bpm = 120)
{
  rhythm*n*60/bpm
}
