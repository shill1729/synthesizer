#' Adjust tonality of a given set of midi notes
#'
#' @param midi notes to convert from major to minor
#'
#' @description {Brings all major thirds, 6ths and 7ths to minors}
#' @return vector
#' @export majorToMinor
majorToMinor <- function(midi)
{
  m <- length(midi)
  if(m == 1)
  {
    if(midi%%12 == 4 || midi%%12 == 9 || midi%%12 == 11)
    {
      midi <- midi -1
    }
    return(midi)
  } else if(m > 1)
  {
    for(i in 1:m)
    {
      if(midi[i]%%12 == 4 || midi[i]%%12 == 9 || midi[i]%%12 == 11)
      {
        midi[i] <- midi[i] -1
      }
    }
    return(midi)
  }
}
