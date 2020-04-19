#' Adjust tonality of a given set of midi notes
#'
#' @param midi notes to convert from minor to major
#'
#' @description {Brings all major thirds, 6ths and 7ths to minors}
#' @return vector
#' @export minorToMajor
minorToMajor <- function(midi)
{
  m <- length(midi)
  if(m == 1)
  {
    if(midi%%12 == 3 || midi%%12 == 8 || midi%%12 == 10)
    {
      midi <- midi +1
    }
    return(midi)
  } else if(m > 1)
  {
    for(i in 1:m)
    {
      if(midi[i]%%12 == 3 || midi[i]%%12 == 8 || midi[i]%%12 == 10)
      {
        midi[i] <- midi[i]+1
      }

    }
    return(midi)
  }
}

