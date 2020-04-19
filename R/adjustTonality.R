#' Adjust tonality between major and minor
#'
#' @param midi midi notes to convert tonalities of
#' @param what which tonality to go from and switch to the opposite
#'
#' @return numeric
#' @export adjustTonality
adjustTonality <- function(midi, what = "major")
{
  m <- ""
  if(what == "major")
  {
    m <- majorToMinor(midi)
  } else
  {
    m <- minorToMajor(midi)
  }
  return(m)
}
