#' Convenient function for getting keys
#'
#' @param what what key to return major or minor
#'
#' @return vector
#' @export getKey
getKey <- function(what = "major")
{
  if(what == "major")
  {
    return(c(0, 2, 4, 5, 7, 9, 11, 12, 14, 16))
  } else if(what == "minor")
  {
    return(0, 2, 3, 5, 7, 8, 9, 10, 11, 12, 14, 15)
  }
}
