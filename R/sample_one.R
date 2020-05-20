#' Sample one element from a vector
#'
#' @param from the vector to sample from
#'
#' @description {Wrapper to \code{sample} to handle singleton sets.}
#' @return numeric
#' @export sample_one
sample_one <- function(from)
{
  if(length(from) > 1)
  {
    return(sample(from, 1))
  } else if(length(from) == 1)
  {
    return(from)
  } else
  {
    stop("Empty set")
  }
}
