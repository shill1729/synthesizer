#' Return all dissonances
#'
#' @param exclude_fourth whether to exclude fourths as dissonant or not
#'
#' @description {Return all dissonances}
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
