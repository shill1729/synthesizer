#' Return all consonances
#'
#' @description {Return all consonances}
#' @return vector
#' @export get_consonances
get_consonances <- function()
{
  return(union(get_imperfect_consonances(), get_perfect_consonances()))
}
