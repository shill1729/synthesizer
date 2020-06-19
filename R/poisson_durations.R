#' Exponential waiting times
#'
#' @param t total duration to simulate for
#' @param lambda mean number of notes per unit
#'
#' @description {Durations of notes given by waiting times of a Poisson process}
#' @details {We assume from the start of the zero-th arrival, a note is played so the waiting
#' time to the first arrival is the duration of the first note. }
#' @return vector
#' @export poisson_durations
poisson_durations <- function(t, lambda)
{
  # This is perhaps silly (and at worst wasteful),
  # when all we need is the exponential waiting times, but...
  n <- stats::rpois(1, lambda = lambda*t)
  if(n == 0)
  {
    return(t)
  } else
  {
    u <- stats::runif(n, min = 0, max = t)
    u <- sort(u)
    w <- diff(c(0, u))
    return(w)
  }

}
