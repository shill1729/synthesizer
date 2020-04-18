#' Convert from frequencies to midi numbers
#'
#' @param freq the frequency in hertz
#'
#' @description {Converts frequencies in hertz to midi numbers in 0-127}
#' @return numeric
#' @export freqToMidi
freqToMidi <- function(freq)
{
  69+12*log(freq/440, base = 2)
}
