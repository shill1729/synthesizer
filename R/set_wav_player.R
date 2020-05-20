#' Set the wave player and return the sample rate
#'
#' @param computer "windows" or "mac"
#'
#' @description {Return the sample rate and set player for each computer. A wrapper to \code{setWavPlayer}}
#' @return numeric
#' @export set_wave_player
set_wave_player <- function(computer = "windows")
{
  if(computer == "mac")
  {
    # Set player for MAC OSX
    tuneR::setWavPlayer('/usr/bin/afplay')
    sample_rate <- 44100
  } else if(computer == "windows")
  {
    # Set player for Windows OS
    tuneR::setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
    # Sample rate for Windows
    sample_rate <- 48000
  }
  return(sample_rate)
}
