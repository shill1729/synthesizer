#' Get the midi mod 127 representation of a mode
#'
#' @param mode name mode
#'
#' @description {Get the mod 127 midi representation of a mode given the mode name}
#' @return vector
#' @export get_mode_127
get_mode_127 <- function(mode)
{
  mode127 <- list()
  for(i in 1:12)
  {
    mode127[[i]] <- get_mode_12(mode)+12*(i-1)
  }
  mode127 <- unlist(mode127)
  mode127 <- mode127[mode127 <= 127]
  mode127 <- unique(mode127)
  return(mode127)
}
