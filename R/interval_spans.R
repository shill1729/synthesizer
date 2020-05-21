#' Get the span of each interval among the mod 127 numbers of midi encoding
#'
#' @description {Get the span of each interval among the mod 127 numbers of midi encoding}
#' @return list of vectors
#' @export interval_spans
interval_spans <- function()
{
  midi_range <- 0:127
  interval_equivalences <- list()
  for(i in 1:12)
  {
    interval_equivalences[[i]] <- midi_range[midi_range%%12 == i - 1]
  }
  names(interval_equivalences) <- c("tonic",
                                    "minor_second",
                                    "major_second",
                                    "minor_third",
                                    "major_third",
                                    "perfect_fourth",
                                    "tritone",
                                    "perfect_fifth",
                                    "minor_sixth",
                                    "major_sixth",
                                    "minor_seventh",
                                    "major_seventh"
  )
  return(interval_equivalences)
}
