#' Classify motions as "contrary", "direct", or "oblique"
#'
#' @param cpt_step the current counterpoint step interval
#' @param cf_step the current cantus firmus step interval
#'
#' @description {Classify motions based off their step signs}
#' @return string
classify_motion1 <- function(cpt_step, cf_step)
{
  if(cpt_step == 0 && cf_step == 0)
  {
    return("oblique")
  } else if(sign(cpt_step) == sign(cf_step))
  {
    return("direct")
  } else if(cpt_step == 0 && cf_step != 0 || cpt_step != 0 && cf_step == 0)
  {
    return("oblique")
  } else if(sign(cpt_step) == -sign(cf_step))
  {
    return("contrary")
  }
}

#' Classify motions as "contrary", "direct", or "oblique"
#'
#' @param cpt_step the counterpoint step intervals
#' @param cf_step the cantus firmus step intervals
#'
#' @description {Classify motions based off their step signs}
#' @return string
#' @export classify_motion
classify_motion <- Vectorize(classify_motion1, vectorize.args = c("cpt_step", "cf_step"))
