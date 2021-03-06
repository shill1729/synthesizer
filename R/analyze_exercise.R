#' Analyze a counterpoint (above) exercise
#'
#' @param cpt the vector of counterpoint
#' @param cf the vector of the cantus firmus
#' @param voice above or below the cantus firmus
#'
#' @description {Creates a data.frame describing the characteristics of the exercise.}
#' @return data.frame
#' @export analyze_exercise
analyze_exercise <- function(cpt, cf, voice = "above")
{
  if(voice == "above")
  {
    h12 <- cpt-cf
  } else if(voice == "below")
  {
    h12 <- cf-cpt
  }

  cpt_step <- c(0, diff(cpt))
  cpt_sign <- c(0, sign(diff(cpt)))
  cf_step <- c(0, diff(cf))
  cf_sign <- c(0, sign(diff(cf)))
  piece <- data.frame(cf, cpt, h12,
             harmony = classify_harmonies(h12),
             harmony_interval = z12_to_intervals(h12),
             motion = classify_motion(cpt_step, cf_step),
             cpt_step, cf_step, cpt_sign, cf_sign)
  return(piece)
}
