#' Remove perfect harmonies approached by direct motion
#'
#' @param cpt_targets set of counterpoint targets
#' @param analyses analyses of them
#'
#' @description {Remove perfect harmonies approached by direct motion}
#' @return vector
#' @export remove_bad_direct_motion
remove_bad_direct_motion <- function(cpt_targets, analyses)
{
  # Get rid of any targets that produce direct-perfect motion and harmony
  dp_targets <- lapply(analyses, function(x) x$cpt[intersect(which(x$motion == "direct"), which(x$harmony == "perfect"))])
  dp_targets <- unlist(dp_targets)
  x <- setdiff(cpt_targets, dp_targets)
  return(x)
}

#' Remove non-steps
#'
#' @param cpt_targets set of counterpoint targets
#' @param analyses analyses of them
#'
#' @description {Remove non steps}
#' @return vector
#' @export remove_non_steps
remove_non_steps <- function(cpt_targets, analyses)
{
  possible_steps <- lapply(analyses, function(x) x$cpt[which(!abs(x$cpt_step[-1]) %in% get_steps())+1])
  possible_steps <- unlist(possible_steps)
  x <- setdiff(cpt_targets, possible_steps)
  return(x)
}

#' Remove same-steps
#'
#' @param i current index of counterpoint we want to write
#' @param cpt_targets set of counterpoint targets
#' @param analyses analyses of them
#' @param current_piece the latest composed piece
#'
#' @description {Remove steps in same direction as previous leap}
#' @return vector
#' @export remove_same_steps
remove_same_steps <- function(i, cpt_targets, analyses, current_piece)
{
  possible_steps_in_opp <- lapply(analyses, function(x) x$cpt[which(!x$cpt_sign[-1] == -current_piece$cpt_sign[i-1])+1])
  possible_steps_in_opp <- unlist(possible_steps_in_opp)
  x <- setdiff(cpt_targets, possible_steps_in_opp)
  return(x)
}


