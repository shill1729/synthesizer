#' Decision rules for counterpoint
#'
#' @param cpt_targets collection of possible counterpoint targets
#' @param prev_note the previous counterpoint note
#' @param cf the current cantus firmus note
#' @param decision_rule the decision rule to use
#'
#' @return numeric
#' @export writing_rules
writing_rules <- function(cpt_targets, prev_note, cf, decision_rule = "naive")
{
  if(decision_rule == "naive")
  {
    target <- sample_one(cpt_targets)
    return(target)
  } else if(decision_rule == "test")
  {
    stop("TODO")


  }
}
