#' Generate the next counterpoint note
#'
#' @param prev_note the previous counterpoint note
#' @param cf the current cantus firmus note
#' @param prev_harmony the previous harmony interval
#' @param prev_interval the previous melodic interval
#' @param root the root of the exercise
#' @param mode the mode of the exercise
#' @param decision_rule the decision rule to use
#'
#' @description {Generate the next counterpoint note}
#' @return numeric
#' @export gen_cpt
gen_cpt <- function(prev_note, cf, prev_harmony, prev_interval, root, mode, decision_rule)
{
  cpt_targets <- gen_cpt_choices(prev_note, prev_harmony, cf, root, mode)
  target <- writing_rules(cpt_targets, prev_note, cf, decision_rule)
  print(target)
  return(target)
}
