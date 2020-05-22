#' Compose cantus firmus above
#'
#' @param cf cantus firmus
#' @param mode mode of the melody
#' @param root root note in midi 127
#'
#' @return list
#' @export counterpoint_below
counterpoint_below <- function(cf, mode = "dorian", root = 60)
{
  n <- length(cf)
  # Initialize the first, penultimate and ultimate notes of the counterpoint
  cpt <- initialize_below(cf, mode, root)
  bad_paths <- list()

  for(i in 2:3)
  {
    # Depends on previous counterpoint note, previous harmony, current CF note,...
    cpt_targets <- gen_cpt_choices(cpt[i-1], cf[i-1]-cpt[i-1], cf[i], root, mode, "below")
    analyses <- analyze_current(i, cpt_targets, cpt, cf, "below")
    cpt_targets <- remove_bad_direct_motion(cpt_targets, analyses)

    cpt[i] <- sample_one(cpt_targets)
  }
  i <- 4
  while(i <= n-2)
  {
    # Depends on previous counterpoint note, previous harmony, current CF note,...
    cpt_targets <- gen_cpt_choices(cpt[i-1], cf[i-1]-cpt[i-1], cf[i], root, mode, "below")
    analyses <- analyze_current(i, cpt_targets, cpt, cf, "below")
    cpt_targets <- remove_bad_direct_motion(cpt_targets, analyses)
    current_piece <- analyze_exercise(cpt[1:(i-1)], cf[1:(i-1)], "below")
    if(check_for_non_steps(i, current_piece))
    {
      print("Skipped Or Leaped previous index")
      print("Last counterpoint note")
      print(cpt[i-1])
      print("Counterpoint targets before removing non-steps")
      print(cpt_targets)
      # check if last counterpoint is a valid harmony
      if(cpt[i-1] %in% cpt_targets)
      {
        analyses <- analyze_current(i, cpt_targets, cpt, cf, "below")
        cpt_targets <- c(remove_non_steps(cpt_targets, analyses), cpt[i-1])
      } else
      {
        analyses <- analyze_current(i, cpt_targets, cpt, cf, "below")
        test <- remove_non_steps(cpt_targets, analyses)
        if(length(test) != 0)
        {
          print("Can compensate skip with a step")
          cpt_targets <- test
          print("After:")
          print(cpt_targets)
        } else{
          warning(paste("Couldn't compensate a skip/leap on index", i))
        }
      }
      print("Now trying to go in opposite direction")
      # Now try to find opposite direction
      test <- remove_same_steps(i, cpt_targets, analyses, current_piece)
      if(length(test) !=0)
      {
        cpt_targets <- test
      } else
      {
        warning(paste("Couldn't step in opposite direction of a skip/leap on index", i))
      }
    }
    cpt[i] <- sample_one(cpt_targets)
    i <- i+1
  }
  finished_exercise <- analyze_exercise(cpt, cf, "below")
  print(finished_exercise)
  return(list(piece = finished_exercise, cpt = cpt))
}
