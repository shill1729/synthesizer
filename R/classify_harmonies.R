#' Classify harmonies as either perfect and imperfect
#'
#' @param harmony the harmony or vector of harmonies to classify
#' @param exclude_fourth boolean whether to exclude fourths or not
#'
#' @description {Classify harmonies in z12 as either "perfect" or "imperfect".}
#' @return vector of strings
#' @export classify_harmonies
classify_harmonies <- function(harmony, exclude_fourth = TRUE)
{
  if(length(harmony) == 1)
  {
    if(harmony %in% get_dissonances(exclude_fourth))
    {
      return("dissonant")
    } else if(harmony %in% get_consonances())
    {
      if(harmony %in% get_perfect_consonances())
      {
        return("perfect")
      } else
      {
        return("imperfect")
      }
    }
  } else
  {
    classifs <- list()
    for(i in 1:length(harmony))
    {
      if(harmony[i] %in% get_dissonances(exclude_fourth))
      {
        classifs[[i]] <- "dissonant"
      } else if(harmony[i] %in% get_consonances())
      {
        if(harmony[i] %in% get_perfect_consonances())
        {
          classifs[[i]] <- "perfect"
        } else
        {
          classifs[[i]] <- "imperfect"
        }
      }
    }
    return(unlist(classifs))
  }

}
