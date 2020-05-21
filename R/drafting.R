# root <- 60
# mode <- "dorian"
# cf <- c(0, 3, 2, 0, 5, 3, 7, 5, 3, 2, 0)+root
# n <- length(n)
# # Initialize
# cpt <- initialize_above(cf, mode, root)
# i <- 2
# # Generate choices within a tenth of the cantus firmus
# cpt_targets <- gen_cpt_choices(cpt[i-1], cpt[i-1]-cf[i-1], cf[i], root, mode)
# analyses <- analyze_current(i, cpt_targets, cpt, cf)
# # Get rid of any targets that produce direct-perfect motion and harmony
# dp_targets <- lapply(analyses, function(x) x$cpt[intersect(which(x$motion == "direct"), which(x$harmony == "perfect"))])
# dp_targets <- unlist(dp_targets)
# cpt_targets <- setdiff(cpt_targets, dp_targets)
# analyses <- analyze_current(i, cpt_targets, cpt, cf)
# print(analyses)
# print(cpt_targets)
# # On the first free cpt we can just pick randomly from no direct motions. Then we will keep track of skips/steps
# if(i == 2)
# {
#   cpt[i] <- target <- sample_one(cpt_targets)
# } else if(i > 2)
# {
#   print("TODO")
# }
