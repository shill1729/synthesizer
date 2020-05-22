# library(synthesizer)
# library(tuneR)
# library(seewave)
# sample_rate <- set_wave_player("mac")
# root <- 62
# mode <- "mixolydian"
# voice <- "below"
# bpm <- 120
# n <- 6
# v1 <- c(0, 2, 0, 5, 5, 5, 7, 9, 7, 12, 9, 7, 9, 2, 2, 0)
# v2 <- v1
# v2[length(v2)] <- 5
# v3 <- c(9, 5, 9, 9, 7, 2, 7, 7, 5, 2, 5, 5, 5, 2)
# v4 <- c(9, 5, 9, 9, 7, 2, 7, 7, 5, 2, 5, 5, 5, 2, 0)
# cf <- c(v1, v2, v3, v4)+root
# q <- 1/4
# et <- 1/8
# dot8 <- et+et/2
# st <- 1/16
# dot4 <- q+q/2
# dot2 <- 1/2+1/4
# r1 <- c(dot8, st, et, q, st, st,
#         dot8, st, et, dot4,
#         dot8, st, et, q, et,
#         dot2)
# r3 <- c(et, et, et, dot4, et, et, et, dot4, et, et, et, q, et, dot2)
# r4 <- c(et, et, et, dot4, et, et, et, dot4, et, et, et, q, et, dot4, dot4)
# rhythms <- c(r1, r1, r3, r4)
#
# cpt <- first_species(cf, root, mode, voice)
# cf_harmonics <- list()
# cpt_harmonics <- list()
# lbs <- sort(runif(length(cf)))
# ubs <- sort(runif(length(cpt$cpt)))
#
# for(i in 1:length(cf))
# {
#   cf_harmonics[[i]] <- c(1, rbeta(rpois(1, 20), lbs[i], ubs[i]))
# }
# for(i in 1:length(cpt$cpt))
# {
#   cpt_harmonics[[i]] <- c(1, rbeta(rpois(1, 20), lbs[i], ubs[i]))
# }
#
# cf.freq <- tones(cf, rhythms = rhythms, sampleRate = sample_rate, harmonics = cf_harmonics, n = n, bpm = bpm)
# cpt.freq <- tones(cpt$cpt, rhythms = rhythms, harmonics = cpt_harmonics, sampleRate = sample_rate, n = n, bpm = bpm)
# piece <- cf.freq+cpt.freq
# cpt <- first_species(cf-12, root-12, mode, "above")
# cf.freq <- tones(cf, rhythms = rhythms, sampleRate = sample_rate, harmonics = cf_harmonics, n = n, bpm = bpm)
# cpt.freq <- tones(cpt$cpt, rhythms = rhythms, harmonics = cpt_harmonics, sampleRate = sample_rate, n = n, bpm = bpm)
# piece1 <- cf.freq+cpt.freq
# piece <- c(piece, piece1)
# listen(piece, f = sample_rate)
# # savewav(wave = piece, f = sample_rate, filename = "skye_boat_song_counterpoint2.wav")
