#' Markov chain outline model of a fugue
#'
#' @param N number of statements of the subject
#' @param m number of voices in the fugue
#' @param P.voice the probability transition matrix of the voice-chain: which voice states the subject at what time
#' @param P.transform the probability transition matrix of the transformations of the subject
#' @param P.key the probability transition matrix of the subject-keys
#' @param P.answer the probability transition matrix of the answer-keys
#' @param keys the subject-keys state space
#' @param answers the answer-keys state-space
#'
#' @description {Model a fugue outline as a random walk of a subject through different voices, keys, and transformations via 3 Markov chains}
#' @return data.frame containing time, voice, key, transform
#' @export fugueOutline
fugueOutline <- function(N, m, P.voice, P.transform, P.key, P.answer, keys = c(0, 2, 4, 9), answers = c(0, 4, 5, 7))
{
  # Simulate the discrete time chains representing the fugue outline
  voice_chain <- markovChains::rdtmc(n = N, P = P.voice, mu = c(1, rep(0, m-1)), states = c(1:m))
  transform_chain <- markovChains::rdtmc(n = N, P = P.transform, mu = c(1, rep(0, 3)), states = c("id", "retrograde", "inversion", "retrograde-inversion"))
  # Generate subject-key and answer-key chains separately and glue them together
  key_chain <- markovChains::rdtmc(n = N, P = P.key, mu = c(1, rep(0, length(keys)-1)), states = keys)
  answer_chain <- markovChains::rdtmc(n = N, P = P.answer, mu = c(1, rep(0, length(answers)-1)), states = answers)
  key_chain$state[is.na(key_chain$state)] <- answer_chain$state[answer_chain$state > 0]
  # Put all into one data.frame
  fugue_chain <- data.frame(time = voice_chain$time,
                            voice = voice_chain$state,
                            key = key_chain$state,
                            transform = transform_chain$state)
  return(fugue_chain)
}
