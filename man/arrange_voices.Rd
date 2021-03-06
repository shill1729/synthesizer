% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markov_minimalism.R
\name{arrange_voices}
\alias{arrange_voices}
\title{Given a sample path of an entry-exit process, sample which voices play}
\usage{
arrange_voices(num_voices, v)
}
\arguments{
\item{num_voices}{the number of voices, a positive integer, in the piece}

\item{v}{the sample-path of the entry-exit process, produced from \code{entryExitProcess}}
}
\value{
list
}
\description{
{Returns a list, where each element is a vector of indexes, the indexes representing
which voice is present in the \eqn{i}-th section of the piece.}
}
