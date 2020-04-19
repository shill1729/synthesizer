
# synthesizer

<!-- badges: start -->
<!-- badges: end -->

The synthesizer package provides functions for synthesizing melodies, chords, and stochastic compositions directly in R. Note that this project is a *work in progress* and will likely be missing features for quite some time and errors will propagate in e.g. counterpoint functions that will simply break the program rather than handled in the global composer-function call.

## Table of contents
1. [Installation](#installation)

2. [Synthesizer basics](#synthesizer-basics)

3. [Stochastic fugue generation](#stochastic-fugue-generation)

## Installation

You can install the GitHub version wtih

``` r
devtools::install_github(shill1729/synthesizer)
```

## Setting up

How to set up for usage:

``` r
library(synthesizer)
# Set player for Windows OS
setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
# Sample rate for Windows
f <- 48000

# Set player for MAC OSX
setWavPlayer('/usr/bin/afplay')
# Sample rate for Mac
f <- 44100
```
## Synthesizer basics

### Playing a pure tone

How to synthesize and listen to a pure tone

```r
library(synthesizer)
library(tuneR)
library(seewave)
library(markovChains)
# Set player for Windows OS
setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
# Sample rate for Windows
f <- 48000
# Play middle C for one second
x <- tone(midi = 60, rhythm = 1)
listen(x, f = f)
```
### Synthesizing a melody
How to synthesize and listen to a melody with constant harmonics
```r
library(synthesizer)
library(tuneR)
library(seewave)
library(markovChains)
# Set player for Windows OS
setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
# Sample rate for Windows
f <- 48000
# Play a simple melody: C D E F E D C at middle C
midi <- c(0, 2, 4, 5, 4, 2, 0)+60
# In quarter notes
r <- 1/4
x <- tones(midi = midi, rhythms = r)
# Listen
listen(x, f = f)
```
### Synthesizing a chord
How to synthesize and listen to a chord with constant harmonics
```r
library(synthesizer)
library(tuneR)
library(seewave)
library(markovChains)
# Set player for Windows OS
setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
# Sample rate for Windows
f <- 48000
# Play a Cm11 cluster
midi <- c(0, 7, 10, 0+12, 2 +12, 3 +12, 5 +12, 10+12, 12+12, 14+12, 15+12)+60
# For one measure
r <- 1
x <- tones(midi = midi, rhythms = r, type = "chord")
# Listen
listen(x, f = f)
```

## Stochastic fugue generation

### Two voice fugue with first species counterpoint
May stop on a counterpoint error, rerun it until it succeeds (won't take long). Eventually, these errors will be caught and handled...
Also, this is mostly set up for subjects in a major key. The function will likely error out for minor keys, or just sound awful/unintended.
```r
library(synthesizer)
library(tuneR)
library(seewave)
library(markovChains)
# Set player for Windows OS
setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
# Sample rate for Windows
f <- 48000
# Number of statements
N <- 10
# Fugue subject
subject <- c(0, 4, 7, 9, 7, 5, 5, 4, 2, 0)
# Rhythm of fugue subject
rhythms <- 1/8
# Register of voices
registers <- c(48, 60)
# Probability transition matrices
P.voice <- rbind(c(0, 1),
                 c(1, 0))

I <- 0.1
R <- 0.1
RI <- 0.1
id <- 1-(RI+I+R)

P.transform <- rbind(c(id, I, R, RI),
                     c(id, I, R, RI),
                     c(id, I, R, RI),
                     c(id, I, R, RI))
keys <- c(0, 2, 4, 9, NA)
P.key <- rbind(c(0, 0, 0, 0, 1),
               c(0, 0, 0, 0, 1),
               c(0, 0, 0, 0, 1),
               c(0, 0, 0, 0, 1),
               c(0.7, 0.1, 0.1, 0.1, 0))
answers <- c(0, 2, 4, 5, 7)
P.answer <- rbind(c(0, 0.1, 0.1, 0.1, 0.7),
                  c(1, 0, 0, 0, 0),
                  c(1, 0, 0, 0, 0),
                  c(1, 0, 0, 0, 0),
                  c(1, 0, 0, 0, 0)
                  )
transition_matrices <- list(P.voice = P.voice, P.transform = P.transform, P.key = P.key, P.answer = P.answer)
# Generate stochastic fugue
s <- stochasticFugue(subject = subject, rhythms = rhythms, N = N, registers = registers, transition_matrices = transition_matrices, subjectKeys = keys, answerKeys = answers)
listen(wave = s$wave, f = f)
```


