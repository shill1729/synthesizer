
# synthesizer

<!-- badges: start -->
<!-- badges: end -->

The synthesizer package provides functions for synthesizing melodies, chords, and stochastic compositions directly in R.

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
## Synthesizing basics

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

