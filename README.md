
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
f <- 48000

# Set player for MAC OSX
setWavPlayer('/usr/bin/afplay')
f <- 44100
```
