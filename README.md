
# synthesizer

<!-- badges: start -->
<!-- badges: end -->

The goal of synthesizer is to ...

## Installation

You can install the GitHub version wtih

``` r
devtools::install_github(shill1729/synthesizer)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(synthesizer)

# Set player for Windows OS
setWavPlayer(shQuote("C:/Program Files/Windows Media Player/wmplayer.exe"))
f <- 48000

# Set player for MAC OSX
setWavPlayer('/usr/bin/afplay')
f <- 44100
```

