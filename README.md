
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LMR

<!-- badges: start -->

<!-- badges: end -->

Theoretical modelling of lossy mode resonance (LMR).

## Installation

You can install the released version of LMR from
[GitHub](https://github.com/) with:

``` r
library(remotes)
install_github("KrzyGajow/LMR")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library("LMR")
waveMin <- 0.85
waveMax <- 1.65
waveStep <- 0.01
temperature <- 300
coreD <- 200
L <- 40000
angleMax <- 90
thickLayers <- c( 220 )

Cladding <- function( lambda, temperature ){

  a1 <- 0.6961663
  a2 <- 0.4079426
  a3 <- 0.8974794
  b1 <- 0.0684043
  b2 <- 0.1162414
  b3 <- 9.896161
  cladding <- sqrt(1.0+(a1*lambda^2/(lambda^2-b1^2))+(a2*lambda^2/(lambda^2-b2^2))+(a3*lambda^2/(lambda^2-b3^2)))

  thermal_drift <- 1+1.28e-5 * temperature
  thermal_drift <- 1
  cladding <- cladding * thermal_drift

  return( cladding )

}
Core <- function( lambda, temperature, cladding ){

  core <- 1.0036 * cladding

  return( core )

}
L1 <- function( lambda, temperature ){

  lambda <- lambda*10^(-6)
  epsinf <- 3.5
  tau <- 6.58e-15
  omegap <- 1.533e15
  vlight <- 3*10^8
  omega <- 2.0 * pi * vlight/ lambda
  eps <- epsinf - omegap^2 / ( omega^2 + 1i*(omega/tau) )
  res <- sqrt(eps)

  thermal_drift <- 1.0 - 1.49e-4 * temperature
  thermal_drift <- 1
  res <- res * thermal_drift

  return( res )

}

Layers <- data.frame( lambda = seq( waveMin, waveMax, by = waveStep ) )
Layers$Cladding <- Cladding( Layers$lambda, temperature )
Layers$Sensing <- 1
Layers$Core <- Core( Layers$lambda, temperature, Layers$Cladding )
Layers$Layers1 <- L1( Layers$lambda, temperature )

Layers$Sensing <- 1.436
Result436 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
Layers$Sensing <- 1.422
Result422 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
Layers$Sensing <- 1.321
Result321 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )

plot( x = Result436$Wavelength, y = Result436$Transmittance, ylim = range( c(Result436$Transmittance,
                                                                             Result422$Transmittance,
                                                                             Result321$Transmittance) ),
      type = "l", xlab = "Wavelength", ylab = "Transmittance", col = "blue" )
lines( x = Result422$Wavelength, y = Result422$Transmittance, col = "green" )
lines( x = Result321$Wavelength, y = Result321$Transmittance, col = "purple" )

# Run Shiny web application 
runShinyLMR()
```
## Tutorials

You can install the released version of LMR from
[FitFun](https://github.com/KrzyGajow/moviesLMR/blob/main/FitFun.gif) with:
