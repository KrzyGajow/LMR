
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LMR

<!-- badges: start -->

<!-- badges: end -->

Lossy mode resonance (LMR) is a physical phenomenon recently exploited for fiber optic sensing. LMR-based devices are widely used for detecting refractive index variation, humidity, pH (acidity or basicity of an aqueous solution), chemical or biological species, gases or even voltage. Two main types of geometries can be distinguished: prism-based and waveguide-based. In both cases, however, the manufacture as well as prototype development of the sensor requires a very precise and at the same time expensive technology. Therefore, reliable and fast modeling of these devices is desired to reduce costs of their investigation, designing and production. 

LMR is an R-toolbox for simulating LMR sensors of straight-core geometry (which is the most common type of waveguide-based one). The mathematical model is based mainly on geometrical optics. In addition, for determining the reflection coefficients from stratified media, the equations of classical electromagnetism and transfer matrix method were used. 

The approach may seem simple, but is perfectly sufficient to predict the most important parameters of the sensor. The results of calculations were experimentally verified in many works. The package allows one to load up to 8 layers in the sensing region and provides access to the material database. The researcher can use the dependencies contained therein describing the complex refractive index as a function of the wavelength of a given material or use his own data. Another important feature of the package is reverse engineering capability. The applied genetic algorithms and constrained optimization allows to design a sensor with a
given transmission spectral characteristics. 

Using the program is facilitated by an intuitive graphical interface. The final result is the transmission of the optical fiber in decibels versus the wavelength.

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

In order to create the LMR model, the user has to set in advance some [parameters](https://github.com/KrzyGajow/moviesLMR/blob/main/Parameters.gif) values. These values have to be specified
in [Parameters](https://github.com/KrzyGajow/moviesLMR/blob/main/SetParameters.gif) sub-panel. Next, the user has to specify real part of the refractive indices for core, cladding, sensing region and complex refractive indices for all coating layers. On the left-hand side of each sub-panel there is a drop-down list specifying how to define the refraction index. When selecting [Value](https://github.com/KrzyGajow/moviesLMR/blob/main/RefInd2.gif), one must manually enter one value that will be the same for each wavelength. By selecting [Table](https://github.com/KrzyGajow/moviesLMR/blob/main/RefInd1.gif) the software provides access to the material database. Third option, [Draw curve](https://github.com/KrzyGajow/moviesLMR/blob/main/RefInd3.gif) is most comprehensive. This option allows to define the functional relationship between the wavelength and the refractive index. The first step is to define the number of points that will be distributed on the plane. Based on this number, the fields defining the values on the horizontal axis (e.g. *x 1*) and the values on the vertical axis (e.g. *y 1*) will appear dynamically below. Changing the position of the points on
the plane (marked in blue) is possible at the same time by changing the coordinates on the left side of the graph or by grabbing a point and dragging it to the selected place. Another possibility to determine this relationship is to load an external file ([Upload Coordinates](https://github.com/KrzyGajow/moviesLMR/blob/main/RefInd4.gif)) containing a column with the wavelength and the [refractive index](https://github.com/KrzyGajow/moviesLMR/blob/main/RefInd5.gif). Once the parameters are properly setup the user should click at the [Start Modelling](https://github.com/KrzyGajow/moviesLMR/blob/main/Modelling.gif) button in the ”Model” sub-panel. If anything is setup incorrectly the software will display a caution. If all constraints are met, the software will start modeling. On the left side of the screen, there will be an additional button allowing to [save all results](https://github.com/KrzyGajow/moviesLMR/blob/main/SaveModelling.gif).

The first step of the Reverse LMR will be to either load an existing model using [Insert Existing Model](https://github.com/KrzyGajow/moviesLMR/blob/main/InsertOpt.gif) button or [load](https://github.com/KrzyGajow/moviesLMR/blob/main/UploadOpt.gif) it from an external file (”Model.RData”). The user can change the shape of the [LMR model curve](https://github.com/KrzyGajow/moviesLMR/blob/main/NewModelOpt.gif) by either dragging points on the graph or updating the values of the left-hand side interactive fields. Next required step is to choose parameters that have to be optimized. After selecting the desired parameters, additional fields will appear in the [Parameters Options](https://github.com/KrzyGajow/moviesLMR/blob/main/SetParamOpt.gif) sub-panel that require definition. Inserted
values in the fields *Min*, *Max*, *Precision* and *Step* come directly from the parameters of the loaded LMR model. After defining the search space, the user should choose the form of the
fitness function. If *weightedMSE* fitness function is chosen new sub-panel will be displayed [Weighted Fitness Function](https://github.com/KrzyGajow/moviesLMR/blob/main/FitFun.gif). After defining proper number of intervals the user can change start and end points. By default, the intervals are of equal width. All point within a given interval have assigned the same weight. The last step is pressing [Start Optimization](https://github.com/KrzyGajow/moviesLMR/blob/main/StartOpt.gif) button and waiting for the results to appear in the *Results* sub-panel. One can save all results by pressing [Save Optimization Results](https://github.com/KrzyGajow/moviesLMR/blob/main/SaveResOpt.gif) button.

## Package documentation

Package documentation can be found at [LMR](https://github.com/KrzyGajow/moviesLMR/blob/main/LMR_1.0.0.pdf).
