#' consoleLMR
#' @title consoleLMR
#'
#' @description Theoretical modelling of lossy mode resonance (LMR)
#'
#' @param waveMin Wavelength start [um].
#' @param waveMax Wavelength end [um].
#' @param waveStep Wavelength step [um].
#' @param coreD Core diameter [um].
#' @param L Length of the modified region [um].
#' @param dL Thickness of the coating layers [nm].
#' @param Layers Layers.
#' @param angleMax Maximum skewness angle [deg].
#'
#' @return Data frame with the wavelength and real part of the transmittance.
#'
#' @import pracma
#' @import foreach
#' @import doRNG
#' @import doSNOW
#' @import utils
#' @import stats
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs hidden
#' @importFrom Hmisc approxExtrap
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom purrr map2
#' @importFrom plotly plot_ly
#' @importFrom plotly add_lines
#' @importFrom plotly layout
#' @importFrom plotly config
#' @importFrom plotly event_register
#' @importFrom plotly event_data
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyOutput
#' @importFrom plotly add_lines
#' @importFrom graphics lines
#' @importFrom readr parse_number
#' @importFrom GA ga
#' @importFrom GA startParallel
#' @importFrom GA stopParallel
#'
#'
#' @export consoleLMR
#'
#' @examples
#' \dontrun{
#'waveMin <- 0.85
#'waveMax <- 1.65
#'waveStep <- 0.01
#'temperature <- 300
#'coreD <- 200
#'L <- 40000
#'angleMax <- 90
#'thickLayers <- c( 220 )
#'
#'Cladding <- function( lambda, temperature ){
#'
#'  a1 <- 0.6961663
#'  a2 <- 0.4079426
#'  a3 <- 0.8974794
#'  b1 <- 0.0684043
#'  b2 <- 0.1162414
#'  b3 <- 9.896161
#'  cladding <- sqrt(1.0+(a1*lambda^2/(lambda^2-b1^2))+(a2*lambda^2/(lambda^2-b2^2))+(a3*lambda^2/(lambda^2-b3^2)))
#'
#'  thermal_drift <- 1+1.28e-5 * temperature
#'  thermal_drift <- 1
#'  cladding <- cladding * thermal_drift
#'
#'  return( cladding )
#'
#'}
#'Core <- function( lambda, temperature, cladding ){
#'
#'  core <- 1.0036 * cladding
#'
#'  return( core )
#'
#'}
#'L1 <- function( lambda, temperature ){
#'
#'  lambda <- lambda*10^(-6)
#'  epsinf <- 3.5
#'  tau <- 6.58e-15
#'  omegap <- 1.533e15
#'  vlight <- 3*10^8
#'  omega <- 2.0 * pi * vlight/ lambda
#'  eps <- epsinf - omegap^2 / ( omega^2 + 1i*(omega/tau) )
#'  res <- sqrt(eps)
#'
#'  thermal_drift <- 1.0 - 1.49e-4 * temperature
#'  thermal_drift <- 1
#'  res <- res * thermal_drift
#'
#'  return( res )
#'
#'}
#'
#'Layers <- data.frame( lambda = seq( waveMin, waveMax, by = waveStep ) )
#'Layers$Cladding <- Cladding( Layers$lambda, temperature )
#'Layers$Sensing <- 1
#'Layers$Core <- Core( Layers$lambda, temperature, Layers$Cladding )
#'Layers$Layers1 <- L1( Layers$lambda, temperature )
#'
#'Layers$Sensing <- 1.436
#'Result436 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
#'Layers$Sensing <- 1.422
#'Result422 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
#'Layers$Sensing <- 1.321
#'Result321 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
#'
#'plot( x = Result436$Wavelength, y = Result436$Transmittance, ylim = range( c(Result436$Transmittance,
#'                                                                             Result422$Transmittance,
#'                                                                             Result321$Transmittance) ),
#'      type = "l", xlab = "Wavelength", ylab = "Transmittance", col = "blue" )
#'lines( x = Result422$Wavelength, y = Result422$Transmittance, col = "green" )
#'lines( x = Result321$Wavelength, y = Result321$Transmittance, col = "purple" )
#' }

consoleLMR <- function( waveMin, waveMax, waveStep, coreD, L, dL, Layers, angleMax ){

  lamb_seq <- seq( waveMin, waveMax, by = waveStep )
  N <- length(lamb_seq)

  out <- data.frame( Wavelength = rep( 0, N ), Transmittance = rep( 0, N ) )

  k <- 1
  for( lambda in lamb_seq ){

    n_L <- unlist( Layers[ k, -c(1:4) ] )
    n_cladding <- Layers[ k, 2 ]
    n_sensing <- Layers[ k, 3 ]
    n_core <- Layers[ k, 4 ]

    theta_critical <- asin( n_cladding / n_core )
    theta <- seq( theta_critical, pi/2, length.out = angleMax )

    alpha_max <- pi/2
    alpha <- seq( 0, alpha_max, length.out = angleMax )

    numerator <- matrix( 0, angleMax, angleMax )
    for( i in 1:angleMax ){

      for( j in 1:angleMax ){

        k0 <- 2 * pi/lambda
        R_to_Nth <- calculate_R_to_Nth( lambda, theta[i], alpha[j], L, coreD, dL, n_core, n_L, n_sensing )
        numerator[i,j] <- R_to_Nth * k0^2 * n_core^2 * sin( theta[i] ) * cos( theta[i] )

      }

    }

    numerator_integ2d <- do.call( "rbind", apply( numerator, 2, function(x){ trapz( theta, x ) }, simplify = FALSE ) )
    numerator_integ2d <- trapz( alpha, numerator_integ2d )

    denominator_integ2d <- ( n_core * k0 )^2 * (1/4) * alpha_max * ( 1 + cos( 2 * theta_critical ) )

    t <- numerator_integ2d / denominator_integ2d
    tdB <- 10 * log( t, 10 )

    out[k,1] <- lambda
    out[k,2] <- tdB

    k <- k + 1

  }

  out$Transmittance <- Re( out$Transmittance )

  return( out )

}

shinyLMR <- function( waveMin, waveMax, waveStep, coreD, L, dL, Layers, angleMax ){

  lamb_seq <- seq( waveMin, waveMax, by = waveStep )
  N <- length(lamb_seq)

  out <- data.frame( Wavelength = rep( 0, N ), Transmittance = rep( 0, N ) )

  withProgress( message = "Process:", detail = sprintf( "%.f%%", 1/N*100 ), value = 0, {
  k <- 1
  for( lambda in lamb_seq ){

    n_L <- unlist( Layers[ k, -c(1:4) ] )
    n_cladding <- Layers[ k, 2 ]
    n_sensing <- Layers[ k, 3 ]
    n_core <- Layers[ k, 4 ]

    theta_critical <- asin( n_cladding / n_core )
    theta <- seq( theta_critical, pi/2, length.out = angleMax )

    alpha_max <- pi/2
    alpha <- seq( 0, alpha_max, length.out = angleMax )

    numerator <- matrix( 0, angleMax, angleMax )
    for( i in 1:angleMax ){

      for( j in 1:angleMax ){

        k0 <- 2 * pi/lambda
        R_to_Nth <- calculate_R_to_Nth( lambda, theta[i], alpha[j], L, coreD, dL, n_core, n_L, n_sensing )
        numerator[i,j] <- R_to_Nth * k0^2 * n_core^2 * sin( theta[i] ) * cos( theta[i] )

      }

    }

    numerator_integ2d <- do.call( "rbind", apply( numerator, 2, function(x){ trapz( theta, x ) }, simplify = FALSE ) )
    numerator_integ2d <- trapz( alpha, numerator_integ2d )

    denominator_integ2d <- ( n_core * k0 )^2 * (1/4) * alpha_max * ( 1 + cos( 2 * theta_critical ) )

    t <- numerator_integ2d / denominator_integ2d
    tdB <- 10 * log( t, 10 )

    out[k,1] <- lambda
    out[k,2] <- tdB

    k <- k + 1

    incProgress( 1/N, detail = sprintf( "%.f%%", k/N*100 ) )

  }

  })

  out$Transmittance <- Re( out$Transmittance )

  return( out )

}
