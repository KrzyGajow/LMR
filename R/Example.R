#### Test structure 1 - ITO ####
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
#
# Layers$Sensing <- 1.436
# Result436 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
# Layers$Sensing <- 1.422
# Result422 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
# Layers$Sensing <- 1.321
# Result321 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
#
# plot( x = Result436$Wavelength, y = Result436$Transmittance, ylim = range( c(Result436$Transmittance,
#                                                                              Result422$Transmittance,
#                                                                              Result321$Transmittance) ),
#       type = "l", xlab = "Wavelength", ylab = "Transmittance", col = "blue" )
# lines( x = Result422$Wavelength, y = Result422$Transmittance, col = "green" )
# lines( x = Result321$Wavelength, y = Result321$Transmittance, col = "purple" )

#### Test structure 2 - TiO2 ####
waveMin <- 0.5
waveMax <- 1.5
waveStep <- 0.01
temperature <- 300
coreD <- 200
L <- 25000
angleMax <- 90
thickLayers <- c( 1165 )

Cladding <- function( lambda, temperature ){

  a1 <- 0.6961663
  a2 <- 0.4079426
  a3 <- 0.8974794
  b1 <- 0.0684043
  b2 <- 0.1162414
  b3 <- 9.896161
  cladding <- sqrt(1.0+(a1*lambda^2/(lambda^2-b1^2))+(a2*lambda^2/(lambda^2-b2^2))+(a3*lambda^2/(lambda^2-b3^2)))

  thermal_drift <- 1.0 + 1.28e-5 * temperature
  thermal_drift <- 1.0
  cladding <- cladding * thermal_drift

  return( cladding )

}
Core <- function( lambda, temperature, cladding ){

  core <- 1.0036 * cladding

  return( core )

}
L1 <- function( lambda, temperature ){

  lambda <- lambda*10^(-6)
  epsinf <- 1.0
  Ak <- 101.0
  Bk <- 1.2
  Ek <- 6.2
  hplanck <- 6.62607004*10^(-34)
  vlight <- 3*10^8
  ladel <- 1.60217733*10^(-19)
  E <- ( hplanck * vlight / lambda ) / ladel
  eps <- epsinf + Ak / ( Ek^2 - E^2 - 1i * Bk * E )
  res <- sqrt(eps)

  thermal_drift <- 1.0 - 1.49e-4 * temperature
  thermal_drift <- 1.0
  res <- res * thermal_drift

  return( res )

}

Layers <- data.frame( lambda = seq( waveMin, waveMax, by = waveStep ) )
Layers$Cladding <- Cladding( Layers$lambda, temperature )
Layers$Sensing <- 1.321
Layers$Core <- Core( Layers$lambda, temperature, Layers$Cladding )
Layers$Layers1 <- L1( Layers$lambda, temperature )

# thickLayers <- c( 1165 )
# Result1165 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
# thickLayers <- c( 333 )
# Result333 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
#
# plot( x = Result1165$Wavelength, y = Result1165$Transmittance, ylim = range( c(Result1165$Transmittance,
#                                                                                Result333$Transmittance) ),
#       type = "l", xlab = "Wavelength", ylab = "Transmittance", col = "green" )
# lines( x = Result333$Wavelength, y = Result333$Transmittance, col = "purple" )

#### Test structure 3 - ZnO ####
waveMin <- 0.2
waveMax <- 2.2
waveStep <- 0.01
temperature <- 300
coreD <- 400
L <- 10000
angleMax <- 90
thickLayers <- c( 70 )

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
  epsinf <- 1.0
  tau <- 1.58e-15
  omegap <- 4.28e15
  omega0 <- 10.02e15
  gamma <- 3.103e14
  s0 <- 1.9
  vlight <- 3.0e8
  omega <- 2.0 * pi * vlight / lambda
  eps = epsinf - omegap^2/(omega^2 + 1i*(omega/tau))+s0*omega0^2/(omega0^2-omega^2+1i*gamma*omega)
  res <- sqrt(eps)

  thermal_drift <- 1.0 - 1.49e-4 * temperature
  thermal_drift <- 1.0
  res <- res * thermal_drift

  return( res )

}

Layers <- data.frame( lambda = seq( waveMin, waveMax, by = waveStep ) )
Layers$Cladding <- 1
Layers$Sensing <- 1.333
Layers$Cladding <- Cladding( Layers$lambda, temperature )
Layers$Core <- Core( Layers$lambda, temperature, Layers$Cladding )
Layers$Layers1 <- L1( Layers$lambda, temperature )

# Result70 <- consoleLMR( waveMin, waveMax, waveStep, coreD, L, thickLayers, Layers, angleMax )
# plot( x = Result70$Wavelength, y = Result70$Transmittance, type = "l", xlab = "Wavelength", ylab = "Transmittance" )
