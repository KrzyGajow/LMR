calculate_R_to_Nth <- function( wavelength, theta_angle, alpha_angle, L, d_core, dL, n_core, n_L, n_sensing ){

  if( Re(theta_angle) < pi/2 ){

    Nref <- L / ( d_core * cos( alpha_angle ) * tan( theta_angle ) )

    d_vector <- c( NaN, dL, NaN )
    n_vector <- c( n_core, n_L, n_sensing )

    wavelength <- wavelength * 1000

    RTE <- jreftran_rt( wavelength, d_vector, n_vector, theta_angle, 0 )
    RTM <- jreftran_rt( wavelength, d_vector, n_vector, theta_angle, 1 )

    res <- ( RTE^Nref + RTM^Nref ) / 2

  }else{

    res <- 1

  }

  return( res )

}

jreftran_rt <- function( l, d, n, t0, polarization ){

  Z0 <- 376.730313
  Y <- n / Z0
  g <- 1i * 2 * pi * n / l

  ct1 <- ( n[1] / n * sin( t0 ) )^2
  ct2 <- 1 - ct1
  ct <- sqrt(ct2)

  # ct <- ifelse( Im(ct1) == 0, Conj(ct), ct )
  # ct[ Im(ct1) == 0 ] <- Conj( ct[ Im(ct1) == 0 ] )

  if( polarization ){

    eta <- Y / ct

  }else{

    eta <- Y * ct

  }

  len <- length( eta )

  delta <- 1i * g * d * ct

  M <- rep( list( matrix( 0, 2, 2) ), length(d) )

  for( j in 1:length(d) ){

    M[[j]][1,1] <- cos( delta[j] )
    M[[j]][1,2] <- 1i / eta[j] * sin( delta[j] )
    M[[j]][2,1] <- 1i * eta[j] * sin( delta[j] )
    M[[j]][2,2] <- cos( delta[j] )

  }

  M_t <- matrix( c(1, 0, 0, 1), 2, 2 )
  for( j in 2:(length(d)-1) ){

    M_t <- M_t %*% M[[j]]

  }

  r <- ( eta[1] * (M_t[1,1] + M_t[1,2] * eta[len] ) - ( M_t[2,1] + M_t[2,2] * eta[len] ) ) /
       ( eta[1] * (M_t[1,1] + M_t[1,2] * eta[len] ) + ( M_t[2,1] + M_t[2,2] * eta[len] ) )

  t <- 2 * eta[1] / ( eta[1] * ( M_t[1,1] + M_t[1,2] * eta[len] ) + ( M_t[2,1] + M_t[2,2] * eta[len] ) )

  R <- abs(r)^2
  TT <- Re( eta[len] / eta[1] ) * abs(t) ^2

  return( R )

}

Time <- function( comb, L ){
  divis <- c( 60*60*24*365, 60*60*24, 60*60, 60, 1 )
  sec <- comb * L
  takes <- sec / divis
  time <- paste0( if( takes[1] > 1 ){ y <- sec %/% divis[1]; if( y > 10^6 ){ sprintf( "%s years", formatC( y, format = "e", digits = 2 ) ) }else{ sprintf( "%.0f years", y ) } },
                  if( takes[2] > 1 ){ sprintf( " %.0f days", if( takes[1] > 1 ){ max( floor( (sec %% divis[1])/divis[2] ), 0 ) }else{ max( sec %/% divis[2], 0 ) } ) },
                  if( takes[3] > 1 ){ sprintf( " %.0f hours", if( takes[2] > 1 ){ max( floor( (sec %% divis[2])/divis[3] ), 0 ) }else{ max( sec %/% divis[3], 0 ) } ) },
                  if( takes[4] > 1 ){ sprintf( " %.0f minutes", if( takes[3] > 1 ){ max( floor( (sec %% divis[3])/divis[4] ), 0 ) }else{ max( sec %/% divis[4], 0 ) } ) },
                  if( takes[5] > 1 ){ sprintf( " %.0f seconds", if( takes[4] > 1 ){ max( floor( (sec %% divis[4])/divis[5] ), 0 ) }else{ max( sec %/% divis[5], 0 ) } ) } )
  return( time )
}

RemTime <- function( comb, tim, cores, iter ){
  comb <- ( comb * tim ) / cores
  divis <- c( 60*60*24*365, 60*60*24, 60*60, 60, 1 )
  sec <- comb
  takes <- sec / divis
  time <- paste0( if( takes[1] > 1 ){ y <- sec %/% divis[1]; if( y > 10^6 ){ sprintf( "%s years", formatC( y, format = "e", digits = 2 ) ) }else{ sprintf( "%.0f years", y ) } },
                  if( takes[2] > 1 ){ sprintf( " %.0f days", if( takes[1] > 1 ){ max( floor( (sec %% divis[1])/divis[2] ), 0 ) }else{ max( sec %/% divis[2], 0 ) } ) },
                  if( takes[3] > 1 ){ sprintf( " %.0f hours", if( takes[2] > 1 ){ max( floor( (sec %% divis[2])/divis[3] ), 0 ) }else{ max( sec %/% divis[3], 0 ) } ) },
                  if( takes[4] > 1 ){ sprintf( " %.0f minutes", if( takes[3] > 1 ){ max( floor( (sec %% divis[3])/divis[4] ), 0 ) }else{ max( sec %/% divis[4], 0 ) } ) },
                  if( takes[5] > 1 ){ sprintf( " %.0f seconds", if( takes[4] > 1 ){ max( floor( (sec %% divis[4])/divis[5] ), 0 ) }else{ max( sec %/% divis[5], 0 ) } ) } )
  time <- paste0( "Remaining time is:", time, " for ", iter, " iterations." )
  return( time )
}

EstCurve <- function( dat, Xmin, Xmax, step ){

  colnames( dat ) <- c("x","y")

  Pred <- Est <- Mod <- list()

  i <- 1
  repeat{

    Form <- paste0( "y~", paste0( paste0( "a", 1:i, "*" ),
                                  paste0( "x^", i:1 ), collapse = "+" ),
                    paste0( "+a", i + 1 ) )

    start <- as.list( rep( 1, i + 1 ) )
    names( start ) <- paste0( "a", 1:(i+1) )

    model <- tryCatch( nls( formula = Form,
                            data = dat, start = start,
                            control = nls.control( maxiter = 10000 ) ),
                       error = function(x){ 1 }, warning = function(x){ 1 } )

    if( !is.list(model) | (i+1 > 15) ) break

    pred <- predict( model, data.frame( x = seq( Xmin, Xmax, by = step ) ) )
    pred[pred<0] <- 0

    Mod[[ i ]] <- model

    Pred[[ i ]] <- pred

    coeff <- coefficients(model)
    Est[[ i ]] <- paste0( "f(x) = ",
                          paste0( round( head(coeff,-1), 2 ), "x^", (length(coeff)-1):1, collapse = " + "),
                          " + ", round( tail( coeff, 1 ) ) )
    Est[[ i ]] <- gsub( '\\+ -', '- ', gsub(' \\* ', '*', Est[[ i ]]) )

    i <- i + 1

  }

  return( list( Pred = Pred, Est = Est, Mod = Mod ) )

}

LastChar <- function( dat ){

  substr( dat, nchar( dat ), nchar( dat ) )

}

MidChar <- function( dat ){

  re <- grep( "Re", dat )
  im <- grep( "Im", dat )
  ifelse( length(re) > 0, 1, ifelse( length(im) > 0, 2, 3 ) )

}

KIJ <- function( dat ){
  if( !is.null( dat ) ){
    out <- gsub( "[^[:digit:]]", "", dat )
  }else{
    out <- ""
  }
  if( nchar( out ) != 0 ){
    out <- as.numeric( c( substr( out, 3, 3 ), substr( out, 2, 2 ), substr( out, 1, 1 ) ) )
  }
  return( out )
}

pointKIJ <- function( dat ){
  if( length( grep( "[[:digit:]]", dat ) ) == 1 ){
    out <- c( grep( substr( dat, nchar( dat )-3, nchar( dat ) ),  c( "(Re)", "(Im)" ) ),
              as.numeric( gsub( "[^[:digit:]]", "", dat ) ), 4 )
  }else{
    out <- c( grep( substr( dat, nchar( dat )-3, nchar( dat ) ),  c( "(Re)", "(Im)" ) ), 1,
              grep( substr( dat, 1, 2 ),  c( "Cladding", "Sensing", "Core" ) ) )
  }
  return( out )
}

ClickPoint <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$clickPlot%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$clickPlot%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

AcceptPlot <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$acceptPlot%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$acceptPlot%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

ClearPlot <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$clearPlot%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$clearPlot%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

FitPlot <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$fitCurve%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$fitCurve%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

SelectCurve <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$selectCurve%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$selectCurve%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

SelectIndex <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$selectIndex%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$selectIndex%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

SelectValue <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$selectValue%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$selectValue%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( react ) ), 2 ) ) )
  )
}

layerRenderUI <- function( input, react ){
  list(
    eval( parse( text = sprintf( 'input$optTP%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, MidChar( react ) ), 1 ) ) )
    ,eval( parse( text = sprintf( 'input$optTP%s%s%s', which( cPanels[-c(1,6)] == input$Panels ),
                                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, MidChar( react ) ), 2 ) ) )
  )
}

MSE <- function( y, est ){
  mean( ( y - est )^2 )
}

BestModel <- function( y, est ){
  n <- length( est )
  out <- double( n )
  for( i in 1:n ){
    out[i] <- MSE( y, est[[ i ]] )
  }
  return( which.min( out ) )
}

StartParallel <- function(parallel = TRUE, ...){

  if(!all(requireNamespace("parallel", quietly = TRUE),
          requireNamespace("doSNOW", quietly = TRUE)))
    stop("packages 'parallel' and 'doSNOW' required for parallelization!")

  if(any(class(parallel) == "cluster")){
    cl <- parallel
    parallel <- TRUE
    attr(parallel, "type") <- foreach::getDoParName()
    attr(parallel, "cores") <- foreach::getDoParWorkers()
    attr(parallel, "cluster") <- cl
    return(parallel)
  }

  parallelType <- if(.Platform$OS.type == "windows")
    "snow" else "multicore"

  numCores <- parallel::detectCores()

  if(is.logical(parallel)) {
    NULL
  } else if(is.numeric(parallel)){
    numCores <- as.integer(parallel)
    parallel <- TRUE
  } else if(is.character(parallel)){
    parallelType <- parallel
    parallel <- TRUE
  }else {
    parallel <- FALSE
  }

  attr(parallel, "type") <- parallelType
  attr(parallel, "cores") <- numCores

  if(parallel) {
    if( parallelType == "snow" ) {#parallelType == "snow"

      cl <- snow::makeCluster(numCores)
      attr(parallel, "cluster") <- cl

      varlist <- ls(envir = parent.frame(), all.names = TRUE)
      varlist <- varlist[varlist != "..."]
      snow::clusterExport( cl, list = varlist, envir = parent.frame() )

      snow::clusterExport( cl, list = ls(envir = globalenv(),all.names = TRUE),envir = globalenv())

      pkgs <- .packages()
      lapply( pkgs, function(pkg){ snow::clusterCall(cl, library, package = pkg, character.only = TRUE) } )

      registerDoSNOW(cl)

    } else if(parallelType == "multicore"){
      cl <- parallel::makeCluster(numCores)
      registerDoSNOW(cl)
      attr(parallel, "cluster") <- cl
    } else{
      stop("Only 'snow' and 'multicore' clusters allowed!")
    }
  }

  return(parallel)
}

ValidInput <- function( input, k, i, j ){

  c1 <- if( is.na( input$input$waveMin ) ){ FALSE }else{ input$input$waveMin > 0 }
  c2 <- if( is.na( input$input$waveMax ) ){ FALSE }else{ input$input$waveMax > 0 }
  c3 <- if( is.na( input$input$waveStep ) ){ FALSE }else{ input$input$waveStep > 0 }
  c12 <- if( is.na( input$input$waveMin ) | is.na( input$input$waveMax ) ){ FALSE }else{
    input$input$waveMin < input$input$waveMax }

  c4 <- if( is.na( input$input$temperature ) ){ FALSE }else{ input$input$temperature > 0 }
  c5 <- if( is.na( input$input$coreD ) ){ FALSE }else{ input$input$coreD > 0 }
  c6 <- if( is.na( input$input$L ) ){ FALSE }else{ input$input$L > 0 }
  c7 <- if( is.na( input$input$angleMax ) ){ FALSE }else{ input$input$angleMax > 0 & input$input$angleMax <= 90 }

  c8 <- eval( parse( text = sprintf( "input$input$nCoord%s%s%s", k,i,j ) ) )
  c8 <- if( is.null( c8 ) ){ TRUE }else{ if( is.na( c8 ) ){ FALSE }else{ c8 > 0 } }

  out <- c1 & c2 & c12 & c4 & c5 & c6 & c7 & c8

  return( out )

}
