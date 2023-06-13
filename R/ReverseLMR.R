decode <- function( x, Min, Max, prec, gr = FALSE ){

  n <- ceiling( log( ( Max - Min ) / prec + 1, 2 ) )
  x <- (x - Min) * ( 2^n - 1 ) / ( Max - Min )
  binary <- decimal2binary( round( x/prec )*prec, n ) #10^-prec )

  out <- if( gr ){ binary2gray( binary ) }else{ binary }

  return( out )

}

encode <- function( x, Min, Max, prec, n, gr = FALSE ){

  # out <- Min + binary2decimal( if( gr ){ gray2binary( x ) }else{ x } ) * ( Max - Min )/( 2^n - 1 )
  # out <- round( out, prec )
  n <- ceiling( log( ( Max - Min ) / prec + 1, 2 ) )
  sek <- seq( Min, Max, prec )
  dec <- do.call( "rbind", lapply( 0:(2^n-1), function(i){ decimal2binary( i, n ) } ) )
  out <- apply( dec, 1, function(x){ encode( x, Min, Max, prec, n, gr ) } )
  out <- dec[ round( x/prec )*prec == out, , drop = FALSE ][1,]

  return( out )

}

gaLMR <- function( x, var, init, typ, tar, prec, spli, w, repr, vMin, vMax, gray, waveMin, waveMax, waveStep, coreD,
                   Length, thickLayers, Layers, angleMax, pop, iter, par, pathDef ){

  lambda <- seq( waveMin, waveMax, waveStep )
  Comb <- data.frame( )
  for( i in var ){

    if( repr == "binary" ){

      if( i %in% c("temperature","coreD","Length") ){

        vars <- encode( x[ spli[[ i ]] ], vMin[[ i ]],
                        vMax[[ i ]], prec[[ i ]], length( x[ spli[[ i ]] ] ), gray )
        eval( parse( text = sprintf( "%s <- vars", i ) ) )
        Comb[1,i] <- vars

      }else if( length( grep( "thickLayers[[:digit:]]", i ) ) == 1 ){

        vars <- encode( x[ spli[[ i ]] ], vMin[[ i ]], vMax[[ i ]],
                        prec[[ i ]], length( x[ spli[[ i ]] ] ), gray )
        eval( parse( text = sprintf( "thickLayers[%s] <- vars", substr( i, 12, 12 ) ) ) )
        Comb[1,i] <- vars

      }else{

        name <- grep( i, names( spli ), value = TRUE )
        if( ( typ[[i]][1] == "poly" ) ){
          coef <- attr( init[[i]], "coef" )
          vars <- sapply( name, function(j){ encode( x[ spli[[ j ]] ], vMin[[ i ]], vMax[[ i ]],
                                                     prec[[ i ]], length( x[ spli[[ j ]] ] ), gray ) } )
          vars[ vars < vMin[[ i ]] ] <- vMin[[ i ]][1]
          vars[ vars > vMax[[ i ]] ] <- vMax[[ i ]][1]
          vars <- coef + coef * vars
          vars <- rowSums( sweep( data.frame( sapply( (length(vars)-1):1, function(i,x){ x^i }, x = lambda ), 1 ), 2, coef, "*" ) )
          Comb[1, paste0(i, 1:length(coef)) ] <- coef
        }else{
          vars <- sapply( name, function(j){ encode( x[ spli[[ j ]] ], vMin[[ i ]], vMax[[ i ]],
                                                     prec[[ i ]], length( x[ spli[[ j ]] ] ), gray ) } )
          Comb[1, paste0(i, 1:length(vars)) ] <- vars
        }

        eval( parse( text = sprintf( "Layers[['%s']] <- vars", i ) ) )

      }

    }else{

      if( i %in% c("temperature","coreD","Length") ){

        vars <- round( x[ spli[[ i ]] ] / prec[[ i ]] ) * prec[[ i ]]
        eval( parse( text = sprintf( "%s <- vars", i ) ) )
        Comb[1,i] <- vars

      }else if( length( grep( "thickLayers[[:digit:]]", i ) ) == 1 ){

        vars <- round( x[ spli[[ i ]] ] / prec[[ i ]] ) * prec[[ i ]]
        eval( parse( text = sprintf( "thickLayers[%s] <- vars", substr( i, 12, 12 ) ) ) )
        Comb[1,i] <- vars

      }else{

        if( ( typ[[i]][1] == "poly" ) ){
          coef <- attr( init[[i]], "coef" )
          vars <- round( x[ spli[[ i ]] ] / prec[[ i ]] ) * prec[[ i ]]
          vars[ vars < vMin[ spli[[ i ]] ] ] <- vMin[ spli[[ i ]] ][1]
          vars[ vars > vMax[ spli[[ i ]] ] ] <- vMax[ spli[[ i ]] ][1]
          coef <- coef + coef * vars
          vars <- rowSums( sweep( data.frame( sapply( (length(vars)-1):1, function(i,x){ x^i }, x = lambda ), 1 ), 2, coef, "*" ) )
          Comb[1, paste0(i, 1:length(coef)) ] <- coef
        }else{
          vars <- round( x[ spli[[ i ]] ] / prec[[ i ]] ) * prec[[ i ]]
          Comb[1, paste0(i, 1:length(vars)) ] <- vars
        }

        eval( parse( text = sprintf( "Layers[['%s']] <- vars", i ) ) )

      }

    }

  }

  LayersDf <- vector( "list", length( unique( gsub( "Re|Im", "", names(Layers) ) ) ) )
  names(LayersDf) <- unique( gsub( "Re|Im", "", names(Layers) ) )
  for( i in unique( gsub( "Re|Im", "", names(Layers) ) ) ){

    LayersDf[[i]] <- complex( real = Layers[[ sprintf( "%sRe", i ) ]], imaginary = Layers[[ sprintf( "%sIm", i ) ]] )

  }
  LayersDf <- data.frame( lambda = lambda, do.call( "cbind", LayersDf ) )

  Re_clad_core <- all( Layers$CladdingRe < Layers$CoreRe )
  # Im_clad_core <- any( CladdingIm != CoreIm )
  Im_clad_core <- all( Layers$CladdingIm == 0 & Layers$CoreIm == 0 )
  Im_layers <- any( unlist( lapply( Layers[ grep( "^Layers[[:digit:]]Im", names(Layers) ) ], function(x){ any( x!=0 ) } ) ) )

  N <- pop * iter
  past <- read.table( paste0(pathDef, "/", "time.txt" ), sep = ";" )
  tim <- RemTime( N - max( past$V1 ), mean( past$V2 ), par, N - max( past$V1 ) )
  write.table( tim, paste0(pathDef, "/", "RemTime.txt" ),
               sep = ",", row.names = FALSE, col.names = FALSE )
  start <- Sys.time()

  if( Re_clad_core & Im_clad_core & Im_layers ){

    res <- consoleLMR( waveMin, waveMax, waveStep, coreD, Length, thickLayers, LayersDf, angleMax )

    if( anyNA( res$Transmittance ) ){

      Loss <- 10^6
      Transmit <- data.frame( Wavelength = 666, Transmittance = 666 )

    }else{

      Loss <- 1 / sum( w ) * sum( w * ( res$Transmittance - tar )^2 )
      Transmit <- res

    }

  }else{

    Loss <- 10^6
    Transmit <- data.frame( Wavelength = 666, Transmittance = 666 )

  }

  end <- difftime( Sys.time(), start, units = "secs" )
  out <- data.frame( iter = max( past$V1 ) + 1, time = end )
  write.table( out, paste0(pathDef, "/", "time.txt" ),
               append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE )

  resGA <- list( Loss = Loss, Transmit = Transmit, Layers = LayersDf, Comb = Comb )
  save( resGA, file = sprintf( "gaOpt%s%s.RData", Sys.time(), runif(1) ) )

  return( -Loss )

}

fullLMR <- function( x, var, typ, tar, w, waveMin, waveMax, waveStep, coreD, Length, thickLayers, Layers, angleMax ){

  lambda <- seq( waveMin, waveMax, waveStep )

  for( i in var ){

    if( i %in% c("temperature","coreD","Length") ){

      eval( parse( text = sprintf( "%s <- x[,i]", i ) ) )

    }else if( length( grep( "thickLayers[[:digit:]]", i ) ) == 1 ){

      eval( parse( text = sprintf( "thickLayers[%s] <- x[,i]", substr( i, 12, 12 ) ) ) )

    }else{

      Which <- grep( sprintf( "%s[[:digit:]]", i ), colnames( x ) )
      if( length( Which ) > 1 ){

        tab <- x[,Which]
        poly <- rowSums( sweep( data.frame( sapply( (ncol(tab)-1):1, function(i,x){ x^i }, x = lambda ), 1 ), 2, unlist(tab), "*" ) )
        eval( parse( text = sprintf( "Layers[['%s']] <- poly", i ) ) )

      }else{

        eval( parse( text = sprintf( "Layers[['%s']] <- x[,i]", i ) ) )

      }

    }

  }

  LayersDf <- vector( "list", length( unique( gsub( "Re|Im", "", names(Layers) ) ) ) )
  names(LayersDf) <- unique( gsub( "Re|Im", "", names(Layers) ) )

  for( i in unique( gsub( "Re|Im", "", names(Layers) ) ) ){

    LayersDf[[i]] <- complex( real = Layers[[ sprintf( "%sRe", i ) ]], imaginary = Layers[[ sprintf( "%sIm", i ) ]] )

  }

  LayersDf <- data.frame( lambda = lambda, do.call( "cbind", LayersDf ) )

  Re_clad_core <- all( Layers$CladdingRe < Layers$CoreRe )
  # Im_clad_core <- any( CladdingIm != CoreIm )
  Im_clad_core <- all( Layers$CladdingIm == 0 & Layers$CoreIm == 0 )
  Im_layers <- any( unlist( lapply( Layers[ grep( "^Layers[[:digit:]]Im", names(Layers) ) ], function(x){ any( x!=0 ) } ) ) )

  if( Re_clad_core & Im_clad_core & Im_layers ){

    out <- consoleLMR( waveMin, waveMax, waveStep, coreD, Length, thickLayers, LayersDf, angleMax )
    # plot( x = out$Wavelength, y = tar, ylim = range( na.omit(out$Transmittance), na.omit(tar) ) ,
    #       type = "l", xlab = "Wavelength", ylab = "Transmittance" )
    # lines( x = out$Wavelength, y = out$Transmittance, col = "red", lty = 2 )
    if( anyNA( out$Transmittance ) ){

      Loss <- 10^6
      Transmit <- data.frame( Wavelength = 666, Transmittance = 666 )

    }else{

      Loss <- 1 / sum( w ) * sum( w * ( out$Transmittance - tar )^2 )
      Transmit <- out

    }

  }else{

    Loss <- 10^6
    Transmit <- data.frame( Wavelength = 666, Transmittance = 666 )

  }

  return( list( Loss = -Loss, Transmit = Transmit, Layers = LayersDf ) )

}

ReverseLMR <- function( var, type, tar, init, prec, w, cuts, repr, varMin, varMax, gray,
                        waveMin, waveMax, waveStep, coreD, Length, thickLayers, angleMax,
                        popSize, pcrossover, pmutation, maxiter, run, parallel, seed, meth ){

  precX <- nchar( gsub( "[^0]", "", as.character( waveStep ) ) )
  lambda <- seq( waveMin, waveMax, waveStep )

  ws <- double( length( lambda ) )

  for( i in 2:length(cuts) ){

    ws[ which( round( lambda, precX ) == round( cuts[i-1], precX ) ):
          which( round( lambda, precX ) == round( cuts[i], precX ) ) ] <- w[i-1]

  }
  ws <- ws / sum(ws)

  thickLayers <- unlist( init[ grep( "thickLayers[[:digit:]]", names(init) ) ] )
  Layers <- init[ grep( "^Cladding|^Sensing|^Core|^Layers[[:digit:]]", names(init) ) ]
  Layers <- Layers[ !unlist( lapply( Layers, is.null ) ) ]

  Var <- sapply( names(init[ var ]), function( i, init, type ){
    if( type[[i]][1] == "value" ){ init[[i]][1] }else{ if( type[[i]][1] == "poly" ){ attr( init[[i]], "coef" ) }else{ init[[i]] } }
  }, init = init[ var ], type = type )

  initLen <- Var
  Out <- init

  currdir <- getwd()
  tmpdir <- tempdir()
  setwd( tempdir() )
  file.remove( grep( "gaOpt", list.files( getwd() ), value = TRUE ) )

  pathDef <- if( rstudioapi::isAvailable() ){
    if( is.null( rstudioapi::getActiveProject() ) ){ currdir }else{ rstudioapi::getActiveProject() }  }else{ currdir }
  write.table( NULL, paste0( pathDef, "/", "RemTime.txt" ),
               sep = ",", row.names = FALSE, col.names = TRUE )
  write.table( data.frame( 0, attr( init, "time" ) ), paste0( pathDef, "/", "time.txt" ),
               sep = ";", row.names = FALSE, col.names = FALSE )

  if( repr == "binary" & meth == "Evolutionary Search" ){

    for( i in var ){

      temp <- sapply( 1:length(Var[[ i ]]), function(j){
        decode( Var[[ i ]][ j ], varMin[[ i ]], varMax[[ i ]], prec[[ i ]], gray )
      }, simplify = FALSE )

      Var[[ i ]] <- do.call( "c", temp )
      initLen[[ i ]] <- lapply( temp, length )

    }

    initLen <- cumsum( unlist( initLen ) )
    initSplit <- vector( "list", length( initLen ) )
    names( initSplit ) <- names( initLen )
    for( i in 2:(length(initLen)+1) ){

      initSplit[[i-1]] <- ( c(0,initLen)[i-1]+1 ):( c(0,initLen)[i] )

    }

    GA <- ga( type = "binary",
              fitness = gaLMR,
              var = var, typ = type, tar = tar, prec = prec, spli = initSplit, w = ws,
              repr = repr, vMin = varMin, vMax = varMax, gray = gray,
              waveMin = waveMin, waveMax = waveMax, waveStep = waveStep,
              coreD = coreD, Length = Length, thickLayers = thickLayers,
              Layers = Layers, angleMax = angleMax,
              pop = popSize, iter = maxiter, par = parallel, pathDef = pathDef,
              nBits = length( unlist( Var ) ),
              suggestions = matrix( unlist( Var ), 1 ),
              popSize = popSize, pcrossover = pcrossover, pmutation = pmutation,
              maxiter = maxiter, run = run, seed = seed, parallel = parallel )

    solut <- GA@solution

    for( i in var ){

      if( length( grep( "^Cladding|^Sensing|^Core|^Layers[[:digit:]]", i ) ) == 0 ){

        vars <- encode( solut[ initSplit[[ i ]] ], varMin[[ i ]],
                        varMax[[ i ]], prec[[ i ]], length( solut[ initSplit[[ i ]] ] ), gray )
        Out[[ i ]] <- vars

      }else{

        name <- grep( i, names( initSplit ), value = TRUE )
        vars <- sapply( name, function(j){ encode( solut[ initSplit[[ j ]] ], varMin[[ i ]], varMax[[ i ]],
                                                   prec[[ i ]], length( solut[ initSplit[[ j ]] ] ), gray ) } )
        Out[[ i ]] <- vars

      }

    }

  }else if( repr == "real-valued" & meth == "Evolutionary Search" ) {

    initLen <- cumsum( unlist( lapply( Var, length ) ) )
    initSplit <- vector( "list", length(initLen) )
    names(initSplit) <- var
    for( i in 2:(length(initLen)+1) ){
      initSplit[[i-1]] <- ( c(0,initLen)[i-1]+1 ):( c(0,initLen)[i] )
    }

    VarMin <- sapply( names(init[ var ]), function( i, init, type, Min ){
      if( type[[i]][1] == "value" ){ Min[[i]] }else{ if( type[[i]][1] == "poly" ){
        rep( Min[[i]], as.numeric( type[[i]][2] )+1 ) }else{ rep( Min[[i]], length(init[[i]]) ) } }
    }, init = init[ var ], type = type, Min = varMin )
    VarMax <- sapply( names(init[ var ]), function( i, init, type, Max ){
      if( type[[i]][1] == "value" ){ Max[[i]] }else{ if( type[[i]][1] == "poly" ){
        rep( Max[[i]], as.numeric( type[[i]][2] )+1 ) }else{ rep( Max[[i]], length(init[[i]]) ) } }
    }, init = init[ var ], type = type, Max = varMax )

    GA <- ga( type = "real-valued",
              fitness = gaLMR,
              var = var, init = init, typ = type, tar = tar, prec = prec, spli = initSplit, w = ws,
              repr = repr, vMin = unlist( VarMin ), vMax = unlist( VarMax ), gray = NULL,
              waveMin = waveMin, waveMax = waveMax, waveStep = waveStep,
              coreD = coreD, Length = Length, thickLayers = thickLayers,
              Layers = Layers, angleMax = angleMax,
              pop = popSize, iter = maxiter, par = parallel, pathDef = pathDef,
              suggestions = matrix( unlist( Var ), 1 ),
              lower = unlist( VarMin ), upper = unlist( VarMax ),
              popSize = popSize, pcrossover = pcrossover, pmutation = pmutation,
              maxiter = maxiter, run = run, seed = seed, parallel = F )#parallel

    Out <- data.frame( FitFun = rep( 10^6, popSize * maxiter ) )
    Transmit <- Laye <- vector( "list", popSize * maxiter )
    gaOpt <- grep( "gaOpt", list.files(), value = TRUE )
    j <- 1

    for( i in gaOpt ){
      load( i )
      Out[j,"FitFun"] <- resGA$Loss
      Out[j, 2:( ncol(resGA$Comb)+1 ) ] <- resGA$Comb
      Transmit[[ j ]] <- resGA$Transmit
      Laye[[ j ]] <- resGA$Layers
      j <- j + 1
      rm( resGA )
    }

    ordBest <- order( Out[,"FitFun"] )
    Out <- Out[ordBest,][ 1:min( 100, length(ordBest) ), ]
    Transmit <- Transmit[ ordBest ][ 1:min( 100, length(ordBest) ) ]
    Laye <- Laye[ ordBest ][ 1:min( 100, length(ordBest) ) ]

  }else if( meth == "Full Search" ){

    Var <- var[ !unlist( lapply( Type[ var ], function(x){ x[1] == "inter" } ) ) ]

    VarMin <- sapply( Var, function( i, init, type, Min ){
      Min[[i]]
    }, init = init[ Var ], type = type, Min = varMin )
    VarMax <- sapply( Var, function( i, init, type, Max ){
      Max[[i]]
    }, init = init[ Var ], type = type, Max = varMax )
    Comb <- varMin

    for( i in Var ){
      Comb[[ i ]] <- seq( VarMin[[ i ]], VarMax[[ i ]], prec[[ i ]] )
    }

    for( i in Var ){
      if( ( type[[i]][1] == "poly" ) ){
        temp <- rep( list( Comb[[ i ]] ), as.numeric( type[[i]][2] )+1 )
        for( j in 1:length(temp) ){
          val <- attr( init[[i]], "coef" )[j]
          temp[[ j ]] <- val + val * temp[[ j ]]
        }
        names( temp ) <- paste0( i, 1:( as.numeric( type[[i]][2] )+1) )
        Comb <- c( Comb, temp )
        Comb[[ i ]] <- NULL
      }
    }

    Comb <- Comb[ grep( paste0( "^", Var, collapse = "|" ), names(Comb) ) ]

    Comb <- tryCatch( expand.grid( Comb ), error = function(e){ expand.grid( 1 ) } )
    Comb <- data.frame( FitFun = 10^6, Comb )

    par <- parallel
    stopCluster <- if( inherits( parallel, "cluster" ) ) { FALSE } else { TRUE }
    parallel <- StartParallel( parallel )

    on.exit( if( parallel & stopCluster ){
      stopParallel( attr( parallel, "cluster") )
    })
    `%DO%` <- if( parallel && requireNamespace( "doRNG", quietly = TRUE ) ){
      doRNG::`%dorng%`
    } else if( parallel ) {
      `%dopar%`
    } else {
      `%do%`
    }
    set.seed( seed )
    i. <- NULL

    N <- nrow(Comb)
    if( par > 1 ){

      Loss <- 10^6
      Transmit <- data.frame( Wavelength = 666, Transmittance = 666 )

      resComb <- foreach( i. = 1:N, .errorhandling = "remove" ) %DO% {

        past <- read.table( paste0( pathDef, "/", "time.txt" ), sep = ";" )
        tim <- RemTime( N - max( past$V1 ), mean( past$V2 ), par, N - max( past$V1 ) )
        write.table( tim, paste0( pathDef, "/", "RemTime.txt" ),
                     sep = ",", row.names = FALSE, col.names = FALSE )
        start <- Sys.time()

        res <- fullLMR( x = Comb[i.,-1,drop=FALSE], var = Var, tar = tar, w = ws,
                        waveMin = waveMin, waveMax = waveMax, waveStep = waveStep,
                        coreD = coreD, Length = Length, thickLayers = thickLayers,
                        Layers = Layers, angleMax = angleMax )

        end <- difftime( Sys.time(), start, units = "secs" )
        out <- data.frame( iter = i., time = end )
        write.table( out, paste0( pathDef, "/", "time.txt" ),
                     append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE )

        c( iter = i., res )

      }

      Comb[ unlist( lapply( resComb, function(x){ x$iter } ) ) ,"FitFun"] <- unlist( lapply( resComb, function(x){ -x$Loss } ) )
      Transmit <- lapply( resComb, function(x){ x$Transmit } )
      Laye <- lapply( resComb, function(x){ x$Layers } )

    }else{

      Transmit <- vector( "list", N )
      Laye <- vector( "list", N )
      withProgress( message = "Process:", detail = sprintf( "%.f%%", 0/N*100 ), value = 0, {

        for( i in 1:N ){

          past <- read.table( paste0( pathDef, "/", "time.txt" ), sep = ";" )
          tim <- RemTime( N - max( past$V1 ), mean( past$V2 ), par, N - max( past$V1 ) )
          write.table( tim, paste0( pathDef, "/", "RemTime.txt" ),
                       sep = ",", row.names = FALSE, col.names = FALSE )
          start <- Sys.time()

          res <- fullLMR( x = Comb[i,-1,drop=FALSE], var = Var, tar = tar, w = ws,
                          waveMin = waveMin, waveMax = waveMax, waveStep = waveStep,
                          coreD = coreD, Length = Length, thickLayers = thickLayers,
                          Layers = Layers, angleMax = angleMax )

          end <- difftime( Sys.time(), start, units = "secs" )
          out <- data.frame( iter = i, time = end )
          write.table( out, paste0( pathDef, "/", "time.txt" ),
                       append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE )

          Comb[i,"FitFun"] <- -res$Loss
          Transmit[[ i ]] <- res$Transmit
          Laye[[ i ]] <- res$Layers

          incProgress( 1/N, detail = sprintf( "%.f%%", i/N*100 ) )

        }

      })

    }

    ordBest <- order( Comb[,"FitFun"] )
    Out <- Comb[ordBest,][1:min( 100, length(ordBest) ),]
    rownames( Out ) <- NULL
    Transmit <- Transmit[ ordBest ][ 1:min( 100, length(ordBest) ) ]
    Laye <- Laye[ ordBest ][ 1:min( 100, length(ordBest) ) ]

  }

  file.remove( grep( "gaOpt", list.files( getwd() ), value = TRUE ) )
  setwd( currdir )

  return( list( Out = Out, Transmit = Transmit, Layers = Laye ) )

}
