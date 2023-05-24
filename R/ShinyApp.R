#' runShinyLMR
#' @title runShinyLMR
#'
#' @description Function to run Shiny application
#'
#' @param launch.browser By default Shiny application is opened in the default browser.
#'
#' @export runShinyLMR
#'
#' @examples
#' \dontrun{
#' runShinyLMR()
#' }
#'
runShinyLMR <- function( launch.browser = TRUE ){

  app <- shinyApp(

    #### UI ####
    ui = navbarPage( "LMR",

      #### Description ####
      tabPanel( "Description",
        tags$head(
          tags$script(
            "$(document).on('shiny:inputchanged', function(event) {
              if (event.name != 'changed') {
                Shiny.setInputValue('changed', event.name);
              }
            });"
          )
        ),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        fluidPage(
          h2("Theoretical modelling of lossy mode resonance (LMR)"),
          br(),

          h3("Description"),
          "Description: Lossy mode resonances can be obtained in the transmission spectrum of cladding
          removed multimode optical fiber coated with a thin-film. The sensitivity of these devices to
          changes in the properties ofthe coating or the surrounding medium can be optimized by means of
          the adequate parameterization of the coating refractive index, the coating thickness, and the
          surrounding medium refractive index. Means of design, which enable the selection of the best
          parameters for each specific sensing application, are indicated in this package.",
          br(),

          h3("Author"),
          a(href="http://krzysztof_gajowniczek.users.sggw.pl/", "Krzysztof Gajowniczek, PhD DSc"),

          h3("Parameters for LMR Modelling"),
          p(strong("waveMin:"), "Wavelength start [um]."),
          p(strong("waveMax:"), "Wavelength end [um]."),
          p(strong("waveStep:"), "Wavelength step [um]."),
          p(strong("coreD:"), "Core diameter [um]."),
          p(strong("L:"), "Length of the modified region [um]."),
          p(strong("Layers:"), "Thickness of the coating layers [nm]."),
          p(strong("angleMax:"), "Maximum skewness angle [deg]."),

          br(),

          h3("Parameters for Reverse LMR Optimization"),
          p(strong("Upload Model from File:"), "Saved structure obtained based on the Modelling window (shinyLMR function)."),
          p(strong("Optimization Method:"), "The type of optimization algorithm. Possible values are: Evolutionary Search for genetic algorithm or Full Search for exhausted full search."),
          p(strong("Optimized Parameters:"), "List of parameters to be optimized."),
          p(strong("Fitness Function:"), "Minimized loss function for a given optimization algorithm. MSE stands for mean squared error or weightedMSE stands for weighted mean squared error."),
          p(strong("Representation:"), "The type of GA to be run depending on the nature of decision variables. Possible values are: binary for binary representations of decision variables, real-valued for optimization problems where the decision variables are floating-point representations of real numbers."),
          p(strong("Encoding:"), "If Representation is set at binary user can choose standard or gray encoding type."),
          p(strong("Population Size:"), "The population size for Evolutionary Search."),
          p(strong("Probability of Crossover:"), "The probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8."),
          p(strong("Probability of Mutation:"), "The probability of mutation in a parent chromosome. Usually mutation occurs with a small probability, and by default is set to 0.1."),
          p(strong("Maximum Number of Iterations:"), "The maximum number of iterations to run before the GA search is halted."),
          p(strong("Number of Consecutive Generations Without any Improvement:"), "The number of consecutive generations without any improvement in the best fitness value before the GA is stopped."),
          p(strong("Number of cores for parallel execution:"), "An optional argument which allows to specify if the GA should be run sequentially or in parallel."),
          p(strong("Seed for RNG:"), "An integer value containing the random number generator state. This argument can be used to replicate the results of a GA search."),

        )
      ),

      #### Modelling ####
      tabPanel("Modelling",
       shinyjs::useShinyjs(),
        tabsetPanel( id = "Panels",
          tabPanel( "Parameters",
            sidebarLayout(
              sidebarPanel(
                fileInput( "UploadSettings", "Upload Settings from File", multiple = TRUE, accept = c("RData") ),
                textOutput("wrongSettings"),
                hr(),
                numericInput( "waveMin", "Wavelength start", value = 0.5, 0, Inf, 0.1),
                numericInput( "waveMax", "Wavelength end", value = 1.5, 0, Inf, 0.1),
                numericInput( "waveStep", "Wavelength step", value = 0.01, 1.5, Inf, 0.01),
                numericInput( "temperature", "Temperature (K)", value = 300, 0, Inf, 0.01),
                numericInput( "coreD", "Core diameter", value = 200, 0, Inf, 1),
                numericInput( "L", "Length of the modified region", value = 25000, 5000, 50000, 1),
                numericInput( "angleMax", "Maximum skewness angle", value = 90, 0, 90, 1 ),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "Void",
                ),
                width = 10
              )
            )
          ),

          tabPanel( "Cladding",
            sidebarLayout(
              sidebarPanel(
                uiOutput( "optionTB111" ),
                # uiOutput( "optionTB112" ),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "tabClad",
                  tabPanel( "Cladding (Re)", uiOutput( "out111" ) )
                  # ,tabPanel( "Cladding (Im)", uiOutput( "out112" ) )
                ),
                width = 10
              )
            )
          ),

          tabPanel( "Sensing",
            sidebarLayout(
              sidebarPanel(
                uiOutput( "optionTB211" ),
                # uiOutput( "optionTB212" ),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "tabSens",
                  tabPanel( "Sensing (Re)", uiOutput( "out211" ) ),
                  # tabPanel( "Sensing (Im)", uiOutput( "out212" ) )
                ),
                width = 10
              )
            )
          ),

          tabPanel( "Core",
            sidebarLayout(
              sidebarPanel(
                uiOutput( "optionTB311" ),
                # uiOutput( "optionTB312" ),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "tabCore",
                  tabPanel( "Core (Re)", uiOutput( "out311" ) )
                  # ,tabPanel( "Core (Im)", uiOutput( "out312" ) )
                ),
                width = 10
              )
            )
          ),

          tabPanel( "Layers",
            sidebarLayout(
              sidebarPanel(
                selectInput( "nLayers", "Number of layers", choices = c( "", 1:8 ), selected = "" ),
                hr(),
                uiOutput( "dLayers" ),
                hr(),
                uiOutput( "optLayers" ),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "tabLayers",
                  tabPanel( "Layer 1",
                    tabsetPanel( id = "tabL1",
                      tabPanel( "Layer 1 (Re)", uiOutput( "out411" ) ),
                      tabPanel( "Layer 1 (Im)", uiOutput( "out412" ) )
                    ),
                    width = 10
                  ),
                  tabPanel( "Layer 2",
                    tabsetPanel( id = "tabL2",
                      tabPanel( "Layer 2 (Re)", uiOutput( "out421" ) ),
                      tabPanel( "Layer 2 (Im)", uiOutput( "out422" ) )
                    ),
                    width = 10
                  ),
                  tabPanel( "Layer 3",
                    tabsetPanel( id = "tabL3",
                      tabPanel( "Layer 3 (Re)", uiOutput( "out431" ) ),
                      tabPanel( "Layer 3 (Im)", uiOutput( "out432" ) )
                    ),
                    width = 10
                  ),
                  tabPanel( "Layer 4",
                    tabsetPanel( id = "tabL4",
                      tabPanel( "Layer 4 (Re)", uiOutput( "out441" ) ),
                      tabPanel( "Layer 4 (Im)", uiOutput( "out442" ) )
                    ),
                    width = 10
                  ),
                  tabPanel( "Layer 5",
                    tabsetPanel( id = "tabL5",
                      tabPanel( "Layer 5 (Re)", uiOutput( "out451" ) ),
                      tabPanel( "Layer 5 (Im)", uiOutput( "out452" ) )
                    ),
                    width = 10
                  ),
                  tabPanel( "Layer 6",
                  tabsetPanel( id = "tabL5",
                    tabPanel( "Layer 6 (Re)", uiOutput( "out461" ) ),
                    tabPanel( "Layer 6 (Im)", uiOutput( "out462" ) )
                  ),
                  width = 10
                  ),
                  tabPanel( "Layer 7",
                  tabsetPanel( id = "tabL7",
                    tabPanel( "Layer 7 (Re)", uiOutput( "out471" ) ),
                    tabPanel( "Layer 7 (Im)", uiOutput( "out472" ) )
                  ),
                  width = 10
                  ),
                  tabPanel( "Layer 8",
                  tabsetPanel( id = "tabL8",
                    tabPanel( "Layer 8 (Re)", uiOutput( "out481" ) ),
                    tabPanel( "Layer 8 (Im)", uiOutput( "out482" ) )
                  ),
                  width = 10
                  )
                ),
                width = 10
              )
            )
          ),

          tabPanel( "Model",
            sidebarLayout(
              sidebarPanel(
                actionButton( "StartModel", "Start Modelling"),
                # hr(),
                # actionButton( "FitModel", "Fit Model"),
                hr(),
                shinyjs::hidden(div(
                  id = "AreaSave",
                  downloadButton("save", "Save Analysis")
                )),
                width = 2
              ),
              mainPanel(
                tabPanel( "Result",
                  uiOutput( "result" )
                ),
                width = 10
              )
            )
          )
        )
      ),

      #### Optimization ####
      tabPanel("Optimization",
        shinyjs::useShinyjs(),
        tabsetPanel( id = "OptPanels",
          tabPanel( "Parameters",
            sidebarLayout(
              sidebarPanel(
                actionButton( "StartOpt", "Start Optimization"),
                hr(),
                textOutput("optTime1"),
                textOutput("optTime2"),
                textOutput("optTime3"),
                hr(),
                fileInput( "UploadModel", "Upload Model from File", multiple = TRUE, accept = c("RData") ),
                textOutput("wrongFile"),
                hr(),
                actionButton( "InsertModel", "Insert Existing Model" ),
                hr(),
                downloadButton( "saveChanged", "Save New Model" ),
                hr(),
                selectInput( "methOpt", "Optimization Method", choices = c( "Evolutionary Search", "Full Search" ), selected = "Full Search" ),
                selectInput( "varOpt", "Optimized Parameters", "", multiple = TRUE ),
                selectInput( "fitFun", "Fitness Function", choices = c( "MSE", "weightedMSE" ), selected = "MSE" ),
                selectInput( "repType", "Representation", choices = c( "real-valued", "binary" ), selected = "real-valued" ),
                shinyjs::hidden(div(
                  id = "AreaEnc",
                  selectInput( "encType", "Encoding", choices = c( "standard", "gray" ), selected = "standard" )
                )),
                numericInput( "popSize", "Population Size", value = 10, 1, 30, 1 ),
                numericInput( "pcrossover", "Probability of Crossover", value = 0.8, 0, 1, 0.01 ),
                numericInput( "pmutation", "Probability of Mutation", value = 0.1, 0, 1, 0.01 ),
                numericInput( "maxiter", "Maximum Number of Iterations", value = 10, 1, 10000, 1 ),
                numericInput( "run", "Number of Consecutive Generations Without any Improvement", value = 10, 1, 10000, 1 ),
                numericInput( "parallel", "Number of cores for parallel execution", value = 1, 1, 100, 1 ),
                numericInput( "seed", "Seed for RNG", value = 123, 0, 1000000, 1 ),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "OptVars",
                  tabPanel( "Plot",
                    uiOutput( "OptPlot" )
                  ),
                  tabPanel( "Parameters Options",
                    fluidRow(
                      column( 2, uiOutput( "OptVarsMin" ) ),
                      column( 2, uiOutput( "OptVarsMax" ) ),
                      column( 2, uiOutput( "OptVarsPrec" ) ),
                      column( 2, htmlOutput( "OptVarsForb" ) )
                    )
                  ),
                  tabPanel( "Weighted Fitness Function",
                    column( width = 5, style='padding:0px;',
                      fluidPage( fluidRow(
                        column( 8, style='padding:0px;', numericInput( 'nFitFun', 'Number of intervals', value = 2, 2, 20, 1 ) )
                      ) ),
                      fluidPage( fluidRow(
                        column( 4, style='padding:0px;', uiOutput( "FitFunMin" ) ),
                        column( 4, style='padding:0px;', uiOutput( "FitFunMax" ) ),
                        column( 4, style='padding:0px;', uiOutput( "FitFunVal" ) )
                      ))
                    )
                  )
                ),
                width = 10
              )
            )
          ),
          tabPanel( "Results",
            sidebarLayout(
              sidebarPanel(
                shinyjs::hidden(div(
                  id = "AreaSaveOpt",
                  selectInput( "ChooseResultOpt", "Choose Result", "" ),
                  hr(),
                  downloadButton("saveOpt", "Save Optimization Results")
                )),
                width = 2
              ),
              mainPanel(
                tabsetPanel( id = "tabResOpt",
                  tabPanel( "Results", DTOutput( 'RevResTab' ) ),
                  tabPanel( "Plot", plotlyOutput( 'RevResPlot' ) ),
                ),
                width = 10
              )
            )
          )
        )
      )
    ),

    #### Server ####
    server <- function( input, output, session ) {

      options( warn = -1 )

      #### Init ####
      Comb <- c( c( paste0( paste0( 1:4, 1 ), rep(1:2,each=4) ), paste0( paste0( 4, 2:8 ), rep(1:2,each=7) ) ) )
      sapply( c( paste0( "acceptPlot", Comb ), paste0( "drawPlot", Comb ), paste0( "afterInit", Comb ), "nL" ),
              function(x){ assign(x, FALSE, envir = .GlobalEnv) } )
      List <- rep( list( list( list( c(), c() ) ) ), 4 )
      List[[ 4 ]] <- rep( list( list( c(), c() ) ), 8 )
      sapply( c("Curve","dfCurve","LayersList"), function(x){ assign(x, List, envir = .GlobalEnv) } )
      assign( "cPanels", c("Parameters","Cladding","Sensing","Core","Layers","Model"), env = .GlobalEnv )
      sapply( c("outputGlob","outputTabGlob","Result","Model"), function(x){ assign(x, c(), envir = .GlobalEnv) } )

      assign( "reactGlob", c("Cladding (Re)","Sensing (Re)","Core (Re)"), env = .GlobalEnv )

      for( i in paste0( "XY", Comb ) ){
        assign( i, reactiveValues( x = NULL, y = NULL ) )
      }
      reactCoord <- reactiveVal( 0 )
      observe( reactCoord( c( lapply( Comb, function(x){ eval( parse( text = sprintf( "input$nCoord%s",  x ) ) ) } ),
                              lapply( Comb, function(x){ lapply( paste0( rep( c("X","Y"), each = 10 ), 1:10 ), function(y){ eval( parse( text = sprintf( "input$Coord%s%s", x, y ) ) ) } ) } ) ) ) )

      observeEvent( reactCoord(), {
        for( i in Comb ){
          nc <- eval( parse( text = sprintf( "input$nCoord%s",  i ) ) )
          if( is.null(nc) ){
            eval( parse( text = sprintf( "XY%s$x <- sapply(1:2, function(x){ 1 } )", i ) ) )
            eval( parse( text = sprintf( "XY%s$y <- sapply(1:2, function(y){ 1 } )", i ) ) )
          }else{
            nc <- as.numeric(nc)
            eval( parse( text = sprintf( "XY%s$x <- sapply(1:%s, function(x){ eval( parse( text = paste0( 'input$Coord%sX', x) ) ) } )", i,nc,i ) ) )
            eval( parse( text = sprintf( "XY%s$y <- sapply(1:%s, function(y){ eval( parse( text = paste0( 'input$Coord%sY', y) ) ) } )", i,nc,i ) ) )
            # print( eval( parse( text = sprintf( "sapply(1:%s, function(y){ eval( parse( text = paste0( 'input$Coord%sY', y) ) ) } )", nc,i ) ) ) )
          }
        }
      })

      reactYrange <- reactiveVal( 0 )
      observe( reactYrange( c( lapply( Comb, function(x){ eval( parse( text = sprintf( "input$minY%s", x ) ) ) } ),
                               lapply( Comb, function(x){ eval( parse( text = sprintf( "input$maxY%s", x ) ) ) } ),
                               input$minYo, input$maxYo) ) )


      reactFitFun <- reactiveVal( 0 )
      observe( reactFitFun( c( lapply( 1:as.numeric( input$nFitFun ), function(x){ eval( parse( text = sprintf( "input$FitFunMin%s",  x ) ) ) } ),
                               lapply( 1:as.numeric( input$nFitFun ), function(x){ eval( parse( text = sprintf( "input$FitFunMax%s",  x ) ) ) } ),
                               lapply( 1:as.numeric( input$nFitFun ), function(x){ eval( parse( text = sprintf( "input$FitFunVal%s",  x ) ) ) } ) ) ) )

      reactOptVars <- reactiveVal( 0 )
      observe( reactOptVars( c( lapply( input$varOpt, function(x){ eval( parse( text = sprintf( "input$OptVarsMin%s",  x ) ) ) } ),
                                lapply( input$varOpt, function(x){ eval( parse( text = sprintf( "input$OptVarsMax%s",  x ) ) ) } ),
                                lapply( input$varOpt, function(x){ eval( parse( text = sprintf( "input$OptVarsPrec%s",  x ) ) ) } ) ) ) )

      reactResUpl <- reactiveVal( 0 )
      observe( reactResUpl( lapply( Comb, function(x){ eval( parse( text = sprintf( "input$resetUpload%s",  x ) ) ) } ) ) )

      reactUplCoord <- reactiveVal( 0 )
      observe( reactUplCoord( lapply( Comb, function(x){ eval( parse( text = sprintf( "input$uplCoords%s",  x ) ) ) } ) ) )

      nam <- c( paste( rep( c( "Cladding","Sensing","Core" ), each = 2 ), c( "(Re)", "(Im)" ) ),
                paste( rep( paste( c( "Layers"), 1:8 ), each = 2 ), c( "(Re)", "(Im)" ) ) )
      check <- data.frame( Name = rep( nam, each = 3 ), indx = rep( sort( Comb ), each = 3 ),
                           Val = 0, Type = 0, Which = c( "table","curve","value" ), Time = "" )
      check$Nr <- 1:nrow( check )
      assign( "check", check, envir = .GlobalEnv )

      #### Reactive Val ####
      reactP <- reactiveVal( 0 )
      observe( reactP( input$Panels ) )

      reactL <- reactiveVal( 0 )
      observe( reactL( input$tabLayers ) )

      reactLe <- reactiveVal( 0 )
      observe( reactLe( eval( parse( text = sprintf( 'input$tabL%s', LastChar( reactL() ) ) ) ) ) )

      reactC <- reactiveVal( 0 )
      observe( reactC( input$tabClad ) )

      reactS <- reactiveVal( 0 )
      observe( reactS( input$tabSens ) )

      reactK <- reactiveVal( 0 )
      observe( reactK( input$tabCore ) )

      reactOptTP1 <- reactiveVal( 0 )
      observe( reactOptTP1( eval( parse( text =
        sprintf( 'input$optTP%s%s%s',
                 which( cPanels[-c(1,6)] == input$Panels ),
                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( reactL() ) ),
                 1 ) ) ) ) )
      reactOptTP2 <- reactiveVal( 0 )
      observe( reactOptTP2( eval( parse( text =
        sprintf( 'input$optTP%s%s%s',
                  which( cPanels[-c(1,6)] == input$Panels ),
                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( reactL() ) ),
                  2 ) ) ) ) )

      reactDP1 <- reactiveVal( 0 )
      observe( reactDP1( eval( parse( text =
        sprintf( 'input$drawPlot%s%s%s',
                 which( cPanels[-c(1,6)] == input$Panels ),
                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( reactL() ) ),
                 1 ) ) ) ) )
      reactDP2 <- reactiveVal( 0 )
      observe( reactDP2( eval( parse( text =
        sprintf( 'input$drawPlot%s%s%s',
                  which( cPanels[-c(1,6)] == input$Panels ),
                  ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( reactL() ) ),
                  2 ) ) ) ) )

      reactCP1 <- reactiveVal( 0 )
      observe( reactCP1( eval( parse( text =
        sprintf( 'input$clickPlot%s%s%s',
                 which( cPanels[-c(1,6)] == input$Panels ),
                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( reactL() ) ),
                 1 ) ) ) ) )
      reactCP2 <- reactiveVal( 0 )
      observe( reactCP2( eval( parse( text =
        sprintf( 'input$clickPlot%s%s%s',
                 which( cPanels[-c(1,6)] == input$Panels ),
                 ifelse( which( cPanels[-c(1,6)] == input$Panels ) %in% 1:3, 1, LastChar( reactL() ) ),
                 2 ) ) ) ) )

      reactRI <- reactiveVal( 0 )

      observeEvent( list( reactP(), reactL(), reactC(), reactS(), reactK(), reactLe() ), {

        val <- c( reactC(), reactS(), reactK(), reactLe() )
        k <- which( cPanels[-c(1,6)] == input$Panels )

        req( length(k) > 0 )
        reactRI( val[ k ] )

      })

      #### Import Settings Value ####
      observeEvent( input$UploadSettings, {

        load( input$UploadSettings$datapath, envir = .GlobalEnv )
        req( exists( "Parameters", envir = .GlobalEnv ) )

        updateNumericInput( session, "waveMin", value = Parameters$waveMin[1] )
        updateNumericInput( session, "waveMax", value = Parameters$waveMax[1] )
        updateNumericInput( session, "waveStep", value = Parameters$waveStep[1] )
        updateNumericInput( session, "temperature", value = Parameters$temperature[1] )
        updateNumericInput( session, "coreD", value = Parameters$coreDiameter[1] )
        updateNumericInput( session, "L", value = Parameters$LengthModifiedRegion[1] )
        updateNumericInput( session, "angleMax", value = Parameters$angleMax[1] )

        # for( i in c("CladdingRe") ){
        #   type <- Type$CladdingRe["Type"]
        #   if( type == "value" ){
        #     type <- "Value"
        #     val <- as.numeric( Type$CladdingRe["Val"] )
        #   }
        #   print( parse( text = sprintf("
        #   output$optionTB%s%s%s <-
        #     renderUI({selectInput( 'optTP%s%s%s', '%s',
        #     list( '', 'Table', 'Draw curve', 'Value' ), selected = '%s' ) })"
        #                               ,1,1,1, 1,1,1, paste( "Cladding", "(Re)" ), type ) ) )
        #   eval( parse( text = sprintf("
        #   output$optionTB%s%s%s <-
        #     renderUI({selectInput( 'optTP%s%s%s', '%s',
        #     list( '', 'Table', 'Draw curve', 'Value' ), selected = '%s' ) })"
        #   ,1,1,1, 1,1,1, paste( "Cladding", "(Re)" ), type ) ) )
        #
        #   eval( parse( text = sprintf(
        #   "output$out%s%s%s <- renderUI({
        #     tabPanel( 'Index',
        #       br(),
        #       numericInput( 'selectValue%s%s%s', 'Insert Refractive index', value = %s, 0, 5, 0.00001 ),
        #     )
        #   })", 1,1,1, 1,1,1, val ) ) )
        #
        # }

      })

      #### Dynamic C S K Render UI ####
      observeEvent( list( reactP(), reactL(), reactRI(), reactOptTP1(), reactOptTP2() ), {

        req( length( grep( "optTP", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        choose <- eval( parse( text = sprintf( 'input$optTP%s%s%s', k,i,j ) ) )
        val <- eval( parse( text = sprintf( 'input$selectIndex%s%s%s', k,i,j ) ) )

        req( if( is.null(choose) ){ FALSE }else{ choose == 'Table' } )

        eval( parse( text = sprintf(
          "output$out%s%s%s <- renderUI({
            tabPanel( 'Table',
              br(),
              selectInput( 'selectIndex%s%s%s', 'Select Index', choices = c( '', 1:nrow(RefInd) ) ),
              hr(),
              DTOutput( 'File%s%s%s' )
            )
        })", k,i,j, k,i,j, k,i,j ) ) )

      })

      observeEvent( list( reactP(), reactL(), reactRI(), reactOptTP1(), reactOptTP2() ), {

        req( length( grep( "optTP", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]
        choose <- eval( parse( text = sprintf( 'input$optTP%s%s%s', k,i,j ) ) )
        val <- eval( parse( text = sprintf( 'input$selectIndex%s%s%s', k,i,j ) ) )
        req( if( is.null(choose) ){ FALSE }else{ choose == 'Draw curve' } )

        eval( parse( text = sprintf(
          "output$out%s%s%s <- renderUI({
           fluidPage( fluidRow(
              column( width = 2, style='padding:0px;',
                fluidPage( fluidRow(
                  column( 6, style='padding:0px;', numericInput( 'minY%s%s%s', 'Y axis min', value = 0, 0, 4, 0.01 ) ),
                  column( 6, style='padding:0px;', numericInput( 'maxY%s%s%s', 'Y axis max', value = 4, 0, 4, 0.01 ) ),
                ) ),
                hr(),
                fluidPage( fluidRow(
                  column( 12, style='padding:0px;', numericInput( 'nCoord%s%s%s', 'Number of coordinates', value = 2, 2, 1000, 1 ) )
                ) ),
                hr(),
                fluidPage( fluidRow(
                  column( 6, style='padding:0px;', uiOutput( 'PlotX%s%s%s' ) ),
                  column( 6, style='padding:0px;', uiOutput( 'PlotY%s%s%s' ) )
                ))
              ),
              column( width = 10, style='padding:0px;',
                tabPanel( 'Plot',
                  br(),
                  fluidPage( fluidRow(
                    column( 2, fileInput( 'uplCoords%s%s%s', 'Upload Coordinates', multiple = TRUE,
                               accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv' ) ),
                               textOutput('wrongCoords%s%s%s') ),
                    column( 2, actionButton( 'resetUpload%s%s%s', 'Reset Upload/Change') ),
                    # column( 2, actionButton( 'drawPlot%s%s%s', 'Draw Plot') ),
                    # column( 2, actionButton( 'acceptPlot%s%s%s', 'Accept Plot') ),
                    column( 2, actionButton( 'fitCurve%s%s%s', 'Fit Curve') ),
                    column( 2, uiOutput( 'selectCurve%s%s%s' ) )
                  )),
                  hr(),
                  plotlyOutput( 'Plot%s%s%s' )
                )
              )
           ))
        })", k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j ) ) )

      })

      observeEvent( list( reactP(), reactL(), reactRI(), reactOptTP1(), reactOptTP2(), reactCoord() ), {

        req( length( grep( "optTP", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        eval( parse( text = sprintf(
          "output$PlotX%s%s%s <- renderUI({
            n <- max( as.numeric( input$nCoord%s%s%s ), 2 )
            lambda <- seq( input$waveMin, input$waveMax, by = input$waveStep )
            lambda <- lambda[ round( seq( 2, length(lambda)-1, length.out = n ) , 0 ) ]
            lapply( 1:n, function(x){
              numericInput( paste0( 'Coord%s%s%sX', x ), paste0( 'x_', x ), value = lambda[x], input$waveMin, input$waveMax, input$waveStep )
            })
        })", k,i,j, k,i,j, k,i,j ) ) )
        eval( parse( text = sprintf(
          "output$PlotY%s%s%s <- renderUI({
            n <- max( as.numeric( input$nCoord%s%s%s ), 2 )
            lapply( 1:n, function(y){
              numericInput( paste0( 'Coord%s%s%sY', y ), paste0( 'y_', y ), value = 1, 0, 4, 0.01 )
            })
        })", k,i,j, k,i,j, k,i,j ) ) )
# print("value = 1")
      })

      observeEvent( list( reactResUpl() ), {

        req( length( grep( "resetUpload[[:digit:]]", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        eval( parse( text = sprintf(
          "output$PlotX%s%s%s <- renderUI({
            n <- max( as.numeric( input$nCoord%s%s%s ), 2 )
            lambda <- seq( input$waveMin, input$waveMax, by = input$waveStep )
            lambda <- lambda[ round( seq( 2, length(lambda)-1, length.out = n ) , 0 ) ]
            lapply( 1:n, function(x){
              numericInput( paste0( 'Coord%s%s%sX', x ), paste0( 'x_', x ), value = lambda[x], input$waveMin, input$waveMax, input$waveStep )
            })
        })", k,i,j, k,i,j, k,i,j ) ) )

        eval( parse( text = sprintf(
          "output$PlotY%s%s%s <- renderUI({
            n <- max( as.numeric( input$nCoord%s%s%s ), 2 )
            lapply( 1:n, function(y){
              numericInput( paste0( 'Coord%s%s%sY', y ), paste0( 'y_', y ), value = 1, 0, 4, 0.01 )
            })
        })", k,i,j, k,i,j, k,i,j ) ) )
        updateNumericInput( session, sprintf( "minY%s%s%s", k,i,j ), value = 0 )
        updateNumericInput( session, sprintf( "maxY%s%s%s", k,i,j ), value = 4 )
        updateNumericInput( session, sprintf( "nCoord%s%s%s", k,i,j ), value = 2, min = 2, max = 1000 )
        eval( parse( text = sprintf( "output$selectCurve%s%s%s <- renderText( '' )", k,i,j ) ) )
        Curve[[ k ]][[ i ]][[ j ]] <<- list( Pred = list(), Est = list(), Mod = list() )
        dfCurve[[ k ]][[ i ]][[ j ]] <<- c()
# print("reset")
      })

      observeEvent( list( reactUplCoord() ), {

        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]
        req( eval( parse( text = sprintf( "input$uplCoords%s%s%s", k,i,j ) ) ) )

        df <- read.table( eval( parse( text = sprintf( "input$uplCoords%s%s%s$datapath", k,i,j ) ) ), header = TRUE, sep = ";", check.names = FALSE, stringsAsFactors = TRUE )
        lambda <- seq( input$waveMin, input$waveMax, input$waveStep )

        if( !all( colnames(df) %in% c( "lambda", "Index" ) ) ){
          eval( parse( text = sprintf( "output$wrongCoords%s%s%s <- renderText( 'Wrong file, should have two columns: lambda, Index' )", k,i,j ) ) )
        }else{
          eval( parse( text = sprintf( "output$wrongCoords%s%s%s <- renderText( '' )", k,i,j ) ) )
          n <- nrow( df )
          eval( parse( text = sprintf(
            "output$PlotX%s%s%s <- renderUI({
              lapply( 1:n, function(x){
                XY%s%s%s$x[x] <- df[x,'lambda']
                numericInput( paste0( 'Coord%s%s%sX', x ), paste0( 'x_', x ), value = df[x,'lambda'], 0, Inf, input$waveStep )
              })
            })", k,i,j, k,i,j, k,i,j ) ) )
          eval( parse( text = sprintf(
            "output$PlotY%s%s%s <- renderUI({
              lapply( 1:n, function(y){
                XY%s%s%s$y[y] <- df[y,'Index']
                numericInput( paste0( 'Coord%s%s%sY', y ), paste0( 'y_', y ), value = df[y,'Index'], 0, 4, 0.01 )
              })
            })", k,i,j, k,i,j, k,i,j ) ) )
          updateNumericInput( session, sprintf( "nCoord%s%s%s", k,i,j ), value = n, min = n, max = n )
          yMin <- min( df[,'Index'] ) - 0.1
          yMax <- max( df[,'Index'] ) + 0.1
          updateNumericInput( session, sprintf( "minY%s%s%s", k,i,j ), value = yMin )
          updateNumericInput( session, sprintf( "maxY%s%s%s", k,i,j ), value = yMax )
          # print("Upload")
        }

      })

      observeEvent( list( reactP(), reactL(), reactRI(), reactOptTP1(), reactOptTP2() ), {

        req( length( grep( "optTP", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]
        choose <- eval( parse( text = sprintf( 'input$optTP%s%s%s', k,i,j ) ) )
        req( if( is.null(choose) ){ FALSE }else{ choose == 'Value' } )

        eval( parse( text = sprintf(
          "output$out%s%s%s <- renderUI({
            tabPanel( 'Index',
              br(),
              numericInput( 'selectValue%s%s%s', 'Insert Refractive index', value = 1, 0, 5, 0.00001 ),
            )
        })", k,i,j, k,i,j ) ) )

      })

      #### Dynamic Layers options ####
      observeEvent( input$nLayers, {

        change <- input$changed
        req( input$nLayers != "" & change == "nLayers" )

        output$optLayers <- renderUI({

          k <- 4
          n <- as.numeric( input$nLayers )
          nL <<- n

          lapply( 1:n, function(i){
            list(
              eval( parse( text = sprintf( "selectInput( 'optTP%s%s%s', '%s',
                                          list( '', 'Table', 'Draw curve', 'Value' ),
                                          selected = '' )", k,i,1, sprintf( "Layer %s (Re)", i ) ) ) ),
              eval( parse( text = sprintf( "selectInput( 'optTP%s%s%s', '%s',
                                          list( '', 'Table', 'Draw curve', 'Value' ),
                                          selected = '' )", k,i,2, sprintf( "Layer %s (Im)", i ) ) ) )
            )
          })
        })

      })

      observeEvent( list( reactP(), reactL(), reactRI() ), {

        k <- which( cPanels[-c(1,6)] == input$Panels )
        i <- ifelse( k %in% 1:3, 1, LastChar( reactL() ) )
        j <- MidChar( reactRI() )

        name1 <- sprintf( "optionTB%s%s%s",k,i,1 )
        name2 <- sprintf( "optionTB%s%s%s",k,i,2 )

        req( !( c( name1, name2 ) %in% outputGlob ) )

        eval( parse( text = sprintf("
          output$optionTB%s%s%s <-
            renderUI({selectInput( 'optTP%s%s%s', '%s',
            list( '', 'Table', 'Draw curve', 'Value' ), selected = '' ) })"
        ,k,i,1, k,i,1, paste( reactP(), "(Re)" ) ) ) )

        eval( parse( text = sprintf("
          output$optionTB%s%s%s <-
            renderUI({selectInput( 'optTP%s%s%s', '%s',
            list( '', 'Table', 'Draw curve', 'Value' ), selected = '' ) })"
        ,k,i,2, k,i,2, paste( reactP(), "(Im)" ) ) ) )

        outputGlob <<- c( outputGlob, name1, name2 )

      })

      #### Dynamic Layers thickness ####
      output$dLayers <- renderUI({

        req( input$nLayers != "" )
        n <- as.numeric( input$nLayers )

        lapply( 1:n, function(i){
          eval( parse( text = sprintf( "numericInput( 'dlayer%s', 'Layer %s thickness',
                                        value = 1000, 10, 10000, 1 )", i, i ) ) )
        })

      })

      #### Hide Tabs ####
      observeEvent( input$nLayers, {

        if( is.null(input$nLayers) | if( is.null(input$nLayers) ){ TRUE }else{ input$nLayers == "" } ){
          n <- 0
        }else{
          n <- as.numeric( input$nLayers )
        }
        lapply( (n+1):9, function(i){
          eval( parse( text = sprintf( "hideTab( inputId = 'tabLayers', target = 'Layer %s' )", i ) ) )
        })

      })

      #### Show Tabs ####
      observeEvent( input$nLayers, {

        req( !is.null(input$nLayers) & if( is.null(input$nLayers) ){ FALSE }else{ input$nLayers != "" } )
        n <- as.numeric( input$nLayers )
        lapply( 1:n, function(i){
          eval( parse( text = sprintf( "showTab( inputId = 'tabLayers', target = 'Layer %s' )", i ) ) )
        })

      })

      #### Display Init Plot ####
      observeEvent( list( reactOptTP1(), reactOptTP2(), reactCoord(),
                          lapply( Comb, function(x){ event_data("plotly_relayout", source = sprintf( "PP%s", x ) ) } )), {

        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        add <- FALSE
        if( length( Curve[[ k ]][[ i ]][[ j ]]$Pred ) > 0 ){
          dat1 <- as.data.frame( do.call( "cbind", Curve[[ k ]][[ i ]][[ j ]]$Pred ) )
          # dat1[ dat1 < 0 ] <- 0
          # dat1[ dat1 > 4 ] <- 4
          dat <- data.frame( x = seq( input$waveMin, input$waveMax, by = input$waveStep ), dat1 )
          add <- TRUE
        }

        eval( parse( text = sprintf(
          "output$Plot%s%s%s <- renderPlotly({
             n <- 1:max( as.numeric( input$nCoord%s%s%s ), 2 )
             XX <- XY%s%s%s$x[ n ]
             YY <- XY%s%s%s$y[ n ]
             circles <- map2( XX, YY, ~list( type = 'circle', xanchor = .x, yanchor = .y,
                              x0 = -4, x1 = 4, y0 = -4, y1 = 4, xsizemode = 'pixel',
                              ysizemode = 'pixel', fillcolor = 'blue', line = list(color = 'ransparent') ) )
             req( !is.null(XX[[length(XX)]]) )
             req( !anyNA(XX) & !anyNA(YY) )
             App <- tryCatch( approxExtrap( XX, YY, c( input$waveMin, XX, input$waveMax) ),
                    error = function(e){ list( x = 1, y = 1) })
             p <- plot_ly( height = 500, source = 'PP%s%s%s' ) %%>%%
               layout( shapes = circles,
                  xaxis = list( range = c( input$waveMin, input$waveMax ), title = 'Wavelength' ),
                  yaxis = list( range = c( input$minY%s%s%s, input$maxY%s%s%s ), title = 'Refractive' ),
                  legend = list( orientation = 'h', xanchor = 'center', x = 0.5, y = 1.05 ) ) %%>%%
               config( edits = list( shapePosition = TRUE ) ) %%>%% event_register('plotly_relayout') %%>%%
               add_lines(x = App$x, y = App$y, color = I('red'), name = 'InterPol')
             if( add ){
               for( i in 2:ncol(dat) ){
                 p <- p %%>%% add_lines( x = dat$x, y = dat[,i], line = list( dash = 'dash' ), name = paste0( 'Poly^', i-1 ) )
               }
             }
             p
        })", k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j ) ) )
# print("Init plot")
      })

      observeEvent( lapply( Comb, function(x){ event_data("plotly_relayout", source = sprintf( "PP%s", x ) ) } ), {

        req( reactRI() != 0 )
        kij <- pointKIJ( reactRI() )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        ed <- event_data( "plotly_relayout", source = sprintf( "PP%s%s%s", k,i,j ) )
        shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
        if (length(shape_anchors) != 2) return()
        row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
        pts <- as.numeric(shape_anchors)
        precX <- nchar( gsub( "\\.|[1-9]", "", as.character( input$waveStep ) ) )
        eval( parse( text = sprintf( "XY%s%s%s$x[row_index] <- round(pts[1],precX)", k,i,j ) ) )
        eval( parse( text = sprintf( "XY%s%s%s$y[row_index] <- round(pts[2],5)", k,i,j ) ) )
        updateNumericInput( session, sprintf( "Coord%s%s%sX%s", k,i,j, row_index ), value = round(pts[1],precX) )
        updateNumericInput( session, sprintf( "Coord%s%s%sY%s", k,i,j, row_index ), value = round(pts[2],5) )
# print("plot")
      })

      #### Display Init Table ####
      observeEvent( list( reactOptTP1(), reactOptTP2(), reactRI() ), {

        req( length( grep( "optTP", input$changed ) ) == 1 )

        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        val <- eval( parse( text = sprintf( "input$optTP%s%s%s", k,i,j ) ) )
        req( val == "Table" )
        eval( parse( text = sprintf( "output$File%s%s%s <- renderDT( RefInd )", k,i,j ) ) )

      })

      #### Fit Curves ####
      observeEvent( FitPlot( input, reactL() ), {

        req( length( grep( "fitCurve", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]

        n <- 1:max( as.numeric( eval( parse( text = sprintf( "input$nCoord%s%s%s", k,i,j ) ) ) ), 2 )
        XX <- eval( parse( text = sprintf( "XY%s%s%s$x[ n ]", k,i,j ) ) )
        YY <- eval( parse( text = sprintf( "XY%s%s%s$y[ n ]", k,i,j ) ) )
        df <- data.frame( Wavelength = XX, Refractive = YY )

        Curve[[ k ]][[ i ]][[ j ]] <<- EstCurve( df, as.numeric( input$waveMin ),
                                                 as.numeric( input$waveMax ),
                                                 as.numeric( input$waveStep ) )
        dfCurve[[ k ]][[ i ]][[ j ]] <<- df
        n <- length( Curve[[ k ]][[ i ]][[ j ]]$Est )

        if( n > 0 ){

          dat1 <- as.data.frame( do.call( "cbind", Curve[[ k ]][[ i ]][[ j ]]$Pred ) )
          # dat1[ dat1 < 0 ] <- 0
          # dat1[ dat1 > 4 ] <- 4
          dat <- data.frame( x = seq( input$waveMin, input$waveMax, by = input$waveStep ), dat1 )

          eval( parse( text = sprintf(
            "output$Plot%s%s%s <- renderPlotly({
             n <- 1:max( as.numeric( input$nCoord%s%s%s ), 2 )
             XX <- XY%s%s%s$x[ n ]
             YY <- XY%s%s%s$y[ n ]
             circles <- map2( XX, YY, ~list( type = 'circle', xanchor = .x, yanchor = .y,
                              x0 = -4, x1 = 4, y0 = -4, y1 = 4, xsizemode = 'pixel',
                              ysizemode = 'pixel', fillcolor = 'blue', line = list(color = 'ransparent') ) )
             req( !is.null(XX[[length(XX)]]) )
             req( !anyNA(XX) & !anyNA(YY) )
             App <- tryCatch( approxExtrap( XX, YY, seq( input$waveMin, input$waveMax, input$waveStep ) ),
                    error = function(e){ list( x = 1, y = 1) })
             assign( 'App%s%s%s', App, envir = .GlobalEnv )
             p <- plot_ly( height = 500, source = 'PP%s%s%s' ) %%>%%
               layout( shapes = circles,
                  xaxis = list( range = c( input$waveMin, input$waveMax ), title = 'Wavelength' ),
                  yaxis = list( range = c( input$minY%s%s%s, input$maxY%s%s%s ), title = 'Refractive' ),
                  legend = list( orientation = 'h', xanchor = 'center', x = 0.5, y = 1.05 ) ) %%>%%
               config( edits = list( shapePosition = TRUE ) ) %%>%% event_register('plotly_relayout') %%>%%
               add_lines(x = App$x, y = App$y, color = I('red'), name = 'InterPol')
             for( i in 2:ncol(dat) ){
               p <- p %%>%% add_lines( x = dat$x, y = dat[,i], line = list( dash = 'dash' ), name = paste0( 'Poly^', i-1 ) )
             }
             p
          })", k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j, k,i,j ) ) )
          eval( parse( text = sprintf( "output$selectCurve%s%s%s <- renderUI( selectInput( 'selectCurve%s%s%s', 'Select Curve', choices = c( '', c( 'InterPol', paste0( 'Poly^', 1:n )) ), selected = '' ) )", k,i,j, k,i,j ) ) )

        }else{
          eval( parse( text = sprintf(
            "n <- 1:max( as.numeric( input$nCoord%s%s%s ), 2 )
             XX <- XY%s%s%s$x[ n ]
             YY <- XY%s%s%s$y[ n ]
             req( !is.null(XX[[length(XX)]]) )
             req( !anyNA(XX) & !anyNA(YY) )
             App <- tryCatch( approxExtrap( XX, YY, seq( input$waveMin, input$waveMax, input$waveStep ) ),
                    error = function(e){ list( x = 1, y = 1) })
             assign( 'App%s%s%s', App, envir = .GlobalEnv )", k,i,j, k,i,j, k,i,j, k,i,j ) ) )
          eval( parse( text = sprintf( "output$selectCurve%s%s%s <- renderUI( selectInput( 'selectCurve%s%s%s', 'Select Curve', choices = c( '', 'InterPol' ), selected = '' ) )", k,i,j, k,i,j ) ) )
        }

      })

      #### Accept Index ####
      observeEvent( SelectIndex( input, reactL() ), {

        req( length( grep( "selectIndex", input$changed ) ) == 1 )

        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]
        val <- as.numeric( eval( parse( text = sprintf( "input$selectIndex%s%s%s", k,i,j ) ) ) )
        req( val != "" )
        df <- data.frame( Index = rep( RefInd[ val, "Index" ], length( seq( input$waveMin,
                                                                            input$waveMax, by = input$waveStep ) ) ) )
        LayersList[[ k ]][[ i ]][[ j ]] <<- df[,1]
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "table", "Time" ] <<- Sys.time()
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "table", "Val" ] <<- val
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "table", "Type" ] <<- "value"

      })

      #### Accept Curve ####
      observeEvent( SelectCurve( input, reactL() ), {

        req( length( grep( "selectCurve", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]
        val <- eval( parse( text = sprintf( "input$selectCurve%s%s%s", k,i,j ) ) )
        req( val != "" )
        if( val == "InterPol" ){
          app <- eval( parse( text = sprintf( "App%s%s%s", k,i,j ) ) )
          df <- app$y
          val <- 0
        }else{
          v <- as.numeric( gsub( "[^[:digit:]]", "", val ) )
          df <- Curve[[ k ]][[ i ]][[ j ]]$Pred[[v]]
          val <- v
        }
        if( val > 0 ){
          attr( df, "coef" ) <- coefficients( Curve[[ k ]][[ i ]][[ j ]]$Mod[[v]] )
        }
        LayersList[[ k ]][[ i ]][[ j ]] <<- df
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "curve", "Time" ] <<- Sys.time()
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "curve", "Val" ] <<- val
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "curve", "Type" ] <<- ifelse( val == 0, "inter", "poly" )

      })
      #### Accept Value ####
      observeEvent( SelectValue( input, reactL() ), {

        req( length( grep( "selectValue", input$changed ) ) == 1 )
        kij <- KIJ( input$changed )
        k <- kij[3]
        i <- kij[2]
        j <- kij[1]
        val <- as.numeric( eval( parse( text = sprintf( "input$selectValue%s%s%s", k,i,j ) ) ) )
        req( val != "" )
        df <- data.frame( Index = rep( val, length( seq( input$waveMin, input$waveMax, by = input$waveStep ) ) ) )
        LayersList[[ k ]][[ i ]][[ j ]] <<- df[,1]
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "value", "Time" ] <<- Sys.time()
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "value", "Val" ] <<- val
        check[ check[,"indx"] == paste0(k,i,j) & check[,"Which"] == "value", "Type" ] <<- "value"

      })

      #### CladdingIm SensingIm CoreIm override ####
      observe({
        n <- length( seq( input$waveMin, input$waveMax, by = input$waveStep ) )
        LayersList[[ 1 ]][[ 1 ]][[ 2 ]] <<- rep( 0, n )
        LayersList[[ 2 ]][[ 1 ]][[ 2 ]] <<- rep( 0, n )
        LayersList[[ 3 ]][[ 1 ]][[ 2 ]] <<- rep( 0, n )
        check[ check[,"indx"] %in% c("112","212","312") & check[,"Which"] == "value", "Time" ] <<- Sys.time()
        check[ check[,"indx"] %in% c("112","212","312") & check[,"Which"] == "value", "Val" ] <<- 0
        check[ check[,"indx"] %in% c("112","212","312") & check[,"Which"] == "value", "Type" ] <<- "value"
      })

      #### Run Modelling ####
      observeEvent( input$StartModel, {

        Layers <- do.call( "c", do.call( "c", LayersList ) )
        DFCurve <- do.call( "c", do.call( "c", dfCurve ) )
        names( Layers ) <- c( paste0( rep( c("Cladding", "Sensing", "Core"), each=2 ), c("Re","Im") ),
                              paste0( rep( paste0( "Layers", 1:8 ), each=2 ), c("Re","Im") ) )
        names( DFCurve ) <- names( Layers )

        Re_clad_core <- all( Layers$CladdingRe < Layers$CoreRe )
        # Im_clad_core <- any( CladdingIm != CoreIm )
        Im_clad_core <- all( Layers$CladdingIm == 0 & Layers$CoreIm == 0 )
        Im_layers <- any( unlist( lapply( Layers[ grep( "^Layers[[:digit:]]Im", names(Layers) ) ], function(x){ any( x!=0 ) } ) ) )

        Ok <- aggregate( check$Time, list( check$Name ), function(x){ sum( x == "" ) != 3 } )
        Ok <- Ok[ !Ok$Group.1 %in% grep( sprintf( "Layers [%s-8]", nL+1), Ok$Group.1, value = TRUE ), ]
        # Ok <- Ok[ !( Ok$Group.1 %in% c( "Cladding (Ok)", "Core (Ok)" ) ), ]
        out <- Ok[ Ok[,2] == FALSE, 1 ]

        last <- split( check, list( check[,"Name"] ) )
        type <- lapply( last, function(x){ unlist( x[ which.max( x[,"Time"] ), c( "Type", "Val" ) ] ) } )
        names( type ) <- gsub( " |\\(|\\)", "", names( type ) )
        type <- lapply( type, function(x){ if( length(x) != 0 ){ x }else{ NULL } } )

        output$result <- renderUI({
          if( length(out) == 0 & Re_clad_core & Im_clad_core & Im_layers ){
            tabPanel( 'Plot',
              br(),
              renderText( "Modelling......." )
            )
          }else{
            tabPanel( 'Plot',
              br(),
              renderText( if( length(out) != 0 ){ sprintf( "Please make a decision regarding the following: %s.", paste( out, collapse = ", ") ) }else{ "" } ),
              br(),
              renderText( if( !Re_clad_core ){ "Following condition is forbidden: Re(Cladding) >= Re(Core)." }else{ "" } ),
              br(),
              renderText( if( !Im_clad_core ){ "Im(Cladding) and Im(Core) should be 0." }else{ "" } ),
              br(),
              renderText( if( !Im_layers ){ "At least one layer should have Im(Layer) > 0." }else{ "" } )
            )
          }
        })

        req( length(out) == 0 & Re_clad_core & Im_clad_core & Im_layers )

        Init <- vector( "list", 33 )
        varMin <- varMax <- Type <- Prec <- Init
        names( Init ) <- names( Prec ) <- names( Type ) <- names( varMin ) <- names( varMax ) <-
          c("temperature","coreD","L", paste0( "thickLayers", 1:8 ),
            paste0( rep( c("Cladding", "Sensing", "Core"), each=2 ), c("Re","Im") ),
            paste0( rep( paste0( "Layers", 1:8 ), each=2 ), c("Re","Im") ) )

        Type[ c("temperature","coreD","L", paste0( "thickLayers", 1:nL ) ) ] <- "value"
        Type[ names(type) ] <- type

        Init$temperature <- input$temperature
        Init$coreD <- input$coreD
        Init$L <- input$L

        thickLayers <- unlist( lapply( 1:nL, function(i){ eval( parse( text = paste0( "input$dlayer", i ) ) ) } ) )
        Init[ paste0( "thickLayers", 1:nL ) ] <- thickLayers
        thickLayers <- unlist( thickLayers )

        Init[ grep( "^Cladding|^Sensing|^Core|^Layers[[:digit:]]", names(Init) ) ] <- Layers

        Layers <- Layers[ !unlist( lapply( Layers, is.null ) ) ]

        LayersDf <- vector( "list", length( unique( gsub( "Re|Im", "", names(Layers) ) ) ) )
        names(LayersDf) <- unique( gsub( "Re|Im", "", names(Layers) ) )
        for( i in unique( gsub( "Re|Im", "", names(Layers) ) ) ){

          LayersDf[[i]] <- complex( real = Layers[[ sprintf( "%sRe", i ) ]], imaginary = Layers[[ sprintf( "%sIm", i ) ]] )

        }

        LayersDf <- data.frame( lambda = seq( input$waveMin, input$waveMax, by = input$waveStep ), do.call( "cbind", LayersDf ) )

        start <- Sys.time()
        Result <<- shinyLMR( input$waveMin, input$waveMax, input$waveStep, input$coreD,
                             input$L, thickLayers, LayersDf, input$angleMax )
        end <- difftime( Sys.time(), start, units = "secs" )
        attr( Result, "time" ) <- as.numeric( end )
# print(Result)
        if( all( is.na( Result$Transmittance ) ) ){
          output$PlotResErr <- renderText( "All values returned by the model are NaN." )
        }

        output$PlotRes <- renderPlotly({
             XX <- seq( input$waveMin, input$waveMax, by = input$waveStep )
             YY <- Result$Transmittance
             circles <- map2( XX, YY, ~list( type = 'circle', xanchor = .x, yanchor = .y,
                              x0 = -4, x1 = 4, y0 = -4, y1 = 4, xsizemode = 'pixel',
                              ysizemode = 'pixel', fillcolor = 'blue', line = list(color = 'ransparent') ) )
             req( !is.null(XX[[length(XX)]]) )
             req( !anyNA(XX) & !anyNA(YY) ) # req( !anyNA(XX) & !anyNA(YY) )
             App <- tryCatch( approxExtrap( XX, YY, c( input$waveMin, XX, input$waveMax) ),
                    error = function(e){ list( x = 1, y = 1) })
             p <- plot_ly( height = 500, source = 'rPP' ) %>%
               add_lines(x = App$x, y = App$y, color = I('red')) %>%
               layout( shapes = circles,
                  xaxis = list( title = 'Wavelength' ),
                  yaxis = list( title = 'Transmittance' ) ) %>%
               config( edits = list( shapePosition = TRUE ) ) %>% event_register('plotly_relayout')
             p
        })

        output$result <- renderUI({
          tabPanel( 'Plot',
            br(),
            br(),
            if( all( is.na( Result$Transmittance ) ) ){
              textOutput( 'PlotResErr' )
            }else{
              plotlyOutput( 'PlotRes' )
            }
          )
        })

        Parameters <- data.frame( waveMin = input$waveMin, waveMax = input$waveMax, waveStep = input$waveStep,
                                  temperature = input$temperature, coreDiameter = input$coreD,
                                  LengthModifiedRegion = input$L, ThicknessCoatingLayers = thickLayers,
                                  angleMax = input$angleMax )

        sapply( c("Result","Parameters","LayersDf","DFCurve"), function(x){ assign(x, get(x), envir = .GlobalEnv) } )
        sapply( c("varMin","varMax","Type","Prec","Init"), function(x){ assign(x, get(x), envir = .GlobalEnv) } )

        shinyjs::show("AreaSave")

      })

      #### Save Model ####
      output$save <- downloadHandler(

        filename = function(){
          paste0( "LMR-", Sys.time(), ".zip" )
        },

        content = function( file ){
          fs <- c()
          currdir <- getwd()
          tmpdir <- tempdir()
          setwd( tempdir() )
          write.table( Result, "Result.txt", sep = ";", row.names = FALSE )
          write.table( Parameters, "Parameters.txt", sep = ";", row.names = FALSE )
          write.table( LayersDf, "Layers.txt", sep = ";", row.names = FALSE )
          cols <- colnames(LayersDf)[-1]
          for( i in cols ){
            write.table( data.frame( lambda = LayersDf[,1], Index = Re( LayersDf[,i] ) ),
                         sprintf( "%sRe.txt", i ), sep = ";", row.names = FALSE )
            write.table( data.frame( lambda = LayersDf[,1], Index = Im( LayersDf[,i] ) ),
                         sprintf( "%sIm.txt", i ), sep = ";", row.names = FALSE )
          }
          save( Result, Init, varMin, varMax, Prec, Type, Parameters, file = "Model.RData" )
          fs <- c( "Result.txt", "Parameters.txt", "Layers.txt", "Model.RData",
                   paste0( cols, rep( c("Re","Im"), each = length(cols) ), ".txt" ) )
          DFCurve <- DFCurve[ !unlist( lapply( DFCurve, is.null ) ) ]
          if( length(DFCurve) > 1 ){
            for( i in names(DFCurve) ){
              write.table( data.frame( lambda = DFCurve[[i]][,1], Index = DFCurve[[i]][,2] ),
                           sprintf( "raw%s.txt", i ), sep = ";", row.names = FALSE )
            }
            fs <- c( fs, paste0( "raw", names(DFCurve), ".txt" ) )
          }
          zip( zipfile = file, files = fs )
          setwd( currdir )
        },
        contentType = "application/zip"
      )

      #### Save Changed Model ####
      output$saveChanged <- downloadHandler(

        filename = function(){
          paste0( "LMR-", Sys.time(), ".zip" )
        },

        content = function( file ){
          fs <- c()
          currdir <- getwd()
          tmpdir <- tempdir()
          setwd( tempdir() )
          ResultOpt <- data.frame( Wavelength = oXY$x, Transmittance = oXY$y )
          save( Result, ResultOpt, Init, varMin, varMax, Prec, Type, Parameters, file = "Model.RData" )
          fs <- c( "Model.RData" )
          zip( zipfile = file, files = fs )
          setwd( currdir )
        },
        contentType = "application/zip"
      )

      #### Reverse LMR Optimization ####
      observeEvent( input$UploadModel, {
        req( length( grep( "\\.RData", input$UploadModel$name ) ) != 0 )
        if( exists( "ResultOpt", envir = .GlobalEnv ) ){
          rm( "ResultOpt", envir = .GlobalEnv )
        }
        load( input$UploadModel$datapath, envir = .GlobalEnv )
      })
      observeEvent( input$UploadModel, {
        if( length( grep( "\\.RData", input$UploadModel$name ) ) == 0 ){
          output$wrongFile <- renderText( "Wrong file, should be *.RData" )
        }else{
          output$wrongFile <- renderText( "" )
        }
      })
      observeEvent( list( input$UploadModel ), {
        req( input$UploadModel )
        req( length( grep( "\\.RData", input$UploadModel$name ) ) != 0 )
        req( exists( "Init" ) )
        initNam <- names( Init[ !unlist( lapply( Init, is.null ) ) ] )
        initNam <- initNam[-which( initNam %in% c( "temperature","CladdingIm","SensingIm","CoreIm" ) )] # adjust for temperature it can be removed
        updateSelectInput( session, "varOpt", choices = initNam, selected = "" )
        yMin <- round( min( c( Result[,'Transmittance'],
                            if( exists( "ResultOpt", envir = .GlobalEnv ) ){ ResultOpt[,'Transmittance'] }else{ NULL } ) - 0.1 ), 2 )
        yMax <- round( max( c( Result[,'Transmittance'],
                            if( exists( "ResultOpt", envir = .GlobalEnv ) ){ ResultOpt[,'Transmittance'] }else{ NULL } ) + 0.1 ), 2 )
        updateNumericInput( session, "minYo", value = yMin )
        updateNumericInput( session, "maxYo", value = yMax )
      })
      observeEvent( list( input$InsertModel ), {
        req( input$InsertModel )
        req( exists("Init") )
        initNam <- names( Init[ !unlist( lapply( Init, is.null ) ) ] )
        initNam <- initNam[-which( initNam %in% c( "temperature","CladdingIm","SensingIm","CoreIm" ) )] # adjust for temperature
        updateSelectInput( session, "varOpt", choices = initNam, selected = "" )
        yMin <- round( min( c( Result[,'Transmittance'],
                               if( exists( "ResultOpt", envir = .GlobalEnv ) ){ ResultOpt[,'Transmittance'] }else{ NULL } ) - 0.1 ), 2 )
        yMax <- round( max( c( Result[,'Transmittance'],
                               if( exists( "ResultOpt", envir = .GlobalEnv ) ){ ResultOpt[,'Transmittance'] }else{ NULL } ) + 0.1 ), 2 )
        updateNumericInput( session, "minYo", value = yMin )
        updateNumericInput( session, "maxYo", value = yMax )
      })

      observeEvent( input$repType, {
        if(  input$repType == "binary" ){
          shinyjs::show("AreaEnc")
        }else{
          shinyjs::hide("AreaEnc")
        }
      })
      observeEvent( input$fitFun, {
        if(  length( grep( "weighted", input$fitFun ) ) == 0 ){
          hideTab( inputId = 'OptVars', target = "Weighted Fitness Function" )
        }else{
          showTab( inputId = 'OptVars', target = "Weighted Fitness Function" )
        }
      })

      output$OptVarsMin <- renderUI({
        vars <- input$varOpt
        lapply( vars, function(i){
          if( Type[[ i ]][1] == "poly" ){
            eval( parse( text = sprintf( "numericInput( 'OptVarsMin%s', '%s Min in %%', value = -0.1, -1, 0, 0.01 )", i, i ) ) )
          }else{
            Min <- max( min( Init[[ i ]] ) - ( min( Init[[ i ]] ) * 0.1 ), 0 )
            eval( parse( text = sprintf( "numericInput( 'OptVarsMin%s', '%s Min', value = %s, 0, Inf, 0.01 )", i, i, Min ) ) )
          }
        })
      })
      output$OptVarsMax <- renderUI({
        vars <- input$varOpt
        lapply( vars, function(i){
          if( Type[[ i ]][1] == "poly" ){
            eval( parse( text = sprintf( "numericInput( 'OptVarsMax%s', '%s Max in %%', value = 0.1, 0, 1, 0.01 )", i, i ) ) )
          }else{
            Max <- max( Init[[ i ]] ) + ( max( Init[[ i ]] ) * 0.1 )
            eval( parse( text = sprintf( "numericInput( 'OptVarsMax%s', '%s Max', value = %s, 0, Inf, 0.01 )", i, i, Max ) ) )
          }
        })
      })
      output$OptVarsPrec <- renderUI({
        vars <- input$varOpt
        lapply( vars, function(i){
          if( Type[[ i ]][1] == "poly" ){
            eval( parse( text = sprintf( "numericInput( 'OptVarsPrec%s', '%s Step', value = 0.05, 0.01, 1, 0.01 )", i, i ) ) )
          }else{
            eval( parse( text = sprintf( "numericInput( 'OptVarsPrec%s', '%s Precison', value = 1, 0.001, 1, 0.001 )", i, i ) ) )
          }
        })
      })
      output$OptVarsForb <- renderUI({
        vars <- input$varOpt
        lapply( vars, function(i){
          ok <- eval( parse( text = sprintf( 'input$OptVarsMin%s', i ) ) ) <=
            eval( parse( text = sprintf( 'input$OptVarsMax%s', i ) ) )
          ok <- ifelse( ok, "", " (caution! Min > Max)" )
          if( Type[[ i ]][1] == "inter" ){
            HTML( sprintf( "<span style=\"color:red\"><br>interpolation%s<br><br></span>", ok ) )
          }else if( Type[[ i ]][1] == "poly" ){
            HTML( sprintf( "<br>polynomial%s ^%s<br><br>", ok, Type[[ i ]][2] ) )
          }else{
            HTML( sprintf( "<br><br>value%s<br><br>", ok ) )
          }
        })
      })
      observeEvent( list( reactOptVars(), input$varOpt ), {
        req( !is.null( input$changed ) )
        req( !is.null( eval( parse( text = sprintf( "input$OptVarsMin%s",  input$varOpt ) ) ) ) )
        varMin[input$varOpt] <<- sapply( input$varOpt, function(x){ eval( parse( text = sprintf( "input$OptVarsMin%s", x ) ) ) }, simplify = FALSE )
        varMax[input$varOpt] <<- sapply( input$varOpt, function(x){ eval( parse( text = sprintf( "input$OptVarsMax%s", x ) ) ) }, simplify = FALSE )
        Prec[input$varOpt] <<- sapply( input$varOpt, function(x){ eval( parse( text = sprintf( "input$OptVarsPrec%s", x ) ) ) }, simplify = FALSE )
      })

      observeEvent( input$nFitFun, {
        output$FitFunMin <- renderUI({
          n <- as.numeric( input$nFitFun )
          lapply( 1:n, function(x){
            cuts <- cut( c(input$waveMin, input$waveMax), n )
            cuts <- lapply( strsplit( gsub( "\\(","", levels( cuts ) ), "]" ),function(x){ as.numeric( unlist( strsplit( x, "," ) ) ) } )
            numericInput( paste0( 'FitFunMin', x ), paste0( 'start_', x ), value = cuts[[x]][1],
                          input$waveMin, input$waveMax, input$waveStep )
          })
        })
        output$FitFunMax <- renderUI({
          n <- as.numeric( input$nFitFun )
          lapply( 1:n, function(x){
            cuts <- cut( c(input$waveMin, input$waveMax), n )
            cuts <- lapply( strsplit( gsub( "\\(","", levels( cuts ) ), "]" ),function(x){ as.numeric( unlist( strsplit( x, "," ) ) ) } )
            numericInput( paste0( 'FitFunMax', x ), paste0( 'end_', x ), value = cuts[[x]][2],
                          input$waveMin, input$waveMax, input$waveStep )
          })
        })
        output$FitFunVal <- renderUI({
          n <- as.numeric( input$nFitFun )
          lapply( 1:n, function(x){
            numericInput( paste0( 'FitFunVal', x ), paste0( 'weight_', x ), value = 1, 0, 10, 1 )
          })
        })
      })
      observeEvent( list( reactFitFun() ), {
        # print( input$changed )
        req( !is.null( input$changed ) )
        change <- eval( parse( text = sprintf( "input$%s", input$changed ) ) )
        n <- as.numeric( gsub( "[^[:digit:]]", "", input$changed ) )
        if( length( grep( "Min", input$changed ) ) == 1 ){
          updateNumericInput( session, sprintf( "FitFunMax%s", n-1 ), value = change )
        }
        if( length( grep( "Max", input$changed ) ) == 1 ){
          updateNumericInput( session, sprintf( "FitFunMin%s", n+1 ), value = change )
        }
      })
      observeEvent( list( reactFitFun(), input$nFitFun ), {
        n <- as.numeric( input$nFitFun )
        req( !is.null( eval( parse( text = sprintf( "input$FitFunMin%s", n ) ) ) ) )
        df <- data.frame( s = sapply( 1:n, function(x){ eval( parse( text = sprintf( "input$FitFunMin%s", x ) ) ) } ),
                          e = sapply( 1:n, function(x){ eval( parse( text = sprintf( "input$FitFunMax%s", x ) ) ) } ) )
        weight <- sapply( 1:n, function(x){ eval( parse( text = sprintf( "input$FitFunVal%s", x ) ) ) } )
        assign( "cuts", c( input$waveMin, df[,2] ), envir = .GlobalEnv )
        assign( "weight", weight, envir = .GlobalEnv )
      })

      observeEvent( list( input$UploadModel ), {

        req( input$UploadModel )
        req( length( grep( "RData", input$UploadModel$name ) ) == 1 )
        req( !is.null( "Result" ) )

        output$OptPlot <- renderUI({
          fluidPage( fluidRow(
            column( width = 2, style='padding:0px;',
              fluidPage( fluidRow(
                column( 6, style='padding:0px;', numericInput( 'minYo', 'Y axis min', value = 0, -Inf, Inf, 0.01 ) ),
                column( 6, style='padding:0px;', numericInput( 'maxYo', 'Y axis max', value = 0, -Inf, Inf, 0.01 ) ),
              ) ),
              hr(),
              fluidPage( fluidRow(
                column( 6, style='padding:0px;', uiOutput( 'oPlotX' ) ),
                column( 6, style='padding:0px;', uiOutput( 'oPlotY' ) )
              ))
            ),
            column( width = 10, style='padding:0px;',
              plotlyOutput( 'PlotO' )
            )
          ))
        })

        if( exists( "ResultOpt", envir = .GlobalEnv ) ){
          Result <- ResultOpt
        }

        output$oPlotX <- renderUI({
          n <- nrow( Result )
          lapply( 1:n, function(x,res){
            prec <- nchar( gsub("\\.|[1:9]", "", as.character( input$waveStep ) ) )
            numericInput( paste0( 'oCoordX', x ), paste0( 'x_', x ), value = round( res[x], prec ),
                          input$waveMin, input$waveMax, prec )
          }, res = Result$Wavelength )
        })
        output$oPlotY <- renderUI({
          n <- nrow( Result )
          lapply( 1:n, function(x,res){
            prec <- 5
            numericInput( paste0( 'oCoordY', x ), paste0( 'y_', x ), value = round( res[x], prec ), -Inf, Inf, 10^-prec )
          }, res = Result$Transmittance )
        })

      })
      observeEvent( list( input$InsertModel ), {

        req( input$changed == "InsertModel" )
        req( !is.null(Result) )

        output$OptPlot <- renderUI({
          fluidPage( fluidRow(
            column( width = 2, style='padding:0px;',
                    fluidPage( fluidRow(
                      column( 6, style='padding:0px;', uiOutput( 'oPlotX' ) ),
                      column( 6, style='padding:0px;', uiOutput( 'oPlotY' ) )
                    ))
            ),
            column( width = 10, style='padding:0px;',
              plotlyOutput( 'PlotO' )
            )
          ))
        })

        output$oPlotX <- renderUI({
          n <- nrow( Result )
          lapply( 1:n, function(x,res){
            prec <- nchar( gsub("\\.|[1:9]", "", as.character( input$waveStep ) ) )
            numericInput( paste0( 'oCoordX', x ), paste0( 'x_', x ), value = round( res[x], prec ),
                          input$waveMin, input$waveMax, prec )
          }, res = Result$Wavelength )
        })
        output$oPlotY <- renderUI({
          n <- nrow( Result )
          lapply( 1:n, function(x,res){
            prec <- 5
            numericInput( paste0( 'oCoordY', x ), paste0( 'y_', x ), value = round( res[x], prec ), -Inf, Inf, 10^-prec )
          }, res = Result$Transmittance )
        })

      })

      if( exists( "ResultOpt", envir = .GlobalEnv ) ){
        oXY <- reactiveValues( x = ResultOpt$Wavelength, y = ResultOpt$Transmittance )
      }else{
        oXY <- reactiveValues( x = Result$Wavelength, y = Result$Transmittance )
      }
      observe({
        # oXY$x = sapply( 1:if( is.null(Result) ){ 1 }else{ nrow( Result ) }, function(i){
        #   eval( parse( text = sprintf( "input$oCoordX%s", i ) ) ) } )
        oXY$x = sapply( 1:if( is.null(Result) ){ 1 }else{ nrow( Result ) }, function(i){
          Result$Wavelength[i] } )
        oXY$y = sapply( 1:if( is.null(Result) ){ 1 }else{ nrow( Result ) }, function(i){
          eval( parse( text = sprintf( "input$oCoordY%s", i ) ) ) } )
      })
      output$PlotO <- renderPlotly({
        circles <- map2( oXY$x, oXY$y, ~list( type = 'circle', xanchor = .x, yanchor = .y,
                                          x0 = -4, x1 = 4, y0 = -4, y1 = 4, xsizemode = 'pixel',
                                          ysizemode = 'pixel', fillcolor = 'blue', line = list(color = 'ransparent') ) )
        req( !is.null(oXY$x[[length(oXY$x)]]) )
        req( !anyNA(oXY$x) & !anyNA(oXY$y) )
        App <- approxExtrap( oXY$x, oXY$y, c( oXY$x ) )
        p <- plot_ly( height = 500, source = 'oPP' ) %>%
          add_lines(x = App$x, y = App$y, color = I('red'), name = "New Model" ) %>%
          add_lines(x = Result$Wavelength, y = Result$Transmittance, color = I('blue'), name = "Original Model" ) %>%
          layout( shapes = circles,
                  xaxis = list( title = 'Wavelength' ),
                  yaxis = list( range = c( input$minYo, #- abs( input$minYo ) * 0.3,
                                           input$maxYo ),#+ abs( input$maxYo ) * 0.3 ),
                                title = 'Transmittance' ),
                  legend = list( orientation = 'h', xanchor = 'center', x = 0.5, y = 1.05 ) ) %>%
          config( edits = list( shapePosition = TRUE ) ) %>% event_register('plotly_relayout')
      })

      observeEvent( event_data("plotly_relayout", source = "oPP" ), {
        ed <- event_data("plotly_relayout", source = "oPP" )
        shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
        if (length(shape_anchors) != 2) return()
        row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
        pts <- as.numeric(shape_anchors)
        # precX <- nchar( gsub( "\\.|[1-9]", "", as.character( input$waveStep ) ) )
        # oXY$x[row_index] <- round( pts[1], precX )
        oXY$y[row_index] <- round( pts[2], 5 )
        # updateNumericInput( session, sprintf( "oCoordX%s", row_index ), value = round( pts[1], precX ) )
        updateNumericInput( session, sprintf( "oCoordY%s", row_index ), value = round( pts[2], 5 ) )
      })

      observe({
        if( is.null( input$varOpt ) ){
          output$optTime <- renderText( "" )
        }else{
          comb <- 0
          start <- all( unlist( lapply( input$varOpt, function(x){
            out <- eval( parse( text = sprintf( "input$OptVarsMin%s", x ) ) )
            ifelse( is.null(out), FALSE, ifelse( is.na(out), FALSE, TRUE ) ) } ) ) )
          stop <- all( unlist( lapply( input$varOpt, function(x){
            out <- eval( parse( text = sprintf( "input$OptVarsMax%s", x ) ) )
            ifelse( is.null(out), FALSE, ifelse( is.na(out), FALSE, TRUE ) ) } ) ) )
          prec <- all( unlist( lapply( input$varOpt, function(x){
            out <- eval( parse( text = sprintf( "input$OptVarsPrec%s", x ) ) )
            ifelse( is.null(out), FALSE, ifelse( is.na(out), FALSE, TRUE ) ) } ) ) )
          if( start & stop & prec ){
            j <- 1
            for( i in input$varOpt ){
              Min <- eval( parse( text = sprintf( "input$OptVarsMin%s", i) ) )
              Max <- eval( parse( text = sprintf( "input$OptVarsMax%s", i) ) )
              Prec <- eval( parse( text = sprintf( "input$OptVarsPrec%s", i) ) )
              space <- ( ( Max - Min ) / Prec ) + 1
              comb[j] <- space ^ ifelse( Type[[ i ]][1] == "value", 1,
                                         ifelse( Type[[ i ]][1] == "inter", length( Init[[ i ]] ),
                                                 as.numeric( Type[[ i ]][2] ) + 1 ) )
              j <- j + 1
            }
          }
          comb <- prod( comb )
          nL <- length( grep( "Layers", colnames( Parameters ) ) )
          output$optTime1 <- renderText( "Based on the current settings it will take approx:" )
          time1 <- Time( comb / input$parallel, attr( Result, "time" ) )
          if( any( unlist( lapply( Type[ input$varOpt ], function(x){ x[1] == "inter" } ) ) ) ){
            cond <- ", but due to the extremely big search space it does not accept interpolation based parameters"
          }else{
            cond <- ""
          }
          output$optTime2 <- renderText( sprintf( "1) Full Search: %s%s.", time1, cond ) )
          it <- ( input$popSize * input$maxiter )
          time2 <- Time( ( it * nrow( Result ) ) / input$parallel, attr( Result, "time" ) )
          output$optTime3 <- renderText( sprintf( "2) Evolutionary Search based on %s combinations: %s.", it, time2 ) )
        }
      })

      observeEvent( input$StartOpt, {
        req( !is.null( input$varOpt ) )
        req( !is.null( eval( parse( text = sprintf( 'input$OptVarsMin%s', input$varOpt[1] ) ) ) ) )
        req( all( unlist( lapply( input$varOpt , function(x){
          eval( parse( text = sprintf( 'input$OptVarsMin%s', x ) ) ) <=
          eval( parse( text = sprintf( 'input$OptVarsMax%s', x ) ) )} ) ) ) )
        req( !anyNA(oXY$y) )

        RevRes <- ReverseLMR( input$varOpt, Type, oXY$y, Init, Prec,
                              if( exists("weight") ){ weight }else{ 1 },
                              if( exists("cuts") ){ cuts }else{ c( Parameters[1,"waveMin"], Parameters[1,"waveMax"] ) },
                              input$repType, varMin, varMax, ifelse( input$repType == "gray", TRUE, FALSE ),
                              Parameters[1,"waveMin"], Parameters[1,"waveMax"], Parameters[1,"waveStep"],
                              Parameters[1,"coreDiameter"], Parameters[1,"LengthModifiedRegion"], thickLayers, LayersDf, Parameters[1,"angleMax"],
                              input$popSize, input$pcrossover, input$pmutation,
                              input$maxiter, input$run, input$parallel, input$seed, input$methOpt )

        updateSelectInput( session, "ChooseResultOpt", choices = 1:min( 100, nrow( RevRes$Out ) ), selected = "" )
        output$RevResTab <- renderDT( RevRes$Out )
        shinyjs::show("AreaSaveOpt")
        assign( "RevRes", RevRes, envir = .GlobalEnv )
      })

      observeEvent( list( input$ChooseResultOpt ), {
        val <- input$ChooseResultOpt
        req( val != "" )
        output$RevResPlot <- renderPlotly({
          XX <- seq( Parameters[1,"waveMin"], Parameters[1,"waveMax"], by = Parameters[1,"waveStep"] )
          YY <- RevRes$Transmit[[ as.numeric( val ) ]]$Transmit
          circles <- map2( XX, YY, ~list( type = 'circle', xanchor = .x, yanchor = .y,
                                          x0 = -4, x1 = 4, y0 = -4, y1 = 4, xsizemode = 'pixel',
                                          ysizemode = 'pixel', fillcolor = 'blue', line = list(color = 'ransparent') ) )
          req( !is.null(XX[[length(XX)]]) )
          req( !anyNA(XX) & !anyNA(YY) )
          req( !anyNA(oXY$y) )

          App <- tryCatch( approxExtrap( XX, YY, c( Parameters[1,"waveMin"], XX, Parameters[1,"waveMax"]) ),
                           error = function(e){ list( x = 1, y = 1) })
          p <- plot_ly( height = 500, source = 'fPP' ) %>%
            add_lines(x = App$x, y = App$y, color = I('purple'), name = "Optimized Model" ) %>%
            add_lines(x = Result$Wavelength, y = Result$Transmittance, color = I('blue'), name = "Original Model" ) %>%
            add_lines(x = Result$Wavelength, y = oXY$y, color = I('red'), name = "New Model" ) %>%
            layout( shapes = circles,
                    xaxis = list( title = 'Wavelength' ),
                    yaxis = list( title = 'Transmittance' ),
                    legend = list( orientation = 'h', xanchor = 'center', x = 0.5, y = 1.05 ) ) %>%
            config( edits = list( shapePosition = TRUE ) ) %>% event_register('plotly_relayout')
          p
        })
      })

      #### Save Model Optimized ####
      output$saveOpt <- downloadHandler(

        filename = function(){
          paste0( "optimLMR-", Sys.time(), ".zip" )
        },

        content = function( file ){
          fs <- c()
          currdir <- getwd()
          tmpdir <- tempdir()
          setwd( tempdir() )
          val <- as.numeric( input$ChooseResultOpt )
          write.table( RevRes$Transmit[[val]], "optResult.txt", sep = ";", row.names = FALSE )
          Parameters <- data.frame( waveMin = Parameters[1,"waveMin"], waveMax = Parameters[1,"waveMax"], waveStep = Parameters[1,"waveMax"],
                                    temperature = Init$temperature, coreDiameter = Init$coreD,
                                    LengthModifiedRegion = Init$L,
                                    ThicknessCoatingLayers = unlist( Init[ grep("thickLayers", names( Init ) ) ] ),
                                    angleMax = Parameters[1,"angleMax"] )
          Parameters <- data.frame( RevRes$Out[val,,drop=FALSE], Parameters, row.names = NULL )
          write.table( Parameters, "optParameters.txt", sep = ";", row.names = FALSE )
          write.table( RevRes$Layers[[val]], "optLayers.txt", sep = ";", row.names = FALSE )
          fs <- c( "optResult.txt", "optLayers.txt", "optParameters.txt" )
          zip( zipfile = file, files = fs )
          setwd( currdir )
        },
        contentType = "application/zip"
      )

    },

    #### Options ####

    # Function to clear global environment
    onStart <- function(){

      onStop( function() {

        rm( list = unique(
          c( unlist( sapply( c( "Curve", "LayersList", "cPanels", "outputGlob","outputTabGlob", "nL", "check",
                               "reactGlob", "varMin", "varMax", "Type", "Prec", "Init", "Result", "Model",
                               "LayersDf", "Parameters", "cuts", "weight", "RevRes" ),
                            function(x){ grep( x, ls( env = .GlobalEnv ), value = TRUE ) } )),
             grep( "acceptPlot[[:digit:]]", ls( env = .GlobalEnv ), value = TRUE ),
             grep( "drawPlot[[:digit:]]", ls( env = .GlobalEnv ), value = TRUE ),
             grep( "afterInit[[:digit:]]", ls( env = .GlobalEnv ), value = TRUE ),
             grep( "dfPlot[[:digit:]]", ls( env = .GlobalEnv ), value = TRUE ),
             grep( "App[[:digit:]]", ls( env = .GlobalEnv ), value = TRUE ) )
          ),
            envir = .GlobalEnv )

        options( warn = 0 )

      } )

    }

  )

  #### RunApp ####
  runApp( app, launch.browser = launch.browser )

}
