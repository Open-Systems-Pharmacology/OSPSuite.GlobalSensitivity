
distSelection <- function(path) {
  paste0("distributionSelection", path, collapse = "_")
}

distParameterForm <- function(path) {
  paste0("miniform", path, collapse = "_")
}
distValParameterDisplayName <- function(path) {
  paste0("parameterDisplayName_", path)
}
distValUniformMinimum <- function(path) {
  paste0("uniformMin_", path)
}
distValUniformMaximum <- function(path) {
  paste0("uniformMax_", path)
}
distValUniformUnit <- function(path) {
  paste0("uniformUnit_", path)
}
distValLogUniformMinimum <- function(path) {
  paste0("logUniformMin_", path)
}
distValLogUniformMaximum <- function(path) {
  paste0("logUniformMax_", path)
}
distValLogUniformUnit <- function(path) {
  paste0("logUniformUnit_", path)
}
distValNormalMean <- function(path) {
  paste0("normalMean_", path)
}
distValNormalStdv <- function(path) {
  paste0("normalStdv_", path)
}
distValNormalUnit <- function(path) {
  paste0("normalUnit_", path)
}
distValLogNormalMean <- function(path) {
  paste0("logNormalMu_", path)
}
distValLogNormalStdv <- function(path) {
  paste0("logNormalCV_", path)
}
distValLogNormalUnit <- function(path) {
  paste0("logNormalUnit_", path)
}
distValDimension <- function(path) {
  paste0("dimension_", path)
}

getDistValsList <- function(path) {
  distValKey <- list(
    "Uniform" = list(
      "minimum" = distValUniformMinimum(path),
      "maximum" = distValUniformMaximum(path),
      "unit" = distValUniformUnit(path)
    ),
    "LogUniform" = list(
      "minimum" = distValLogUniformMinimum(path),
      "maximum" = distValLogUniformMaximum(path),
      "unit" = distValLogUniformUnit(path)
    ),
    "Normal" = list(
      "mean" = distValNormalMean(path),
      "stdv" = distValNormalStdv(path),
      "unit" = distValNormalUnit(path)
    ),
    "LogNormal" = list(
      "mean" = distValLogNormalMean(path),
      "CV" = distValLogNormalStdv(path),
      "unit" = distValLogNormalUnit(path)
    ),
    "dimension" = distValDimension(path)
  )
  return(distValKey)
}

verifyQuantileVec <- function(quantileVec) {
  if (any(quantileVec < 0) | any(quantileVec > 1)) {
    stop("Quantile out of bounds")
  }
}

pkParameterSelection <- function(path) {
  paste0("pkParameterSelection", path, collapse = "_")
}

squareTheCircle <- function(x){
  x <-sub(pattern = "[)]",x = x,replacement = "]")
  x <-sub(pattern = "[(]",x = x,replacement = "[")
  return(x)
}


#' @export
runGUI <- function(){

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 30 * 1024^2)
    parameterPathsList <- reactiveValues(paths = NULL)
    outputPathsList <- reactiveValues(paths = NULL)

    shinyjs::disable("saveLocalSAResults")
    shinyjs::disable("saveGSAResults")
    shinyjs::disable("saveEFASTResults")
    shinyjs::disable("saveMorrisResults")

    shinyjs::disable("saveTornadoGGPLOT")
    shinyjs::disable("saveGSAGGPLOT")
    shinyjs::disable("saveEFASTGGPLOT")
    shinyjs::disable("saveMorrisGGPLOT")

    shinyjs::disable("saveTornadoPNG")
    shinyjs::disable("saveGSAPNG")
    shinyjs::disable("saveEFASTPNG")
    shinyjs::disable("saveMorrisPNG")

    shinyjs::js$disableTab("Parameters")
    shinyjs::js$disableTab("Outputs")
    shinyjs::js$disableTab("Analyses")


    uniformParametersList <- function(distributionParameterIdList) {
      minimum <- input[[distributionParameterIdList$minimum]]
      maximum <- input[[distributionParameterIdList$maximum]]
      return(list(minimum = minimum, maximum = maximum))
    }

    logUniformParametersList <- function(distributionParameterIdList) {
      minimum <- input[[distributionParameterIdList$minimum]]
      maximum <- input[[distributionParameterIdList$maximum]]
      return(list(minimum = minimum, maximum = maximum))
    }

    normalParametersList <- function(distributionParameterIdList) {
      mean <- input[[distributionParameterIdList$mean]]
      stdv <- input[[distributionParameterIdList$stdv]]
      return(list(mean = mean, stdv = stdv))
    }

    logNormalParametersList <- function(distributionParameterIdList) {
      mean <- input[[distributionParameterIdList$mean]]
      CV <- input[[distributionParameterIdList$CV]]
      return(list(mean = mean, CV = CV))
    }

    getDistributionParameterList <- list(
      "Uniform" = uniformParametersList,
      "LogUniform" = logUniformParametersList,
      "Normal" = normalParametersList,
      "LogNormal" = logNormalParametersList
    )

    vals <- reactiveValues()
    initiateDistributionValues <- reactive({
      n <- length(parameterPathsList$paths)
      if (n > 0) {
        sapply(1:n, function(i) {

          ospParameter <- ospsuite::getParameter(path = parameterPathsList$paths[[i]], container = sim$obj)
          ospParameterValue <- ospParameter$value
          ospParameterDimension <- ospParameter$dimension
          ospParameterUnit <- getBaseUnit(ospParameterDimension)

          if(ospParameterDimension == ospDimensions$`Molecular weight`){
            ospParameterValue <- toUnit(quantityOrDimension = ospDimensions$`Molecular weight`,
                                        values = ospParameterValue,
                                        targetUnit = ospUnits$`Molecular weight`$`g/mol`)
            ospParameterUnit <- ospUnits$`Molecular weight`$`g/mol`
          }

          distValKey <- getDistValsList(parameterPathsList$paths[[i]])

          vals[[distValKey$Uniform$minimum]] <- min(0.9*ospParameterValue,1.1*ospParameterValue)
          vals[[distValKey$Uniform$maximum]] <- max(0.9*ospParameterValue,1.1*ospParameterValue)
          vals[[distValKey$Uniform$unit]] <- ospParameterUnit
          vals[[distValKey$LogUniform$minimum]] <- min(0.1*ospParameterValue,10*ospParameterValue)
          vals[[distValKey$LogUniform$maximum]] <- max(0.1*ospParameterValue,10*ospParameterValue)
          vals[[distValKey$LogUniform$unit]] <- ospParameterUnit
          vals[[distValKey$Normal$mean]] <- ospParameterValue
          vals[[distValKey$Normal$stdv]] <- 1
          vals[[distValKey$Normal$unit]] <- ospParameterUnit
          vals[[distValKey$LogNormal$mean]] <- ospParameterValue
          vals[[distValKey$LogNormal$CV]] <- 1
          vals[[distValKey$LogNormal$unit]] <- ospParameterUnit
          vals[[distValKey$dimension]] <- ospParameterDimension

        })
      }
    })

    selectedPKParameters <- reactiveValues()
    initiatePKParameterValues <- reactive({
      n <- length(outputPathsList$paths)
      if (n > 0) {
        sapply(1:n, function(i) {
          input_string <- pkParameterSelection(outputPathsList$paths[[i]])
          selectedPKParameters[[input_string]] <- c()
        })
      }
    })

    updateSelectedPKParameters <- reactive({
      n <- length(outputPathsList$paths)
      if (n > 0) {
        sapply(1:n, function(i) {
          input_string <- pkParameterSelection(outputPathsList$paths[[i]])
          observeEvent(input$getOutputPaths, {
            selectedPKParameters[[input_string]] <- input[[input_string]]
          })
        })
      }
    })

    selectedDistributions <- reactiveValues()
    getDistributionSelection <- reactive({
      if (length(parameterPathsList$paths) > 0) {
        for (pth in parameterPathsList$paths){
          isolate({selectedDistributions[[pth]] <- input[[distSelection(pth)]]})
          #selectedDistributions[[pth]] <- input[[distSelection(pth)]]
        }
      }
    })

    sim <- reactiveValues()
    DDIsim <- reactiveValues()

    observeEvent(eventExpr = input$loadPKML, handlerExpr = {

      if(is.null(input$selectPKML$datapath)){
        showModal(modalDialog(
          title = "Error",
          "Please select a PKML simulation file.",
          easyClose = TRUE
        ))
        return()
      }
      sim$obj <- ospsuite::loadSimulation(filePath = input$selectPKML$datapath)

      DDIsim$obj <- NULL
      if(!is.null(input$DDIselectPKML$datapath)){
        DDIsim$obj <- ospsuite::loadSimulation(filePath = input$DDIselectPKML$datapath)
      }

      parTree <- getSimulationTree(simulationOrFilePath = sim$obj, quantityType = "Parameter")
      outTree <- getSimulationTree(simulationOrFilePath = sim$obj, quantityType = c("Molecule", "Observer"))
      output$parameterTree <- renderTree({
        parTree
      })
      output$outputTree <- renderTree({
        outTree
      })
      updateTabsetPanel(inputId = "mainTabPanel", selected = "Parameters")
    })

    observeEvent(eventExpr = input$backToloadPKML, handlerExpr = {
      updateTabsetPanel(inputId = "mainTabPanel", selected = "Start")
    })

    observeEvent(eventExpr = input$goToOutputs, handlerExpr = {
      updateTabsetPanel(inputId = "mainTabPanel", selected = "Outputs")
    })

    observeEvent(eventExpr = input$backToParameters, handlerExpr = {
      updateTabsetPanel(inputId = "mainTabPanel", selected = "Parameters")
    })

    observeEvent(eventExpr = input$getPaths, handlerExpr = {
      parameterPaths <- shinyTree::get_selected(tree = input$parameterTree, format = "names")
      selecterParameterPaths <- sapply(parameterPaths[parameterPaths == "path"], function(pp) {
        ospsuite::toPathString(attr(x = pp, "ancestry"))
      })
      parameterPathsList$paths <- selecterParameterPaths
      initiateDistributionValues()
      getDistributionSelection()
    })

    observeEvent(eventExpr = input$getOutputPaths, handlerExpr = {
      outputPaths <- shinyTree::get_selected(tree = input$outputTree, format = "names")
      selecterOutputPaths <- sapply(outputPaths[outputPaths == "path"], function(pp) {
        ospsuite::toPathString(attr(x = pp, "ancestry"))
      })
      outputPathsList$paths <- selecterOutputPaths
      initiatePKParameterValues()
    })

    makeDistributionValuesUpdateEvents <- reactive({
      #This reactive maintains the values of selected distribution's parameters even if the user temporarily selects a different distribution
      n <- length(parameterPathsList$paths)
      if (n > 0) {
        sapply(1:n, function(i) {
          distValKey <- getDistValsList(parameterPathsList$paths[[i]])

          observeEvent(input[[distValKey$Uniform$minimum]], {
            vals[[distValKey$Uniform$minimum]] <- input[[distValKey$Uniform$minimum]]
          })

          observeEvent(input[[distValKey$Uniform$maximum]], {
            vals[[distValKey$Uniform$maximum]] <- input[[distValKey$Uniform$maximum]]
          })

          observeEvent(input[[distValKey$Uniform$unit]], {
            vals[[distValKey$Uniform$unit]] <- input[[distValKey$Uniform$unit]]
          })

          observeEvent(input[[distValKey$LogUniform$minimum]], {
            vals[[distValKey$LogUniform$minimum]] <- input[[distValKey$LogUniform$minimum]]
          })

          observeEvent(input[[distValKey$LogUniform$maximum]], {
            vals[[distValKey$LogUniform$maximum]] <- input[[distValKey$LogUniform$maximum]]
          })

          observeEvent(input[[distValKey$LogUniform$unit]], {
            vals[[distValKey$LogUniform$unit]] <- input[[distValKey$LogUniform$unit]]
          })

          observeEvent(input[[distValKey$Normal$mean]], {
            vals[[distValKey$Normal$mean]] <- input[[distValKey$Normal$mean]]
          })

          observeEvent(input[[distValKey$Normal$stdv]], {
            vals[[distValKey$Normal$stdv]] <- input[[distValKey$Normal$stdv]]
          })

          observeEvent(input[[distValKey$Normal$unit]], {
            vals[[distValKey$Normal$unit]] <- input[[distValKey$Normal$unit]]
          })

          observeEvent(input[[distValKey$LogNormal$mean]], {
            vals[[distValKey$LogNormal$mean]] <- input[[distValKey$LogNormal$mean]]
          })

          observeEvent(input[[distValKey$LogNormal$CV]], {
            vals[[distValKey$LogNormal$CV]] <- input[[distValKey$LogNormal$CV]]
          })

          observeEvent(input[[distValKey$LogNormal$unit]], {
            vals[[distValKey$LogNormal$unit]] <- input[[distValKey$LogNormal$unit]]
          })
        })
      }
    })

    makeDistributionMenuEvents <- reactive({
      n <- length(parameterPathsList$paths)
      if (n > 0) {
        sapply(1:n, function(i) {
          distValKey <- getDistValsList(parameterPathsList$paths[[i]])
          input_string <- distSelection(parameterPathsList$paths[[i]])
          output_string <- distParameterForm(parameterPathsList$paths[[i]])
          parameterDimension <- vals[[distValKey$dimension]]
          baseUnit <- ospsuite::getBaseUnit(dimension = parameterDimension)
          availableUnits <- names(ospsuite::ospUnits[[squareTheCircle(parameterDimension)]])

          observeEvent(input[[input_string]], {
            slotWidth <- NULL
            incrementStep <- 0.01
            distributionInputs <- list(
              "Uniform" = fluidRow(
                column(4, numericInput(inputId = distValKey$Uniform$minimum, label = "Minimum", value = vals[[distValKey$Uniform$minimum]], width = slotWidth, step = incrementStep)),
                column(4, numericInput(inputId = distValKey$Uniform$maximum, label = "Maximum", value = vals[[distValKey$Uniform$maximum]], width = slotWidth, step = incrementStep)),
                column(4, selectInput(inputId = distValKey$Uniform$unit, label = "Unit", choices = availableUnits, width = slotWidth, selected = vals[[distValKey$Uniform$unit]]))
              ),
              "LogUniform" = fluidRow(
                column(4, numericInput(inputId = distValKey$LogUniform$minimum, label = "Minimum", value = vals[[distValKey$LogUniform$minimum]], width = slotWidth, step = incrementStep)),
                column(4, numericInput(inputId = distValKey$LogUniform$maximum, label = "Maximum", value = vals[[distValKey$LogUniform$maximum]], width = slotWidth, step = incrementStep)),
                column(4, selectInput(inputId = distValKey$LogUniform$unit, label = "Unit", choices = availableUnits, width = slotWidth, selected = vals[[distValKey$LogUniform$unit]]))
              ),
              "Normal" = fluidRow(
                column(4, numericInput(inputId = distValKey$Normal$mean, label = "Mean", value = vals[[distValKey$Normal$mean]], width = slotWidth, step = incrementStep)),
                column(4, numericInput(inputId = distValKey$Normal$stdv, label = "SD", value = vals[[distValKey$Normal$stdv]], width = slotWidth, min = 0, step = incrementStep)),
                column(4, selectInput(inputId = distValKey$Normal$unit, label = "Unit", choices = availableUnits, width = slotWidth, selected = vals[[distValKey$Normal$unit]]))
              ),
              "LogNormal" = fluidRow(
                column(4, numericInput(inputId = distValKey$LogNormal$mean, label = "Mean", value = vals[[distValKey$LogNormal$mean]], width = slotWidth, step = incrementStep)),
                column(4, numericInput(inputId = distValKey$LogNormal$CV, label = "CV", value = vals[[distValKey$LogNormal$CV]], width = slotWidth, min = 0, step = incrementStep)),
                column(4, selectInput(inputId = distValKey$LogNormal$unit, label = "Unit", choices = availableUnits, width = slotWidth, selected = vals[[distValKey$LogNormal$unit]]))
              )
            )
            output[[output_string]] <- renderUI(distributionInputs[[input[[input_string]]]])
          })
        })
      }
    })

    output$interactionUI <- renderUI({
      makeDistributionValuesUpdateEvents()
      makeDistributionMenuEvents()
      getDistributionSelection()
      n <- length(parameterPathsList$paths)
      if (n > 0) {
        interaction <- list()
        for (i in 1:n) {
          input_string <- distSelection(parameterPathsList$paths[[i]])
          output_string <- distParameterForm(parameterPathsList$paths[[i]])

          interaction[[i]] <- fluidRow(
            h3(paste0("Distribution for ", parameterPathsList$paths[[i]])),
            textInput(inputId = distValParameterDisplayName(parameterPathsList$paths[[i]]),#paste0("parameterDisplayName_",parameterPathsList$paths[[i]]),
                      label = "Display name",
                      value = parameterPathsList$paths[[i]] ),
            column(
              4,
              selectInput(
                inputId = input_string,
                label = "Distribution type", choices = c("Uniform", "LogUniform", "Normal" , "LogNormal"),
                selected = selectedDistributions[[ parameterPathsList$paths[[i]]  ]]
              )
            ),
            column(
              4,
              fluidRow(uiOutput(outputId = output_string))
            )
          )
        }
        do.call(fluidRow, interaction)
      }
    })

    output$outputsUI <- renderUI({
      updateSelectedPKParameters()
      n <- length(outputPathsList$paths)
      if (n > 0) {
        interaction <- list()
        for (i in 1:n) {
          input_string <- pkParameterSelection(outputPathsList$paths[[i]])

          paste0("PK parameters for ", outputPathsList$paths[[i]])

          interaction[[i]] <- fluidRow(
            #GGGG
            h3(paste0("Output ", outputPathsList$paths[[i]])),
            textInput(inputId = paste0("outputDisplayName_",i), label = "Display name",
                      value = outputPathsList$paths[[i]] ),
            helpText(paste0("PK parameters for output ", outputPathsList$paths[[i]])),
            column(4,
                   checkboxGroupInput(inputId = input_string, label = NULL, choices = list("Cmax" = "C_max", "AUC_tEnd" = "AUC_tEnd", "C_tEnd" = "C_tEnd"), selected = selectedPKParameters[[input_string]])
            ))
        }
        do.call(fluidRow, interaction)
      }
    })

    observeEvent(
      eventExpr = input$goToEvaluation,
      handlerExpr = {
        results$dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))
        results$settings <- buildSettingsCMD(buildParameterList(),buildOutputList())
        updateTabsetPanel(inputId = "mainTabPanel", selected = "Analyses")
      }
    )

    verifyParametersSelected <- function(additionalCondition = TRUE){
      if(isFALSE(additionalCondition) & !(length(parameterPathsList$paths) > 0)){
        showModal(modalDialog(
          title = "Error",
          "Please select at least one parameter from the Parameter Tree or select 'Run for all constants'.",
          easyClose = TRUE
        ))
        return(FALSE)
      }
    }

    verifyOutputsSelected <- function(){
      selectedPKParameters <- unlist(sapply(outputPathsList$paths, function(pth){input[[ pkParameterSelection(pth) ]]}))

      if(!(length(selectedPKParameters) > 0)){
        showModal(modalDialog(
          title = "Error",
          "Please select at least one output from the Output Tree.",
          easyClose = TRUE
        ))
        return(FALSE)
      }
    }

    checkPathsInDDISimulation <- function(parameterList = parameterList,outputList = outputList){

      parameterChecks <- checkParametersExistInSimulation(simulation = DDIsim$obj,
                                                          parameterPaths = sapply(parameterList,function(par){par$path}),
                                                          simulationName =  "DDI simulation")

      outputChecks <- checkOutputsExistInSimulation(simulation = DDIsim$obj,
                                                    outputPaths = sapply(outputList,function(par){par$path}),
                                                    simulationName = "DDI simulation")

      missingPaths <- NULL
      if(!all(unlist(parameterChecks))){
        missingPaths <- c(missingPaths,"<b>Parameters not found in DDI simulation:</b>")
        for (pth in names(parameterChecks)){
          if(!parameterChecks[[pth]]){
            missingPaths <- c(missingPaths,"<br>",pth)}
        }
      }

      if(!all(unlist(outputChecks))){
        if(!is.null(missingPaths)){
          missingPaths <- c(missingPaths,"<br><br>")
        }
        missingPaths <- c(missingPaths,"<b>Outputs not found in DDI simulation:</b>")
        for (pth in names(outputChecks)){
          if(!outputChecks[[pth]]){
            missingPaths <- c(missingPaths,"<br>",pth)}
        }
      }

      passed <- all(c(unlist(parameterChecks),unlist(outputChecks)))

      if(!passed){
        showModal(modalDialog(
          title = "Error",
          HTML(paste0(missingPaths,collapse = "")),
          easyClose = FALSE
        ))
      }

      return(passed)

    }

    observeEvent(eventExpr = input$backToOutputs, handlerExpr = {
      updateTabsetPanel(inputId = "mainTabPanel", selected = "Outputs")
    })

    results <- reactiveValues()

    buildParameterList <- function(){

      parameterList <- list()

      for (i in seq_along(parameterPathsList$paths)) {

        path <- parameterPathsList$paths[[i]]
        distValKey <- getDistValsList(path)


        #get dimension of distribution parameters for the selected model parameter with path = "path"
        dimension <- vals[[distValKey$dimension]]

        #get unit of distribution parameters for the selected model parameter with path = "path"
        unit = vals[[distValKey[[input[[distSelection(path)]]]]$unit]]

        #get name of distribution for the selected model parameter with path = "path"
        selectedDistributionForPath <- input[[distSelection(path)]]


        #get ui key f0r distribution parameter values for the selected model parameter with path = "path"
        distributionParameterListKeys <- getDistValsList(path)[[selectedDistributionForPath]]

        #get the list of parameters of the distribution for the selected model parameter with path = "path"
        distributionParameterList <- getDistributionParameterList[[selectedDistributionForPath]](distributionParameterListKeys)


        parameterList[[i]] <- SAParameter$new(simulation = sim$obj,
                                              path = path,
                                              unit = unit,
                                              displayName = input[[ distValParameterDisplayName(path) ]],
                                              parameterDistribution = do.call(distribution[[selectedDistributionForPath]], distributionParameterList))

      }

      return(parameterList)
    }

    buildOutputList <- function(){
      outputList <- list()
      for (i in seq_along(outputPathsList$paths)) {
        pth <- outputPathsList$paths[[i]]
        input_string <- pkParameterSelection(pth)
        outputList[[pth]] <- SAOutput$new(simulation = sim$obj,path = pth,displayName = input[[paste0("outputDisplayName_",i)]])
        for (pk in input[[input_string]]){
          outputList[[pth]]$addPKParameter(standardPKParameter = pk)
        }
      }
      return(outputList)
    }

    observeEvent(
      eventExpr = input$startLocalSA,
      handlerExpr = {

        if( isFALSE(verifyParametersSelected(additionalCondition = input$runSensitivityAnalysisForAllParameters)) ) return()
        if( isFALSE(verifyOutputsSelected()) ) return()

        numberOfUncertaintySamples <- input$numberOfUncertaintySamples

        parameterList <- buildParameterList()
        outputList <- buildOutputList()

        verifiedPaths <- TRUE
        if(!is.null(DDIsim$obj)){
          verifiedPaths <- checkPathsInDDISimulation(parameterList = parameterList,outputList = outputList)
        }

        if(verifiedPaths){
          print("Starting local sensitivity...")


          progressSensitivity <- shiny::Progress$new()
          progressSensitivity$set(message = "Computing local sensitivity`", value = 0)
          on.exit(progressSensitivity$close())

          updateProgressSensitivity <- function(value, detail = NULL) {
            progressSensitivity$set(value = value, detail = detail)
          }

          updateProgressUncertainty <- NULL
          if(input$runUncertaintyCheckbox){

            progressUncertainty <- shiny::Progress$new()
            progressUncertainty$set(message = "Computing uncertainty`", value = 0)
            on.exit(progressUncertainty$close())

            updateProgressUncertainty <- function(value, detail = NULL) {
              progressUncertainty$set(value = value, detail = detail)
            }
          }

          results$su <- tryCatch({
            runSU(simulation = sim$obj,
                  DDIsimulation = DDIsim$obj,
                  runUncertaintyAnalysis = input$runUncertaintyCheckbox,
                  evaluateForAllParameters = input$runSensitivityAnalysisForAllParameters, #****** IF FALSE, ONLY THE PARAMETERS IN parametersList WILL BE ANALYZED
                  customParameters = parameterList,
                  sensitivityThreshold = input$sensitivityThreshold,
                  variationRange = input$variationRangeNumericInput, #
                  numberOfSensitivityAnalysisSteps = input$numberOfStepsNumericInput,
                  quantiles = eval(parse(text = paste0("c(", input$quantilesTestInput ,")"))),
                  outputs = outputList,
                  saveFolder = NULL,
                  saveResults = FALSE,
                  numberOfUncertaintyAnalysisSamples = input$numberOfUncertaintySamples, #****** FOR THE UNCERTAINTY ANALYSIS ONLY, THESE SAMPLES WILL BE TAKEN FROM LOGUNIFORM +/- 10% BY DEFAULT, UNLESS A DIFFERENT DISTRIBUTION WAS SPECIFIED IN parametersList
                  runParallel = TRUE,
                  updateProgressSensitivity = updateProgressSensitivity,
                  updateProgressUncertainty = updateProgressUncertainty)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              "Evaluation of local sensitivity was not completed successfully.  Verify whether the PKML model can be successfully run at all points in the specified parameter space.",
              easyClose = TRUE
            ))
            shinyjs::disable("saveLocalSAResults")
            shinyjs::disable("saveTornadoGGPLOT")
            shinyjs::disable("saveTornadoPNG")
            return(FALSE)
          })

          if(is.null(results$su$Results)){
            showModal(modalDialog(
              title = "Error",
              "No non-negligible local sensitivities found.",
              easyClose = TRUE
            ))
            shinyjs::disable("saveLocalSAResults")
            shinyjs::disable("saveTornadoGGPLOT")
            shinyjs::disable("saveTornadoPNG")
            return()
          }

          if(!isFALSE(results$su$Results)){
            shinyjs::enable("saveLocalSAResults")
            shinyjs::enable("saveTornadoGGPLOT")
            shinyjs::enable("saveTornadoPNG")
          }
        }
      }
    )




    observeEvent(
      eventExpr = input$startMorris,
      handlerExpr = {
        if( isFALSE(verifyParametersSelected()) ) return()
        if( isFALSE(verifyOutputsSelected()) ) return()

        numberOfSamplesMorris <- input$numberOfSamplesMorris

        if (!is.integer(numberOfSamplesMorris)){
          showModal(modalDialog(
            title = "Error",
            HTML("The number of samples must an integer."),
            easyClose = TRUE
          ))
          return(NULL)
        }

        if (numberOfSamplesMorris < 1){
          showModal(modalDialog(
            title = "Error",
            HTML("The number of samples must be 1 or greater."),
            easyClose = TRUE
          ))
          return(NULL)
        }

        if (numberOfSamplesMorris < 2){
          showModal(modalDialog(
            title = "Warning",
            HTML("Evaluation of Morris sensitivity interaction effect <em>&sigma;</em> requires the number of samples to be 2 or greater."),
            easyClose = TRUE
          ))
        }

        parameterList <- buildParameterList()
        outputList <- buildOutputList()

        verifiedPaths <- TRUE
        if(!is.null(DDIsim$obj)){
          verifiedPaths <- checkPathsInDDISimulation(parameterList = parameterList,outputList = outputList)
        }

        if(verifiedPaths){
          print("Starting Morris sensitivity...")
          progress <- shiny::Progress$new()
          progress$set(message = "Computing Morris sensitivity`", value = 0)
          on.exit(progress$close())

          updateProgress <- function(value, detail = NULL) {
            progress$set(value = value, detail = detail)
          }

          results$morris <- tryCatch({

            runMorris(simulation = sim$obj,
                      DDIsimulation = DDIsim$obj,
                      parameters = parameterList,
                      outputs = outputList,
                      numberOfSamples = numberOfSamplesMorris,
                      runParallel = TRUE,
                      updateProgress = updateProgress)

          }, error = function(e) {

            showModal(modalDialog(
              title = "Error",
              "Evaluation of Morris sensitivity was not completed successfully.  Verify whether the PKML model can be successfully run at all points in the specified parameter space.",
              easyClose = TRUE
            ))
            shinyjs::disable("saveMorrisResults")
            shinyjs::disable("saveMorrisGGPLOT")
            shinyjs::disable("saveMorrisPNG")
            return(FALSE)
          })

          if(!isFALSE(results$morris$Results)){
            shinyjs::enable("saveMorrisResults")
            shinyjs::enable("saveMorrisGGPLOT")
            shinyjs::enable("saveMorrisPNG")
          }

        }

      }
    )


    observeEvent(
      eventExpr = input$startGSA,
      handlerExpr = {

        if( isFALSE(verifyParametersSelected()) ) return()
        if( isFALSE(verifyOutputsSelected()) ) return()

        numberOfSamplesGSA <- input$numberOfSamplesGSA


        parameterList <- buildParameterList()
        outputList <- buildOutputList()

        verifiedPaths <- TRUE
        if(!is.null(DDIsim$obj)){
          verifiedPaths <- checkPathsInDDISimulation(parameterList = parameterList,outputList = outputList)
        }

        if(verifiedPaths){

          print("Starting GSA...")

          progress <- shiny::Progress$new()
          progress$set(message = "Computing GSA", value = 0)
          on.exit(progress$close())

          updateProgress <- function(value, detail = NULL) {
            progress$set(value = value, detail = detail)
          }

          results$sobol <- tryCatch({

            runSobol(simulation = sim$obj,
                     DDIsimulation = DDIsim$obj,
                     parameters = parameterList,
                     outputs = outputList,
                     numberOfSamples = numberOfSamplesGSA,
                     runParallel = TRUE,
                     updateProgress =  updateProgress)

          }, error = function(e) {

            showModal(modalDialog(
              title = "Error",
              "Evaluation of global sensitivity was not completed successfully.  Verify whether the PKML model can be successfully run at all points in the specified parameter space.",
              easyClose = TRUE
            ))
            shinyjs::disable("saveGSAResults")
            shinyjs::disable("saveGSAGGPLOT")
            shinyjs::disable("saveGSAPNG")
            return(FALSE)
          })

          if(!isFALSE(results$sobol$Results)){
            shinyjs::enable("saveGSAResults")
            shinyjs::enable("saveGSAGGPLOT")
            shinyjs::enable("saveGSAPNG")
          }
        }

      }
    )


    observeEvent(
      eventExpr = input$startEFAST,
      handlerExpr = {

        if( isFALSE(verifyParametersSelected()) ) return()
        if( isFALSE(verifyOutputsSelected()) ) return()



        ###
        numberOfReSamplesEFAST <- input$numberOfReSamplesEFAST

        parameterList <- buildParameterList()
        outputList <- buildOutputList()

        verifiedPaths <- TRUE
        if(!is.null(DDIsim$obj)){
          verifiedPaths <- checkPathsInDDISimulation(parameterList = parameterList,outputList = outputList)
        }

        if(verifiedPaths){
          print("Starting EFAST...")
          progress <- shiny::Progress$new()
          progress$set(message = "Computing EFAST", value = 0)
          on.exit(progress$close())

          updateProgress <- function(value, detail = NULL) {
            progress$set(value = value, detail = detail)
          }

          results$efast <- tryCatch({

            runEFAST(simulation = sim$obj,
                     DDIsimulation = DDIsim$obj,
                     parameters = parameterList,
                     outputs = outputList,
                     numberOfResamples = numberOfReSamplesEFAST,
                     runParallel = TRUE,
                     updateProgress = updateProgress)

          }, error = function(e) {

            showModal(modalDialog(
              title = "Error",
              "Evaluation of EFAST was not completed successfully.  Verify whether the PKML model can be successfully run at all points in the specified parameter space.",
              easyClose = TRUE
            ))
            shinyjs::disable("saveEFASTResults")
            shinyjs::disable("saveEFASTGGPLOT")
            shinyjs::disable("saveEFASTPNG")
            return(FALSE)
          })

          if(!isFALSE(results$efast$Results)){
            shinyjs::enable("saveEFASTResults")
            shinyjs::enable("saveEFASTGGPLOT")
            shinyjs::enable("saveEFASTPNG")
          }
        }
      }
    )



    observeEvent(input$runUncertaintyCheckbox, {
      if(input$runUncertaintyCheckbox){
        shinyjs::enable("quantilesTestInput")
        shinyjs::enable("numberOfUncertaintySamples")
      } else {
        shinyjs::disable("quantilesTestInput")
        shinyjs::disable("numberOfUncertaintySamples")
      }
    })


    output$saveLocalSAResults <- downloadHandler(
      filename = function() {
        paste("one-at-a-time-results-", results$dateTime, ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(x=results$su,path = file)
      }
    )

    #### saveGSAGGPLOT
    output$saveTornadoGGPLOT <- downloadHandler(
      filename = function() {
        paste("TornadoGGPLOT_", results$dateTime , ".rds", sep = "")
      },
      content = function(file) {
        plt <- generateTornadoPlot(sensitivityDataFrame = results$su$Results)
        saveRDS(object = plt,file = file)
      }
    )

    #### saveGSAPNG
    output$saveTornadoPNG <- downloadHandler(
      filename = function() {
        paste("TornadoPNG_", results$dateTime , ".zip", sep = "")
      },
      content = function(file) {
        pltList <- generateTornadoPlot(sensitivityDataFrame = results$su$Results)
        fileNames <- NULL
        for(op in names(pltList)){
          for (pk in names(pltList[[op]])){
            fname <-  paste("TornadoPNG_", results$dateTime , ".png", sep = "")
            fname <-  paste(op,pk,fname,sep = "_")
            fname <-  gsub(pattern = "[|]",replacement = "-",x = fname)
            ggsave(filename = fname,
                   plot = pltList[[op]][[pk]],height = 7*length(parameterPathsList$paths)/4,width = 7,units = "in",
                   device = "png")
            fileNames <- c(fileNames,fname)
          }
        }

        zip(file, files = fileNames)
        sapply(fileNames,file.remove)

      }
    )

    ###############


    #### saveGSAResults
    output$saveGSAResults <- downloadHandler(
      filename = function() {
        paste("gsaResults_", results$dateTime, ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(x=results$sobol,path = file)
      }
    )


    #### saveGSAGGPLOT
    output$saveGSAGGPLOT <- downloadHandler(
      filename = function() {
        paste("GSAGGPLOT_", results$dateTime , ".rds", sep = "")
      },
      content = function(file) {
        plt <- generateLowryPlot(gsaResultsDataframe = results$sobol$Results)
        saveRDS(object = plt,file = file)
      }
    )

    #### saveGSAPNG
    output$saveGSAPNG <- downloadHandler(
      filename = function() {
        paste("GSAPNG_", results$dateTime , ".zip", sep = "")
      },
      content = function(file) {
        pltList <- generateLowryPlot(gsaResultsDataframe = results$sobol$Results)

        fileNames <- NULL
        for(op in names(pltList)){
          for (pk in names(pltList[[op]])){
            fname <-  paste("GSAPNG_", results$dateTime , ".png", sep = "")
            fname <-  paste(op,pk,fname,sep = "_")
            fname <-  gsub(pattern = "[|]",replacement = "-",x = fname)
            ggsave(filename = fname,
                   plot = pltList[[op]][[pk]],width = 7*length(parameterPathsList$paths)/4,height = 7,units = "in",
                   device = "png")
            fileNames <- c(fileNames,fname)
          }
        }

        zip(file, files = fileNames)
        sapply(fileNames,file.remove)

      }
    )


    #### saveEFASTGGPLOT
    output$saveEFASTGGPLOT <- downloadHandler(
      filename = function() {
        paste("EFASTGGPLOT_", results$dateTime , ".rds", sep = "")
      },
      content = function(file) {
        plt <- generateLowryPlot(gsaResultsDataframe = results$efast$Results)
        saveRDS(object = plt,file = file)
      }
    )

    #### saveEFASTPNG
    output$saveEFASTPNG <- downloadHandler(
      filename = function() {
        paste("EFASTPNG_", results$dateTime , ".zip", sep = "")
      },
      content = function(file) {

        pltList <- generateLowryPlot(gsaResultsDataframe = results$efast$Results)

        fileNames <- NULL
        for(op in names(pltList)){
          for (pk in names(pltList[[op]])){
            fname <-  paste("EFASTPNG_", results$dateTime , ".png", sep = "")
            fname <-  paste(op,pk,fname,sep = "_")
            fname <-  gsub(pattern = "[|]",replacement = "-",x = fname)
            ggsave(filename = fname,
                   plot = pltList[[op]][[pk]],width = 7*length(parameterPathsList$paths)/4,height = 7,units = "in",
                   device = "png")
            fileNames <- c(fileNames,fname)
          }
        }

        zip(file, files = fileNames)
        sapply(fileNames,file.remove)

      }
    )

    #### saveEFASTResults
    output$saveEFASTResults <- downloadHandler(
      filename = function() {
        paste("efastResults_", results$dateTime, ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(x=results$efast,path = file)
      }
    )


    #### getSUCode
    output$getSUCode <- downloadHandler(
      filename = function() {
        paste("SUCode_", results$dateTime , ".R", sep = "")
      },
      content = function(file) {
        script <- writeParametersAndOutputsToFile(parametersList = buildParameterList(),outputsList = buildOutputList())
        script <- append(x = script , values = writeSUFunctionToFile(input$runUncertaintyCheckbox,
                                                                     input$runSensitivityAnalysisForAllParameters,
                                                                     input$sensitivityThreshold,
                                                                     input$variationRangeNumericInput,
                                                                     input$numberOfStepsNumericInput,
                                                                     input$quantilesTestInput,
                                                                     input$numberOfUncertaintySamples,
                                                                     input$runParallel))

        con <- file(file, open = "wt", encoding = "UTF-8")
        writeLines(text =  script ,con = con )
        close(con)
      }
    )

    #### getSobolCode
    output$getSobolCode <- downloadHandler(
      filename = function() {
        paste("SobolCode_", results$dateTime , ".R", sep = "")
      },
      content = function(file) {
        script <- writeParametersAndOutputsToFile(parametersList = buildParameterList(),outputsList = buildOutputList())
        script <- append(x = script , values = writeSobolSensitivityFunctionToFile(numberOfSamplesGSA = input$numberOfSamplesGSA))

        con <- file(file, open = "wt", encoding = "UTF-8")
        writeLines(text =  script ,con = con )
        close(con)
      }
    )

    #### getEFASTCode
    output$getEFASTCode <- downloadHandler(
      filename = function() {
        paste("EFASTCode_", results$dateTime , ".R", sep = "")
      },
      content = function(file) {
        script <- writeParametersAndOutputsToFile(parametersList = buildParameterList(),outputsList = buildOutputList())
        script <- append(x = script , values = writeEFASTSensitivityFunctionToFile(numberOfReSamplesEFAST = input$numberOfReSamplesEFAST))

        con <- file(file, open = "wt", encoding = "UTF-8")
        writeLines(text =  script ,con = con )
        close(con)
      }
    )

    #### getMorrisCode
    output$getMorrisCode <- downloadHandler(
      filename = function() {
        paste("morrisCode_", results$dateTime , ".R", sep = "")
      },
      content = function(file) {
        script <- writeParametersAndOutputsToFile(parametersList = buildParameterList(),outputsList = buildOutputList())
        script <- append(x = script , values = writeMorrisSensitivityFunctionToFile(numberOfSamples = input$numberOfSamplesMorris))

        con <- file(file, open = "wt", encoding = "UTF-8")
        writeLines(text =  script ,con = con )
        close(con)
      }
    )

    #### saveMorrisResults
    output$saveMorrisResults <- downloadHandler(
      filename = function() {
        paste("morrisResults_", results$dateTime , ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(x=results$morris,path = file)
      }
    )


    #### saveMorrisGGPLOT
    output$saveMorrisGGPLOT <- downloadHandler(
      filename = function() {
        paste("morrisGGPLOT_", results$dateTime , ".rds", sep = "")
      },
      content = function(file) {
        plt <- generateMorrisPlot(morrisResults = results$morris$Results)
        saveRDS(object = plt,file = file)
      }
    )

    #### saveMorrisPNG
    output$saveMorrisPNG <- downloadHandler(
      filename = function() {
        paste("morrisPNG_", results$dateTime , ".zip", sep = "")
      },
      content = function(file) {

        pltList <- generateMorrisPlot(morrisResults = results$morris$Results)

        fileNames <- NULL
        for(op in names(pltList)){
          for (pk in names(pltList[[op]])){
            fname <-  paste("morrisPNG_", results$dateTime , ".png", sep = "")
            fname <-  paste(op,pk,fname,sep = "_")
            fname <-  gsub(pattern = "[|]",replacement = "-",x = fname)
            ggsave(filename = fname,
                   plot = pltList[[op]][[pk]],
                   width = 7,
                   height = 7,
                   units = "in",
                   device = "png")
            fileNames <- c(fileNames,fname)
          }
        }

        zip(file, files = fileNames)
        sapply(fileNames,file.remove)

      }
    )

  }


  jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
cursor: not-allowed !important;
}"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
  shinyjs::inlineCSS(css),
  tabsetPanel(
    id = "mainTabPanel",
    tabPanel(title = "Start",
             value = "Start",
             column(12, align="right",
                    actionButton(inputId = "loadPKML", label = strong("Next"),style="color: #ffffff; background-color: #41719C;")),
             hr(),
             fluidRow(
               column(
                 3,
                 helpText(h4(strong("Simulation file")),style = "color: #888888;"),
                 fileInput(inputId = "selectPKML", label = NULL, placeholder = "Select file...", accept = ".pkml")
               )
             ),
             fluidRow(
               column(
                 3,
                 helpText(h4(strong("DDI simulation file (optional)")),style = "color: #888888;"),
                 fileInput(inputId = "DDIselectPKML", label = NULL, placeholder = "Select file...", accept = ".pkml")
               )
             )
    ),


    tabPanel(
      title = "Parameters",
      value = "Parameters",
      br(),
      column(12,
             align="right",
             actionButton(inputId = "backToloadPKML", label = strong("Back"), style = "color: #ffffff; background-color: #41719C;"),
             actionButton(inputId = "goToOutputs", label = strong("Next"), style = "color: #ffffff; background-color: #41719C;")),
      hr(),
      fluidRow(
        column(
          5,
          h4(strong("Parameters tree"),style = "color: #888888;"),
          shinyTree(outputId = "parameterTree", checkbox = TRUE)
        ),
        column(
          7,
          actionButton(inputId = "getPaths", label = strong("Specify distributions"), style="color: #ffffff; background-color: #41719C;"),
          br(),
          br(),
          uiOutput("interactionUI")
        )
      )
    ),
    tabPanel(
      title = "Outputs",
      value = "Outputs",
      br(),
      column(12, align="right",
             actionButton(inputId = "backToParameters", label = strong("Back"),style="color: #ffffff; background-color: #41719C;"),
             actionButton(inputId = "goToEvaluation", label = strong("Next"),style="color: #ffffff; background-color: #41719C;")),
      hr(),
      column(
        5,
        h4(strong("Outputs tree"),style = "color: #888888;"),
        shinyTree(outputId = "outputTree", checkbox = TRUE)
      ),
      column(
        7,
        actionButton(inputId = "getOutputPaths", label = strong("Specify PK parameters"), style="color: #ffffff; background-color: #41719C;"),
        br(),
        br(),
        uiOutput("outputsUI")
      )
    ),
    tabPanel(
      title = "Run sensitivity analyses", value = "Analyses",
      column(12, align="right",
             actionButton(inputId = "backToOutputs", label = strong("Back"),style="color: #ffffff; background-color: #41719C")),
      hr(),
      verticalLayout(column(
        12,
        h3(strong("Local sensitivity analysis"),style = "color: #888888;"),
        fluidRow(
          column(width = 2, offset = 0.1,  checkboxInput(inputId = "runSensitivityAnalysisForAllParameters", label = "Run for all constants", value = FALSE)),
          column(width = 2, offset = 0.1,  numericInput(inputId = "variationRangeNumericInput", label = h5("Variation range"), value = 0.2,min = 0,max = 1,step = 0.1)),
          column(width = 2, offset = 0.1,  numericInput(inputId = "numberOfStepsNumericInput", label = h5("Number of steps"), value = 2,min = 1,step = 1)),
          column(width = 2, offset = 0.1,  numericInput(inputId = "sensitivityThreshold", label = h5("Minimum sensitivity threshold"), value = 0.1,min = 0,step = 0.05))
        ),
        fluidRow(
          column(width = 2, offset = 0.1,   checkboxInput(inputId = "runUncertaintyCheckbox", label = "Run uncertainty analysis", value = FALSE)),
          column(width = 2, offset = 0.1,   numericInput(inputId = "numberOfUncertaintySamples", label = h5("Number of uncertainty analysis samples"), value = 100)),
          column(width = 2, offset = 0.1,  textInput("quantilesTestInput", label = h5("Quantiles for uncertainty analysis"), value = "0.05,0.25,0.5,0.75,0.95"))
        ),
        fluidRow(
          column(width = 10, offset = 0.1,
                 actionButton(inputId = "startLocalSA", label = strong("Run local sensitivity analysis"), style="color: #ffffff; background-color: #41719C;"),
                 downloadButton(inputId = "getSUCode ", outputId = "getSUCode", label = "Get code"),
                 downloadButton(inputId = "saveLocalSAResults", outputId = "saveLocalSAResults", label = "Save local sensitivity analysis results"),
                 downloadButton(inputId = "saveTornadoGGPLOT", outputId = "saveTornadoGGPLOT", label = "ggplot"),
                 downloadButton(inputId = "saveTornadoPNG", outputId = "saveTornadoPNG", label = "PNG"))
        ),
        tags$hr(style="border-color: black;"),

        h3(strong("Global sensitivity analysis"),style = "color: #888888;"),
        h4(strong("Sobol"),style = "color: #888888;"),
        fluidRow(
          column(width = 2,  offset = 0.1,
                 numericInput(inputId = "numberOfSamplesGSA", label = h5("Number of samples"), value = 10)),
          column(width = 10,  offset = 0.1,
                 actionButton(inputId = "startGSA", label = strong("Run Sobol") , style = "margin-top: 40px; color: #ffffff; background-color: #41719C;"),
                 downloadButton(inputId = "getSobolCode", outputId = "getSobolCode", label = "Get code" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveGSAResults", outputId = "saveGSAResults", label = "Save Sobol results", style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveGSAGGPLOT", outputId = "saveGSAGGPLOT", label = "ggplot" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveGSAPNG", outputId = "saveGSAPNG", label = "PNG" , style = "margin-top: 40px;"))
        ),
        tags$hr(style="border-color: black;"),

        h4(strong("EFAST"),style = "color: #888888;"),
        fluidRow(
          column(width = 2,  offset = 0.1,
                 numericInput(inputId = "numberOfReSamplesEFAST", label = h5("Number of re-sampling runs"), value = 10)),
          column(width = 10,  offset = 0.1,
                 actionButton(inputId = "startEFAST", label = strong("Run EFAST") , style = "margin-top: 40px; color: #ffffff; background-color: #41719C;"),
                 downloadButton(inputId = "getEFASTCode", outputId = "getEFASTCode", label = "Get code" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveEFASTResults", outputId = "saveEFASTResults", label = "Save EFAST results", style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveEFASTGGPLOT", outputId = "saveEFASTGGPLOT", label = "ggplot" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveEFASTPNG", outputId = "saveEFASTPNG", label = "PNG" , style = "margin-top: 40px;")),

        ),
        tags$hr(style="border-color: black;"),


        h4(strong("Morris sensitivity analysis"),style = "color: #888888;"),
        fluidRow(
          column(width = 2,  offset = 0.1,
                 numericInput(inputId = "numberOfSamplesMorris", label = h5("Number of samples"), value = 10)),
          column(width = 10,  offset = 0.1,
                 actionButton(inputId = "startMorris", label = strong("Run Morris sensitivity analysis") , style = "margin-top: 40px; color: #ffffff; background-color: #41719C;"),
                 downloadButton(inputId = "getMorrisCode", outputId = "getMorrisCode", label = "Get code" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveMorrisResults", outputId = "saveMorrisResults", label = "Save Morris sensitivity analysis results" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveMorrisGGPLOT", outputId = "saveMorrisGGPLOT", label = "ggplot" , style = "margin-top: 40px;"),
                 downloadButton(inputId = "saveMorrisPNG", outputId = "saveMorrisPNG", label = "PNG" , style = "margin-top: 40px;"))
        )
      )
      )
    )
  )
)


shinyApp(ui, server)
}
