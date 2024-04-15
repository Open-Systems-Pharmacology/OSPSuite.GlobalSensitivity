
getUncertaintyClass <- function(uncertaintyValue) {
  if (is.na(uncertaintyValue)) {
    return(NA)
  }

  if (is.nan(uncertaintyValue)) {
    return(NA)
  }

  absVal <- abs(uncertaintyValue)

  if (absVal >= 2) {
    return("High")
  }

  if (absVal >= 0.3) {
    return("Medium")
  }

  return("Low")
}

getSensitivityClass <- function(sensitivityValue) {
  if (is.na(sensitivityValue)) {
    return(NA)
  }

  if (is.nan(sensitivityValue)) {
    return(NA)
  }

  absVal <- abs(sensitivityValue)

  if (absVal >= 0.5) {
    return("High")
  }

  if (absVal >= 0.2) {
    return("Medium")
  }

  if (absVal >= 0.1) {
    return("Low")
  }

  return("Negligible")
}



#' @export
getParameterListForUncertaintyAnalysis <- function(parameterPaths, simulation, variationRange = 0.1) {

  parameters <- list()

  # Get all valid parameters from simulation

  for (path in parameterPaths) {
    ospParameter <- ospsuite::getParameter(path = path, container = simulation)

    if (is.nan(ospParameter$value)) {
      next
    }

    if (ospParameter$value == 0) {
      next
    }

    boundaryValues <- c(ospParameter$value / (1 + variationRange), ospParameter$value * (1 + variationRange))

    parameters <- c(parameters, SAParameter$new(
      simulation = simulation,
      path = path,
      defaultVariationRangeForLogUniformDistributions = variationRange
    ))
  }

  # Name parameters list with paths of each parameter
  names(parameters) <- sapply(parameters, function(x) {
    x$path
  })

  return(parameters)
}


#' @export
runSU <- function(simulation,
                  DDIsimulation = NULL,
                  customParameters = NULL,
                  outputs,
                  evaluateForAllParameters = FALSE,
                  ##Sensitivity analysis settings
                  variationRange = 0.1,
                  numberOfSensitivityAnalysisSteps = 2,
                  sensitivityThreshold = 0.1,
                  ##Uncetainty analysis settings
                  runUncertaintyAnalysis = TRUE,
                  runUncertaintlyOnlyForSensitiveParameters = TRUE,
                  numberOfUncertaintyAnalysisSamples = 100,
                  quantiles = c(0.05, 0.5, 0.95),
                  ##Save settings
                  saveResults = FALSE,
                  saveFolder = NULL,
                  saveFileName = NULL,
                  #Parallel run and updating settings
                  runParallel = TRUE,
                  updateProgressSensitivity = NULL,
                  updateProgressUncertainty = NULL) {
  print("Loading parameters.")
  error(condition = is.null(outputs),errorMessage = "No model outputs have been selected.")
  error(!(variationRange > 0 & variationRange < 1), paste0("Argument 'variationRange' must lie strictly between 0 and 1."))
  error(!(numberOfSensitivityAnalysisSteps %% 1 == 0), paste0("Argument 'numberOfSensitivityAnalysisSteps' must be an integer."))
  error(!(numberOfSensitivityAnalysisSteps > 0), paste0("Argument 'numberOfSensitivityAnalysisSteps' must be positive."))
  error(any(quantiles > 1), "Elements of the vector 'quantiles' must be less than 1.")
  error(any(quantiles < 0), "Elements of the vector 'quantiles' must be greater than 0.")

  #Set nominal parameter values in simulation based on any customParameters
  parameterPaths <- NULL
  customParameterPaths <- NULL
  originalParameterValues <- NULL
  if (!is.null(customParameters)) {

    names(customParameters) <- sapply(customParameters, function(x){x$path})
    customParameterPaths <- names(customParameters)

    originalParameterValues <- sapply(customParameterPaths, function(pth) {
      return(ospsuite::getParameter(path = pth,container = simulation)$value)
    })

    ospsuite::setParameterValuesByPath(
      simulation = simulation,
      parameterPaths = customParameterPaths,
      values = sapply(customParameterPaths, function(pth) {
        medianValue <- customParameters[[pth]]$distribution$quantilesToSample(quantiles = 0.5)
        medianValueInBaseUnits <- ospsuite::toBaseUnit(
          quantityOrDimension = customParameters[[pth]]$dimension,
          values = medianValue,
          unit = customParameters[[pth]]$unit
        )
        return(medianValueInBaseUnits)
      })
    )

    if(!is.null(DDIsimulation)){
      ospsuite::setParameterValuesByPath(
        simulation = DDIsimulation,
        parameterPaths = customParameterPaths,
        values = sapply(customParameterPaths, function(pth) {
          medianValue <- customParameters[[pth]]$distribution$quantilesToSample(quantiles = 0.5)
          medianValueInBaseUnits <- ospsuite::toBaseUnit(
            quantityOrDimension = customParameters[[pth]]$dimension,
            values = medianValue,
            unit = customParameters[[pth]]$unit
          )
          return(medianValueInBaseUnits)
        })
      )
    }


  }

  additionalParameterPaths <- NULL
  if(evaluateForAllParameters){
    additionalParameterPaths <- sapply(ospsuite::getAllParametersForSensitivityAnalysisMatching(paths = "**",simulation = simulation),function(x){x$path})
    additionalParameterPaths <- setdiff(additionalParameterPaths,customParameterPaths)
  }

  #Set outputs
  outputPaths <- sapply(outputs, function(op) {
    op$path
  })
  names(outputs) <- outputPaths


  checkParametersExistInSimulation(simulation = simulation,
                                   parameterPaths = c(additionalParameterPaths,customParameterPaths),
                                   simulationName =  "simulation",
                                   stopIfNotFound = TRUE)

  checkOutputsExistInSimulation(simulation = simulation,
                                outputPaths = outputPaths,
                                simulationName =  "simulation",
                                stopIfNotFound = TRUE)


  if(!is.null(DDIsimulation)){
    checkParametersExistInSimulation(simulation = DDIsimulation,
                                     parameterPaths = c(additionalParameterPaths,customParameterPaths),
                                     simulationName =  "DDI simulation",
                                     stopIfNotFound = TRUE)

    checkOutputsExistInSimulation(simulation = DDIsimulation,
                                  outputPaths = outputPaths,
                                  simulationName =  "DDI simulation",
                                  stopIfNotFound = TRUE)
  }

  simulation$outputSelections$clear()
  ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = simulation)


  if(!is.null(DDIsimulation)){
    DDIsimulation$outputSelections$clear()
    ospsuite::addOutputs(quantitiesOrPaths = names(outputs), simulation = DDIsimulation)
  }

  allParameterPaths <- c(customParameterPaths,additionalParameterPaths)
  sensitivityDataframe <- getLocalSensitivity(simulation,
                                              DDIsimulation = DDIsimulation,
                                              parameterPaths = allParameterPaths,
                                              outputs,
                                              variationRange = variationRange,
                                              numberOfSteps = numberOfSensitivityAnalysisSteps,
                                              runParallel = TRUE,
                                              updateProgress = updateProgressSensitivity)

  sensitiveParameterPaths <- allParameterPaths
  if(runUncertaintlyOnlyForSensitiveParameters){
    sensitivityDataframe <- na.omit(sensitivityDataframe)
    sensitiveParameterPaths <- as.character(unique(sensitivityDataframe$parameterPath[abs(sensitivityDataframe$sensitivity) > sensitivityThreshold]))
    sensitivityDataframe <- sensitivityDataframe[sensitivityDataframe$parameterPath %in% sensitiveParameterPaths,]
  }

  parameters <- customParameters
  if(evaluateForAllParameters){
    parameters <- c(parameters,
                    getParameterListForUncertaintyAnalysis(additionalParameterPaths, simulation, variationRange = variationRange))
  }
  if (runUncertaintyAnalysis) {

    uncertaintyAnalysisResults <- getUncertaintyAnalysisResults(simulation,
                                                                DDIsimulation = DDIsimulation,
                                                                parameters,
                                                                outputs,
                                                                numberOfUncertaintyAnalysisSamples,
                                                                sensitiveParameterPaths,
                                                                runParallel = TRUE,
                                                                updateProgress = updateProgressUncertainty)
  }

  # Build summary of sensitivity and uncertainty results
  allSUResults <- NULL
  suResultsList <- list()
  for (outPth in names(outputs)) {
    suResultsList[[outPth]] <- list()

    pkNames <- unique( sensitivityDataframe$pkParameter[sensitivityDataframe$outputPath == outPth] )
    if (runUncertaintyAnalysis) {
      pkNames <- unique(c( sensitivityDataframe$pkParameter[sensitivityDataframe$outputPath == outPth] ,
                           uncertaintyAnalysisResults$pkParameter[uncertaintyAnalysisResults$outputPath == outPth] ))
    }


    for (pk in pkNames){
      #CHECK
      #for (pk in outputs[[outPth]]$pkParameterList) {
      suResultsList[[outPth]][[pk]] <- NULL

      for (pth in sensitiveParameterPaths) {
        sensitivityValue <- sensitivityDataframe$sensitivity[sensitivityDataframe$parameterPath == pth & sensitivityDataframe$outputPath == outPth & sensitivityDataframe$pkParameter == pk]
        if (length(sensitivityValue) == 0) {
          sensitivityValue <- NA
        }

        df <- data.frame(
          Output = outPth,
          OutputDisplayName = outputs[[outPth]]$displayName,
          PK = pk,
          Parameter = pth,
          ParameterDisplayName = parameters[[pth]]$displayName,
          Sensitivity = sensitivityValue,
          SensitivityClass = getSensitivityClass(sensitivityValue)
        )


        if (runUncertaintyAnalysis) {
          uncertaintyVec <- na.omit(uncertaintyAnalysisResults$value[uncertaintyAnalysisResults$parameterPath == pth & uncertaintyAnalysisResults$outputPath == outPth & uncertaintyAnalysisResults$pkParameter == pk])
          u02p5 <- unname(quantile(uncertaintyVec, 0.025))
          u50 <- unname(quantile(uncertaintyVec, 0.5))
          u97p5 <- unname(quantile(uncertaintyVec, 0.975))
          uncertaintyValue <- (u97p5 - u02p5)/u50
          uMean <- mean(uncertaintyVec)
          # Case where PK can't be evaluated
          if (any(c(NaN, NA) %in% c(u02p5, u50, u97p5))) {
            uncertaintyValue <- 0
          }
          # Case where u50 = u95 = 0
          if (uncertaintyValue %in% c(NaN, NA)) {
            uncertaintyValue <- 0
          }

          df[["Mean"]] <- uMean
          df[["Percentile_2.5"]] <- u02p5
          df[["Median"]] <- u50
          df[["Percentile_97.5"]] <- u97p5
          df[["UncertaintyRatio"]] <- uncertaintyValue
          df[["UncertaintyClass"]] <- getUncertaintyClass(uncertaintyValue)
          for (qv in quantiles) {
            df[[paste0("Percentile_", 100 * qv)]] <- unname(quantile(uncertaintyVec, qv))
          }
        }
        suResultsList[[outPth]][[pk]] <- rbind.data.frame(suResultsList[[outPth]][[pk]], df)
      }

      if(!is.null(suResultsList[[outPth]][[pk]])){
        if (runUncertaintyAnalysis) {
          suResultsList[[outPth]][[pk]] <- suResultsList[[outPth]][[pk]][with(suResultsList[[outPth]][[pk]], order(-UncertaintyRatio, -Sensitivity)), ]
          exclusionRows <- (suResultsList[[outPth]][[pk]]$SensitivityClass %in% c(NA)) & (suResultsList[[outPth]][[pk]]$UncertaintyClass %in% NA)
        } else {
          suResultsList[[outPth]][[pk]] <- suResultsList[[outPth]][[pk]][with(suResultsList[[outPth]][[pk]], order(-Sensitivity)), ]
          exclusionRows <- (suResultsList[[outPth]][[pk]]$SensitivityClass %in% c(NA))
        }

        # Exclude rows that have negligible sensitivity or NA sensitivity or NA uncertainty
        suResultsList[[outPth]][[pk]] <- suResultsList[[outPth]][[pk]][!exclusionRows, ]

        allSUResults <- rbind.data.frame(allSUResults , suResultsList[[outPth]][[pk]])
      }
    }
  }

  summaryTable <- getSUSummaryDf(su = allSUResults)
  settings <- buildSettingsCMD(parameters = parameters,outputs = outputs)
  suResults <- list(Results = allSUResults, Settings = settings)

  suResults <- c(suResults,summaryTable)

  if(saveResults){

    dateTime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"))

    if(is.null(saveFileName)){
      saveFileName <- paste0("sensitivity-uncertainty-summary-", dateTime, ".xlsx" )
    }

    if (is.null(saveFolder)) {
      saveFolder <- getwd()
    }

    writexl::write_xlsx(x = suResults,
                        path = file.path(saveFolder,saveFileName))

  }

  print(allSUResults)

  if(!is.null(originalParameterValues)){
    ospsuite::setParameterValuesByPath(parameterPaths = names(originalParameterValues) ,values = unname(originalParameterValues),simulation = simulation)
  }

  return(suResults)
}

sensitivityLevels <- c("High","Medium","Low","Negligible",NA)

cleanUpSUDf <- function(df){
  newDf <- NULL
  for (sen in sensitivityLevels){
    subdf <- df[df$Index %in% sen,]
    for (unc in c("High","Medium","Low","Parameter")){
      if(!(unc %in% names(subdf))){
        next
      }
      subdf[[unc]] <- sort(x = subdf[[unc]],decreasing = TRUE)
    }
    if(nrow(subdf)>0){
      subdf$Index <- rep("",nrow(subdf))
    }

    if("Parameter" %in% names(subdf)){
      subdf <- rbind( data.frame("Index" = sen,"Parameter" = "") , subdf )
    } else {
      subdf <- rbind( data.frame("Index" = sen,"High" = "","Medium" = "","Low" = "") , subdf )
    }

    for (nr in rev(seq_len(nrow(subdf)))  ){
      whichRowsEmpty <- sapply(names(subdf),function(x){subdf[nr,x] == ""})
      whichRowsEmpty[is.na(whichRowsEmpty)] <- FALSE
      allEmpty <- all(whichRowsEmpty)
      if(allEmpty){
        subdf <- subdf[-nr,]
      }
    }
    newDf <- rbind.data.frame(newDf,subdf)
  }
  return(newDf)
}

renameSUDf <- function(df){
  names(df)[names(df) == "High"] <- "High uncertainty"
  names(df)[names(df) == "Medium"] <- "Medium uncertainty"
  names(df)[names(df) == "Low"] <- "Low uncertainty"
  df$Index[df$Index == "High"] <- "High sensitivity"
  df$Index[df$Index == "Medium"] <- "Medium sensitivity"
  df$Index[df$Index == "Low"] <- "Low sensitivity"
  df$Index[df$Index == "Negligible"] <- "Negligible sensitivity"
  df$Index[df$Index %in% NA] <- "Sensitivity NA"
  return(df)
}

addLabelsSUDF <- function(df,output,pkParameter){
  nr <- nrow(df)
  outputCol <- rep("",nr)
  outputCol[1] <- output
  pkCol <- rep("",nr)
  pkCol[1] <- pkParameter
  df <- cbind(data.frame("Output" = outputCol, PKparameter = pkCol),df)
}

getSUSummaryDf<- function(su){
  sumlist <- list()
  counter <- 0
  for (op in unique(su$Output)){
    for (pk in unique(su$PK[su$Output == op])){
      counter <- counter + 1
      subsu <- su[su$Output == op & su$PK == pk,]
      df <- NULL
      for (sens in sensitivityLevels){

        pars <- subsu$Parameter[subsu$SensitivityClass %in% sens]
        if(length(pars) > 0){
          for (pp in pars){
            ppDisplayName <- subsu$ParameterDisplayName[subsu$SensitivityClass %in% sens & subsu$Parameter == pp]
            if ("UncertaintyClass" %in% names(subsu)){
              uncLevel <- subsu$UncertaintyClass[subsu$SensitivityClass %in% sens & subsu$Parameter == pp]
              newRow <- data.frame("Index" = sens,"High" = "","Medium" = "","Low" = "")
              newRow[[uncLevel]] <- ppDisplayName
              df <- rbind(df,newRow)
            } else {
              newRow <- data.frame("Index" = sens,"Parameter" = "")
              newRow[["Parameter"]] <- ppDisplayName
              df <- rbind(df,newRow)
            }
          }
        }
      }
      df <- cleanUpSUDf(df)
      df <- renameSUDf(df)
      df <- addLabelsSUDF(df,op,pk)
      summaryName <- paste0(tail(ospsuite::toPathArray(op),1),"_",pk)
      sumlist[[summaryName]] <- df
    }
  }
  return(sumlist)
}
