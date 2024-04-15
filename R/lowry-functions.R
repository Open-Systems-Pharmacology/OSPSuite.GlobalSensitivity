#' @export
generateLowryPlot <- function(gsaResultsDataframe){
  plotList <- list()

  for (op in unique( gsaResultsDataframe[["Output"]] )){
    plotList[[op]] <- list()
    for (pk in unique( gsaResultsDataframe[["PK"]][ gsaResultsDataframe[["Output"]] == op ] )){
      df <- gsaResultsDataframe[gsaResultsDataframe$Output == op & gsaResultsDataframe$PK == pk,]
      outputDisplayName <- unique(df$OutputDisplayName)
      plotList[[op]][[pk]] <- getLowryPlot(df,outputDisplayName,pk)
    }
  }
  return(plotList)
}

getLowryPlot <- function(df,outputDisplayName,pk){
  sortedParameterOrder <- order(-df[df$Measure == "FirstOrder",]$Value)
  sortedParameters <- df[df$Measure == "FirstOrder",][sortedParameterOrder,]$Parameter
  sortedParameterDisplayName <- df[df$Measure == "FirstOrder",][sortedParameterOrder,]$ParameterDisplayName
  df$ParameterRanks <- sapply(df$Parameter,function(par){which(sortedParameters == par)})

  df$Measure <- factor(df$Measure, levels = c("Total","FirstOrder"))

  dfFirstOrder <- df[df$Measure == "FirstOrder",][sortedParameterOrder,]
  dfTotal <- df[df$Measure == "Total",][sortedParameterOrder,]

  N <- nrow(dfFirstOrder)

  LB <- NULL
  UB <- NULL

  if (N == 1){
    LB <- 1
    UB <- 1
  }

  if (N > 1){
    for (n in 1:(N-1)){
      LB[n] <- sum(dfFirstOrder$Value[1:n])
      UB[n] <- 1 - sum(dfFirstOrder$Value[(n+1):N])
    }
    LB[N] <- sum(dfFirstOrder$Value[1:N])
    UB[N] <- 1
  }


  dfBar <- dfFirstOrder
  dfBar$Measure <- "Interaction"
  dfBar$Value <- sapply(UB - LB,function(x){max(x,0)})
  dfBar <- rbind(dfFirstOrder,dfBar)
  dfBar$Measure <- factor(dfBar$Measure, levels = c("Interaction","FirstOrder"))

  dfRibbon <- dfFirstOrder
  dfRibbon$ymin <- LB
  dfRibbon$ymax <- UB
  dfRibbon$ymean <- rowMeans(cbind(LB,UB))

  plt <- ggplot(data = dfBar) + geom_bar(mapping = aes(x = ParameterRanks,
                                                       y = Value,
                                                       fill = Measure),
                                         stat = "identity")

  plt <- plt + geom_ribbon(data = dfRibbon,
                           mapping = aes(x = ParameterRanks,
                                         ymin = ymin,
                                         ymax = ymax),alpha = 0.5)

  plt <- plt + geom_line(data = dfRibbon,
                         mapping = aes(x = ParameterRanks,
                                       y = ymean))

  plt <- plt + scale_x_continuous(name="Parameter",
                                  labels = sortedParameterDisplayName,
                                  breaks=seq_along(sortedParameters))

  plt <- plt + scale_y_continuous(name="First order effects and interactions")
  plt <- plt + ggtitle(label = paste("Global sensitivity of" , pk, "of" , outputDisplayName))
  return(plt)
}

#' @export
pltGSABarGraph <- function(df){
  parameterOrder <- df[df$Measure == "FirstOrder",]$Parameter[order(-df$Value[df$Measure == "FirstOrder"])]
  df$Parameter <- factor(df$Parameter , levels = parameterOrder)
  plt <- ggplot(data = df) + geom_bar(mapping = aes(x = Parameter,
                                                    y = Value,
                                                    fill = Measure),
                                      stat = "identity",
                                      color = "black",
                                      position = position_dodge(0.8)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                         hjust = 1));

  xlabels <- unique(df[,c("Parameter","ParameterDisplayName")])$ParameterDisplayName
  names(xlabels) <- unique(df[,c("Parameter","ParameterDisplayName")])$Parameter
  plt <- plt + scale_x_discrete(labels =  xlabels)
  return(plt)
}
