#' @title runGUI
#' @description Function to run Shiny App GUI interface to `ospsuite.globalsensitivity` package.
#' @export
runGUI <- function() {
  options(shiny.maxRequestSize = 30 * 1024^2)
  shinyApp(ui = guiUI(), server = guiServer)
}

guiSteps <- c(
  "Start" = "Simulation",
  "Parameters" = "Parameters",
  "Outputs" = "Outputs",
  "Analyses" = "Analyses"
)

guiDistributionArguments <- list(
  "Uniform" = c("minimum" = "Minimum", "maximum" = "Maximum"),
  "LogUniform" = c("minimum" = "Minimum", "maximum" = "Maximum"),
  "Normal" = c("mean" = "Mean", "stdv" = "Standard deviation"),
  "LogNormal" = c("mean" = "Mean", "CV" = "Coefficient of variation")
)

guiPKParameterChoices <- c("Cmax" = "C_max", "AUC (tEnd)" = "AUC_tEnd", "C (tEnd)" = "C_tEnd")

guiTheme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2f5bd3",
    secondary = "#64748b",
    success = "#15803d",
    info = "#0e7490",
    warning = "#b45309",
    danger = "#b91c1c",
    bg = "#ffffff",
    fg = "#1e293b",
    base_font = bslib::font_collection("Inter", "Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    code_font = bslib::font_collection("Cascadia Code", "Consolas", "SFMono-Regular", "Menlo", "monospace"),
    "border-radius" = "0.6rem",
    "card-border-color" = "#e2e8f0"
  )
}

guiCSS <- "
body { background: #f3f5fa; }
.app-header {
  background: linear-gradient(120deg, #1e3a8a 0%, #2f5bd3 55%, #0e7490 100%);
  color: #fff;
  padding-top: 1.1rem;
  box-shadow: 0 4px 18px rgba(30, 58, 138, .25);
}
.app-brand { display: flex; align-items: center; gap: .85rem; }
.app-brand .app-logo {
  width: 44px; height: 44px; border-radius: 12px;
  background: rgba(255, 255, 255, .16);
  display: grid; place-items: center; font-size: 1.3rem;
}
.app-brand h1 { font-size: 1.3rem; font-weight: 700; margin: 0; letter-spacing: -.01em; }
.app-brand p { margin: 0; font-size: .85rem; opacity: .8; }
.stepper { display: flex; gap: .5rem; margin-top: 1rem; overflow-x: auto; }
.stepper .step {
  flex: 1; min-width: 140px;
  display: flex; align-items: center; gap: .6rem;
  background: transparent; border: 0; border-bottom: 3px solid transparent;
  color: rgba(255, 255, 255, .72); font-weight: 600; text-align: left;
  padding: .55rem .25rem .8rem; transition: color .15s, border-color .15s;
}
.stepper .step .step-index {
  flex: none; width: 28px; height: 28px; border-radius: 50%;
  display: grid; place-items: center; font-size: .8rem;
  border: 2px solid rgba(255, 255, 255, .55);
}
.stepper .step:not(:disabled):hover { color: #fff; }
.stepper .step.complete .step-index { background: rgba(255, 255, 255, .22); border-color: transparent; }
.stepper .step.active { color: #fff; border-bottom-color: #fff; }
.stepper .step.active .step-index { background: #fff; color: #1e3a8a; border-color: #fff; }
.stepper .step:disabled { opacity: .45; cursor: not-allowed; }
.app-main { padding: 1.75rem 0 3rem; }
.page-heading { display: flex; justify-content: space-between; align-items: flex-end; flex-wrap: wrap; gap: 1rem; margin-bottom: 1.25rem; }
.page-heading h2 { font-size: 1.45rem; font-weight: 700; margin: 0; }
.page-heading p { margin: .25rem 0 0; color: #64748b; }
.card { box-shadow: 0 1px 2px rgba(15, 23, 42, .04), 0 6px 18px rgba(15, 23, 42, .05); }
.card-header { background: #fff; font-weight: 600; }
.tree-card { position: sticky; top: 1rem; }
.tree-card .card-body { max-height: 72vh; overflow: auto; }
.tree-card .jstree-proton { font-size: .9rem; }
.selection-list { display: flex; flex-direction: column; gap: 1rem; }
.selection-list > .card { margin-bottom: 0; }
.selection-card .card-header { display: flex; justify-content: space-between; align-items: center; gap: .75rem; }
.path-chip {
  font-family: var(--bs-font-monospace); font-size: .78rem; font-weight: 500;
  background: #eef2ff; color: #3730a3; padding: .2rem .55rem; border-radius: .4rem;
  word-break: break-all;
}
.meta { font-size: .8rem; color: #64748b; white-space: nowrap; }
.count-badge { background: #eef2ff; color: #3730a3; font-weight: 600; border-radius: 999px; padding: .15rem .65rem; font-size: .8rem; }
.empty-state { text-align: center; padding: 3rem 1rem; color: #64748b; }
.empty-state .empty-icon { font-size: 2.2rem; opacity: .45; margin-bottom: .75rem; }
.empty-state h5 { color: #334155; font-weight: 600; }
.wizard-footer { display: flex; justify-content: space-between; gap: 1rem; margin-top: 1.5rem; }
.btn { font-weight: 600; }
.pk-pills .shiny-options-group { display: flex; flex-wrap: wrap; gap: .5rem; }
.pk-pills .checkbox-inline { position: relative; margin: 0; padding: 0; }
.pk-pills .checkbox-inline input { position: absolute; opacity: 0; }
.pk-pills .checkbox-inline span {
  display: inline-block; padding: .35rem .9rem; border-radius: 999px; cursor: pointer;
  border: 1px solid #cbd5e1; background: #fff; transition: all .15s;
}
.pk-pills .checkbox-inline input:checked + span { background: var(--bs-primary); border-color: var(--bs-primary); color: #fff; }
.pk-pills .checkbox-inline input:focus-visible + span { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
.start-card { max-width: 760px; margin: 0 auto; }
.file-slot { border: 1.5px dashed #cbd5e1; border-radius: .75rem; padding: 1rem 1.1rem .25rem; background: #f8fafc; }
.file-slot h6 { font-weight: 600; margin-bottom: .15rem; }
.file-slot p { font-size: .82rem; color: #64748b; margin-bottom: .6rem; }
.loaded-status { display: flex; align-items: center; gap: .5rem; color: #15803d; font-weight: 600; }
.analysis-sidebar .shiny-input-container { width: 100%; }
.sidebar-section { font-size: .72rem; font-weight: 700; text-transform: uppercase; letter-spacing: .06em; color: #64748b; margin: .5rem 0 .4rem; }
.export-group { display: grid; grid-template-columns: repeat(3, 1fr); gap: .4rem; }
.export-group .btn { font-size: .8rem; padding: .35rem .25rem; }
.viewer-controls { display: flex; flex-wrap: wrap; gap: 1rem; align-items: flex-end; }
.viewer-controls .shiny-input-container { margin-bottom: .5rem; }
.method-intro { color: #475569; font-size: .9rem; }
.bslib-value-box .value-box-title { font-size: .8rem; text-transform: uppercase; letter-spacing: .05em; }
.bslib-value-box .value-box-value { font-size: 1.2rem; overflow-wrap: anywhere; }
"

#' @title guiSelectionTree
#' @description Converts a tree returned by `ospsuite::getSimulationTree` into a `shinyTree` list whose leaves are the quantities themselves.
#' A quantity that also contains other quantities, such as a molecule with observers, is represented by a leaf with its own name inside its node.
#' @param simulationTree A list returned by `ospsuite::getSimulationTree`.
#' @return A list with the `shinyTree` list `tree` and an environment `leafPaths` mapping the path of each leaf in the tree to the path of its quantity.
#' @noRd
guiSelectionTree <- function(simulationTree) {
  leafPaths <- new.env(hash = TRUE)
  convert <- function(node, ancestry) {
    children <- setdiff(names(node), "path")
    tree <- list()
    if (!is.null(node$path) && length(children) > 0) {
      selfName <- ancestry[length(ancestry)]
      if (selfName %in% children) {
        selfName <- paste(selfName, "(self)")
      }
      tree[[selfName]] <- ""
      assign(ospsuite::toPathString(c(ancestry, selfName)), node$path, envir = leafPaths)
    }
    for (child in children) {
      childAncestry <- c(ancestry, child)
      if (identical(names(node[[child]]), "path")) {
        tree[[child]] <- ""
        assign(ospsuite::toPathString(childAncestry), node[[child]]$path, envir = leafPaths)
      } else {
        tree[[child]] <- convert(node[[child]], childAncestry)
      }
    }
    return(tree)
  }
  return(list(tree = convert(simulationTree, character(0)), leafPaths = leafPaths))
}

#' @title guiUI
#' @description Builds the user interface of the GUI.
#' @noRd
guiUI <- function() {
  # actionButton() and downloadButton() always add "btn-default", which would override the Bootstrap 5 button styles
  withoutDefaultStyle <- function(button) {
    htmltools::tagQuery(button)$removeClass("btn-default")$allTags()
  }
  styledActionButton <- function(inputId, label, class, icon = NULL) {
    withoutDefaultStyle(actionButton(inputId = inputId, label = label, icon = icon, class = class))
  }
  styledDownloadButton <- function(outputId, label, iconName, class) {
    withoutDefaultStyle(downloadButton(outputId = outputId, label = label, icon = icon(iconName), class = class))
  }
  # Selection step: a sticky tree card next to the forms of the selected quantities.
  # Plain Bootstrap markup is used so that the tree card is not stretched by bslib's fill layout.
  selectionLayout <- function(treeId, treeTitle, formsId) {
    tree <- shinyTree::shinyTree(outputId = treeId, checkbox = TRUE, search = TRUE, theme = "proton", themeIcons = FALSE)
    tree <- htmltools::tagQuery(tree)$filter("input")$addClass("form-control form-control-sm mb-2")$addAttrs(placeholder = "Search the tree...")$allTags()
    div(
      class = "row g-4",
      div(
        class = "col-lg-5",
        div(
          class = "card tree-card",
          div(class = "card-header", icon("sitemap"), " ", treeTitle),
          div(class = "card-body", tree)
        )
      ),
      div(class = "col-lg-7", uiOutput(formsId))
    )
  }
  wizardFooter <- function(back = NULL, nextButton = NULL) {
    div(
      class = "wizard-footer",
      div(back),
      div(nextButton)
    )
  }
  backButton <- function(id) {
    styledActionButton(inputId = id, label = "Back", icon = icon("arrow-left"), class = "btn-outline-secondary")
  }
  nextButton <- function(id, label = "Next") {
    styledActionButton(inputId = id, label = tagList(label, icon("arrow-right")), class = "btn-primary px-4")
  }
  pageHeading <- function(title, subtitle, ...) {
    div(class = "page-heading", div(h2(title), p(subtitle)), div(...))
  }
  exportButtons <- function(resultsId, ggplotId, pngId) {
    tagList(
      div(class = "sidebar-section", "Export results"),
      div(
        class = "export-group",
        styledDownloadButton(resultsId, "Excel", "file-excel", "btn-outline-primary btn-sm"),
        styledDownloadButton(ggplotId, "ggplot", "file-code", "btn-outline-primary btn-sm"),
        styledDownloadButton(pngId, "PNG", "file-image", "btn-outline-primary btn-sm")
      )
    )
  }
  analysisPanel <- function(title, viewerId, intro, settings, runId, runLabel, codeId, exports) {
    bslib::nav_panel(
      title = title,
      bslib::layout_sidebar(
        border = FALSE,
        fillable = FALSE,
        sidebar = bslib::sidebar(
          width = 340,
          open = "always",
          class = "analysis-sidebar",
          p(class = "method-intro", intro),
          div(class = "sidebar-section", "Settings"),
          settings,
          div(
            class = "d-grid gap-2 mt-2",
            styledActionButton(inputId = runId, label = runLabel, icon = icon("play"), class = "btn-primary"),
            styledDownloadButton(codeId, "Get R code", "code", "btn-outline-secondary")
          ),
          exports
        ),
        uiOutput(paste0(viewerId, "ResultsUI"))
      )
    )
  }

  bslib::page(
    title = "OSP Global Sensitivity",
    theme = guiTheme(),
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(guiCSS))),
    tags$header(
      class = "app-header",
      div(
        class = "container-xl",
        div(
          class = "app-brand",
          div(class = "app-logo", icon("chart-simple")),
          div(
            h1("OSP Global Sensitivity"),
            p("Local, global and uncertainty analyses for PK-Sim and MoBi simulations")
          )
        ),
        uiOutput("stepper")
      )
    ),
    tags$main(
      class = "app-main container-xl",
      bslib::navset_hidden(
        id = "wizard",
        bslib::nav_panel_hidden(
          value = "Start",
          bslib::card(
            class = "start-card",
            bslib::card_header(tagList(icon("file-import"), " Load simulation")),
            bslib::card_body(
              div(
                class = "file-slot",
                h6("Simulation file"),
                p("The PKML simulation whose parameters and outputs will be analyzed."),
                fileInput(inputId = "selectPKML", label = NULL, placeholder = "No file selected", accept = ".pkml", width = "100%")
              ),
              div(
                class = "file-slot",
                h6("DDI simulation file ", tags$span(class = "badge text-bg-light fw-normal", "optional")),
                p("A second simulation, e.g. with a perpetrator drug, to analyze drug-drug interaction ratios."),
                fileInput(inputId = "DDIselectPKML", label = NULL, placeholder = "No file selected", accept = ".pkml", width = "100%")
              ),
              uiOutput("loadedStatus")
            ),
            bslib::card_footer(
              class = "d-flex justify-content-end",
              styledActionButton(inputId = "loadPKML", label = tagList("Load and continue", icon("arrow-right")), class = "btn-primary px-4")
            )
          )
        ),
        bslib::nav_panel_hidden(
          value = "Parameters",
          pageHeading(
            "Select parameters",
            "Tick parameters in the simulation tree, then specify the probability distribution of each one.",
            uiOutput("parameterCount", inline = TRUE)
          ),
          selectionLayout("parameterTree", "Simulation parameters", "parametersUI"),
          wizardFooter(backButton("backToloadPKML"), nextButton("goToOutputs"))
        ),
        bslib::nav_panel_hidden(
          value = "Outputs",
          pageHeading(
            "Select outputs",
            "Tick molecules or observers in the simulation tree, then choose the PK parameters to analyze for each.",
            uiOutput("outputCount", inline = TRUE)
          ),
          selectionLayout("outputTree", "Simulation outputs", "outputsUI"),
          wizardFooter(backButton("backToParameters"), nextButton("goToEvaluation"))
        ),
        bslib::nav_panel_hidden(
          value = "Analyses",
          pageHeading(
            "Run sensitivity analyses",
            "Choose an algorithm, adjust its settings and run it. Results can be inspected here and exported."
          ),
          uiOutput("setupSummary"),
          bslib::navset_card_pill(
            id = "analysisTabs",
            analysisPanel(
              title = "Local sensitivity",
              viewerId = "su",
              intro = "One-at-a-time sensitivity of each PK parameter to small parameter variations, optionally followed by an uncertainty analysis.",
              settings = tagList(
                bslib::input_switch(id = "runSensitivityAnalysisForAllParameters", label = "Run for all constant parameters", value = FALSE),
                numericInput(inputId = "variationRangeNumericInput", label = "Variation range", value = 0.2, min = 0, max = 1, step = 0.05),
                numericInput(inputId = "numberOfStepsNumericInput", label = "Number of steps", value = 2, min = 1, step = 1),
                numericInput(inputId = "sensitivityThreshold", label = "Minimum sensitivity threshold", value = 0.1, min = 0, step = 0.05),
                bslib::input_switch(id = "runUncertaintyCheckbox", label = "Run uncertainty analysis", value = FALSE),
                conditionalPanel(
                  condition = "input.runUncertaintyCheckbox",
                  numericInput(inputId = "numberOfUncertaintySamples", label = "Uncertainty samples", value = 100, min = 1, step = 1),
                  textInput(inputId = "quantilesTestInput", label = "Quantiles (comma separated)", value = "0.05, 0.25, 0.5, 0.75, 0.95")
                )
              ),
              runId = "startLocalSA",
              runLabel = "Run local sensitivity",
              codeId = "getSUCode",
              exports = exportButtons("saveLocalSAResults", "saveTornadoGGPLOT", "saveTornadoPNG")
            ),
            analysisPanel(
              title = "Sobol",
              viewerId = "sobol",
              intro = "Variance-based global sensitivity indices (first order and total) estimated by quasi-Monte Carlo sampling.",
              settings = numericInput(inputId = "numberOfSamplesGSA", label = "Number of samples", value = 10, min = 2, step = 1),
              runId = "startGSA",
              runLabel = "Run Sobol",
              codeId = "getSobolCode",
              exports = exportButtons("saveGSAResults", "saveGSAGGPLOT", "saveGSAPNG")
            ),
            analysisPanel(
              title = "EFAST",
              viewerId = "efast",
              intro = "Extended Fourier amplitude sensitivity test: first order and total sensitivity indices from frequency-based sampling.",
              settings = numericInput(inputId = "numberOfReSamplesEFAST", label = "Number of re-sampling runs", value = 10, min = 1, step = 1),
              runId = "startEFAST",
              runLabel = "Run EFAST",
              codeId = "getEFASTCode",
              exports = exportButtons("saveEFASTResults", "saveEFASTGGPLOT", "saveEFASTPNG")
            ),
            analysisPanel(
              title = "Morris",
              viewerId = "morris",
              intro = "Elementary effects screening: the mean absolute effect μ* ranks parameter importance, σ indicates non-linearity and interactions.",
              settings = numericInput(inputId = "numberOfSamplesMorris", label = "Number of samples", value = 10, min = 1, step = 1),
              runId = "startMorris",
              runLabel = "Run Morris",
              codeId = "getMorrisCode",
              exports = exportButtons("saveMorrisResults", "saveMorrisGGPLOT", "saveMorrisPNG")
            )
          ),
          wizardFooter(backButton("backToOutputs"))
        )
      )
    )
  )
}

#' @title guiServer
#' @description Server function of the GUI.
#' @noRd
guiServer <- function(input, output, session) {
  downloadIds <- list(
    su = c("saveLocalSAResults", "saveTornadoGGPLOT", "saveTornadoPNG"),
    sobol = c("saveGSAResults", "saveGSAGGPLOT", "saveGSAPNG"),
    efast = c("saveEFASTResults", "saveEFASTGGPLOT", "saveEFASTPNG"),
    morris = c("saveMorrisResults", "saveMorrisGGPLOT", "saveMorrisPNG")
  )
  setDownloadsEnabled <- function(method, enabled) {
    for (id in downloadIds[[method]]) {
      if (enabled) shinyjs::enable(id) else shinyjs::disable(id)
    }
  }
  for (method in names(downloadIds)) {
    setDownloadsEnabled(method, FALSE)
  }

  showError <- function(title, ...) {
    showModal(modalDialog(title = tagList(icon("triangle-exclamation", class = "text-danger"), " ", title), ..., easyClose = TRUE, footer = modalButton("Close")))
  }

  # Simulations are kept outside the reactive graph so that reloading them after a failed run does not reset the trees
  simulations <- new.env()
  simulationVersion <- reactiveVal(0)

  # Each selected path is mapped to a key that is safe to use in input ids.
  # Keys are never reused so that inputs left over from a previously loaded simulation cannot leak into new forms.
  keyRegistry <- new.env()
  keyRegistry$counter <- 0
  keyRegistry$keys <- character(0)
  keyRegistry$rendered <- character(0)
  pathKey <- function(path) {
    if (!path %in% names(keyRegistry$keys)) {
      keyRegistry$counter <- keyRegistry$counter + 1
      keyRegistry$keys[[path]] <- paste0("k", keyRegistry$counter)
    }
    return(keyRegistry$keys[[path]])
  }
  parameterDefaultsCache <- new.env()

  parameterTypeId <- function(key) paste0("parDist_", key)
  parameterNameId <- function(key) paste0("parName_", key)
  parameterUnitId <- function(key) paste0("parUnit_", key)
  parameterArgumentId <- function(key, type, argument) paste0("parArg_", key, "_", type, "_", argument)
  outputNameId <- function(key) paste0("outName_", key)
  outputPKId <- function(key) paste0("outPK_", key)

  # Value of an input, or `default` if the input has not been rendered yet
  currentValue <- function(id, default) {
    value <- isolate(input[[id]])
    if (is.null(value) && !id %in% keyRegistry$rendered) {
      return(default)
    }
    return(value)
  }

  # ---- Navigation ----
  currentStep <- reactiveVal(1)
  maxStep <- reactiveVal(1)
  goToStep <- function(step) {
    maxStep(max(maxStep(), step))
    currentStep(step)
    bslib::nav_select(id = "wizard", selected = names(guiSteps)[step])
  }

  output$stepper <- renderUI({
    steps <- lapply(seq_along(guiSteps), function(i) {
      status <- if (i == currentStep()) "active" else if (i <= maxStep()) "complete" else "locked"
      tags$button(
        type = "button",
        class = paste("step", status),
        disabled = if (i > maxStep()) NA,
        onclick = sprintf("Shiny.setInputValue('stepNav', %d, {priority: 'event'})", i),
        span(class = "step-index", if (status == "complete") icon("check") else i),
        span(class = "step-label", guiSteps[[i]])
      )
    })
    div(class = "stepper", steps)
  })

  observeEvent(input$stepNav, {
    if (input$stepNav <= maxStep()) {
      goToStep(input$stepNav)
    }
  })
  observeEvent(input$backToloadPKML, goToStep(1))
  observeEvent(input$backToParameters, goToStep(2))
  observeEvent(input$backToOutputs, goToStep(3))

  # ---- Simulation loading ----
  observeEvent(input$loadPKML, {
    if (is.null(input$selectPKML$datapath)) {
      showError("No simulation selected", "Please select a PKML simulation file.")
      return()
    }

    loaded <- tryCatch(
      withProgress(message = "Loading simulation", value = 0.2, {
        main <- ospsuite::loadSimulation(filePath = input$selectPKML$datapath)
        ddi <- NULL
        if (!is.null(input$DDIselectPKML$datapath)) {
          incProgress(0.4, message = "Loading DDI simulation")
          ddi <- ospsuite::loadSimulation(filePath = input$DDIselectPKML$datapath)
        }
        incProgress(0.3, message = "Building simulation trees")
        list(
          sim = main,
          ddi = ddi,
          parameterTree = guiSelectionTree(ospsuite::getSimulationTree(simulationOrFilePath = main, quantityType = "Parameter")),
          outputTree = guiSelectionTree(ospsuite::getSimulationTree(simulationOrFilePath = main, quantityType = c("Molecule", "Observer")))
        )
      }),
      error = function(e) {
        showError("Could not load simulation", p("The selected file could not be loaded as an OSP simulation."), tags$pre(conditionMessage(e)))
        return(NULL)
      }
    )
    if (is.null(loaded)) {
      return()
    }

    simulations$sim <- loaded$sim
    simulations$ddi <- loaded$ddi
    simulations$parameterTree <- loaded$parameterTree
    simulations$outputTree <- loaded$outputTree
    simulations$files <- list(
      sim = input$selectPKML$datapath,
      ddi = input$DDIselectPKML$datapath,
      simName = input$selectPKML$name,
      ddiName = input$DDIselectPKML$name
    )

    keyRegistry$keys <- character(0)
    keyRegistry$rendered <- character(0)
    rm(list = ls(parameterDefaultsCache), envir = parameterDefaultsCache)
    parameterPaths(character(0))
    outputPaths(character(0))
    for (method in names(downloadIds)) {
      results[[method]] <- NULL
      setDownloadsEnabled(method, FALSE)
    }

    simulationVersion(simulationVersion() + 1)
    maxStep(2)
    goToStep(2)
  })

  # Reload simulations from file, e.g. after an analysis failed and may have left modified parameter values behind
  reloadSimulations <- function() {
    tryCatch(
      {
        simulations$sim <- ospsuite::loadSimulation(filePath = simulations$files$sim)
        if (!is.null(simulations$files$ddi)) {
          simulations$ddi <- ospsuite::loadSimulation(filePath = simulations$files$ddi)
        }
      },
      error = function(e) NULL
    )
  }

  output$loadedStatus <- renderUI({
    req(simulationVersion() > 0)
    files <- simulations$files
    div(
      class = "loaded-status",
      icon("circle-check"),
      paste0("Loaded ", files$simName, if (!is.null(files$ddiName)) paste0(" and ", files$ddiName))
    )
  })

  output$parameterTree <- shinyTree::renderTree({
    req(simulationVersion() > 0)
    simulations$parameterTree$tree
  })
  output$outputTree <- shinyTree::renderTree({
    req(simulationVersion() > 0)
    simulations$outputTree$tree
  })

  # Quantity paths of the leaves ticked in a shinyTree
  selectedTreePaths <- function(tree, selectionTree) {
    if (is.null(tree) || simulationVersion() == 0) {
      return(character(0))
    }
    selected <- shinyTree::get_selected(tree = tree, format = "names")
    treePaths <- vapply(selected, function(node) {
      ospsuite::toPathString(c(attr(x = node, "ancestry"), as.character(node)))
    }, character(1))
    paths <- unlist(mget(unique(treePaths), envir = selectionTree$leafPaths, ifnotfound = list(NULL)))
    return(unique(unname(as.character(paths))))
  }

  parameterPaths <- reactiveVal(character(0))
  outputPaths <- reactiveVal(character(0))

  parameterTreeSelection <- debounce(reactive(selectedTreePaths(input$parameterTree, simulations$parameterTree)), 300)
  outputTreeSelection <- debounce(reactive(selectedTreePaths(input$outputTree, simulations$outputTree)), 300)
  observe(parameterPaths(parameterTreeSelection()))
  observe(outputPaths(outputTreeSelection()))

  countBadge <- function(n, singular, plural) {
    span(class = "count-badge", paste(n, if (n == 1) singular else plural, "selected"))
  }
  output$parameterCount <- renderUI(countBadge(length(parameterPaths()), "parameter", "parameters"))
  output$outputCount <- renderUI(countBadge(length(outputPaths()), "output", "outputs"))

  emptyState <- function(iconName, title, text) {
    bslib::card(bslib::card_body(div(class = "empty-state", div(class = "empty-icon", icon(iconName)), h5(title), p(text))))
  }

  # ---- Parameters ----
  parameterDefaults <- function(path) {
    if (!is.null(parameterDefaultsCache[[path]])) {
      return(parameterDefaultsCache[[path]])
    }
    ospParameter <- ospsuite::getParameter(path = path, container = simulations$sim)
    value <- ospParameter$value
    dimension <- ospParameter$dimension
    unit <- ospsuite::getBaseUnit(dimension)

    if (dimension == ospsuite::ospDimensions$`Molecular weight`) {
      value <- ospsuite::toUnit(
        quantityOrDimension = ospsuite::ospDimensions$`Molecular weight`,
        values = value,
        targetUnit = ospsuite::ospUnits$`Molecular weight`$`g/mol`
      )
      unit <- ospsuite::ospUnits$`Molecular weight`$`g/mol`
    }

    units <- ospsuite::getUnitsForDimension(dimension)
    units[units == ""] <- "Unitless"
    if (unit == "") {
      unit <- "Unitless"
    }

    defaults <- list(
      value = value,
      dimension = dimension,
      unit = unit,
      units = units,
      arguments = list(
        "Uniform" = list(minimum = min(0.9 * value, 1.1 * value), maximum = max(0.9 * value, 1.1 * value)),
        "LogUniform" = list(minimum = min(0.1 * value, 10 * value), maximum = max(0.1 * value, 10 * value)),
        "Normal" = list(mean = value, stdv = 1),
        "LogNormal" = list(mean = value, CV = 1)
      )
    )
    parameterDefaultsCache[[path]] <- defaults
    return(defaults)
  }

  parameterCard <- function(path) {
    key <- pathKey(path)
    defaults <- parameterDefaults(path)
    typeId <- parameterTypeId(key)

    distributionForms <- lapply(names(guiDistributionArguments), function(type) {
      arguments <- guiDistributionArguments[[type]]
      argumentInputs <- lapply(names(arguments), function(argument) {
        id <- parameterArgumentId(key, type, argument)
        numericInput(inputId = id, label = arguments[[argument]], value = currentValue(id, defaults$arguments[[type]][[argument]]), width = "100%")
      })
      conditionalPanel(
        condition = sprintf("input['%s'] === '%s'", typeId, type),
        do.call(bslib::layout_columns, c(list(col_widths = c(6, 6)), argumentInputs))
      )
    })

    card <- bslib::card(
      class = "selection-card",
      bslib::card_header(
        span(class = "path-chip", path),
        span(class = "meta", paste0("Nominal ", signif(defaults$value, 4), " ", if (defaults$unit != "Unitless") defaults$unit))
      ),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(7, 5),
          textInput(inputId = parameterNameId(key), label = "Display name", value = currentValue(parameterNameId(key), path), width = "100%"),
          selectInput(inputId = typeId, label = "Distribution", choices = names(guiDistributionArguments), selected = currentValue(typeId, "Uniform"), width = "100%")
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          div(distributionForms),
          selectInput(inputId = parameterUnitId(key), label = "Unit", choices = defaults$units, selected = currentValue(parameterUnitId(key), defaults$unit), width = "100%")
        )
      )
    )
    keyRegistry$rendered <- union(keyRegistry$rendered, c(typeId, parameterNameId(key), parameterUnitId(key)))
    return(card)
  }

  output$parametersUI <- renderUI({
    paths <- parameterPaths()
    if (length(paths) == 0) {
      return(emptyState("sliders", "No parameters selected", "Tick parameters in the simulation tree to specify their distributions."))
    }
    div(class = "selection-list", lapply(paths, parameterCard))
  })

  # ---- Outputs ----
  outputCard <- function(path) {
    key <- pathKey(path)
    card <- bslib::card(
      class = "selection-card",
      bslib::card_header(span(class = "path-chip", path)),
      bslib::card_body(
        textInput(inputId = outputNameId(key), label = "Display name", value = currentValue(outputNameId(key), path), width = "100%"),
        div(
          class = "pk-pills",
          checkboxGroupInput(
            inputId = outputPKId(key),
            label = "PK parameters",
            choices = guiPKParameterChoices,
            selected = currentValue(outputPKId(key), "C_max"),
            inline = TRUE
          )
        )
      )
    )
    keyRegistry$rendered <- union(keyRegistry$rendered, c(outputNameId(key), outputPKId(key)))
    return(card)
  }

  output$outputsUI <- renderUI({
    paths <- outputPaths()
    if (length(paths) == 0) {
      return(emptyState("bullseye", "No outputs selected", "Tick molecules or observers in the simulation tree to choose PK parameters."))
    }
    div(class = "selection-list", lapply(paths, outputCard))
  })

  # ---- Building analysis inputs ----
  checkDistributionArguments <- function(type, arguments, displayName) {
    problem <- NULL
    values <- unlist(arguments)
    if (length(values) != length(arguments) || any(!is.finite(values))) {
      problem <- "all distribution values must be numbers"
    } else if (type %in% c("Uniform", "LogUniform") && !(arguments$minimum < arguments$maximum)) {
      problem <- "the minimum must be smaller than the maximum"
    } else if (type == "LogUniform" && !(arguments$minimum > 0)) {
      problem <- "the minimum of a log-uniform distribution must be positive"
    } else if (type == "Normal" && !(arguments$stdv > 0)) {
      problem <- "the standard deviation must be positive"
    } else if (type == "LogNormal" && !(arguments$mean > 0 && arguments$CV > 0)) {
      problem <- "the mean and the coefficient of variation of a log-normal distribution must be positive"
    }
    error(!is.null(problem), paste0("Parameter '", displayName, "': ", problem, "."))
  }

  buildParameterList <- function() {
    parameterList <- lapply(parameterPaths(), function(path) {
      key <- pathKey(path)
      defaults <- parameterDefaults(path)
      type <- input[[parameterTypeId(key)]] %||% "Uniform"
      displayName <- trimws(input[[parameterNameId(key)]] %||% "")
      if (displayName == "") {
        displayName <- path
      }
      arguments <- lapply(stats::setNames(nm = names(guiDistributionArguments[[type]])), function(argument) {
        input[[parameterArgumentId(key, type, argument)]] %||% defaults$arguments[[type]][[argument]]
      })
      checkDistributionArguments(type, arguments, displayName)

      SAParameter$new(
        simulation = simulations$sim,
        path = path,
        unit = input[[parameterUnitId(key)]] %||% defaults$unit,
        displayName = displayName,
        parameterDistribution = do.call(distribution[[type]], arguments)
      )
    })

    displayNames <- vapply(parameterList, function(par) par$displayName, character(1))
    error(anyDuplicated(displayNames) > 0, paste0("Parameter display names must be unique. Duplicated: ", paste0(unique(displayNames[duplicated(displayNames)]), collapse = ", "), "."))
    return(parameterList)
  }

  buildOutputList <- function() {
    outputList <- list()
    for (path in outputPaths()) {
      key <- pathKey(path)
      pkParameters <- input[[outputPKId(key)]]
      if (length(pkParameters) == 0) {
        next
      }
      displayName <- trimws(input[[outputNameId(key)]] %||% "")
      if (displayName == "") {
        displayName <- path
      }
      outputList[[path]] <- SAOutput$new(simulation = simulations$sim, path = path, displayName = displayName)
      for (pk in pkParameters) {
        outputList[[path]]$addPKParameter(standardPKParameter = pk)
      }
    }

    displayNames <- vapply(outputList, function(op) op$displayName, character(1))
    error(anyDuplicated(displayNames) > 0, paste0("Output display names must be unique. Duplicated: ", paste0(unique(displayNames[duplicated(displayNames)]), collapse = ", "), "."))
    return(outputList)
  }

  checkPathsInDDISimulation <- function(parameterList, outputList) {
    parameterChecks <- checkParametersExistInSimulation(
      simulation = simulations$ddi,
      parameterPaths = vapply(parameterList, function(par) par$path, character(1)),
      simulationName = "DDI simulation"
    )
    outputChecks <- checkOutputsExistInSimulation(
      simulation = simulations$ddi,
      outputPaths = vapply(outputList, function(op) op$path, character(1)),
      simulationName = "DDI simulation"
    )

    missingList <- function(checks, title) {
      missing <- names(checks)[!unlist(checks)]
      if (length(missing) == 0) {
        return(NULL)
      }
      tagList(strong(title), tags$ul(lapply(missing, tags$li)))
    }
    passed <- all(c(unlist(parameterChecks), unlist(outputChecks)))
    if (!passed) {
      showError(
        "Paths missing from DDI simulation",
        missingList(parameterChecks, "Parameters not found in DDI simulation:"),
        missingList(outputChecks, "Outputs not found in DDI simulation:")
      )
    }
    return(passed)
  }

  # Validated parameter and output lists for an analysis run, or NULL after reporting the problem to the user
  collectAnalysisInputs <- function(requireParameters = TRUE) {
    if (requireParameters && length(parameterPaths()) == 0) {
      showError("No parameters selected", "Please select at least one parameter in the Parameters step.")
      return(NULL)
    }
    analysisInputs <- tryCatch(
      list(parameters = buildParameterList(), outputs = buildOutputList()),
      error = function(e) {
        showError("Invalid settings", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(analysisInputs)) {
      return(NULL)
    }
    if (length(analysisInputs$outputs) == 0) {
      showError("No outputs selected", "Please select at least one output with at least one PK parameter in the Outputs step.")
      return(NULL)
    }
    if (!is.null(simulations$ddi) && !checkPathsInDDISimulation(analysisInputs$parameters, analysisInputs$outputs)) {
      return(NULL)
    }
    return(analysisInputs)
  }

  observeEvent(input$goToOutputs, {
    parameterList <- tryCatch(buildParameterList(), error = function(e) {
      showError("Invalid parameter settings", conditionMessage(e))
      return(NULL)
    })
    if (!is.null(parameterList)) {
      goToStep(3)
    }
  })

  observeEvent(input$goToEvaluation, {
    if (!is.null(collectAnalysisInputs(requireParameters = FALSE))) {
      goToStep(4)
    }
  })

  output$setupSummary <- renderUI({
    req(simulationVersion() > 0)
    nPK <- sum(vapply(outputPaths(), function(pth) length(input[[outputPKId(pathKey(pth))]]), integer(1)))
    bslib::layout_column_wrap(
      width = "220px",
      class = "mb-3",
      bslib::value_box(title = "Simulation", value = simulations$files$simName, showcase = icon("flask"), theme = "primary"),
      bslib::value_box(title = "Parameters", value = length(parameterPaths()), showcase = icon("sliders")),
      bslib::value_box(title = "Outputs", value = paste0(length(outputPaths()), " (", nPK, " PK)"), showcase = icon("bullseye")),
      bslib::value_box(title = "DDI simulation", value = simulations$files$ddiName %||% "None", showcase = icon("pills"))
    )
  })

  # ---- Running analyses ----
  results <- reactiveValues(su = NULL, sobol = NULL, efast = NULL, morris = NULL)

  isWholeNumber <- function(x, minimum) {
    is.numeric(x) && length(x) == 1 && is.finite(x) && x %% 1 == 0 && x >= minimum
  }
  checkWholeNumber <- function(x, label, minimum) {
    if (!isWholeNumber(x, minimum)) {
      showError("Invalid setting", paste0("'", label, "' must be a whole number of at least ", minimum, "."))
      return(FALSE)
    }
    return(TRUE)
  }

  parseQuantiles <- function(text) {
    quantiles <- suppressWarnings(as.numeric(trimws(strsplit(text %||% "", ",")[[1]])))
    if (length(quantiles) == 0 || any(is.na(quantiles)) || any(quantiles < 0 | quantiles > 1)) {
      return(NULL)
    }
    return(quantiles)
  }

  timestamp <- function() {
    format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  # Runs `run`, which receives a function that creates progress bars, and stores its result in `results[[method]]`
  executeAnalysis <- function(method, label, run, extra = list()) {
    progressBars <- list()
    on.exit(for (progress in progressBars) progress$close(), add = TRUE)
    newProgress <- function(message) {
      progress <- shiny::Progress$new()
      progress$set(message = message, value = 0)
      progressBars[[length(progressBars) + 1]] <<- progress
      function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
    }

    results[[method]] <- NULL
    setDownloadsEnabled(method, FALSE)
    message(paste("Starting", label, "..."))

    result <- tryCatch(
      run(newProgress),
      error = function(e) {
        showError(
          paste(label, "failed"),
          p("Verify whether the PKML model can be successfully run at all points in the specified parameter space."),
          tags$pre(conditionMessage(e))
        )
        reloadSimulations()
        return(NULL)
      }
    )
    if (is.null(result)) {
      return()
    }
    if (is.null(result$Results)) {
      showError(paste(label, "returned no results"), "No non-negligible sensitivities were found.")
      return()
    }

    results[[method]] <- c(list(output = result, dateTime = timestamp()), extra)
    setDownloadsEnabled(method, TRUE)
    showNotification(paste(label, "completed."), type = "message")
  }

  observeEvent(input$startLocalSA, {
    analysisInputs <- collectAnalysisInputs(requireParameters = !isTRUE(input$runSensitivityAnalysisForAllParameters))
    if (is.null(analysisInputs)) {
      return()
    }
    variationRange <- input$variationRangeNumericInput
    if (!(is.numeric(variationRange) && is.finite(variationRange) && variationRange > 0 && variationRange < 1)) {
      showError("Invalid setting", "'Variation range' must lie strictly between 0 and 1.")
      return()
    }
    if (!checkWholeNumber(input$numberOfStepsNumericInput, "Number of steps", 1)) {
      return()
    }
    if (!(is.numeric(input$sensitivityThreshold) && is.finite(input$sensitivityThreshold) && input$sensitivityThreshold >= 0)) {
      showError("Invalid setting", "'Minimum sensitivity threshold' must be a non-negative number.")
      return()
    }
    runUncertainty <- isTRUE(input$runUncertaintyCheckbox)
    quantiles <- parseQuantiles(input$quantilesTestInput)
    if (runUncertainty) {
      if (!checkWholeNumber(input$numberOfUncertaintySamples, "Uncertainty samples", 1)) {
        return()
      }
      if (is.null(quantiles)) {
        showError("Invalid setting", "'Quantiles' must be a comma separated list of numbers between 0 and 1.")
        return()
      }
    }

    executeAnalysis("su", "Local sensitivity analysis", extra = list(uncertainty = runUncertainty), run = function(newProgress) {
      customParameters <- analysisInputs$parameters
      if (length(customParameters) == 0) {
        customParameters <- NULL
      }
      runSU(
        simulation = simulations$sim,
        DDIsimulation = simulations$ddi,
        runUncertaintyAnalysis = runUncertainty,
        evaluateForAllParameters = isTRUE(input$runSensitivityAnalysisForAllParameters),
        customParameters = customParameters,
        sensitivityThreshold = input$sensitivityThreshold,
        variationRange = variationRange,
        numberOfSensitivityAnalysisSteps = input$numberOfStepsNumericInput,
        quantiles = quantiles %||% c(0.05, 0.5, 0.95),
        outputs = analysisInputs$outputs,
        saveFolder = NULL,
        saveResults = FALSE,
        numberOfUncertaintyAnalysisSamples = input$numberOfUncertaintySamples,
        runParallel = TRUE,
        updateProgressSensitivity = newProgress("Computing local sensitivity"),
        updateProgressUncertainty = if (runUncertainty) newProgress("Computing uncertainty")
      )
    })
  })

  observeEvent(input$startGSA, {
    analysisInputs <- collectAnalysisInputs()
    if (is.null(analysisInputs) || !checkWholeNumber(input$numberOfSamplesGSA, "Number of samples", 2)) {
      return()
    }
    executeAnalysis("sobol", "Sobol sensitivity analysis", function(newProgress) {
      runSobol(
        simulation = simulations$sim,
        DDIsimulation = simulations$ddi,
        parameters = analysisInputs$parameters,
        outputs = analysisInputs$outputs,
        numberOfSamples = input$numberOfSamplesGSA,
        runParallel = TRUE,
        updateProgress = newProgress("Computing Sobol sensitivity")
      )
    })
  })

  observeEvent(input$startEFAST, {
    analysisInputs <- collectAnalysisInputs()
    if (is.null(analysisInputs) || !checkWholeNumber(input$numberOfReSamplesEFAST, "Number of re-sampling runs", 1)) {
      return()
    }
    executeAnalysis("efast", "EFAST sensitivity analysis", function(newProgress) {
      runEFAST(
        simulation = simulations$sim,
        DDIsimulation = simulations$ddi,
        parameters = analysisInputs$parameters,
        outputs = analysisInputs$outputs,
        numberOfResamples = input$numberOfReSamplesEFAST,
        runParallel = TRUE,
        updateProgress = newProgress("Computing EFAST sensitivity")
      )
    })
  })

  observeEvent(input$startMorris, {
    analysisInputs <- collectAnalysisInputs()
    if (is.null(analysisInputs) || !checkWholeNumber(input$numberOfSamplesMorris, "Number of samples", 1)) {
      return()
    }
    if (input$numberOfSamplesMorris < 2) {
      showNotification(HTML("Evaluation of the Morris interaction effect <em>&sigma;</em> requires 2 or more samples."), type = "warning", duration = 8)
    }
    executeAnalysis("morris", "Morris sensitivity analysis", function(newProgress) {
      runMorris(
        simulation = simulations$sim,
        DDIsimulation = simulations$ddi,
        parameters = analysisInputs$parameters,
        outputs = analysisInputs$outputs,
        numberOfSamples = input$numberOfSamplesMorris,
        runParallel = TRUE,
        updateProgress = newProgress("Computing Morris sensitivity")
      )
    })
  })

  # ---- Plots ----
  # Nested plot lists [[output]][[pk]] are flattened into a list of entries labelled "output | pk"
  flattenPlots <- function(plotList) {
    entries <- list()
    for (op in names(plotList)) {
      for (pk in names(plotList[[op]])) {
        entries[[paste(op, pk, sep = "  →  ")]] <- list(output = op, pk = pk, plot = plotList[[op]][[pk]])
      }
    }
    return(entries)
  }

  tornadoPlots <- function(res) {
    plots <- list(Sensitivity = generateTornadoPlot(sensitivityDataFrame = res$output$Results))
    if (isTRUE(res$uncertainty)) {
      plots$Uncertainty <- generateTornadoPlot(sensitivityDataFrame = res$output$Results, generateForUncertaintyAnalysis = TRUE)
    }
    return(plots)
  }

  numberOfPlottedParameters <- function(plt) {
    data <- plt$data
    column <- intersect(c("ParameterDisplayName", "Parameter"), names(data))[1]
    return(max(1, length(unique(data[[column]]))))
  }

  plotMethods <- list(
    su = list(
      label = "local sensitivity",
      plots = tornadoPlots,
      fileLabel = "TornadoPNG",
      height = function(plt) max(380, 140 + 34 * numberOfPlottedParameters(plt)),
      pngSize = function(plt) c(7, max(4, 7 * numberOfPlottedParameters(plt) / 4))
    ),
    sobol = list(
      label = "Sobol",
      plots = function(res) list(Sobol = generateSobolBarGraph(gsaResultsDataframe = res$output$Results)),
      fileLabel = "GSAPNG",
      height = function(plt) 520,
      pngSize = function(plt) c(max(5, 7 * numberOfPlottedParameters(plt) / 4), 7)
    ),
    efast = list(
      label = "EFAST",
      plots = function(res) list(EFAST = generateEFASTBarGraph(gsaResultsDataframe = res$output$Results)),
      fileLabel = "EFASTPNG",
      height = function(plt) 520,
      pngSize = function(plt) c(max(5, 7 * numberOfPlottedParameters(plt) / 4), 7)
    ),
    morris = list(
      label = "Morris",
      plots = function(res) list(Morris = generateMorrisPlot(morrisResults = res$output$Results)),
      fileLabel = "morrisPNG",
      height = function(plt) 560,
      pngSize = function(plt) c(7, 7)
    )
  )

  plotSets <- lapply(stats::setNames(nm = names(plotMethods)), function(method) {
    reactive({
      res <- results[[method]]
      if (is.null(res)) {
        return(NULL)
      }
      sets <- tryCatch(plotMethods[[method]]$plots(res), error = function(e) {
        showNotification(paste("Could not plot results:", conditionMessage(e)), type = "error")
        return(NULL)
      })
      sets <- lapply(Filter(Negate(is.null), sets), flattenPlots)
      return(Filter(function(set) length(set) > 0, sets))
    })
  })

  lapply(names(plotMethods), function(method) {
    kindId <- paste0(method, "PlotKind")
    selectId <- paste0(method, "PlotSelect")

    selectedPlot <- reactive({
      sets <- plotSets[[method]]()
      req(length(sets) > 0)
      kind <- input[[kindId]]
      if (is.null(kind) || !kind %in% names(sets)) {
        kind <- names(sets)[1]
      }
      entries <- sets[[kind]]
      selected <- input[[selectId]]
      if (is.null(selected) || !selected %in% names(entries)) {
        selected <- names(entries)[1]
      }
      entries[[selected]]$plot
    })

    output[[paste0(method, "ResultsUI")]] <- renderUI({
      sets <- plotSets[[method]]()
      if (length(sets) == 0) {
        return(div(
          class = "empty-state",
          div(class = "empty-icon", icon("chart-column")),
          h5("No results yet"),
          p(paste0("Adjust the settings and run the ", plotMethods[[method]]$label, " analysis to see results here."))
        ))
      }
      tagList(
        div(
          class = "viewer-controls",
          if (length(sets) > 1) radioButtons(inputId = kindId, label = "Result", choices = names(sets), inline = TRUE),
          selectInput(inputId = selectId, label = "Output / PK parameter", choices = names(sets[[1]]), width = "420px"),
          span(class = "meta ms-auto mb-3", paste("Run", format(strptime(results[[method]]$dateTime, "%Y%m%d_%H%M%S"), "%d %b %Y, %H:%M")))
        ),
        plotOutput(outputId = paste0(method, "Plot"), height = "auto")
      )
    })

    observeEvent(input[[kindId]], {
      sets <- plotSets[[method]]()
      req(input[[kindId]] %in% names(sets))
      choices <- names(sets[[input[[kindId]]]])
      selected <- if (isTRUE(input[[selectId]] %in% choices)) input[[selectId]] else choices[1]
      updateSelectInput(session, selectId, choices = choices, selected = selected)
    })

    output[[paste0(method, "Plot")]] <- renderPlot(
      {
        selectedPlot()
      },
      height = function() plotMethods[[method]]$height(selectedPlot()),
      res = 96
    )
  })

  # ---- Downloads ----
  sanitizeFileName <- function(x) {
    gsub(pattern = "[^[:alnum:]._-]+", replacement = "-", x = x)
  }

  resultsDownload <- function(method, filePrefix) {
    downloadHandler(
      filename = function() paste0(filePrefix, results[[method]]$dateTime, ".xlsx"),
      content = function(file) writexl::write_xlsx(x = results[[method]]$output, path = file)
    )
  }

  ggplotDownload <- function(method, filePrefix) {
    downloadHandler(
      filename = function() paste0(filePrefix, results[[method]]$dateTime, ".rds"),
      content = function(file) {
        plots <- plotMethods[[method]]$plots(results[[method]])
        # Tornado plots keep their Sensitivity/Uncertainty grouping; the other methods have a single group
        if (method != "su") {
          plots <- plots[[1]]
        }
        saveRDS(object = plots, file = file)
      }
    )
  }

  pngDownload <- function(method) {
    downloadHandler(
      filename = function() paste0(plotMethods[[method]]$fileLabel, "_", results[[method]]$dateTime, ".zip"),
      content = function(file) {
        plotDirectory <- tempfile("plots")
        dir.create(plotDirectory)
        on.exit(unlink(plotDirectory, recursive = TRUE), add = TRUE)

        fileNames <- NULL
        sets <- plotSets[[method]]()
        for (kind in names(sets)) {
          for (entry in sets[[kind]]) {
            fileName <- file.path(plotDirectory, paste0(sanitizeFileName(paste(entry$output, entry$pk, kind, results[[method]]$dateTime, sep = "_")), ".png"))
            size <- plotMethods[[method]]$pngSize(entry$plot)
            ggsave(filename = fileName, plot = entry$plot, width = size[1], height = size[2], units = "in", device = "png")
            fileNames <- c(fileNames, fileName)
          }
        }
        utils::zip(zipfile = file, files = fileNames, flags = "-j9X")
      }
    )
  }

  output$saveLocalSAResults <- resultsDownload("su", "one-at-a-time-results-")
  output$saveTornadoGGPLOT <- ggplotDownload("su", "TornadoGGPLOT_")
  output$saveTornadoPNG <- pngDownload("su")
  output$saveGSAResults <- resultsDownload("sobol", "gsaResults_")
  output$saveGSAGGPLOT <- ggplotDownload("sobol", "GSAGGPLOT_")
  output$saveGSAPNG <- pngDownload("sobol")
  output$saveEFASTResults <- resultsDownload("efast", "efastResults_")
  output$saveEFASTGGPLOT <- ggplotDownload("efast", "EFASTGGPLOT_")
  output$saveEFASTPNG <- pngDownload("efast")
  output$saveMorrisResults <- resultsDownload("morris", "morrisResults_")
  output$saveMorrisGGPLOT <- ggplotDownload("morris", "morrisGGPLOT_")
  output$saveMorrisPNG <- pngDownload("morris")

  # ---- Code generation ----
  codeDownload <- function(filePrefix, analysisCode) {
    downloadHandler(
      filename = function() paste0(filePrefix, timestamp(), ".R"),
      content = function(file) {
        script <- tryCatch(
          writeParametersAndOutputsToFile(parametersList = buildParameterList(), outputsList = buildOutputList()),
          error = function(e) {
            showError("Invalid settings", conditionMessage(e))
            stop(e)
          }
        )
        script <- append(x = script, values = analysisCode())
        con <- file(file, open = "wt", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        writeLines(text = script, con = con)
      }
    )
  }

  output$getSUCode <- codeDownload("SUCode_", function() {
    quantiles <- parseQuantiles(input$quantilesTestInput) %||% c(0.05, 0.5, 0.95)
    writeSUFunctionToFile(
      runUncertaintyCheckbox = isTRUE(input$runUncertaintyCheckbox),
      runSensitivityAnalysisForAllParameters = isTRUE(input$runSensitivityAnalysisForAllParameters),
      sensitivityThreshold = input$sensitivityThreshold,
      variationRangeNumericInput = input$variationRangeNumericInput,
      numberOfStepsNumericInput = input$numberOfStepsNumericInput,
      quantilesTestInput = paste0(quantiles, collapse = ", "),
      numberOfUncertaintySamples = input$numberOfUncertaintySamples,
      runParallel = TRUE
    )
  })
  output$getSobolCode <- codeDownload("SobolCode_", function() writeSobolSensitivityFunctionToFile(numberOfSamplesGSA = input$numberOfSamplesGSA))
  output$getEFASTCode <- codeDownload("EFASTCode_", function() writeEFASTSensitivityFunctionToFile(numberOfReSamplesEFAST = input$numberOfReSamplesEFAST))
  output$getMorrisCode <- codeDownload("morrisCode_", function() writeMorrisSensitivityFunctionToFile(numberOfSamples = input$numberOfSamplesMorris))
}
