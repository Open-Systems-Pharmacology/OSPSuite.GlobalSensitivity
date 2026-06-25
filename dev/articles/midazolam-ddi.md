# Midazolam DDI example

This example demonstrates the use of the *OSP Global Sensitivity*
package to analyze the impact of PBPK model parameters in a drug-drug
interaction (DDI) scenario in which midazolam is co-administered with
rifampicin. Midazolam is a recognized CYP3A4 substrate, whereas
rifampicin is an inducer of CYP3A4.

In DDI studies, it is often the *ratios* of the AUC and `C_max` with and
without the interaction that are of relevance to clinical pharmacology
decision-making. This example therefore demonstrates how the package
analyzes the sensitivity of the `AUC_inf` and `C_max` ratios of
midazolam with and without co-administration with rifampicin. It
corresponds to *Supplementary Materials 6* of the accompanying
publication ([Najjar et al., 2024](https://doi.org/10.1002/psp4.13256))
and is implemented in the file
`examples/example-DDI-Rifampicin-Midazolam.R` in the installed package
folder.

If you are new to the package, start with the [Getting
started](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/articles/getting-started.md)
article, which explains each of the building blocks used below.

## Background

Two PK-Sim models, previously developed by Hanke et al. (2018),
represent the midazolam and midazolam-plus-rifampicin dosing scenarios
reported in Backman et al. In the control trial, a 15 mg oral midazolam
tablet was administered as a standalone treatment, whereas in the test
arm the same tablet was given 17 hours following the conclusion of a
5-day regimen involving a daily dose of 600 mg rifampicin. The two
models, which may be downloaded from the
[Rifampicin-Midazolam-DDI](https://github.com/Open-Systems-Pharmacology/Rifampicin-Midazolam-DDI)
repository, simulate the midazolam plasma concentration under these two
dosing scenarios for a European adult male.

## Parameters analyzed

Six parameters are analyzed, spanning the metabolism of midazolam, its
distribution, and the induction of CYP3A4 by rifampicin:

| Parameter | Description | Nominal | Uncertainty |
|----|----|----|----|
| `Emax` ($`E_{\max}`$) | Maximal induction effect of CYP3A4 by rifampicin | 9 | LogUniform(Nominal/2, Nominal×2) |
| `EC50` ($`EC_{50}`$) | Half-maximal effective concentration of rifampicin in induction of CYP3A4 | 0.34 µmol/L | LogUniform(Nominal/2, Nominal×2) |
| `kcat` ($`k_{cat}`$) | Maximal turnover rate of midazolam metabolized by CYP3A4 | 8.76 1/min | LogUniform(Nominal/2, Nominal×2) |
| `Km` ($`K_{m}`$) | Concentration of midazolam yielding half-maximal metabolism by CYP3A4 | 4 µmol/L | LogUniform(Nominal/2, Nominal×2) |
| `Lipophilicity` ($`\log P`$) | Midazolam lipophilicity | 2.9 | Uniform(2, 4) |
| `fu` ($`f_{u}`$) | Midazolam fraction unbound | 0.03 | Uniform(0.01, 0.05) |

## Loading the package and simulations

This example begins with loading the *OSP Global Sensitivity* package
followed by the midazolam simulation and the midazolam/rifampicin DDI
simulation:

``` r

rm(list = ls())
# Load the OSP Global Sensitivity R package
library(ospsuite.globalsensitivity)

# Load Midazolam model simulation including Rifampicin
simFilePath <- system.file("extdata", "DDI Control - Midazolam - Backman 1996 - with Rif.pkml",
                           package = "ospsuite.globalsensitivity")
sim <- loadSimulation(simFilePath)

# Load Midazolam model simulation including DDI with Rifampicin
DDIsimFilePath <- system.file("extdata", "DDI Treatment - Rifampicin_Midazolam - Backman 1996.pkml",
                              package = "ospsuite.globalsensitivity")
DDIsim <- loadSimulation(DDIsimFilePath)

# Generate a list containing paths of all parameters in the model for
# convenience of parameter lookup
tree <- getSimulationTree(sim)
```

## Specifying the input parameters

Next, the parameters to analyze are specified along with their
probability distributions. Each parameter must exist at the exact same
path in both the control (`sim`) and DDI (`DDIsim`) simulations:

``` r

# Create a list of Parameter objects corresponding to parameters that exist in
# both the Midazolam and DDI models
parametersList <- list(
  SAParameter$new(simulation = sim,
                  path = tree$`Midazolam-CYP3A4-Optimized`$Km$path,
                  displayName = "Km",
                  unit = ospUnits$`Concentration [molar]`$`µmol/l`,
                  parameterDistribution = LogUniformDistribution$new(minimum = 2, maximum = 8)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$`Midazolam-CYP3A4-Optimized`$kcat$path,
                  displayName = "kcat",
                  unit = ospUnits$`Inversed time`$`1/min`,
                  parameterDistribution = LogUniformDistribution$new(minimum = 8.76 / 2, maximum = 8.76 * 2)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Midazolam$Lipophilicity$path,
                  displayName = "Lipophilicity",
                  unit = ospUnits$`Log Units`$`Log Units`,
                  parameterDistribution = UniformDistribution$new(minimum = 2, maximum = 4)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Midazolam$`Fraction unbound (plasma, reference value)`$path,
                  displayName = "fu",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = UniformDistribution$new(minimum = 0.01, maximum = 0.05)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Rifampicin$`CYP3A4-Templeton 2011`$EC50$path,
                  displayName = "EC50",
                  unit = ospUnits$`Concentration [molar]`$`µmol/l`,
                  parameterDistribution = LogUniformDistribution$new(minimum = 0.34 * 0.5, maximum = 0.34 / 0.5)
  ),
  SAParameter$new(simulation = sim,
                  path = tree$Rifampicin$`CYP3A4-Templeton 2011`$Emax$path,
                  displayName = "Emax",
                  unit = ospUnits$Dimensionless$Unitless,
                  parameterDistribution = LogUniformDistribution$new(minimum = 9 * 0.5, maximum = 9 / 0.5)
  )
)
```

## Specifying the model output

Next, the model output and the PK parameters of the output, for which
sensitivity is to be analyzed, are defined. Here the midazolam plasma
concentration in peripheral venous blood is analyzed, with both `C_max`
and `AUC_inf` as PK parameters of interest:

``` r

# Create an Output object corresponding to a simulated quantity that exists in
# both the Midazolam and DDI models.
Y <- SAOutput$new(simulation = sim,
                  path = tree$Organism$PeripheralVenousBlood$Midazolam$`Plasma (Peripheral Venous Blood)`$path,
                  displayName = "Midazolam-PeripheralVenousBlood")
Y$addPKParameter(standardPKParameter = "C_max")
Y$addPKParameter(standardPKParameter = "AUC_inf")
outputList <- list(Y)
```

## Running the sensitivity analyses

Finally, the sensitivity analyses are run. Providing the `DDIsimulation`
argument causes the package to evaluate the sensitivity of the *ratios*
of the PK parameters between the treatment (`DDIsim`) and control
(`sim`) simulations:

``` r

# Run local sensitivity analysis and uncertainty analysis
su <- runSU(simulation = sim,
            DDIsimulation = DDIsim,
            customParameters = parametersList,
            outputs = outputList,
            evaluateForAllParameters = FALSE,
            # Sensitivity analysis parameters:
            variationRange = 0.2,
            numberOfSensitivityAnalysisSteps = 2,
            sensitivityThreshold = 0,
            # Uncertainty analysis parameters:
            runUncertaintyAnalysis = TRUE,
            numberOfUncertaintyAnalysisSamples = 100,
            quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))

# Run Morris sensitivity analysis
morrisResults <- runMorris(simulation = sim,
                           DDIsimulation = DDIsim,
                           parameters = parametersList,
                           outputs = outputList,
                           numberOfSamples = 100)

# Run Sobol sensitivity analysis
sobolResults <- runSobol(simulation = sim,
                         DDIsimulation = DDIsim,
                         parameters = parametersList,
                         outputs = outputList,
                         numberOfSamples = 100)

# Run EFAST sensitivity analysis
efastResults <- runEFAST(simulation = sim,
                         DDIsimulation = DDIsim,
                         parameters = parametersList,
                         outputs = outputList,
                         numberOfResamples = 1)
```

## Interpreting the results

The `C_max` of the midazolam plasma concentration (in the absence of
rifampicin co-administration) is most strongly influenced by the
`Lipophilicity` ($`\log P`$) and fraction unbound (`fu`) of midazolam in
both the local sensitivity and uncertainty analyses. For the midazolam
`AUC_inf` (without rifampicin), the local sensitivity analysis
emphasizes the metabolism-related parameters `kcat` and `Km`, whereas
the uncertainty analysis indicates that, when the entire uncertainty
range of the fraction unbound is accounted for, `fu` is the most
influential parameter. The global sensitivity analyses re-enforce this
observation: the significant differences between the total effect and
first-order effect in the Sobol and EFAST results indicate that the
influence of these parameters varies over their feasible range.

For the `C_max` and `AUC_inf` DDI ratios, the maximal rifampicin
induction of CYP3A4 (`Emax`) has the greatest impact in both the local
sensitivity and uncertainty analyses, followed by the maximal rate of
midazolam metabolism (`kcat`). The global sensitivity analyses agree
with the uncertainty analyses for these DDI ratios.

The plots of the results may be generated using
[`generateTornadoPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/generateTornadoPlot.md),
[`generateMorrisPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/generateMorrisPlot.md),
[`generateSobolBarGraph()`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/generateSobolBarGraph.md),
and
[`generateEFASTBarGraph()`](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/reference/generateEFASTBarGraph.md),
as described in the [Getting
started](https://www.open-systems-pharmacology.org/OSPSuite.GlobalSensitivity/dev/articles/getting-started.md)
article.
