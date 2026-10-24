# ospsuite.globalsensitivity (development version)

* Redesigned the `runGUI()` app as a four-step wizard built with `bslib`:
  searchable simulation trees, per-quantity forms that update as the selection
  changes, in-app plots of the results of each analysis, and validation of all
  settings before a run.
* Fixed `runGUI()` bugs: Sobol, EFAST and Morris runs no longer start without
  any selected parameters; a failed run no longer crashes the app; progress bars
  of local sensitivity runs with uncertainty analysis are closed; downloaded
  tornado plots reflect the uncertainty setting used for the run; PNG exports no
  longer write files to the working directory or fail when plotting results for
  all constant parameters; loading an invalid PKML file shows an error instead of
  crashing the app.

# ospsuite.globalsensitivity 1.0.0

First release of the package.
