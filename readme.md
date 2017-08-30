# flowDashboard

Shiny modules for visualizing flow cytometry data as well as data transformation methods to enable the rapid display of cytometry data. `flowDashboard` uses data objects that are derived from `GatingSet`s to enable rapid deployment of dashboards for large experiments. It is designed to scale to very large comparisons (100+) across patient populations.

flowDashboard currently works with `GatingSets` (derived from the flowWorkspace package) into data table structures. Using the `CytoML` package, gating schemes from flowJo and Cytobank can also be imported for display.

The shiny modules are intended to address each step of an analysis workflow (preprocessing/data transformation, normalization, gating and comparative analysis).

For a sample reference dashboard [click here for a demo](https://tladeras.shinyapps.io/sampleFlowDashboard/) and the sample dashboard repo: https://github.com/laderast/sampleFlowDashboard

## Installing `flowDashboard`

The shiny modules themselves are not dependent on any Bioconductor packages. However, in building the data objects that plug into the dashboards, `flowDashboard` is dependent upon `flowCore` and `flowWorkspace`.

```
source("http://www.bioconductor.org/biocLite.R")
biocLite(c("flowCore", "flowWorkspace"), dependencies=TRUE)
library(devtools)
install_github("laderast/flowDashboard")
```

Once you have `flowDashboard` installed. You can try out the sample dashboard code here:

```
shiny::runGitHub("laderast/sampleFlowDashboard")
```

## Building Data Objects for `flowDashboard`

Please refer to the vignette in the `sampleFlowDashboard` repo for more info on building the data objects that plug into `flowDashboard`: https://github.com/laderast/sampleFlowDashboard/blob/master/docs/gvhdVignette.Rmd

More documentation on the data objects is forthcoming.

## Interested in Contributing?

We're always interested in having people improve our software!

Please read the [Contributing](contributing.md) file about ways to contribute to this project.

## Licensing

Copyright 2017 Ted Laderas

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   
See the License for the specific language governing permissions and limitations under the License.
