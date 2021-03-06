% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gatingModules.R
\name{gatingModuleGGOutput}
\alias{gatingModuleGGOutput}
\title{Output module for popHeatmapGG}
\usage{
gatingModuleGGOutput(input, output, session, imageDir, popTable, displayNodes,
  annotation, annotCols, plotObj, popSubsets, objId, mapVar)
}
\arguments{
\item{input}{- Shiny input object}

\item{output}{- Shiny}

\item{session}{- Shiny}

\item{imageDir}{- image directory for gating images}

\item{popTable}{- population table}

\item{displayNodes}{- populations to show in shiny UI. Derived from `gatingObj`}

\item{annotation}{- annotation. Usually derived from `gatingObj`}

\item{annotCols}{- column names in annotation to expose}

\item{plotObj}{- reactive data object that contains the gating graph to display
(generated by `gatingModuleOutputGGFromGO`)}

\item{popSubsets}{- named list of population subsets. Each slot should contain
a vector of populations in displayNodes}

\item{mapVar}{- single named character that maps popTable into annotation.}
}
\value{
shiny output module
}
\description{
Output module for popHeatmapGG
}
