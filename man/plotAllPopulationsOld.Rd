% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commonFunctions.R
\name{plotAllPopulationsOld}
\alias{plotAllPopulationsOld}
\title{Plot all Populations}
\usage{
plotAllPopulationsOld(gateSet, nodeList = NULL, pipelineFile = "panel1",
  imagePath = "images/", delimiter = "+", default.y = "Cell_length")
}
\arguments{
\item{gateSet}{- a gatingSet with attached populations}

\item{nodeList}{- a list of populations to plot}

\item{imagePath}{- directory to write population images}
}
\value{
nothing. Side effect is images written to the imagePath.
}
\description{
Given a gatingSet, plot the provenance for each sample and each population.
}
