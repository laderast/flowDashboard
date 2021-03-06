% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{commonDataObj}
\alias{commonDataObj}
\title{SuperClass for other DataObj Classes}
\format{\code{\link{R6Class}} object.}
\usage{
commonDataObj
}
\value{
Object of \code{\link{R6Class}} with methods for getting/setting features for flowDashboard.
}
\description{
SuperClass for other DataObj Classes
}
\section{Fields}{

\describe{
\item{\code{annotation}}{Annotation (can be extracted as phenoData from a GatingSet) as data.table.}

\item{\code{subsetOptions}}{set which columns in annotation to use for subsetting. Set by \code{setSubsetAndSortOptions()}.}

\item{\code{subsetOptionList}}{named list, where every entry corresponds to levels in a column in annotation.}
}}

\section{Methods}{

\describe{
  \item{\code{new()}}{initialize method. }
  \item{\code{checkIntegrity()}}{This method checks whether the identifier used in annotation and data agree and ensures data integrity between the two.}
  \item{\code{setSubsetAndSortOptions()}}{set the subset and sortOptions}
  \item{\code{subsetAnnotation(ids)}}{given a vector of ids, subset both the data and annotation slots.}
  \item{\code{setAnnotationDisplayOptions(annotCols)}}{given a vector of column names in annotation, select those columns and drop the rest.}
  }
}

\examples{
See \\code{\\link{qcFlowObj}}, \\code{\\link{gatingObj}}, and \\code{\\link{populationExpressionObj}} for code examples.
}
\keyword{data}
\keyword{flow}
