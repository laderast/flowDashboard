#' buildSampledDataList
#'
#' @param fSet
#' @param controlMarkers
#' @param controlSize
#'
#' @return
#' @export
#'
#' @examples
buildSampledDataList <- function(fSet, controlMarkers, controlSize=2000) {

  dataList <- fsApply(fSet, function(x){
    #out <- read.FCS(x, transformation = NULL)
    sampRow <- sample(nrow(x),size = controlSize)
    markerPos <- which(pData(parameters(x))$desc %in% controlMarkers)
    return(x[sampRow,markerPos])
  })

  sampleNames(dataList) <- FCSFileInfo$notation

  return(dataList)
}

#' Title
#'
#' @param dataList
#'
#' @return
#' @export
#'
#' @examples
meltDataList <- function(dataList){

  controlMelt
}


#' Find Median Values of Melted Data Frame
#'
#' @param controlMelt
#'
#' @return
#' @export
#'
#' @examples
findMedianValues <- function(controlMelt){


  medianMelt
}


