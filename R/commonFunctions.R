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
buildSampledDataList <- function(fSet, controlMarkers=NULL, controlSize=2000) {

  dataList <- fsApply(fSet, function(x){
    #out <- read.FCS(x, transformation = NULL)
    if(nrow(x) < controlSize){controlSize = nrow(x)}

    sampRow <- sample(nrow(x),size = controlSize)
    #mDesc <- which(pData())

    if(!is.null(controlMarkers)){
      markerPos <- which(pData(parameters(x))$desc %in% controlMarkers)
      if(length(markerPos) ==0){
        markerPos <- which(pData(parameters(x))$name %in% controlMarkers)
      }

      if(length(markerPos) == 0){
        out <- NULL
      }
      else{
        out <- x[sampRow,markerPos]
      }
    }else{
      out <- x[sampRow,]
    }

    #print(markerPos)

    return(out)
  })

  #sampleNames(dataList) <- FCSFileInfo$notation

  return(dataList)
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


#' Title
#'
#' @param fS - a flowSet.
#' @param selectMarkers - character vector of markers to Select. Must correspond to column names of flowFrame.
#' @param samplePopulation
#' @param returnCellNum - Logical. return cell numbers (unique identifier for each row of flowFrame)
#'
#' @return
#' @export
#'
#' @examples
returnMeltedData <- function(fS, selectMarkers =NULL,
                             returnCellNum=FALSE){

  pD <- pData(parameters(fS[[1]]))

  listExprs <- lapply(sampleNames(fS), function(x){
    out <- exprs(fS[[x]])
  #don't replace name entry with desc where desc is NA (for example, SSC-A)
    #dontReplace <- which(!is.na(pD$desc))

    #colnames(out)[dontReplace] <- pD$desc[dontReplace]
    idCol <- rep(x, nrow(out))
    out <- data.frame(idVar=idCol,out)

  cellNum <- 1:nrow(out)
  out <- data.frame(cellNum, out)

  #print(dim(out))

  out
  })

  cellFrame <- do.call(rbind,listExprs)

  print(head(cellFrame))
  print(dim(cellFrame))

  #print(colnames(cellFrame))

  if(!is.null(selectMarkers)){
    cellFrame <- cellFrame[,colnames(cellFrame) %in% c("idVar",selectMarkers)]
  }

  print(head(cellFrame))

  #print(colnames(cellFrame))

  #  if(!is.null(samplePopulation)){
  #    sampleInd <- sample(1:nrow(cellFrame), samplePopulation)
  #    cellFrame <- cellFrame[sampleInd,]
  #  }

  #cellFrame <- cellFrame %>% select(-BEADDIST,-Original.Frame)
  if(returnCellNum){
    idVars <- c("idVar", "cellNum")
  }
  else{
    idVars <- c("idVar")
    cellFrame <- cellFrame[,!colnames(cellFrame) %in% c("cellNum")]
  }


  #cellMelt <- cellMelt %>% arrange(idVar)
  cellMelt <- melt(cellFrame, id.vars=idVars)

  return(cellMelt)

}

setGeneric("buildFileManifest",function(object, ...){
  standardGeneric("buildFileManifest")
})

setMethod("buildFileManifest",signature=c(object="character"),
          definition=function(object, ...){.buildFileManifestPath(object, ...)})
setMethod("buildFileManifest",signature=c(object="GatingSet"),
          definition=function(object,...){.buildFileManifestGS(object, ...)})
setMethod("buildFileManifest", signature=c(object="flowSet"),
          definition=function(object,...) {.buildFileManifestGS(object,...)})

.buildFileManifestPath <- function(dirPath, annotations=NULL){
  FCSFilePaths <- list.files(path=dirPath, recursive=TRUE,
                             pattern=".fcs$", full.names = TRUE)
  FCSFileNames <- list.files(path=dirPath, recursive=TRUE,
                             pattern=".fcs$", full.names = FALSE)
  FCSFiles <- list.files(path=dirPath, recursive=TRUE,
                         pattern=".fcs$", include.dirs =FALSE, full.names=FALSE)

  if(length(grep("/", FCSFiles, fixed=TRUE)) >0){
    FCSFiles <- unlist(lapply(FCSFiles, function(x){out <- strsplit(x, "/")
    #if(length(out)==2){
    ind <- length(out[[1]])
    return(out[[1]][[ind]])
    #      }
    # else(return(out[[1]]))
    }))
  }

  outList <- lapply(FCSFilePaths, function(x){
    fileHead <- read.FCSheader(x)
    dat <- as.character(as.Date(fileHead[[1]]["$DATE"], "%d-%b-%Y"))
    numCells <- as.character(fileHead[[1]]["$TOT"])
    #print(dat)
    return(c(dat, numCells))
  })

  outFrame <- data.frame(do.call(rbind, outList))
  colnames(outFrame) <- c("runDate", "numCells")
  outFrame$numCells <- as.numeric(as.character(outFrame$numCells))

  out <- data.frame(FCSFiles, FileLocation=FCSFileNames,
             fullPath=FCSFilePaths, outFrame)

  out$fullPath <- as.character(out$fullPath)

  return(out)

}

.buildFileManifestGS <- function(gSet){

  if(class(gSet) == "GatingSet"){
    fs <- gSet@data

  }else{
    fs <- gSet
  }

  FCSFiles <- sampleNames(fs)

  outList <- fsApply(fs, function(x){
    #print(keyword(x,"$DATE"))
    dat <- as.character(as.Date(keyword(x,"$DATE")$`$DATE`, "%d-%B-%Y"))
    numCells <- as.character(keyword(x,"$TOT"))
    #print(dat)
    return(c(dat, numCells))
  })

  outFrame <- data.frame(do.call(rbind, outList))
  colnames(outFrame) <- c("runDate", "numCells")
  outFrame$numCells <- as.numeric(as.character(outFrame$numCells))

  out <- data.frame(FCSFiles, outFrame)

  #out$fullPath <- as.character(out$fullPath)

  return(out)

}


