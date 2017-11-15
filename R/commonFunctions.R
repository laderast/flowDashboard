
#make a file friendly population name
#need to avoid "-" and "+" in population names so they
#will be compatible with filenames
makePopulationName <- function(popName){
  popName <- gsub("\\.","", x=popName)
  popName <- gsub("\\-","neg",x = popName)
  popName <- gsub("\\+", "pos", x= popName)
  return(popName)
}


#' Title
#'
#' @param popName
#' @param name
#' @param pipelineFile
#' @param delimiter
#'
#' @return
#' @export
#'
#' @examples
makePopulationIdentifier <- function(popName, name, pipelineFile="Panel1", delimiter="+"){
    popName <- makePopulationName(popName)
    outName <- paste0(name, delimiter, popName, delimiter, pipelineFile)
    return(outName)
}

#' Plot all Populations
#'
#' Given a gatingSet, plot the provenance for each sample and each population.
#'
#'
#' @param gateSet - a gatingSet with attached populations
#' @param nodeList - a list of populations to plot
#' @pipelineFile - unique object identifier used to map population/sample to image
#' @param imagePath - directory to write population images
#' @delimiter - character. what delimiter to use to distinguish idVar. Default is "+"
#'
#' @return nothing. Side effect is images written to the imagePath.
#' @export
#'
#' @examples
plotAllPopulationsOld <- function(gateSet, nodeList=NULL, pipelineFile = "panel1",
                               imagePath= "images/", delimiter="+"){
  if(!dir.exists(imagePath)){
    dir.create(imagePath)
  }
  require(flowWorkspace)
  require(lattice)

  if(is.null(nodeList)){
    nodeList <- getNodes(gateSet, path="full")
  }

  #for each node in the gatingTemplate, plot complete path
  for(i in 1:length(gateSet)){
    #samp <- gateSet[[i]]
    sampName <- sampleNames(gateSet)[i]
    print(sampName)
    for(node in nodeList){
      print(node)
      if(node != "root"){
        outnodes <- strsplit(x = node, split="/")[[1]]
        outnodes <- setdiff(outnodes, c(""))
        outPop <- outnodes[length(outnodes)]
        outPop <- makePopulationName(outPop)
        #outnodes <- unlist(outnodes)
        popID <- makePopulationIdentifier(popName=outPop, name = sampName, pipelineFile = pipelineFile,
                                          delimiter=delimiter)

        fileId <- paste0(imagePath, popID, ".png")
        png(fileId, width=200*length(outnodes), height=200)
        try(flowWorkspace::plotGate(gateSet[[i]], y=outnodes, default.y="Cell_length",checkName=FALSE,
                     marker.only=TRUE, raw.scale=FALSE,
                     gpar = list(nrow=1, ncol=length(outnodes))))
        dev.off()
      }
    }
  }
}




#' Plot all Populations
#'
#' Given a gatingSet, plot the provenance for each sample and each population.
#'
#'
#' @param gateSet - a gatingSet with attached populations
#' @param imagePath - directory to write population images
#'
#' @return nothing. Side effect is images written to the imagePath.
#' @export
#'
#' @examples
plotAllPopulations <- function(gateSet, nodeList, pipelineFile = "panel1",
                               imagePath= "images/", cytof=TRUE, delimiter="+", scaling=FALSE){
  if(!dir.exists(imagePath)){
    dir.create(imagePath)
  }
  require(flowWorkspace)
  require(ggcyto)

  #for each node in the gatingTemplate, plot complete path
  for(i in 1:length(gateSet)){
    #samp <- gateSet[[i]]
    sampName <- sampleNames(gateSet)[i]
    print(sampName)
    pD <- pData(parameters(gateSet@data[[1]]))
    if(cytof == TRUE){
      defaultChan <- "Cell_length"
    }
    else{defaultChan <- "SSC-A"}

    gNs <- getNodes(gateSet, path="full")
    sNodes <- getNodes(gateSet, path=1)


    if(!is.null(nodeList)){
      matches <- which(nodeList %in% gNs)
      #print(matches)

      if(length(matches) == 0){
        stop("NodeList doesn't match nodes in GatingSet")
      }
    }


    if(is.null(nodeList)){
      nodeList <- getNodes(gateSet, path="full")
    }

    #print(nodeList)

    for(j in 1:length(nodeList)){
      node <- nodeList[j]
      print(node)
      if(node != "root"){
        outnodes <- strsplit(x = node, split="/")[[1]]

        print(outnodes)
        print(sNodes[j])

        #replace last node with the auto path
        outnodes[length(outnodes)] <- sNodes[j]
        #outnodes <- node
        outnodes <- setdiff(outnodes, c(""))
        outPop <- outnodes[length(outnodes)]
        outPop <- makePopulationName(outPop)
        outPop <- gsub(pattern = "\\.$", replacement = "",outPop)
        #outnodes <- unlist(outnodes)
        popID <- makePopulationIdentifier(popName=outPop, name = sampName, pipelineFile = pipelineFile,
                                          delimiter=delimiter)

        fileId <- paste0(imagePath, popID, ".png")

        png(fileId, width=200*length(outnodes), height=200)

        #        try(plotGate(gateSet[[i]], y=outnodes, default.y=defaultChan,checkName=FALSE,
        #                    marker.only=TRUE, raw.scale=FALSE,
        #                   gpar = list(nrow=1, ncol=length(outnodes))))
        #colnames(gateSet[[i]]) <- pD$desc

        yDefault <- NULL

        if(cytof){yDefault <- "Cell_length"}

        outplot <- try(autoplot(gateSet[[i]], outnodes, default.y=yDefault))

        if(!inherits(outplot, "try-error")){

          if(scaling==TRUE){
          outplot <- outplot + scale_x_flowJo_biexp() + scale_y_flowJo_biexp()
          }
          outplot <- ggcyto_arrange(outplot, nrow = 1)
          plot(outplot)
        }
        dev.off()
      }
    }
  }
}


#' Title
#'
#' @param node
#' @param imagePath
#'
#' @return
#' @export
#'
#' @examples
plotHierarchy <- function(node, gateSet, imagePath) {
  if(node != "root"){
    outnodes <- strsplit(x = node, split="/")[[1]]
    outnodes <- setdiff(outnodes, c(""))
    outPop <- outnodes[length(outnodes)]
    outPop <- makePopulationName(outPop)
    #outnodes <- unlist(outnodes)

    print(outPop)

    fileId <- paste0(imagePath, outPop, ".png")
    png(fileId, width=200*length(outnodes), height=200)
    try(flowWorkspace::plotGate(gateSet, y=outnodes, default.y="Cell_length",
                                checkName=FALSE,
                                marker.only=TRUE, raw.scale=FALSE,
                                gpar = list(nrow=1, ncol=length(outnodes))))
    dev.off()
  }
}

#' @export
scale_this <- function(x){
  as.vector(scale(x))
}


#' Title
#'
#' @param gateSet
#' @param pipelineFile
#'
#' @return
#' @export
#'
#' @examples
getPopulationsAndZscores <- function(gateSet, pipelineFile="panel1", delimiter="+"){
  popTable <- getPopStats(gateSet, path="auto")

  scale_this <- function(x){
    as.vector(scale(x))
  }

  popTable <- data.frame(popTable) %>% mutate(idVar = makePopulationIdentifier(Population,name,pipelineFile,delimiter),
                                  percentPop =(Count/ParentCount)*100)

  #popMat <- acast(popTable, name~Population, value.var = "percentPop")
  popTable <- popTable %>%
    group_by(Population) %>%
    mutate(zscore = scale_this(percentPop),
           popKey = paste0(name,delimiter,Population))

  return(data.table(popTable))
}



#' buildSampledDataList - deprecated
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
returnMeltedData <- function(fS, selectMarkers =NULL, samplePop=NULL,
                             returnCellNum=FALSE){

  if(class(fS)[1] == "GatingSet"){
    fS <- fS@data
  }

  if(!is.null(selectMarkers)){
    selectMarkers <- make.names(selectMarkers)
  }

    pD <- pData(parameters(fS[[1]]))
    markers <- pD$desc

  listExprs <- lapply(sampleNames(fS), function(x){
    out <- flowCore::exprs(fS[[x]])
    colnames(out) <- pD$desc
  #don't replace name entry with desc where desc is NA (for example, SSC-A)
    #dontReplace <- which(!is.na(pD$desc))

    #colnames(out)[dontReplace] <- pD$desc[dontReplace]
    idCol <- rep(x, nrow(out))
    out <- data.frame(idVar=idCol,out)

  if(returnCellNum){
    cellNum <- 1:nrow(out)
    out <- data.frame(cellNum, out)
  }
  #print(dim(out))

    #return sampled Data if samplePop is a numeric value
    if(!is.null(samplePop)){
      if(nrow(out)>samplePop){
      out <- out[sample(nrow(out), samplePop),]}
    }

  return(data.table(out))
  })

  cellFrame <- rbindlist(listExprs)

  if(!is.null(selectMarkers)){
    colsInMarkers <- colnames(cellFrame) %in% c("idVar","cellNum",selectMarkers)

    if(length(which(colsInMarkers))==0){
      stop("selectMarkers not in dataset")
    }

    cellFrame <- cellFrame[,colsInMarkers, with=FALSE]
  }

  if(returnCellNum){
    idVars <- c("idVar", "cellNum")
  }
  else{
    idVars <- c("idVar")
    #cellFrame <- cellFrame[,!colnames(cellFrame) %in% c("cellNum")]
  }

  cellMelt <- data.table::melt(cellFrame, id.vars=idVars)
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


#' Returns a long data frame with population/marker data
#'
#' @param gS
#' @param population
#' @param removeMarkers
#' @param samplePopulation
#'
#' @return
#' @export
#'
#' @examples
returnMeltedDataFromGS <- function(gS, population, removeMarkers = NULL, samplePopulation = NULL){
  if(!is.character(population)){stop("population must be specified")}

  amlList <- getData(gS, population)

  descFrame <- pData(parameters(amlList[[1]]))
  grepRes <- sapply(removeMarkers, function(x){grep(x, descFrame$desc)})
  nodeIDs <- do.call("c", grepRes)

  exprList <- as(amlList, "list")
  exprList <- lapply(exprList, function(x){data.table(flowCore::exprs(x))})
  filteredExprList <-
    lapply(exprList, function(x){colnames(x) <- descFrame$desc

    if(!is.null(nodeIDs) & length(nodeIDs)>0){
      keepNodes <- colnames(x)[!colnames(x) %in% nodeIDs]
      x <- x[,..keepNodes]
    }
    return(x)
    })

  filteredExprList <- lapply(filteredExprList, function(x){
    if(nrow(x) != 0){
      if(!is.null(samplePopulation)){
        if(nrow(x) > samplePopulation){
          sampleInd <- sample(1:nrow(x), samplePopulation)
          x <- x[sampleInd,]
        }
      }
    }

    return(x)
    #print(dim(out))
  })

  names(filteredExprList) <- sampleNames(amlList)

  filteredExprMeltList <-
    lapply(names(filteredExprList), function(y){

      x <- filteredExprList[[y]]
  #rownames(x) <- 1:nrow(x)
  #print(head(x))
  if(nrow(x)==0){
    cell=NULL
    idVar=NULL
    ##need to figure out data.table way to add columns

    x <- data.table(data.frame(idVar,cell, x))
  }
  else{
    cell <- 1:nrow(x)
    x <- data.table(idVar=y, cell, x)
    #use data.table::melt here
    x <- data.table::melt(x, id.vars=c("cell", "idVar"))

  }
      #print(head(x))
  return(x)})

  filteredExprMeltList <- lapply(filteredExprMeltList, function(x){if(nrow(x)>0){return(x)}})

  adultExprMelt <- rbindlist(filteredExprMeltList)
  adultExprMelt <- adultExprMelt %>% mutate(Population = population)

  return(adultExprMelt)

}

#' Title
#'
#' @param gs
#' @param excludeCols
#' @param excludePopulations
#'
#' @return
#' @export
#'
#' @examples
getAllPopulationMedians <- function(gs, excludeCols=NULL, excludePopulations=NULL){
  require(openCyto)
  require(reshape2)

  if(is.null(excludeCols)){
    excludeCols <- c("TIME", "CELL.LENGTH","XE","BEADS","DNA1","DNA2","PT1","PT2", "BCKG")
  }

  if(is.null(excludePopulations)){
    excludePopulations <- c("root", "singlet7", "singlet", "singlet2", "live", "CD45+")
  }

  populations <- getNodes(gs, path = 1)

  populations <- populations[!populations %in% excludePopulations]

  outMeds <- lapply(populations, function(x){
    print(x)
    outFS <- getData(gs,x)
    outMat <- getPopulationMedians(outFS, excludeCols)
    pop <- rep(x, nrow(outMat))
    outFrame <- data.frame(population=pop, outMat)
    outFrame
  })
  outMedFrame <- do.call(rbind, outMeds)
  outMelt <- melt(outMedFrame, id.vars=c("population","sample"))
  return(outMelt)
}

scale_this <- function(x){
  as.vector(scale(x))
}


#' Title
#'
#' @param subsetChoices
#' @param annotation
#' @param input
#'
#' @return
#' @export
#'
#' @examples
makeAnnotationReactive <- function(subsetList=NULL, annotation, input){

  expr <- {
    annotate2 <- annotation
#   ord <- input$order

    if(!is.null(subsetList)){
      subsetVar <- names(subsetList)[1]
      subsetValues <- subsetList[[subsetVar]]
      #print(subsetVar)
      annotate2 <- annotation %>% dplyr::filter(as.name(subsetVar) %in% subsetValues)
    }
    #print(annotate2)
    annotate2

  }

  substitute(expr)

  #expr <- lazyeval::interp(expr, annotation = annotation)

  return(expr)
  #return(reactive(expr))
}

makePopTableReactive <- function(popTable, input){

  return(reactive(expr))
}

makeMarkerDataReactive <- function(markerData, annotation){

  return(reactive(expr))
}

changeDataFrameLevels <- function(df){
  classList <- lapply(df, class)
  names(classList) <- colnames(df)

  outFrame <- lapply(names(classList), function(x){
    if(classList[[x]] == "factor"){
      df[[x]] <- factor(as.character(df[[x]]))
      }
    })

}

#' Title
#'
#' @param annotation
#' @param data
#' @param mapVar
#'
#' @return
#' @export
#'
#' @examples
reconcileDataAndAnnotation<- function(annotation, data, mapVar){

  data[annotation, on=mapVar, nomatch=0]

  dataColName <- names(mapVar)
  annotationColName <- mapVar



}

makeOutputString <- function(point, annotDisplayOptions){
  point <- as.list(point)[annotDisplayOptions]
  outputString <- paste("<b>",names(point),":</b> ", point, "<br/>", collapse = "")

  outputString
}

#' Finds line in file for geom_tile() heatmaps
#'
#' @param point
#' @param data
#' @param xcol
#' @param ycol
#'
#' @return row in data that corresponds to clicked point
#'
#' @examples
findPointsGeomTile <- function(point, data, xcol, ycol, ps){
  numRows <- length(ycol)
  numCols <- length(xcol)

  #xcols <- length(spreaddata)

  #print(numCols)
  #print(numRows)
  pointXrange <- point$range$right - point$range$left
  xCellSize <- pointXrange / numRows
  #print(xCellSize)
  xCellNum <- ceiling(point$x - 0.5)
  pointYrange <- point$range$bottom - point$range$top
  yCellSize <- pointYrange / numRows
  #print(yCellSize)
  yCellNum <- numRows - ceiling(point$y - 0.5) + 1

  #print(xCellNum)
  #print(yCellNum)

  #ps <- popSubsets[[input$ps]]
  xName <- xcol[xCellNum]
  #print(colnames(spreaddata))
  yName <- ps[yCellNum]

  #print(xName)
  #print(yName)

  outLine <- data[name==xName & Population== yName]

  return(outLine)
}

makeRandomId <- function(){
  randName <- paste0(c(sample(LETTERS, 5), sample(0:9, 2)), collapse="")
  randName
}
