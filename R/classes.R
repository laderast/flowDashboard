library(data.table)
library(R6)

#' Reconcile annotation and data tables in object
#'
#' @param annotation
#' @param data
#' @param mapVar
#' @param idsInBoth
#'
#' @return
#'
#' @examples
reconcileData <- function(annotation, data, mapVar, idsInBoth){
  mapColData <- as.name(names(mapVar))
  mapColAnnotation <- as.name(mapVar)

  annotation <- annotation[eval(mapColAnnotation) %in% idsInBoth]
  data <- data[eval(mapColData) %in% idsInBoth]

  if(class(annotation[[mapVar]])=="character"){
    annotation[[mapVar]] <- factor(annotation[[mapVar]])
  }
  if(class(annotation[[mapVar]])== "factor"){
    annotation[[mapVar]] <- droplevels(annotation[[mapVar]])
  }

  if(class(data[[names(mapVar)]])== "character"){
    data[[names(mapVar)]] <- factor(data[[names(mapVar)]])
  }

  if(class(data[[names(mapVar)]])== "factor"){
    data[[names(mapVar)]] <- droplevels(data[[names(mapVar)]])
  }

  return(list(annotation=annotation, data=data))

}



#' Check integrity of data objects
#'
#' @param annotation - file manifest and annotations
#' @param data - data.table
#' @param mapVar
#' @param reconcile - LOGICAL - reconcile ids in both annotation and data
#'
#' @return
#'
#' @examples
checkIntegrity <- function(annotation, data, mapVar, reconcile=TRUE){

  mapColData <- names(mapVar)
  mapColAnnotation <- mapVar

  dataClass <- class(data)[1]

  #print(dataClass)

  if(dataClass != "data.table"){
    stop("data is not in data.table format")
  }

  if(class(annotation)[1] != "data.table"){
    stop("annotation is not in data.table format")
  }

  if(!mapColData %in% colnames(data)){
    stop(paste("map data column not in dataset:", mapColData))
  }
  if(!mapColAnnotation %in% colnames(annotation)){
    stop(paste("map annotation column not in annotation:", mapColAnnotation))
  }

  idsData <- unique(as.character(data[[mapColData]]))
  idsAnnotation <- unique(as.character(annotation[[mapColAnnotation]]))
  idsInBoth <- intersect(idsData, idsAnnotation)
  idsNotInAnnotation <- setdiff(idsData, idsAnnotation)
  idsNotInData <- setdiff(idsAnnotation, idsData)

  if(length(idsInBoth)==0){
    stop("data and annotation ids do not match")
  }

  if(length(idsNotInAnnotation) > 0){
    warning(paste0("These IDs not in Annotation:\n",
                   paste(idsNotInAnnotation, collapse = "\n")))
  }

  if(length(idsNotInData) > 0){
    warning(paste0("These IDs not in Data:", paste(idsNotInData, collapse = "\n")))
  }

  if(reconcile){
    outList <- reconcileData(annotation, data, mapVar, idsInBoth)
    annotation <- outList$annotation
    data <- outList$data
  }

  return(list(annotation=annotation, data=data))

}


returnMergedData <- function(data, annotation, mapVar){
  return(data[annotation, on=mapVar])
}

#' SuperClass for other DataObj Classes
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for getting/setting features for flowDashboard.
#' @format \code{\link{R6Class}} object.
#' @examples
#' library(flowWorkspace)
#' gs <- load_gs(flowStats::GvHD)
#' #
#' @field annotation Annotation (can be extracted as phenoData from a GatingSet) as data.table.
#' @field subsetOptions set which columns in annotation to use for subsetting. Set by \code{setSubsetAndSortOptions()}.
#' @field subsetOptionList named list, where every entry corresponds to levels in a column in annotation.
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/lightning-viz/lightining-r/}
#'   \item{\code{new()}}{initialize method. }
#'   \item{\code{checkIntegrity()}}{This method checks whether the identifier used in annotation and data agree and ensures data integrity between the two.}
#'   \item{\code{setSubsetAndSortOptions()}}{set the subset and sortOptions}
commonDataObj <-
  R6Class("commonDataObj",
          public=list(
            annotation=NULL,
            subsetOptions=NULL,
            subsetOptionList=NULL,
            sortOptions=NULL,
            sortOptionList=NULL,
            contextID = NULL,
            objId=NULL,
            annotCols = NULL,
            setSubsetAndSortOptions =
                function(subsetOptions, sortOptions,
                         checkIntegrity = TRUE){
                           annotation <- self$annotation

                           #print(head(annotation))
                           #need to check that options agree (Columns are in annotation)

                           annotationCols <- colnames(annotation)
                           #print(annotationCols)
                           if(checkIntegrity){
                             soNotInAnnotation <-
                               subsetOptions[!subsetOptions %in%
                                               annotationCols]
                             sortNotInAnnotation <-
                               sortOptions[!sortOptions %in%
                                             annotationCols]


                            if(length(soNotInAnnotation) > 0){
                              stop(
                                paste("annotation and subset options don't match:",
                                                   soNotInAnnotation, collapse="\n")
                                )
                              }
                            if(length(sortNotInAnnotation) > 0){
                              stop(
                                paste("annotation and sort options don't match:",
                                    sortNotInAnnotation, collapse="\n")
                                )
                              }
                           }
                           annotation <- data.table(annotation)

                        subsetOptionList <- as.list(
                                annotation[,subsetOptions, with=FALSE])

                        subsetOptionList <- lapply(
                                subsetOptionList, function(x)
                                  {unique(as.character(x))}
                                )
                        names(subsetOptionList) <- subsetOptions

                        self$subsetOptions <- subsetOptions
                        self$subsetOptionList <- subsetOptionList
                        self$sortOptions <- sortOptions

                        #add colorOptions

                        invisible(self)

                        },

            checkIntegrity =
              function(reconcile=FALSE){
                        classObj <- class(self)
                        mapVar <- self$mapVar
                        annotation <- self$annotation

                        if(classObj[1] == "commonDataObj"){
                              stop("commonDataObj cannot be used; use children")
                          }

                        if(classObj[1] == "populationExpressionObj"){
                              data <- self$expressionData
                          }

                        if(classObj[1] == "gatingObj"){
                              data <- self$popTable
                          }

                        if(classObj[1] == "qcFlowObj"){
                              data <- self$qcData
                          }

                        #main check Integrity routine
                        outList <- checkIntegrity(annotation,
                                 data, mapVar, reconcile=reconcile)

                        data <- outList$data
                        self$annotation <- outList$annotation

                        if(classObj[1] == "populationExpressionObj"){
                                  self$expressionData <- data
                          }

                        if(classObj[1] == "gatingObj"){
                                  self$popTable <- data
                                  if(is.null(self$imageDir)){
                                    warning("image dir is NULL")
                                  }

                                  if(!is.null(self$imageDir)){
                                    imageDir <- self$imageDir
                                    if(!dir.exists(imageDir)){
                                      warning("image dir doesn't exist")
                                    }
                                  }
                          }

                        if(classObj[1] == "qcFlowObj"){
                                  self$qcData <- data
                          }

                        invisible(self)
                  },

              subsetAnnotation = function(ids){
                    self$checkIntegrity(idsInBoth = ids, reconcile=TRUE)
                    invisible(self)
                    },
              setAnnotationDisplayOptions=function(annotCols){
                annotCols <- annotCols[annotCols %in% colnames(self$returnMergedData())]
                self$annotCols <- annotCols
              }

                  )
        )


setMarkers <- function(markers, data, oldMarkers){

  if(is.null(oldMarkers)){
    oldMarkers <- unique(data[["variable"]])
  }

  notInMarkers <- markers[!markers %in% oldMarkers]

  if(length(notInMarkers) > 0){
    errorMsg <- paste0("These markers not in qcData:\n",
                       paste(notInMarkers, collapse="\n"))
    stop(errorMsg)
  }

  newMarkers <- droplevels(oldMarkers[oldMarkers %in% markers])
  newData <- data[variable %in% newMarkers]
  data$variable <- droplevels(data$variable)

  return(list(markers = newMarkers, data=newData))

}

#Define R6 objects - basically are self-describing data objects for use
#by flowDashboard

##qcFlowObj
##annotation = "data.table"
##qcData = "data.table"
##subsetOptions = "list"
##mapVar = character
#' R6 object for Quality Control of flow data
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for getting/setting features for flowDashboard.
#' @format \code{\link{qcFlowObj}} object.
#' @examples
#' library(flowWorkspace)
#' gs <- load_gs(flowStats::GvHD)
#' #
#' @field annotation Annotation (can be extracted as phenoData from a GatingSet) as data.table.
#' @field subsetOptions set which columns in annotation to use for subsetting. Set by \code{setSubsetAndSortOptions()}.
#' @field subsetOptionList named list, where every entry corresponds to levels in a column in annotation.
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/lightning-viz/lightining-r/}
#'   \item{\code{new()}}{}
#'   \item{\code{checkIntegrity()}}{This method checks whether the identifier used in annotation and data agree and ensures data integrity between the two.}
#'   \item{\code{setSubsetAndSortOptions()}}{set the subset and sortOptions}
#'   @seealso
qcFlowObj <- R6Class(
  "qcFlowObj", inherit=commonDataObj,
  public=list(
    initialize= function(annotation, qcData, mapVar=NULL,
                         checkIntegrity=TRUE, reconcile=TRUE){

                            if(is.null(mapVar)){
                              mapVar=c("idVar"="FCSFiles")
                              }

                            if(checkIntegrity){
                              outList <- checkIntegrity(annotation, qcData, mapVar, reconcile)
                              annotation <- outList$annotation
                              qcData <- outList$data
                            }

                            self$annotation <- annotation
                            self$qcData <- qcData
                            self$mapVar <- mapVar
                            self$markers <- unique(qcData$variable)
                            invisible(self)

                          },
                          returnMergedData=function(){
                            self$qcData[self$annotation, on=self$mapVar]
                          },
                          qcData = NULL, markers=NULL,
                          mapVar=NULL,
    setMarkers = function(markers){
      oldMarkers <- self$markers

      outList <- setMarkers(markers, data=self$qcData,
                            oldMarkers=oldMarkers)

      self$markers <- outList$markers
      self$qcData <- outList$data
      invisible(self)
    }

  )
)

##gatingObj
##annotation = "data.table"
##popTable = "data.table"
##imageDir = "list"
##subsetOptions = list()
##joinVar = character
#' @export
gatingObj <-
  R6Class(
  "gatingObj", inherit=commonDataObj,
  public=list(
        initialize = function(annotation, popTable, mapVar=NULL,
                              gates=NULL, imageDir=NULL,
                              checkIntegrity=TRUE, reconcile=TRUE){

                              if(checkIntegrity){
                                outList <- checkIntegrity(annotation, popTable,
                                                          mapVar, reconcile)
                                annotation <- outList$annotation
                                popTable <- outList$data
                              }

                              if(is.null(mapVar)){
                                mapVar <- c("name"="FCSFiles")
                              }

                              pops <- unique(popTable$Population)
                              #print(pops)

                              #popTable$Population <- factor(self$popTable$Population, levels=pops)

                              self$annotation <- annotation
                              self$popTable <- popTable
                              self$imageDir <- imageDir
                              self$gates <- gates
                              self$mapVar <- mapVar
                              self$populations <- pops
                              self$popSubsets <- list(all=pops)


                              invisible(self)
                            },
        setAnnotationDisplayOptions=function(annotCols){
          annotCols <- annotCols[annotCols %in% colnames(self$returnMergedData())]
          self$annotCols <- annotCols
        },
        returnMergedData =function(){
          self$popTable[self$annotation, on=self$mapVar]
        },
      setPopulations = function(popList){
          popTable <- self$popTable
          populations <- unique(self$populations)
          notInPopulations <- popList[!popList %in% populations]

          if(length(notInPopulations)>0){
            errorMsg <- paste0("These populations not in data:\n",
                               paste(notInPopulations, collapse="\n"))
            warning(errorMsg)
          }

          newPop <- populations[populations %in% popList]
          popTable <- popTable[Population %in% newPop]

          self$populations <- newPop
          self$popTable <- popTable
          invisible(self)
      },
      setPopulationSubset = function(subPopSets=NULL){
        if(!is.null(subPopSets) | !is.list(subPopSets)){warning("Input must be a list")}
        populations = self$populations
        outList <- list(all=populations)
        names(outList) <- c("all")

        if(!is.null(subPopSets)) {
          oL <- lapply(subPopSets, function(x){
          xOut <- x[x %in% populations]
          xOut})

          outList <- c(outList, oL)

          names(outList) <- c("all",names(subPopSets))
        }

        self$popSubsets <- outList
        invisible(self)
      },
      popSubsets = NULL,
      populations = NULL,
      popTable = NULL,
      imageDir = NULL,
      mapVar=NULL,
      gates=NULL
      )
  )

# annotation="data.table",
#expressionData = "data.table",
#subsetOptions="subsetOptionsObj",
# joinVar="character"
#' @export
populationExpressionObj <-
  R6Class("populationExpressionObj", inherit=commonDataObj,
           public=list(
             initialize = function(annotation, expressionData, mapVar=NULL,
                                   checkIntegrity=TRUE, reconcile=TRUE){

                                       if(checkIntegrity){
                                         outList <- checkIntegrity(annotation,
                                                                   expressionData, mapVar,
                                                                   reconcile)
                                         annotation <- outList$annotation
                                         expressionData <- outList$data
                                       }
                                      if(is.null(mapVar)){
                                        mapVar <- c("idVar"="FCSFiles")
                                      }

                                       populations <- as.character(unique(expressionData$Population))
                                       self$annotation <- annotation
                                       self$expressionData <- expressionData
                                       self$mapVar <- mapVar
                                       self$markers <- unique(expressionData$variable)
                                       self$populations <- populations

                                       invisible(self)

                                     },
                  returnMergedData = function(){
                    self$expressionData[self$annotation, on=self$mapVar]
                  },
                  setMarkers = function(markers){
                    expData <- self$expressionData
                    oldMarkers <- unique(expData[["variable"]])

                    outList <- setMarkers(expData,markers = markers,
                               oldMarkers=oldMarkers)
                    self$markers <- outList$markers
                    self$expressionData <- outList$data

                    invisible(self)
                  },

                  expressionData = NULL,
                  mapVar=NULL,
                  markers=NULL,
                  populations=NULL)

          )

#' Build a qcFlowObj from flowSet or gatingSet
#'
#' @param gs - usually a GatingSet, but can also be a flowSet (useful for QC before gating)
#' @param annotation - annotation. if NULL, will attempt to get from phenoData slot
#' @param samplePop - number of points to sample from each flowFrame
#' @param qcMarkers - list of markers to return that represent qcMarkers. Will return warning if the markers don't exist in data
#' @param mapVar - maps the identifier in gs to annotation. If annotation is pulled from GatingSet phenoData, will be generated automatically
#' @param objId - Unique object ID, used in ShinyModule to avoid namespace collisions. If NULL, will be generated automatically
#'
#'
#' @return qcFlowObj
#' @export
#'
#' @examples
#' gsFile <- system.file("extdata", "gvHDgs", package="flowDashboard")
#' gs <- load_gs(gsFile)
#' QCO <- QCOFromGatingSet(gs)
#' QCO
QCOFromGatingSet <- function(gs, annotation=NULL, samplePop=4000,
                               qcMarkers=NULL, mapVar=NULL, objId=NULL){

  #make sure the marker names are R-permitted
  if(!is.null(qcMarkers)){
    qcMarkers <- make.names(qcMarkers)
  }

  if(class(gs)[1] %in% c("flowSet", "ncdfFlowSet", "GatingSet")){
    dataMelt <- returnMeltedData(gs, selectMarkers=qcMarkers,
                                 samplePop = samplePop, returnCellNum = TRUE)

    if(is.null(annotation)){
      annotation <- pData(gs@data@phenoData)
      mapVar = c("idVar"="name")
    }
  }


  if(!is.null(annotation) & is.null(mapVar)){
    stop("You need to supply a mapping variable in the form of mapVar=c('idVar'= X),
           where X is the id column in your annotation that corresponds with qcData$idVar")
  }

  dataMelt <- data.table(dataMelt)
  annotation <- data.table(annotation)

  QCO <- qcFlowObj$new(qcData=dataMelt, annotation=annotation, mapVar=mapVar)

  ##assign default objId if there is none
  ##assign a random string Id to avoid namespace collisions
  if(is.null(objId)){

    randName <- makeRandomId()
    objId <- paste0("QCO-", randName)

  }

  QCO$objId <- objId
  #set default options

  annotCols <- colnames(annotation)

  annotCols <- annotCols[!annotCols %in% mapVar]

  QCO$setAnnotationDisplayOptions(annotCols)
  QCO$setSubsetAndSortOptions(annotCols, annotCols)

  return(QCO)
}

#' Build a gatingObj from a gatingSet
#'
#' @param gs - a GatingSet object.
#' @param annotation - annotation object. if NULL, will try to pull from phenoData in gs@data
#' @param populations - set of populations to display in data. Used in Shiny modules
#' @param objId - unique ID for this object. Used in shiny modules to avoid namespace collisions.
#' @param imageDir - image directory for making all gating images. Will be created if it doesn't exist.
#' @param mapVar - a named variable that will map to populationTable.
#' @param makeGraphs - Boolean that sets whether the function will generate all gating images
#' The column to map in popTable is `name`. Default value is FALSE.
#'
#' @return gatingObj
#' @export
#'
#' @examples
#' gsFile <- system.file("extdata", "gvHDgs", package="flowDashboard")
#' gs <- load_gs(gsFile)
#' tmpDir <- tempdir()
#' GO <- GOFromGatingSet(gs, imageDir=tmpDir, makeGraphs=TRUE)
#' GO
#'
#' annot <- pData(gs@data@phenoData)
#' GO <- GOFromGatingSet(gs, annotation=annot, makeGraphs=FALSE)
GOFromGatingSet <- function(gs, annotation=NULL, populations=NULL,
                              imageDir=NULL, mapVar=NULL, objId=NULL, makeGraphs=FALSE){
    if(is.null(annotation)){
      annotation <- pData(gs@data@phenoData)
      mapVar <- c("name"="name")
    }

    if(is.null(populations)){
      populations <- getNodes(gs, path="auto")[-1]
    }

    if(!is.null(annotation) & is.null(mapVar)){
      stop("You need to supply a mapping variable in the form of mapVar=c('name'= X),
           where X is the id column in your annotation that corresponds with popTable$name")
    }


  if(is.null(objId)){
    randName <- makeRandomId()
    objId <- paste0("GO-", randName)
  }

  annotation <- data.table(annotation)
  popTable <- data.table(getPopulationsAndZscores(gs, pipelineFile=objId))

  GO <- gatingObj$new(popTable=popTable, annotation=annotation, mapVar=mapVar)

    GO$objId <- objId

    if(!is.null(populations)) {GO$setPopulations(populations)}
    GO$setPopulationSubset(subPopSets = NULL)

    annotCols <- colnames(annotation)
    sortCols <- annotCols[!annotCols %in% mapVar]
    annotCols <- c(sampleId="name", sortCols, "Population", "Count")

    GO$setAnnotationDisplayOptions(annotCols)
    GO$setSubsetAndSortOptions(sortCols, sortCols)

    if(!is.null(imageDir)){
      if(makeGraphs){
        if(!dir.exists(imageDir)){
          dir.create(imageDir)
        }
        plotAllPopulationsOld(gs, pipelineFile = objId,imagePath=paste0(imageDir, "/"))
      }
      GO$imageDir <- imageDir
    }else{
      if(makeGraphs){
        stop("You set makeGraphs=TRUE, but didn't specify imageDir")
        }
      }

    return(GO)
}

#' Build a populationExpressionObj from a GatingSet
#'
#' @param gs - a gatingSet
#' @param annotation - Annotations for each sample, where one column = sampleNames(gs).
#' If NULL, then it will attempt to grab annotation from the phenoData slot of
#' the gatingSet.
#' @param populations - A list of populations (must correspond to populationNames in gs).
#' If NULL, will just set populations with all populations in gatingSet
#' @param samplePop - Number of cells per population to sample. If NULL, returns all
#' cells in population.
#' @param objId - A unique object identifier used to avoid namespace collisions.
#' If NULL, a unique ID will be generated for the populationExpressionObj
#'
#' @return populationExpressionObj
#' @export
#'
#' @examples
#' gsFile <- system.file("extdata", "gvHDgs", package="flowDashboard")
#' gs <- load_gs(gsFile)
#' PEO <- PEOFromGatingSet(gs)
#' PEO
PEOFromGatingSet <- function(gs, annotation=NULL, populations=NULL,
                             samplePop=4000, objId=NULL, mapVar=NULL){

  if(is.null(annotation)){
    annotation <- pData(gs@data@phenoData)
    mapVar <- c("idVar"="name")
  }

  if(is.null(populations)){
    populations <- getNodes(gs, path="auto")[-1]
  }

  dataList <- lapply(populations, function(x){
    flowDashboard::returnMeltedDataFromGS(gS=gs,
                                          population=x,
                                          samplePopulation=samplePop)})

  annotation <- data.table(annotation)
  expressionData <- data.table::rbindlist(dataList)

  PEO <- populationExpressionObj$new(expressionData=expressionData,
                                     annotation=annotation, mapVar=mapVar)

  #PEO$setPopulations(populations)

  if(is.null(objId)){
    randName <- makeRandomId()
    objId <- paste0("PEO-", randName)
  }

  annotCols <- colnames(annotation)
  annotCols <- annotCols[!annotCols %in% mapVar]

  PEO$setAnnotationDisplayOptions(annotCols)
  PEO$setSubsetAndSortOptions(annotCols, annotCols)
  PEO$objId <- objId

  return(PEO)
}

