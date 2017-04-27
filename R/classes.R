library(data.table)
library(R6)

##commonDataObj - SuperClass for other classes
##classes that inherit
commonDataObj <- R6Class("commonDataObj", public=list(annotations=NULL,
                                                      subsetAnnotation=subsetAnnotation,
                                                      subsetOptionsList=list(),
                                                      setSubsetOptionsList = setSubsetOptions),
                         private=list()
                         )

#Define R6 objects - basically are self-describing data objects for use
#by flowDashboard

##qcFlowObj
##annotation = "data.table"
##qcData = "data.table"
##subsetOptions = "list"
##joinVar = character

qcFlowObj <- R6Class("qcFlowObj", inherit=commonDataObj,
                     public=list(
                          initialize= initQFO,
                          checkIntegrity = checkIntegrityObj(reconcile=TRUE),
                          qcData = NULL,
                          joinVar=NULL)
                     )

##gatingObj
##annotation = "data.table"
##popTable = "data.table"
##imageDir = "list"
##subsetOptions = list()
##joinVar = character


gatingObj <- R6Class("gatingObj", inherit=commonDataObj,
                     public=list(
                            initialize = initGO,
                            checkIntegrity = checkIntegrityObj(reconcile=TRUE),
                            popTable = NULL,
                            imageDir = NULL,
                            joinVar=NULL,
                            gates=NULL)
                     )

# annotation="data.table",
#expressionData = "data.table",
#subsetOptions="subsetOptionsObj",
# joinVar="character"

populationExpressionObj <- R6Class("populationExpressionObj", inherit=commonDataObj,
                                   public=list(initialize = initPEO,
                                   checkIntegrity = checkIntegrityObj(reconcile=TRUE),
                                     expressionData = NULL,
                                     joinVar=NULL,
                                     population=NULL)
                                   )

#need init objects for each
initGO <- function(annotation, popTable, mapVar, gates=NULL,
                   imageDir=NULL, checkIntegrity=TRUE, reconcile=TRUE){

  if(checkIntegrity){
      outList <- checkIntegrity(annotation, popTable, mapVar, reconcile)
      annotation <- outList$annotation
      popTable <- outList$popTable
  }

  self$annotation <- annotation
  self$popTable <- popTable
  self$imageDir <- imageDir
  self$gates <- gates

}

initQFO <- function(annotation, qcData, mapVar,
                    checkIntegrity=TRUE, reconcile=TRUE){
  if(checkIntegrity){
    outList <- checkIntegrity(annotation, qcData, mapVar, reconcile)
    annotation <- outList$annotation
    qcData <- outList$data
  }

  self$annotation <- annotation
  self$qcData <- qcData
  self$imageDir <- imageDir

}

initPEO <- function(annotation, expressionData, mapVar,
                    checkIntegrity=TRUE, reconcile=TRUE){

  if(checkIntegrity){
    outList <- checkIntegrity(annotation, expressionData, mapVar, reconcile)
    annotation <- outList$annotation
    expressionData <- outList$data
  }

  self$annotation <- annotation
  self$popTable <- expressionData

}

reconcileData <- function(annotation, data, mapVar, idsInBoth){
  mapColData <- names(mapVar)
  mapColAnnotation <- mapVar

  annotation <- annotation[mapColAnnotation %in% idsInBoth, with=FALSE]
  data <- data[mapColData %in% idsInBoth, with=FALSE]

  return(list(annotation=annotation, data=data))

}

checkIntegrityObj <- function(reconcile=FALSE){
  classObj <- class(self)
  mapVar <- self$mapVar

  annotation <- self$annotation

  if(classObj == "populationExpressionObj"){
      data <- self$expressionData
  }

  if(classObj == "gatingObj"){
      data <- self$popTable
  }

  if(classObj == "qcFlowObj"){
      data <- self$expressionData
  }

  outList <- checkIntegrity(annotation, data, mapVar, reconcile=reconcile)

  if(classObj == "populationExpressionObj"){
    self$expressionData <- data
  }

  if(classObj == "gatingObj"){
    self$popTable <- data
  }

  if(classObj == "qcFlowObj"){
    self$expressionData <- data
  }

  invisible(self)

}


checkIntegrity <- function(annotation, data, mapVar, reconcile=TRUE){

  mapColData <- names(mapVar)
  mapColAnnotation <- mapVar

  if(!mapColData %in% colnames(data)){
    stop(paste("map data column not in dataset:", mapColData))
  }
  if(!mapColAnnotation %in% colname(annotation)){
    stop(paste("map annotation column not in annotation:", mapColAnnotation))
  }

  idsData <- unique(data[[mapColData]])
  idsAnnotation <- unique(annotation[[mapColAnnotation]])
  idsInBoth <- union(idsData, idsAnnotation)
  idsNotInAnnotation <- setdiff(idsData, idsAnnotation)
  idsNotInData <- setdiff(idsAnnotation, idsData)

  if(length(idsInBoth)==0){
    stop("data and annotation ids do not match")
  }
  if(length(idsNotInAnnotation) > 0){
    warning(paste("These IDs not in Annotation:", idsNotInAnnotation))
  }
  if(length(idsNotInData) > 0){
    warning(paste("These IDs not in Data:", idsNotInData))
  }

  if(reconcile){
    outList <- reconcileData(annotation, data, mapVar, idsInBoth)
    annotation <- outList$annotation
    data <- outlist$data
  }

  return(list(annotation=annotation, data=data))

}

subsetAnnotation <- function(ids){
  self$checkIntegrityObj(idsInBoth = ids, reconcile=TRUE)
  invisible(self)
}

setSubsetOptions <- function(subsetOptions, sortOptions, checkIntegrity = TRUE){

  annotation <- self$annotation

  #need to check that options agree (Columns are in annotation)
  if(checkIntegrity){
    soNotInAnnotation <- subsetOptions[subsetOptions %in% colnames(annotation)]
    sortNotInAnnotation <- sortOptions[sortOptions %in% colnames(annotation)]

    if(length(soNotInAnnotation) > 0){
      warning(paste("annotation and subset options don't match:", soNotInAnnotation))
    }
    if(length(sortNotInAnnotation) > 0){
      warning(paste("annotation and sort options don't match:", sortNotInAnnotation))
    }
  }

  subsetOptionList <- as.list(annotation[,subsetConditions, with=FALSE])
  subsetOptionList <- lapply(subsetOptionList, function(x){unique(as.character(x))})

  sortOptionList <- as.list(annotation[,sortConditions, with=FALSE])
  subsetOptionList <- lapply(subsetOptionList, function(x){unique(as.character(x))})

  self$subsetOptions <- subsetOptions
  self$subsetOptionList <- subsetOptionList
  self$sortOptions <- sortOptions
  self$sortOptionList <- sortOptionList

  invisible(self)
}


