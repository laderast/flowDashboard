library(data.table)
library(R6)

## To add: check if imageDir exists for GatingObj


#' Reconcile annotation and data tables in object
#'
#' @param annotation
#' @param data
#' @param mapVar
#' @param idsInBoth
#'
#' @return
#' @export
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
#' @export
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

##commonDataObj - SuperClass for other classes
##classes that inherit
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
                annotCols <- annotCols[annotCols %in% colnames(self$annotation)]
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
                       paste(notInMarkers, collapse="\n")
    )
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
#' @export
qcFlowObj <- R6Class(
  "qcFlowObj", inherit=commonDataObj,
  public=list(
    initialize= function(annotation, qcData, mapVar=NULL,
                         checkIntegrity=TRUE, reconcile=TRUE){
                            if(checkIntegrity){
                              outList <- checkIntegrity(annotation, qcData, mapVar, reconcile)
                              annotation <- outList$annotation
                              qcData <- outList$data
                            }
                            if(is.null(mapVar)){
                              mapVar=c("idVar"="FCSFiles")
                            }

                            self$annotation <- annotation
                            self$qcData <- qcData
                            self$mapVar <- mapVar
                            self$markers <- unique(qcData$variable)
                            invisible(self)

                          },
                          returnMergedData=function(self){
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

                              invisible(self)
                            },
        returnMergedData =function(self){
          self$popTable[self$annotation, on=self$mapVar]
        },
      setPopulations = function(popList){
          popTable <- self$popTable
          populations <- unique(self$populations)
          notInPopulations <- popList[popList %in% populations]

          if(length(notInPopulations)>0){
            errorMsg <- paste0("These populations not in data:\n",
                               paste(notInPopulations, collapse="\n"))
            stop(errorMsg)
          }

          newPop <- populations[populations %in% popList]
          popTable <- popTable[Population %in% newPop]

          self$populations <- newPop
          self$popTable <- popTable
          invisible(self)
      },
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
                                                                   expressionData, mapVar, reconcile)
                                         annotation <- outList$annotation
                                         expressionData <- outList$data
                                       }
                                      if(is.null(mapVar)){
                                        mapVar <- c("idVar"="FCSFiles")
                                      }

                                       populations <- unique(expressionData$Population)
                                       self$annotation <- annotation
                                       self$expressionData <- expressionData
                                       self$mapVar <- mapVar
                                       self$markers <- unique(expressionData$variable)
                                       self$populations <- populations

                                       invisible(self)

                                     },
                  returnMergedData = function(self){
                    self$expData[self$annotation, on=self$mapVar]
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

