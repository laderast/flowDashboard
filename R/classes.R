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
#' @export
#'
#' @examples
reconcileData <- function(annotation, data, mapVar, idsInBoth){
  mapColData <- as.name(names(mapVar))
  mapColAnnotation <- as.name(mapVar)

  annotation <- annotation[eval(mapColAnnotation) %in% idsInBoth]
  data <- data[eval(mapColData) %in% idsInBoth]

  return(list(annotation=annotation, data=data))

}



#' Check integrity of data objects
#'
#' @param annotation
#' @param data
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

  idsData <- unique(data[[mapColData]])
  idsAnnotation <- unique(annotation[[mapColAnnotation]])
  idsInBoth <- union(idsData, idsAnnotation)
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


##commonDataObj - SuperClass for other classes
##classes that inherit
commonDataObj <-
  R6Class("commonDataObj",
          public=list(
            annotation=NULL,
            subsetOptionsList=list(),
            setSubsetOptionsList =
                function(subsetOptions, sortOptions,
                         checkIntegrity = TRUE){
                           annotation <- self$annotation
                           #need to check that options agree (Columns are in annotation)

                           if(checkIntegrity){
                             soNotInAnnotation <-
                               subsetOptions[subsetOptions %in%
                                               colnames(annotation)]
                             sortNotInAnnotation <-
                               sortOptions[sortOptions %in%
                                             colnames(annotation)]

                          if(length(soNotInAnnotation) > 0){
                            warning(
                              paste("annotation and subset options don't match:",
                                                   soNotInAnnotation)
                              )
                            }
                          if(length(sortNotInAnnotation) > 0){
                            warning(
                              paste("annotation and sort options don't match:",
                                    sortNotInAnnotation)
                              )
                            }
                        }

                        subsetOptionList <- as.list(
                                annotation[,subsetConditions, with=FALSE])
                        subsetOptionList <- lapply(
                                subsetOptionList, function(x)
                                  {unique(as.character(x))}
                                )

                        sortOptionList <- as.list(
                                annotation[,sortConditions, with=FALSE])
                        subsetOptionList <- lapply(
                                subsetOptionList, function(x)
                                  {unique(as.character(x))}
                                )

                        self$subsetOptions <- subsetOptions
                        self$subsetOptionList <- subsetOptionList
                        self$sortOptions <- sortOptions
                        self$sortOptionList <- sortOptionList

                        invisible(self)

                        },

                        checkIntegrityObj =
                              function(reconcile=FALSE){
                                  classObj <- class(self)
                                  mapVar <- self$mapVar
                                  annotation <- self$annotation

                                  if(classObj == "commonDataObj"){
                                      stop("commonDataObj cannot be used; use children")
                                      }

                                  if(classObj == "populationExpressionObj"){
                                      data <- self$expressionData
                                      }

                                  if(classObj == "gatingObj"){
                                      data <- self$popTable
                                      }

                                   if(classObj == "qcFlowObj"){
                                      data <- self$expressionData
                                   }



                                    outList <- checkIntegrity(annotation,
                                            data, mapVar, reconcile=reconcile)

                                    data <- outList$data
                                    self$annotation <- outList$annotation

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
                                },

                          subsetAnnotation = function(ids){
                              self$checkIntegrityObj(idsInBoth = ids, reconcile=TRUE)
                              invisible(self)
                              }

                        )
              )

#Define R6 objects - basically are self-describing data objects for use
#by flowDashboard

##qcFlowObj
##annotation = "data.table"
##qcData = "data.table"
##subsetOptions = "list"
##joinVar = character

qcFlowObj <- R6Class(
  "qcFlowObj", inherit=commonDataObj,

  public=list(
    initialize= function(annotation, qcData, mapVar,
                         checkIntegrity=TRUE, reconcile=TRUE){
                            if(checkIntegrity){
                              outList <- checkIntegrity(annotation, qcData, mapVar, reconcile)
                              annotation <- outList$annotation
                              qcData <- outList$data
                            }

                            self$annotation <- annotation
                            self$qcData <- qcData

                          },
                          qcData = NULL,
                          joinVar=NULL)

  )

##gatingObj
##annotation = "data.table"
##popTable = "data.table"
##imageDir = "list"
##subsetOptions = list()
##joinVar = character


gatingObj <- R6Class(
  "gatingObj", inherit=commonDataObj,
  public=list(
        initialize = function(annotation, popTable, mapVar, gates=NULL,
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

                            },
      getPopulations = function(){
          return(self$popTable$Population)},

      popTable = NULL,
      imageDir = NULL,
      joinVar=NULL,
      gates=NULL)
      )

# annotation="data.table",
#expressionData = "data.table",
#subsetOptions="subsetOptionsObj",
# joinVar="character"

populationExpressionObj <-
  R6Class("populationExpressionObj", inherit=commonDataObj,
           public=list(
             initialize = function(annotation, expressionData, mapVar,
                                   checkIntegrity=TRUE, reconcile=TRUE, self){

                                       if(checkIntegrity){
                                         outList <- checkIntegrity(annotation,
                                                                   expressionData, mapVar, reconcile)
                                         annotation <- outList$annotation
                                         expressionData <- outList$data
                                       }

                                       self$annotation <- annotation
                                       self$popTable <- expressionData

                                     },

                                     expressionData = NULL,
                                     joinVar=NULL,
                                     population=NULL)
                                   )

