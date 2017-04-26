library(data.table)

#R6 objects - basically are self-describing data objects for use
#by flowDashboard
subsetOptionsObj <- setRefClass("subsetOptions", subsetOptions = "character",
                             subsetOptionList = "list",
                             sortOptions = "character")

qcFlowObj <- setRefClass("qcFlowObj", annotation ="data.table",
                            qcData = "data.table",
                            subsetOptions = "subsetOptionsObj",
                         joinVar="character")

gatingObj <- setRefClass("gatingObj", annotation="data.table",
                            popTable = "data.table",
                            imageDir = "character",
                            subsetOptions = "subsetOptionsObj",
                            joinVar="character")

populationExpressionObj <- setRefClass("populationExpressionObj",
                                       annotation="data.table",
                                       expressionData = "data.table",
                                       subsetOptions="subsetOptionsObj",
                                       joinVar="character")

#need init objects for each
initGO <- function(annotation, popTable, imageDir=NULL, checkIntegrity=TRUE){

}

initQFO <- function(annotation, qcData, checkIntegrity=TRUE){

}

initPEO <- function(annotation, expressionData, checkIntegrity=TRUE){

}

#check to make sure annotation/control
checkQCOIntegrity <- function(obj){

}

checkGOIntegrity <- function(obj){

}

checkPEOIntegrity <- function(obj){

}

changeAnnotation <- function(obj){


}


setSubsetOptions <- function(subsetOptions, sortOptions, checkIntegrity = TRUE){

  annotation <- self$annotation

  #need to check that options agree (Columns are in annotation)
  if(checkIntegrity){
    soNotInAnnotation <- subsetOptions[subsetOptions %in% colnames(annotation)]
    sortNotInAnnotation <- sortOptions[sortOptions %in% colnames(annotation)]

    if(length(soNotInAnnotation) > 0 | length(sortNotInAnnotation) > 0){


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

#subsetOptions$methods(setSubsetOptions=setSubsetOptions)
qcFlowObj$methods(setSubsetOptions=setSubsetOptions)
gatingObj$methods(setSubsetOptions=setSubsetOptions)
populationExpressionObj$methods(setSubsetOptions=setSubsetOptions)
