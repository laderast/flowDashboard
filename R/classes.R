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

initSO <- function(){

}

initQFO <- function(){

}

initGO <- function(){

}

initPEO <- function(){

}

#check to make sure annotation/control
checkQCOIntegrity <- function(obj){

}

checkGOIntegrity <- function(obj){

}

checkPEOIntegrity <- function(obj){

}

setSubsetOptions <- function(subsetOptions, sortOptions, checkIntegrity = TRUE){
  #need to check that options agree (Columns are in annotation)
  if(checkIntegrity){
    soNotInAnnotation <- subsetOptions[subsetOptions %in% colnames(self$annotation)]
    sortNotInAnnotation <- sortOptions[sortOptions %in% colnames(self$annotation)]
  }

  self$subsetOptions <- subsetOptions
  self$subsetOptionList <- subsetOptionList
  self$sortOptions <- sortOptions
  self$sortOptionList <- sortOptionList
}

#subsetOptions$methods(setSubsetOptions=setSubsetOptions)
qcFlowObj$methods(setSubsetOptions=setSubsetOptions)
gatingObj$methods(setSubsetOptions=setSubsetOptions)
populationExpressionObj$methods(setSubsetOptions=setSubsetOptions)
