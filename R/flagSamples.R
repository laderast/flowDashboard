#' Title
#'
#' @param id
#' @param label
#' @param markers
#' @param sortConditions
#' @param colorConditions
#' @param annotation
#'
#' @return
#' @export
#'
#' @examples
flagModuleUI <- function(id, label = "qcViolin", markers, sortConditions,
                       colorConditions, annotation) {
  # Create a namespace function using the provided id
  ns <- NS(id)


}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param data
#' @param annotation
#' @param idColumn
#' @param subsetCondition
#' @param subsetChoices
#' @param sortConditions
#' @param markers
#' @param colorConditions
#' @param mapVar
#'
#' @return
#' @export
#'
#' @examples
flagModuleOutput <- function(input, output, session, data, annotation,
                           idColumn = "patientID", subsetCondition=NULL,
                           subsetChoices=NULL, sortConditions, markers,
                           colorConditions, mapVar = c("idVar"="FCSFiles")) {
  checkedData <- reactive({
    #verify that data maps to annotation, otherwise return NULL

    #each module should verify joins
  })

  #return flagged


}
