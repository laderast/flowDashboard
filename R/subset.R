#' Title
#'
#' @param dataObj
#'
#' @return
#' @export
#'
#' @examples
subsetModuleUICDO <-function(dataObj, objId=NULL){

  if(is.null(objId)){
    objId <- dataObj$objId
  }

  subsetModuleUI(id=objId, subsetOptionList = dataObj$subsetOptionList,
                 sortOptions = dataObj$sortOptions,
                 subsetOptions= dataObj$subsetOptions)
}


#' Title
#'
#' @param id
#' @param subsetOptionList
#'
#' @return
#' @export
#'
#' @examples
subsetModuleUI <- function(id, subsetOptionList, sortOptions, subsetOptions){
  ns <- NS(id)

    outList <- list(
    selectInput(ns("categoryNameSubset"), "Select Variable to Subset On",
                choices=subsetOptions,
                selected=subsetOptions[1]),
    selectInput(ns("subgroup"), "Select Subset", choices =
                     subsetOptionList[[1]], selectize = TRUE,
                       selected=subsetOptionList[[1]], multiple=TRUE))

    if(!is.null(sortOptions)){
          outList <- list(outList,
                       selectInput(ns("sortVariable"), label="Select Sort Condition",
                choices=sortOptions))
    }

    tagList(outList)

}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param dataObj
#'
#' @return
#' @export
#'
#' @examples
subsetModuleDCO <- function(input, output, dataObj, objId=NULL){

  if(is.null(objId)){
    objId <- dataObj$objId
  }

  callModule(subsetModule, id=objId, subsetOptionList=dataObj$subsetOptionList,
               annotation=dataObj$annotation)
}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param subsetOptionList
#'
#' @return
#' @export
#'
#' @examples
subsetModule <- function(input, output, session, subsetOptionList, annotation){
    ns <- session$ns
    observe({

    validate(need(input$categoryNameSubset, "needs category"))
    categoryName <- input$categoryNameSubset
    #print(categoryName)
    subsetChoice <- subsetOptionList[[categoryName]]

    #print(subsetChoice)

    updateSelectInput(session, "subgroup",
                             choices= subsetChoice,
                    selected= subsetChoice)
  })


  annotationOut <- reactive({

    validate(need(input$subgroup,"Need Subgroups"))#,
             #need(input$sortVariable,"Need Sort Variable"))

    sortVariable <- NULL

    if(!is.null(input$sortVariable)){
      sortVariable <- input$sortVariable
    }
    #returnIDs <- list(subsetList=input$subgroup,subsetID=categoryName,
    #                sortID = sortVariable)

    categoryName <- input$categoryNameSubset

    #print(input$subgroup)

    subgroup <- input$subgroup

    subgroup <- paste0("c('",paste(subgroup, collapse = "', '"),"')")
    filterExpression <- paste0("as.character(", categoryName, ") %in% ", subgroup)

    #print(filterExpression)

    annotationSubset <- annotation %>% filter_(.dots=filterExpression)

    if(!is.null(sortVariable)){
          annotationSubset <- annotationSubset %>%
            arrange_(sortVariable)
    }

    #annotationSubset <- annotation[as.name(categoryName) %in% input$subgroup][order(as.name(categoryName))]
    #print(head(annotationSubset))
    outTable <- data.table(annotationSubset)
    #setkeyv(outTable, c(sortVariable, "FCSFiles"))

    return(outTable)
  })

  return(annotationOut)
}
