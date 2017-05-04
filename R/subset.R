#' Title
#'
#' @param dataObj
#'
#' @return
#' @export
#'
#' @examples
subsetModuleUICDO <-function(dataObj){

  subsetModuleUI(id=dataObj$id, subsetOptionList = dataObj$subsetOptionList,
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

  tagList(
    selectInput(ns("categoryNameSubset"), "Select Variable to Subset On", choices=subsetOptions,
                selected=subsetOptions[1]),
    selectizeInput(ns("subgroup"), "Select Subset", choices = subsetOptionList[[1]],
                       selected=subsetOptionList[[1]], multiple=TRUE),
    selectInput(ns("sortVariable"), label="Select Sort Condition", choices=sortOptions)
  )

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
subsetModuleDCO <- function(input, output, session, dataObj){

  callModule(subsetModule, id=dataObj$id, subsetOptionList=dataObj$subsetOptionList,
               annotation=dataObj$annotation, session=session)
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
    subsetChoice <- subsetOptionList[categoryName]

    updateSelectizeInput(session, "subgroup",
                             choices= subsetChoice,
                      selected= subsetChoice)
  })


  annotationOut <- reactive({

    validate(need(input$subgroup,"Need Subgroups"),
             need(input$sortVariable,"Need Sort Variable"))

    sortVariable <- input$sortVariable
    #returnIDs <- list(subsetList=input$subgroup,subsetID=categoryName,
    #                sortID = sortVariable)

    categoryName <- input$categoryNameSubset

    #print(input$subgroup)

    subgroup <- input$subgroup

    subgroup <- paste0("c('",paste(subgroup, collapse = "', '"),"')")
    filterExpression <- paste0("as.character(", categoryName, ") %in% ", subgroup)

    #print(filterExpression)

    annotationSubset <- annotation %>% filter_(.dots=filterExpression) %>%
      arrange_(sortVariable)

    #annotationSubset <- annotation[as.name(categoryName) %in% input$subgroup][order(as.name(categoryName))]
    #print(head(annotationSubset))
    outTable <- data.table(annotationSubset)
    setkeyv(outTable, c(sortVariable, "FCSFiles"))

    return(outTable)
  })

  return(annotationOut)
}
