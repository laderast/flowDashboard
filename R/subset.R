#' Title
#'
#' @param id
#' @param subsetOptions
#'
#' @return
#' @export
#'
#' @examples
subsetModuleUI <- function(id, subsetOptions, sortOptions, subsetAlias){
  ns <- NS(id)

  tagList(
    selectInput(ns("categoryNameSubset"), "Select Variable to Subset On", choices=subsetAlias,
                selected=subsetAlias[1]),
    selectInput(ns("subgroup"), "Select Subset", choices =subsetOptions[[1]],
                       selected=subsetOptions[[1]], multiple=TRUE, width="100%"),
    selectInput(ns("sortVariable"), label="Select Sort Condition", choices=sortOptions)
  )

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param subsetOptions
#'
#' @return
#' @export
#'
#' @examples
subsetModule <- function(input, output, session, subsetOptions, annotation){
    ns <- session$ns
    observe({

    categoryName <- input$categoryNameSubset
    subsetChoice <- subsetOptions[[categoryName]]

    updateSelectInput(session, "subgroup", label="",
                             choices= subsetChoice, selected= subsetChoice)
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
    return(data.table(annotationSubset))
  })

  return(annotationOut)
}
