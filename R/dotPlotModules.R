#' Title
#'
#' @param id
#' @param populationList
#' @param facetList
#'
#' @return
#' @export
#'
#' @examples
dotPlotUI <- function(id, populationList, facetConditions) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    selectInput(ns("Population"), label="Select Population to Compare", choices=displayNodes,
                displayNodes[1]),
    selectInput(ns("xFacet"),label="Select X Facet", choices=facetConditions, selected=facetConditions[1]),
    #selectInput(ns("yFacet"), "Select Y Facet", choices=facetList, selected=facetList),
    plotOutput(ns("dotPlot"), hover= hoverOpts(ns("plotHover"), delay = 100, delayType = "debounce")),
    uiOutput(ns("hoverTip"))
    #selectInput(ns("ConditionVariable"), label="Select Ordering Variable",
    #            choices=orderList)

  )
}

#' Percent Plot Module
#'
#' @param input
#' @param output
#' @param session
#' @param data - populationTable
#' @param annotation - annotationTable (must be consistent with popTable)
#'
#' @return
#' @export
#'
#' @examples
dotPlotOutput <- function(input, output, session, data, annotation, mapVar=c("name"="FCSFiles"),
                          facetOrderList){

  # dotPlotDynamicUI <- renderUI({
  #   ns <- session$ns
  #   displayNodes <- displayNodes[displayNodes %in% popTableReact()$Population]
  #   print(displayNodes)
  #
  #
  #   tagList(
  #   selectInput(ns("Population"), "Select Population to Compare", choices=displayNodes,
  #               displayNodes[1]),
  #   selectInput(ns("xFacet"),"Select X Facet", choices=facetList, selected=facetList[1])
  #   )
  #
  # })



  popTableReact <- reactive({
    #validate(need(input$Population))
    #validate(input$ConditionVariable, FALSE)

    #PopList <- input$PopulationList
    #print(input$PopulationList)

    #orderVariable <- input$ConditionVariable

    #annotation <- data.table(annotation)
#    if(nrow(annotation) == 0){
#      annotation <- TRUE
#    }

    dataOut <- data[annotation(), on=mapVar]
    dataOut <- dataOut[Population %in% input$Population]

    dataOut <- dataOut[!is.na(dataOut$percentPop)]

    #sortVariable <- key(annotation())

    #%>%
    #filter_(ifelse(is.na(input$xFacet),0,input$xFacet) & ifelse(is.na(input$yFacet),0,input$yFacet))

    #print(dataOut)

    #%>%
    #arrange_(orderVariable)

    return(dataOut)
  })

  output$hoverTip <- renderUI({

    #ns <- session$ns

    hover <- input$plotHover

    if(is.null(hover$x)){
      return(NULL)
    }

    #print(hover)
    point <- popTableReact()[floor(hover$x),]
    #print(point)

    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.70); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(#"<b> Car: </b>", rownames(point), "<br/>",
        #"<b> mpg: </b>", point$mpg, "<br/>",
        "<b> Patient: </b>", point$patientID, "<br/>",
        "<b> Condition: </b>", point$NewCondition)))
    )
  })


  output$dotPlot <- renderPlot({

    #print(head(popTableReact()))

    validate(
      need(input$xFacet, "xFacets not Set")
    )

    xFacet <- input$xFacet
    #yFacet <- input$yFacet
    yFacet <- input$xFacet

    facetOrder <- facetOrderList[xFacet]

    facetFormula <- paste0(yFacet,"~",xFacet)

    if(xFacet == yFacet){
      facetFormula <- paste0(".~",xFacet)
    }

    print(facetFormula)
    plotTitle <- paste("Population Comparison for", popTableReact()$Population[1])

    #out <-

    completeFun <- function(data, desiredCols) {
      completeVec <- complete.cases(data[, desiredCols])
      return(data[completeVec, ])
    }

    #remove entries in popTableReact that have no value for faceting variable
    #xInd <- is.na(popTableReact()[[xFacet]])

    dataNew <- completeFun(popTableReact(),xFacet)

    upd.cols = sapply(dataNew, is.factor)
    dataNew[, names(dataNew)[upd.cols] := lapply(.SD, factor), .SDcols = upd.cols]

    #set facet order here
    #dataNew[xFacet] <- facetOrderList[xFacet]

    out  <- ggplot(dataNew, aes(x=Population, y=percentPop)) +
      #labs(list(x = "Subtype", y = "% Cell Population")) +
      theme(axis.title.x = element_text(face="bold"), axis.text.x = element_blank()) +
      theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold")) +
      ggtitle(plotTitle)
    #theme() +

    out <- out + geom_dotplot(binaxis='y', stackdir='center', method="dotdensity", binwidth=1) +
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "crossbar", width = 0.5, colour="red") +
    facet_grid(facets = facetFormula, scales = "free")

    return(out)
  })


}
