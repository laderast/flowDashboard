
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
    selectInput(ns("Population"), label="Select Population to Compare", choices=populationList,
                populationList[1]),
    selectInput(ns("xFacet"),label="Select X Facet", choices=facetConditions, selected=facetConditions[1]),
    #selectInput(ns("yFacet"), "Select Y Facet", choices=facetList, selected=facetList),
    plotOutput(ns("dotPlot"), hover= hoverOpts(ns("plotHover"), delay = 100, delayType = "debounce"))#,
    #uiOutput(ns("hoverTip"))
    #selectInput(ns("ConditionVariable"), label="Select Ordering Variable",
    #            choices=orderList)

  )
}



#' Title
#'
#' @param GO
#'
#' @return
#' @export
#'
#' @examples
dotPlotUIFromGO <- function(GO, objId=NULL){

  if(is.null(objId)){
    objId=GO$objId
  }
  dotPlotUI(id=objId, populationList = GO$populations,
            facetConditions = GO$subsetOptions)

}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param GO
#' @param annotation
#'
#' @return
#' @export
#'
#' @examples
dotplotOutputFromGO <- function(input, output, GO, objId=NULL, annotation){

  if(is.null(objId)){
    objId=GO$objId
  }

  #print(id)

  callModule(dotPlotOutput, id=objId, data=GO$popTable, annotation=annotation,
             facetOrderList = GO$subsetOptionList, mapVar = GO$mapVar)
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
dotPlotOutput <- function(input, output, session, data, annotation,
                          mapVar=c("name"="FCSFiles"),
                          facetOrderList){

  popTableReact <- reactive({
    validate(need(input$Population, "Population Var not here"))
    #validate(input$ConditionVariable, FALSE)

    #print(head(annotation()))

    dataOut <-
      data[annotation(), on=mapVar][Population %in% input$Population][!is.na(percentPop)]

    #print(head(dataOut))
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

    out <- dotPlot(popTableReact(), xFacet = xFacet,
                   facetOrderList = facetOrderList)

    out
  })


}

#' Title
#'
#' @param data
#' @param aesList
#' @param xFacet
#'
#' @return
#' @export
#'
#' @examples
dotPlot <- function(data, aesList=aes_string(x="Population",y="percentPop"), xFacet, facetOrderList){

  facetOrder <- facetOrderList[xFacet]
  facetFormula <- paste0(".~",xFacet)
  if(is.null(xFacet)){

    facetFormula <- NULL
  }

  plotTitle <- paste("Population Comparison for", data$Population[1])

  #function to remove missing values for facet
  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols, with=FALSE])
    return(data[completeVec, ])
  }

  dataNew <- completeFun(data,xFacet)

  dataNew[[xFacet]] <- factor(dataNew[[xFacet]])

  #drop levels if necessary
  #upd.cols = sapply(dataNew, is.factor)
  #dataNew[, names(dataNew)[upd.cols] := lapply(.SD, factor), .SDcols = upd.cols]

  #print(head(dataNew))

  #set facet order here
  #dataNew[xFacet] <- facetOrderList[xFacet]

  lowLim <- min(dataNew$percentPop) - 10

  out  <- ggplot(dataNew, aesList) +
    #labs(list(y = "% Parent Population")) +
    theme(axis.title.x = element_text(face="bold"), axis.text.x = element_blank()) +
    theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold")) +
    scale_y_continuous(limits= c(lowLim,100)) +
    ggtitle(plotTitle)
  #theme() +

  out <- out + geom_dotplot(binaxis='y', stackdir='center', method="dotdensity", binwidth=1) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5, colour="red")

  if(!is.null(facetFormula)){
    out <- out + facet_grid(facets = facetFormula, scales = "free")
  }

  return(out)

}
