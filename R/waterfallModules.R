##waterfall modules for visualizing population percentages
#' Title
#'
#' @param id
#' @param label
#'
#' @return
#' @export
#'
#' @examples
waterfallOutputUI <- function(id, label="waterfall", populationChoices, colorColumns){
  ns <- NS(id)

  tagList(
    selectInput(ns("population"), "Select Population To Display", choices=populationChoices,
                selected = populationChoices[1]),
    selectInput(ns("colorVar"), "Select Outcome to Color", choices=colorColumns,
                selected = colorColumns[1]),
    #uiOutput(ns("waterfallDynamicUI")),
    plotlyOutput(ns("waterfallPlot"), hover = hoverOpts(ns("plotHoverWF"), delay = 100, delayType = "debounce")),
    uiOutput(ns("clickTipG"))#,
    #plotOutput(ns("annotationHeatmap"))

  )

}

#' Title
#'
#' @param GO - a `gatingObj`, built from \code{GOFromGatingSet}
#'
#' @return
#' @export
#'
#' @examples
waterfallOutputUIfromGO <- function(GO, objId = NULL){
  if(is.null(objId)){
    objId <- GO$objId
  }

  waterfallOutputUI(id = objId,label = objId,populationChoices = GO$populations,
                    colorColumns = GO$sortOptions)
}


#' Title
#'
#' @param GO - a gating object
#'
#' @return - waterfallOutput module
#' @export
#'
#' @examples
waterfallOutputFromGO <- function(GO, annotation, objId=NULL){

  if(is.null(objId)){
    objId <- GO$objId
  }

  #print(objId)

  callModule(waterfallOutput, id=objId, data=GO$popTable, annotation=annotation,
             populationChoices=GO$populations, colorColumns=GO$sortOptions,
             mapVar = GO$mapVar, annotDisplayOptions=GO$annotCols)

}


#' Waterfall plot output module
#'
#' @param input - normal shiny input object
#' @param output - normal shiny output object
#' @param session - normal shiny session
#' @param data
#' @param annotation
#' @param populationChoices
#' @param colorColumns
#' @param subsetVariables
#'
#' @return
#' @export
#'
#' @examples
waterfallOutput <- function(input, output, session, data, annotation,
                            populationChoices, colorColumns, covariateChoices=NULL,
                            subsetVariables = NULL, annotDisplayOptions=NULL,
                            mapVar = c("name"="FCSFiles")){

  output$waterfallDynamicUI <- renderUI({

    ns <- session$ns

    if(is.null(subsetVariables)){
      out <- tagList(
        #,
      #checkboxGroupInput(ns("covariates"), "Select Covariates to Display in Heatmap",
      #                   choices=covariateChoices, selected=covariateChoices)
      )
    }
    return(out)
  })

  popTable <- reactive({
    #colorVar <- input$colorVar
    pop <- input$population
    #covariates <- input$covariates
    #key <- key(data)

    #annotOut <- annotation[, .SD, .SDcols = covariates]
    #annotOut <- annotation
    #sort data
    #print(nrow(annotation))

    dataOut <- data[annotation(),
                    on=mapVar][!is.na(percentPop)][order(-percentPop)][Population == pop][,popKey:=fct_reorder(popKey, percentPop, .desc=TRUE)]
    #dataOut$popKey <- factor(dataOut$popKey, levels=unique(dataOut$popKey))

    #print(head(dataOut))
    return(dataOut)

  })


  output$waterfallPlot <- renderPlot({

    #population <- input$population
    #print(population)
    outcomeVar <- input$colorVar
    #print(outcomeVar)
    #print(nrow(popTable()))

    out <- waterfallPlot(popTable(), colorChoice=outcomeVar)
    #print(out)
    out
  })

  output$annotationHeatmap <- renderPlot({
    annotationCols <- colnames(annotation)

    gather_cols <- annotationCols[!annotationCols %in% c(mapVar)]

    annotation() %>%
      gather_(key_col="variable", value_col="value", gather_cols=gather_cols)

  })

  ## TODO: add tooltip for waterfall plots - show provenance of population percentage in plotly
  ## need to change sorting routine to reactive

  output$waterfall <- renderPlotly({
    l <- ggplotly(popHeatmapGG(popTable(),text = FALSE),
                  source="waterfall", tooltip=c("fill", "x", "y", "text"))
    l$x$layout$width <- NULL
    l$x$layout$height <- NULL
    l$width <- NULL
    l$height <- NULL
    l
  })

  output$clickTipG <- renderUI({
    click <- event_data("plotly_click", source="popHeatmap2")

    name_value <- levels(popTable()$name)[click[["x"]]]
    pop_value <- rev(popTable()$Population[outDat()$Population %in% popSubset()])[click[["y"]]]

    print(name_value)
    print(pop_value)

    idVar <- popTable() %>% filter(name == name_value & Population == pop_value) %>%
      pull(idVar)

    #    if(is.null(click$x)){
    #      return(NULL)
    #    }
    print(idVar)

    outClick <- paste0(imageDir, idVar, ".png")
    plotObj[["gating"]] <- outClick
    #print(outClick)
    return(NULL)
  })

  output$hoverTipWF <- renderUI({

    ns <- session$ns

    hover <- input$plotHoverWF

    if(is.null(hover$x)){
      return(NULL)
    }

    #print(hover)
    point <- popTable()[floor(hover$x),]
    outputString <- makeOutputString(point, annotDisplayOptions)

    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(outputString))
    )
  })

}

#' Waterfall graphic expects a sorted table by population percentage
#'
#' @param data - percent population data frame from gatingObj, should be merged with annotation
#' @param population - population to sort on
#' @param colorChoice - categorical variable to color by
#'
#' Given a filtered data set of one population, the function will plot a waterfall plot
#' of those values colored by the factor variable defined in colorChoice
#'
#' @return
#' @export
#'
#' @examples
waterfallPlot <- function(data, colorChoice){

  #TODO:

  levelOrd <- levels(data$name)
  #annot <- annotation[FCSFiles %in% levelOrd]
  #annot$FCSFiles <- factor(annot$FCSFiles, levels=levelOrd)

  #annotation <- annotation[data][,c("popKey",covariateChoices),with=FALSE]
  #print(annotation)

  #dat <- data %>% filter(Population==population) %>% mutate(popKey=fct_reorder(popKey, percentPop))

  #x <- 1:nrow(data)
  ##code from https://www.r-bloggers.com/waterfall-plots-what-and-how/
  plot1 <- ggplot(dat, aes_string(x="popKey", y="percentPop", fill=colorChoice, color=colorChoice)) +
    #scale_fill_discrete(name="Treatmentnarm") +
    scale_color_discrete(guide="none") +
    #labs(list(title = "Waterfall plot for changes in QoL scores", x = NULL, y = "Change from baseline (%) in QoL score")) +
    theme_classic() %+replace%
    theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_text(face="bold",angle=90)) +
    #coord_cartesian(ylim = c(0,100)) +
#    geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4)) +
    geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))


  ##melt annotation

  #annotMelt <- melt.data.table(annotation, id.vars="popKey")
  #levels(annotMelt$FCSFiles) <- levelOrd
  #c <- ggplot(data, aes_string(x=FCSFiles, y=variable))

  plot1


}

heatmapAnnotation <- function(annotation, ids, mapVar){

  annotation %>% gather() %>%
    ggplot(aes(x=mapVar, y=factor(annotationCol))) +
    geom_tile()


}


#waterfallPlot(popTable[Population=="CD13CD33"][order(percentPop)][annotation], )
