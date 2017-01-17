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
    uiOutput(ns("waterfallDynamicUI")),
    plotOutput(ns("waterfallPlot"), hover = hoverOpts(ns("plotHover"), delay = 100, delayType = "debounce")),
    uiOutput(ns("hoverTip"))

  )

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
                            populationChoices, colorColumns, covariateChoices=NULL, subsetVariables = NULL){

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
    key <- key(data)

    #annotOut <- annotation[, .SD, .SDcols = covariates]
    annotOut <- annotation
    #sort data
    dataOut <- data[!is.na(percentPop)]
    dataOut <- dataOut[order(-percentPop)]
    #print(dataOut)

    dataOut <- dataOut[Population == pop]
    dataOut$popKey <- factor(dataOut$popKey, levels=unique(dataOut$popKey))

    #print(dataOut)

    #dataOut <- dataOut[eval(call("==", as.name("Population"), pop))]

    #print(dataOut)
    #dataOut <- annotOut[dataOut]
    #print(dataOut)
    #setkeyv(dataOut,key)
    #print(dataOut)

    return(dataOut)

  })


  output$waterfallPlot <- renderPlot({

    #population <- input$population
    #print(population)
    outcomeVar <- input$colorVar
    #print(outcomeVar)
    #print(popTable())

    out <- waterfallGraphic(popTable(), colorChoice=outcomeVar, annotation=annotation)
    #print(out)
    out
  })

  output$hoverTip <- renderUI({

    ns <- session$ns

    hover <- input$plotHover

    if(is.null(hover$x)){
      return(NULL)
    }

    #print(hover)
    point <- popTable()[floor(hover$x),]

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
      p(HTML(paste0(#"<b> Car: </b>", rownames(point), "<br/>",
                    #"<b> mpg: </b>", point$mpg, "<br/>",
                    "<b> Patient: </b>", point$patientID, "<br/>",
                    "<b> Condition: </b>", point$NewCondition)))
    )
  })

  #waterfall graphic expects a sorted table by population percentage

  waterfallGraphic <- function(data, annotation, colorChoice){

    levelOrd <- levels(data$name)
    #annot <- annotation[FCSFiles %in% levelOrd]
    #annot$FCSFiles <- factor(annot$FCSFiles, levels=levelOrd)

    #annotation <- annotation[data][,c("popKey",covariateChoices),with=FALSE]
    #print(annotation)

    #data <- data[annotation]

    #x <- 1:nrow(data)
    ##code from https://www.r-bloggers.com/waterfall-plots-what-and-how/
    plot1 <- ggplot(data, aes_string(x="popKey", y="percentPop", fill=colorChoice, color=colorChoice)) +
      #scale_fill_discrete(name="Treatmentnarm") +
      scale_color_discrete(guide="none") +
      #labs(list(title = "Waterfall plot for changes in QoL scores", x = NULL, y = "Change from baseline (%) in QoL score")) +
      theme_classic() %+replace%
      theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold",angle=90)) +
      #coord_cartesian(ylim = c(0,100)) +
      geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4)) +
      geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))


    ##melt annotation

    #annotMelt <- melt.data.table(annotation, id.vars="popKey")
    #levels(annotMelt$FCSFiles) <- levelOrd
    #c <- ggplot(data, aes_string(x=FCSFiles, y=variable))

    plot1


  }

}


#waterfallGraphic(popTable[Population=="CD13CD33"][order(percentPop)][annotation], )
