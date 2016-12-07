##waterfall modules for visualizing population percentages
waterfallOutputUI <- function(id, label="waterfall"){
  ns <- NS(id)

  tagList(
    uiOutput(ns("waterfallDynamicUI")),
    plotOutput(ns("waterfallPlot"))

  )

}

waterfallOutput <- function(input, output, session, data, annotation,
                            populationChoices, colorColumns, subsetVariables = NULL){

  output$waterfallDynamicUI <- renderUI({

    ns <- session$ns

    if(is.null(subsetVariables)){
      out <- tagList(
      selectInput(ns("population"), "Select Population To Display", choices=populationChoices,
                selected = populationChoices[1]),
      selectInput(ns("colorVar"), "Select Outcome to Color", choices=colorColumns,
                selected = colorColumns[1]),
      checkboxGroupInput(ns("covariates"), "Select Covariates to Display in Heatmap",
                         choices=covariateChoices, selected=covariateChoices)
      )
    }
    return(out)
  })

  popTable <- reactive({
    colorVar <- input$colorVar
    population <- input$population
    covariates <- input$covariates

    annotOut <- annotation[, .SD, .SDcols = covariates]

    dataOut <- data[Population %in% population,]
    dataOut <- dataOut[annotOut]
    return(dataOut)

  })


  waterfallPlot <- reactive({

    population <- input$population
    outcomeVar <- input$colorVar

    waterfallGraphic(popTable(), population=population, colorChoice=outcomeVar)
  })


  waterfallGraphic <- function(data, population, colorChoice){

    ##code from https://www.r-bloggers.com/waterfall-plots-what-and-how/
    b <- ggplot(data, aes_string(x=patientID, y=population, fill=colorChoice, color=colorChoice)) +
      scale_fill_discrete(name="Treatmentnarm") + scale_color_discrete(guide="none") +
      #labs(list(title = "Waterfall plot for changes in QoL scores", x = NULL, y = "Change from baseline (%) in QoL score")) +
      theme_classic() %+replace%
      theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold",angle=90)) +
      coord_cartesian(ylim = c(-100,100)) + geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4)) +
      geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))

    ##
    #c <- ggplot(data, aes_string(x=patientID, y=ddd))

  }

}
