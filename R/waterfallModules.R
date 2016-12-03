##waterfall modules for visualizing population percentages
waterfallOutputUI <- function(id, label="waterfall", populationChoices, colorColumns){
  ns <- NS(id)

  tagList(
    uiOutput(ns("waterfallDynamicUI"))
  )

}

waterfallOutput <- function(input, output, session, data, annotation){

  output$waterfallDynamicUI <- renderUI({

    ns <- session$ns

    if(is.null()){
      out <- tagList(
      selectInput(ns("population"), "Select Population To Display", choices=populationChoices,
                selected = populationChoices[1]),
      selectInput(ns("colorVar"), "Select Outcome to Color", choices=colorColumns,
                selected = colorColumns[1])
      )
    }
    return(out)
  })

  popTable <- reactive({

  })

  waterfallGraphic <- function(data, population, colorChoice, annotation){
    ##code from https://www.r-bloggers.com/waterfall-plots-what-and-how/

    b <- ggplot(qol2.wide, aes(x=x, y=bestQoL.PerChb, fill=arm, color=arm)) +
      scale_fill_discrete(name="Treatmentnarm") + scale_color_discrete(guide="none") +
      #labs(list(title = "Waterfall plot for changes in QoL scores", x = NULL, y = "Change from baseline (%) in QoL score")) +
      theme_classic() %+replace%
      theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold",angle=90)) +
      coord_cartesian(ylim = c(-100,100)) + geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4)) +
      geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))

  }

}
