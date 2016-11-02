dotPlotUI <- function(id, choiceList, facetList) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    checkboxGroupInput(ns("PopulationList"), label="Select Populations To Compare",
                       choices=choiceList, selected=choiceList[5:8], inline=TRUE),
    #selectInput(ns("xFacet"),"Select X Facet", choices=facetList),
    #selectInput(ns("yFacet"), "Select Y Facet", choices=facetList),
    plotOutput(ns("dotPlot"))
    #selectInput(ns("ConditionVariable"), label="Select Ordering Variable",
    #            choices=orderList)

  )
}

dotPlot <- function(input, output, session, data){

  popTableReact <- reactive({
    #validate(input$PopulationList, FALSE)
    #validate(input$ConditionVariable, FALSE)

    #PopList <- input$PopulationList
    #print(input$PopulationList)

    #orderVariable <- input$ConditionVariable

    dataOut <- data %>% filter(Population %in% input$PopulationList) #%>%
    #filter_(ifelse(is.na(input$xFacet),0,input$xFacet) & ifelse(is.na(input$yFacet),0,input$yFacet))

    #@print(dataOut)

    #%>%
    #arrange_(orderVariable)

    return(dataOut)
  })


  output$dotPlot <- renderPlot({

    print(head(popTableReact()))

    xFacet <- input$xFacet
    yFacet <- input$yFacet

    facetFormula <- paste0(yFacet," ~ ",xFacet)

    if(xFacet == yFacet){
      facetFormula <- paste0(". ~ ",xFacet)
    }

    print(facetFormula)

    #out <-
    out  <- ggplot(popTableReact(), aes(x=Population, y=percentPop)) +
      labs(list(x = "Subtype", y = "% Cell Population")) +
      theme(axis.title.x = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
      theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold"))

    out <- out + geom_dotplot(binaxis='y', stackdir='center', method="dotdensity", binwidth=1) +
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "crossbar", width = 0.5, colour="red") #+
    #facet_grid(facets = facetFormula, scales = "free")

    return(out)
  })


}
