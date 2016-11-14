dotPlotUI <- function(id, populationList, facetList) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    #checkboxGroupInput(ns("PopulationList"), label="Select Populations To Compare",
    #                   choices=choiceList, selected=choiceList[1], inline=TRUE),


    #uiOutput(ns("dotPlotDynamicUI")),

    selectInput(ns("Population"), label="Select Population to Compare", choices=displayNodes,
                displayNodes[1]),
    selectInput(ns("xFacet"),label="Select X Facet", choices=facetList, selected=facetList[1]),

    #selectInput(ns("yFacet"), "Select Y Facet", choices=facetList, selected=facetList),
    plotOutput(ns("dotPlot"))
    #selectInput(ns("ConditionVariable"), label="Select Ordering Variable",
    #            choices=orderList)

  )
}

dotPlotOutput <- function(input, output, session, data, annotation){

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

    dataOut <- data[annotation]
    dataOut <- data[Population %in% input$Population]

    #%>%
    #filter_(ifelse(is.na(input$xFacet),0,input$xFacet) & ifelse(is.na(input$yFacet),0,input$yFacet))

    #@print(dataOut)

    #%>%
    #arrange_(orderVariable)

    return(dataOut)
  })


  output$dotPlot <- renderPlot({

    #print(head(popTableReact()))

    validate(
      need(input$xFacet, "xFacets not Set")
    )

    xFacet <- input$xFacet
    #yFacet <- input$yFacet
    yFacet <- input$xFacet

    facetFormula <- paste0(yFacet,"~",xFacet)

    if(xFacet == yFacet){
      facetFormula <- paste0(".~",xFacet)
    }

    print(facetFormula)

    plotTitle <- paste("Population Comparison for", popTableReact()$Population[1])

    #out <-
    out  <- ggplot(na.omit(popTableReact()), aes(x=Population, y=percentPop)) +
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
