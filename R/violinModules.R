violinUI <- function(id, label = "qcViolin", markers, populations, facets) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    selectInput(ns("populations"), "Select Population", choices=populations, selected = populations[[1]]),
    selectInput(ns("markers"), "Select Marker to Display", choices=markers, selected = markers[1]),
    #selectInput("condition1", "Select Condition", choices= condition1, selected= condition1[1]),
    selectInput(ns("facet1"), "Select Condition to Sort on", choices=facets, selected=facets[2]),
    plotOutput(ns("qcViolinPlot"))

    #selectInput(ns("markerID"), "Select Marker", choices=c("test","test2","test3"))
  )
}


violinOutput <- function(input, output, session, data) {
  require(dplyr)
  # The selected file, if any
  violData <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$markers, message = FALSE))
    validate(need(input$populations, message= FALSE))
    dataOut <- popFrame %>% filter(variable %in% input$markers & Population
                %in% input$populations)
    dataOut
  })

  # Return the reactive that yields the data frame
  output$qcViolinPlot <- renderPlot({
    facets <- input$facet1
    marker <- input$markers

    violinOut(violData(), marker, facets)
  })
}

violinOut <- function(data, marker, facets){
  plotTitle <- marker

  facetForm <- ""

  if(length(facets) ==1){
    facetForm <- paste0(".~", facets[1])
    #print(input$condition1)
    #print(input$facet1)
  }
  if(length(facets ==2)){
    facetForm <- paste0(facets[1], "~", facets[2])
  }
  if(facetForm == ""){
    return()
  }

  out <- ggplot(data, aes(sample,value, col=SampleSource)) +
    geom_violin() + facet_grid(facets=facetForm, scales="free") +
    theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle(plotTitle)

  return(out)
}
