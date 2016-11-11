violinUI <- function(id, label = "qcViolin", markers, sortConditions,
                     subsetCondition, populations, annotation) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  subsetChoices <- unique(as.character(annotation[[subsetCondition]]))

  tagList(
    selectInput(ns("subset"), paste0("Select ", subsetCondition, " to display"), choices=subsetChoices,
                selected = subsetChoices[1]),
#    selectInput(ns('colSortPop'), "Order Samples", choices= sortConditions,
#                selected=sortConditions[1]),
    selectInput(ns("populations"), "Select Population", choices=populations, selected = populations[[1]]),
    uiOutput(ns("violinMarkerUI")),
    #selectInput(ns("markers"), "Select Marker to Display", choices=markers, selected = markers[1]),
    #selectInput("condition1", "Select Condition", choices= condition1, selected= condition1[1]),
    selectInput(ns("facet1"), "Select Condition to Sort on",
                choices=sortConditions, selected=sortConditions[2]),
    plotOutput(ns("qcViolinPlot"))

    #selectInput(ns("markerID"), "Select Marker", choices=c("test","test2","test3"))
  )
}


violinOutput <- function(input, output, session, data, annotation) {

  output$violinMarkerUI <- renderUI({
    ns <- session$ns
    markers <- unique(as.character(data[annotateSelect()]$variable))

        selectInput(ns("markers"), "Select Markers", choices = markers,
                    selected = markers[1])
  })

  annotateSelect <- reactive({
    subsetVar <- input$subset
    #print(subsetVar)
    annotate2 <- annotation[patientID %in% subsetVar] #%>% dplyr::filter(patientID %in% subsetVar)
    #annotate2 <- data.table(annotate2)
    #setkey(annotate2, FCSFiles)
    annotate2
  })



  # # The selected file, if any
  # violData <- reactive({
  #   # If no file is selected, don't do anything
  #   #validate(need(input$markers, message = FALSE))
  #   #validate(need(input$populations, message= FALSE))
  #   dataOut <- data %>% filter(variable %in% input$markers & Population
  #               %in% input$populations)
  #   dataOut
  # })

  # Return the reactive that yields the data frame
  output$qcViolinPlot <- renderPlot({
    facets <- input$facet1
    marker <- input$markers

    dataOut <- data[annotateSelect()]
    dataOut <- dataOut[variable %in% marker]

    violinOut(dataOut, marker, facets)
  })
}

violinOut <- function(data, marker, facets=NULL){
  plotTitle <- marker

  facetForm <- ""

  if(!is.null(facets)){
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
    }
  print(head(data))

  out <- ggplot(data, aes(sample,value, fill=NewCondition)) +
    geom_violin() + #facet_grid(facets=facetForm, scales="free") +
    scale_y_flowJo_biexp() +
    theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle(plotTitle)

  return(out)
}
