#' Title
#'
#' @param id
#' @param label
#' @param markers
#' @param sortConditions
#' @param subsetCondition
#' @param populations
#' @param annotation
#'
#' @return
#' @export
#'
#' @examples
violinUI <- function(id, label = "qcViolin") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  subsetChoices <- unique(as.character(annotation[[subsetCondition]]))

  tagList(
    uiOutput(ns("violinMarkerUI")),
    plotOutput(ns("qcViolinPlot"))
  )
}


#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param data
#' @param annotation
#'
#' @return
#' @export
#'
#' @examples
violinOutput <- function(input, output, session, data, annotation, facetList,
                         aggregateList=NULL, markers=NULL,
                         colorConditions=NULL,
                         mapVar=c("sample"="FCSFiles")) {

  output$violinMarkerUI <- renderUI({
    ns <- session$ns
    tL <- list()

    populations <- unique(as.character(data$Population))

    if(length(populations) > 1){
      tL <- c(tL, selectInput(ns("populations"), "Select Population", choices=populations,
                        selected = populations[[1]]))
    }

    if(!is.null(markers)){
      markers <- sort(unique(as.character(data[annotation(), on=mapVar]$variable)),
                    decreasing = TRUE)
    }
    tL <- c(tL, selectInput(ns("markers"), "Select Markers", choices = markers,
                    selected = markers[1]))

    if(!is.null(colorConditions)){
      tL <- c(tL, selectInput(ns("colorVar"), "Select Condition to Color", choices = colorConditions,
                              selected = colorConditions[1]))
    }

    if(!is.null(aggregateList)){
      tL <- c(tL, selectInput(ns("aggregateVar"), "Select Condition to Aggregate On", choices =
                                aggregateList, selected = aggregateList[1]))
    }

    if(!is.null(facetList)){
      tL <- c(tL, selectInput(ns("facet"), "Select Variable to Facet", choices=facetList,
                              selected=facetList[[1]]))
    }

    return(tagList(tL))

  })

  # annotateSelect <- reactive({
  #   subsetVar <- input$subset
  #   #print(subsetVar)
  #   annotate2 <- annotation[patientID %in% subsetVar] #%>% dplyr::filter(patientID %in% subsetVar)
  #   #annotate2 <- data.table(annotate2)
  #   #setkey(annotate2, FCSFiles)
  #   annotate2
  # })

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
  output$violinPlot <- renderPlot({

    #need to test whether these inputs exist

    marker <- input$markers
    colorVar <- input$colorVar
    aggregateVar <- input$aggregateVar

    if(!is.null(facetList)) {facets <- input$facet}
    else {facets <- NULL}

    dataOut <- data[annotation(), on=mapVar][variable %in% marker]

    #dataOut$idVar <- factor(dataOut$idVar, levels = )

    violinOut(dataOut, facets, colorVar, aggregateVar)
  })
}

violinOut <- function(data, facets=NULL, colorVar=NULL, aggregateVar=NULL){
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
    # if(facetForm == ""){
    #   return()
    #   }
  }

  if(is.null(aggregateVar)){
    x="sample"
  } else{
    x = aggregateVar
  }

  y="value"

  if(is.null(colorVar)){
    fill <- "sample"
  }else{
    fill <- colorVar}

  out <- ggplot(data, aes_string(x=x,y=y, fill=fill)) +
    geom_violin() + theme(axis.text.x=element_text(angle=90, hjust=1)) +
    ggtitle(plotTitle)

  if(facetForm != ""){
    out <- out + facet_grid(facets=facetForm, scales="free")
  }
    #scale_y_flowJo_biexp() +

  transFun <- getOption("scaleTrans")
  if(transFun == "biexp"){
    out <- out + scale_y_continuous(trans=flowTrans)
  }

  return(out)
}


