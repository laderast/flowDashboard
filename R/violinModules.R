#' @export
violinUIFromCDO<- function(dataObj){
  violinUI(id=dataObj$objId, label="violin")
}

#' Title
#'
#' @param id
#' @param label
#'
#' @return
#' @export
#'
#' @examples
violinUI <- function(id, label = "qcViolin") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  #subsetChoices <- unique(as.character(annotation[[subsetCondition]]))

  tagList(
    uiOutput(ns("violinMarkerUI")),
    plotOutput(ns("violinPlot"))
  )

}


#' Title
#'
#' @param input
#' @param output
#' @param PEO
#' @param annotation
#'
#' @return
#' @export
#'
#' @examples
violinOutputFromPEO <- function(input, output, PEO, annotation, aggregateList=NULL){
      callModule(violinOutput, id=PEO$objId, data=PEO$expressionData,
                 annotation=annotation,aggregateList=aggregateList,
                 colorConditions=PEO$subsetOptions,
                 markers=as.character(PEO$markers),
                 mapVar = PEO$mapVar)

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
violinOutput <- function(input, output, session, data, annotation, facetList=NULL,
                         aggregateList=NULL, markers=NULL,
                         colorConditions=NULL,
                         mapVar=c("idVar"="FCSFiles")) {

  output$violinMarkerUI <- renderUI({
    ns <- session$ns
    tL <- list()

    populations <- unique(as.character(data$Population))

    tL <- list(tL,
               radioButtons(ns("plotStyle"), "Select Plot Style",
                            choices=c("violin", "boxplot"), selected = "violin"))

    if(length(populations) > 1){
      tL <- list(tL, selectInput(ns("populations"), "Select Population", choices=populations,
                        selected = populations[[1]]))
    }

    if(!is.null(markers)){
      markers <- sort(unique(as.character(data[annotation(), on=mapVar]$variable)),
                    decreasing = TRUE)
    }

    tL <- list(tL, selectInput(ns("markers"), "Select Markers", choices = markers,
                    selected = markers[1]))

    if(!is.null(colorConditions)){
      tL <- list(tL, selectInput(ns("colorVar"), "Select Condition to Color", choices = colorConditions,
                              selected = colorConditions[1]))
    }

    if(!is.null(aggregateList)){
      tL <- list(tL, selectInput(ns("aggregateVar"), "Select Condition to Aggregate On", choices =
                                aggregateList, selected = aggregateList[1]))
    }

    if(!is.null(facetList)){
      tL <- list(tL, selectInput(ns("facet"), "Select Variable to Facet", choices=facetList,
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

  violData <- reactive({

    marker <- input$markers
    #colorVar <- input$colorVar
    population <- input$populations

    dataOut <- data[annotation(), on=mapVar][variable %in% marker][Population == population]
    head(dataOut)
    dataOut
  })



  # Return the reactive that yields the data frame
  output$violinPlot <- renderPlot({

    #need to test whether these inputs exist
    # validate(
    #   need(input$populations, "need a population"),
    #   need(input$markers, "need marker input"),
    #   need(input$colorVar, "need color variable")
    # )

    marker <- input$markers
    colorVar <- input$colorVar
    aggregateVar <- input$aggregateVar
    population <- input$populations

    if(!is.null(facetList)) {facets <- input$facet}
    else {facets <- NULL}

    #dataOut <- data[annotation(), on=mapVar][variable %in% marker]

    #head(dataOut)

    #dataOut$idVar <- factor(dataOut$idVar, levels = )

    violinPlot(violData(), facets, colorVar, aggregateVar,
              marker=marker, population=population)
  })
}

#' Title
#'
#' @param data
#' @param facets
#' @param colorVar
#' @param aggregateVar
#' @param population
#' @param marker
#'
#' @return
#' @export
#'
#' @examples
violinPlot <- function(data, facets=NULL, colorVar=NULL, aggregateVar=NULL,
                      population, marker, geom="violin"){

  #marker <- unique(data$variable)[1]
  plotTitle <- marker

  if(!is.null(population)){
    plotTitle <- paste0(plotTitle, " (", population, ")")
  }

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
    x <- "idVar"
  } else{
    x <- aggregateVar
  }

  y="value"

  if(is.null(colorVar)){
    fill <- "idVar"
  }else{
    fill <- colorVar}

  out <- ggplot(data, aes_string(x=x,y=y, fill=fill)) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    ggtitle(plotTitle)

    if(geom=="violin"){
      out  <- out + geom_violin()
    }
    if(geom=="boxplot"){

      out <- out + geom_boxplot()
    }

  if(facetForm != ""){
    #print(facetForm)
    out <- out + facet_grid(facets=facetForm, scales="free")
  }
    #scale_y_flowJo_biexp() +

  transFun <- getOption("scaleTrans")


  if(!is.null(transFun)){

    if(transFun == "biexp"){
    out <- out + scale_y_continuous(trans=flowTrans)
    }
  }

  return(out)
}


