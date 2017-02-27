#' Title
#'
#' @param id
#' @param subjectIDList
#' @param markerList
#'
#' @return
#' @export
#'
#' @examples
coexpressionPlotUI <- function(id, subjectIDList, markerList){

  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    selectInput(ns("donorIdHeatmap"),"Select Donor", choices= subjectIDList,
                selected=subjectIDList[1]),
    selectInput(ns("sortMarkerHeatmap"), "Select Marker To Sort By",
                choices=markerList, selected=markerList[1]),
    sliderInput(ns("filterValueHeatmap"),min = -1, max=8,label =
                  "Select Filter Cutoff For Marker",value = 1,step = 0.2)

  )

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param data
#'
#' @return
#' @export
#'
#' @examples
coexpressionPlot <- function(input, output, session, data, markerList){


  output$miniHistogram <- renderPlot({
    if(is.null(input$donorIdHeatmap)){
      donorID <- data$sample[1]
    } else{ donorID <- input$donorIdHeatmap}


    if(is.null(input$sortMarkerHeatmap)){
      sortMarker <- as.character(markerList[1])
    } else {
      sortMarker <- as.character(input$sortMarkerHeatmap)
    }

    hist(data[sample == donorID, get(sortMarker)], main = paste0(sortMarker, " - ", donorID),
         xlab = sortMarker, ylab=NULL,breaks = 30)

  })

  marker <- reactive({
    if(is.null(input$sortMarkerHeatmap)){ sortMarker <- markerList[1]}
    else{sortMarker <- input$sortMarkerHeatmap}

    sortMarker
  })

  observe({

    limits <- apply(data[,-c(1:2)], 2, range)

    #lims <- limits[variable ==input$sortMarkerHeatmap]

    lims <- limits[,marker()]
    print(lims)
    print(marker())
    updateSliderInput(session,inputId = "filterValueHeatmap",
                      min=signif(lims[1],digits=3), max = signif(lims[2], digits=3))
  })

  cells <- reactive({
    if(is.null(input$donorIdHeatmap)){
      donorID <- data$sample[1]
    } else{ donorID <- input$donorIdHeatmap}

    if(is.null(input$sortMarkerHeatmap)){
      sortMarker <- as.character(markerList[1])
    } else {
      sortMarker <- as.character(input$sortMarkerHeatmap)
    }

    if(is.null(input$filterValueHeatmap)){
      filtValue <- 1
    }else{
      filtValue <- input$filterValueHeatmap
    }

    print(filtValue)

    cells <- data[sample == donorID & get(sortMarker) > filtValue, cell]
    cells
  })

  output$maitHeatmap <- renderPlot({
    if(is.null(input$donorIdHeatmap)){
      donorID <- data$sample[1]
    } else{ donorID <- input$donorIdHeatmap}

    if(is.null(input$sortMarkerHeatmap)){
      sortMarker <- as.character(markerList[1])
    } else {
      sortMarker <- as.character(input$sortMarkerHeatmap)
    }

    #reactiveImage()

    #sort marker
    #sortMarker <- "CD103"
    print(sortMarker)

    setkeyv(data, sortMarker)
    #markerOrder - put most similar first?
    markerOrder <- markerList

    #reactivePlot()

    #cells <- ADT2[donor == donorID & as.symbol(sortMarker) < filtValue, cell]

    #cells <- ADT2[donor == donorID & get(sortMarker) > filtValue, cell]

    #cells <- ADT2[donor == donorID & CD103 < filtValue, cell]
    colPanel <- gplots::colorpanel(60, low="black", high="white")

    image(t(as.matrix(data[cell %in% cells() & sample==donorID, markerList, with=FALSE])),col = colPanel, axes=FALSE)
    axis(side=1, labels=markerList, at=0:(length(markerList)-1)/(length(markerList)-1), las=3)
    title(paste0(donorID, " (n=", length(cells()), ")"))

  }, height= function(x){
    plotSize <- floor(length(cells())/8)+200
    if(plotSize < 300){
      plotSize <- 300
    }
    return(plotSize)
  })



}
