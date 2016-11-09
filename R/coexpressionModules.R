coexpressionPlotUI <- function(id, subjectIDList, markerList){

  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    selectInput(ns("donorIdHeatmap"),"Select Donor", choices= subjectIDList,
                selected=subjectIDList[1]),
    selectInput(ns("sortMarkerHeatmap"), "Select Marker To Sort By",
                choices=markerList, selected=markerList[1]),
    sliderInput(ns("filterValueHeatmap"),min = -1, max=8,label =
                  "Select Filter Cutoff For Marker",value = -1,step = 0.2)

  )

}

coexpressionPlot <- function(input, output, session, data){
  output$miniHistogram <- renderPlot({
    if(is.null(input$donorIdHeatmap)){
      donorID <- "CD27"
    } else{ donorID <- input$donorIdHeatmap}


    if(is.null(input$sortMarkerHeatmap)){
      sortMarker <- as.character(markersHeatmap[1])
    } else {
      sortMarker <- as.character(input$sortMarkerHeatmap)
    }

    hist(data[donor == donorID, get(sortMarker)], main = paste0(sortMarker, " - ", donorID),
         xlab = sortMarker, ylab=NULL,breaks = 30)

  })


  observe({
    lims <- limits[variable ==input$sortMarkerHeatmap]
    #print(lims)
    updateSliderInput(session,inputId = "filterValueHeatmap",
                      min=signif(lims$min,digits=3), max = signif(lims$max, digits=3))
  })

  cells <- reactive({
    if(is.null(input$donorIdHeatmap)){
      donorID <- "CD27"
    } else{ donorID <- input$donorIdHeatmap}


    if(is.null(input$sortMarkerHeatmap)){
      sortMarker <- as.character(markersHeatmap[1])
    } else {
      sortMarker <- as.character(input$sortMarkerHeatmap)
    }

    if(is.null(input$filterValueHeatmap)){
      filtValue <- -1
    }else{
      filtValue <- input$filterValueHeatmap
    }


    cells <- data[donor == donorID & get(sortMarker) > filtValue, cell]
    cells
  })

  output$maitHeatmap <- renderPlot({
    if(is.null(input$donorIdHeatmap)){
      donorID <- donorHeatmap[1]
    } else{ donorID <- input$donorIdHeatmap}

    if(is.null(input$sortMarkerHeatmap)){
      sortMarker <- as.character(markersHeatmap[1])
    } else {
      sortMarker <- as.character(input$sortMarkerHeatmap)
    }

    #reactiveImage()

    #sort marker
    #sortMarker <- "CD103"

    setkeyv(ADT2, sortMarker)
    #markerOrder - put most similar first?
    markerOrder <- markersHeatmap

    #reactivePlot()

    #cells <- ADT2[donor == donorID & as.symbol(sortMarker) < filtValue, cell]

    #cells <- ADT2[donor == donorID & get(sortMarker) > filtValue, cell]

    #cells <- ADT2[donor == donorID & CD103 < filtValue, cell]
    colPanel <- colorpanel(60, low="black", high="white")

    image(t(as.matrix(data[cell %in% cells() & donor==donorID, eval(markersHeatmap), with=FALSE])),col = colPanel, axes=FALSE)
    axis(side=1, labels=markersHeatmap, at=0:(length(markersHeatmap)-1)/(length(markersHeatmap)-1), las=3)
    title(paste0(donorID, " (n=", length(cells()), ")"))

  }, height= function(x){
    plotSize <- floor(length(cells())/8)+200
    if(plotSize < 300){
      plotSize <- 300
    }
    return(plotSize)
  })



}