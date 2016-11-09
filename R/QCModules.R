

qcModuleUI <- function(id, label = "qcViolin", markers, conditions, colorVars) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    selectInput(ns("Order"), "Select Condition to Sort on", choices=conditions, selected=conditions[1]),
    ggvisOutput("qcHeatmap"),
    selectInput(ns("Marker"), "Select Marker for Violin Plots", choices=qcMarkers, selected = markers[1]),
    selectInput(ns("Color"), "Select Condition to Color", choices=colorVars, selected=colorVars[1]),
    plotOutput(ns("qcViolinPlot"))
  )
}


qcModuleOutput <- function(input, output, session, data) {
  require(dplyr)
  # The selected file, if any
  violData <- reactive({
    # If no file is selected, don't do anything
    #validate(need(input$Marker, message = FALSE))
    #validate(need(input$Population, message= FALSE))
    dataOut <- data %>% dplyr::filter(variable %in% input$Marker) #%>% arrange_(input$Order)
    dataOut
  })

  # Return the reactive that yields the data frame
  output$qcViolinPlot <- renderPlot({
    colors <- input$Color
    marker <- input$Markers

    qcViolinOut(violData(), marker, colors)
  })


  medData <- reactive({
    order <- input$Order

    medTable <- summarise(group_by(data,variable,idVar),med = median(value)) %>%
      group_by(variable) %>%
      mutate(zscore = scale_this(med), uniqueID = paste0(idVar,"-",variable)) #%>% arrange_(input$order)
    #print(medTable)

    medTable
  })

  qcHeatmapReact <- reactive({
    #print(medData())
    qcHeatmapPlot(medData())
  })

  qcHeatmapReact %>% bind_shiny("qcHeatmap")
}

qcViolinOut <- function(data, marker, colors){
  plotTitle <- marker

  out <- ggplot(data, aes(x=factor(idVar),value, fill=factor(colors))) + geom_violin() +
    #facet_grid(. ~ notation) +
    #ggtitle(plotTitle) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

  return(out)
}

qcHeatmapPlot <- function(data)
  {
  domX <- unique(data$idVar)

  noSamples <- length(unique(data$idVar))
  #print(paste0("number samples: ",noSamples))
  #noMarkers <- length(unique(MedTable()$Var1))
  noMarkers <- length(unique(data$variable))
  #domX <- Samples()
  #medNotation <- as.character(unique(data$idVar))
  #domX <- domX[domX %in% medNotation]
  #print(medNotation)
  #print(domX)
  #print(setdiff(medNotation, domX))
  #print(setdiff(domX, medNotation))
  #print(domX)

  Blue <- colorRampPalette(c("darkblue","lightblue"))
  Orange <- colorRampPalette(c("orange","darkorange3"))
  #pal <- c(Blue(5), "#E5E5E5", Orange(5))


  levs <- sort(unique(round(data$zscore)))

  #print(levs)

  belowAverage <- length(which(levs < 0))
  aboveAverage <- length(which(levs > 0))

  pal <- c(Blue(belowAverage), "#E5E5E5", Orange(aboveAverage))
  #print(pal)
  #print(nrow(data))

  data %>%
    #filter(as.character(notation) %in% domX) %>%
    ggvis(x=~idVar,y= ~variable, fill=~factor(round(zscore))) %>%
    layer_rects(height = band(), width = band(), key:=~uniqueID) %>%
    scale_ordinal('fill',range = pal) %>%
    add_axis("x", properties = axis_props(labels = list(angle = 270)), orient="top",
             title_offset = 90, tick_padding=40, title="Sample/Panel") %>%
    add_axis("y", orient="left", title_offset = 80, title = "Marker") %>%
    #add_tooltip(heatmapTooltip,on="hover") %>%
    scale_nominal("x", padding = 0, points = FALSE, domain = domX) %>%
    layer_text(text:=~signif(med,digits=2), stroke:="black", align:="left",
               baseline:="top", dx := 10, dy:=10) %>%
    set_options(width = 60 * (noSamples), height = 50 * (noMarkers))

}

heatmapTooltip <- function(x,annotation){
  if(is.null(x)){return(NULL)}
  if(is.null(x$idVar)){return(NULL)}

  IDRow <- x$idVar

  # showInfo <- c("BeatAML ID"="patientID", "Panel"="Panel", "Run Date"="runDate",
  #               "PanelSampleID"="notation", "FCS File Name" = "FCSFileName", "Number of Cells"="NumberCells")
  #print(IDRow)
  outRow <- annotation[annotation$idVar == IDRow,]
  #print(outRow)

  #print(IDRow)
  #print("<br>")

  outList <- unlist(lapply(1:length(colnames(annotation)), function(x){
    paste0("<b>",colnames(annotation)[x],":</b> ", outRow[[x]], "<br>")
  }))

  outInfo <- paste(outList, sep= " ")
  outInfo
  #SampleRow <- SampleTable[]
}
