#' Title
#'
#' @param id
#' @param label
#' @param markers
#' @param sortConditions
#' @param colorConditions
#' @param annotation
#'
#' @return
#' @export
#'
#' @examples
qcModuleUI <- function(id, label = "qcViolin", markers, sortConditions,
                       colorConditions, annotation) {
  # Create a namespace function using the provided id
  ns <- NS(id)
#  if(!subsetCondition %in% colnames(annotation)){
#    stop("subset condition is not in annotation file")
#  }

#  sortConditions <- sortConditions[sortConditions %in% colnames(annotation)]
#  colorConditions <- colorConditions[colorConditions %in% colnames(annotation)]

#  if(length(sortConditions) == 0 | length(colorConditions) == 0){
#    stop("subset or color conditions are not in annotation file")
#  }

  #subsetChoices = unique(as.character(annotation[[subsetCondition]]))
  #print(subsetChoices)

  tagList(
    uiOutput(ns("qcHeatmapUI")),
    ggvisOutput(ns("qcHeatmap")),
    uiOutput(ns("qcViolinUI")),
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
#' @param idColumn
#'
#' @return
#' @export
#'
#' @examples
qcModuleOutput <- function(input, output, session, data, annotation,
                           idColumn = "patientID", subsetCondition=NULL,
                           subsetChoices=NULL, sortConditions, markers, colorConditions) {

  #require(dplyr)
  # The selected file, if any

  output$qcHeatmapUI <- renderUI({
    ns <- session$ns
    #markers <- unique(as.character(data[annotateSelect()]$variable))

    #print(markers)

    out <- list()

    if(!is.null(subsetChoices)){

       out <- list(out, checkboxGroupInput(ns("subset"), paste0("Select ",
                                                                subsetCondition, " to display"),
                                           choices = subsetChoices,
                                           selected=subsetChoices, inline=TRUE, width='400px')
       )
    }

     #out <- list(out, selectInput(ns("Marker"), "Select Markers", choices = markers,
    #            selected = markers[1]))

     out <- list(out,selectInput(ns("Order"), "Select Condition to Sort on", choices=sortConditions,
                 selected=sortConditions[1]))

     out <- tagList(out)

     out
  })


  annotateSelect <- reactive({
    annotate2 <- annotation
    ord <- input$order

    if(!is.null(subsetChoices)){
    subsetVar <- input$subset

    if(is.null(input$subset)){subsetVar <- subsetChoices[1]}

    #print(subsetVar)
    #annotate2 <- annotation %>% dplyr::filter(patientID %in% subsetVar)
    }
    #print(annotate2)
    annotate2
    #annotate2 <- annotate2 %>% arrange_(ord)
  })

  medData <- reactive({
    ord <- input$Order
    subdata <- data[annotateSelect(), nomatch=0]
    #print(subdata)

    medTable <- summarise(group_by(subdata,variable,idVar),med = median(value)) %>%
      group_by(variable) %>%
      mutate(zscore = scale_this(med), popKey=paste0(idVar, "-", variable)) #%>%
      #arrange_(ord)
    medTable <- data.table(medTable)
    setkey(medTable, popKey)
    #print(medTable)
    #medTable <- medTable[annotateSelect()]

    #  inner_join(y=annotateSelect(), by=c("idVar"="FCSFiles"))

    #medTable <- data.table(medTable)
    #setkey(medTable, idVar)
    #%>%
      #dplyr::filter(patientID %in% subsetVar)
      #dplyr::filter_(interp(~v==patientID, v=as.name(subsetVar)))#%>% arrange_(input$order)
    #print(medTable)

    medTable
  })

  qcHeatmapReact <- reactive({
    #print(medData())
    qcHeatmapPlot(medData(), annotation=annotation)
  })



  #' Title
  #'
  #' @param data
  #' @param annotation
  #'
  #' @return
  #' @export
  #'
  #' @examples
  qcHeatmapPlot <- function(data, annotation)
  {
    #print(head(data))

    #print(data)

    #namesDomX <- unique(data$notation)
    domX <- data$idVar
    #names(domX) <- namesDomX
    domY <- unique(as.character(data$variable))
    print(domY)

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
      layer_rects(height = band(), width = band(), key:=~popKey) %>%
      scale_ordinal('fill',range = pal) %>%
      add_axis("x", properties = axis_props(labels = list(angle = 270)), orient="top",
               title_offset = 120, tick_padding=40, title="Sample/Panel") %>%
      add_axis("y", orient="left", title_offset = 80, title = "Marker") %>%
      add_tooltip(heatmapTooltip,on="hover") %>%
      scale_nominal("y", padding = 0, points = FALSE, domain = domY) %>%
      # scale_nominal("x", padding = 0, points = FALSE, domain = namesDomX) %>%
      scale_nominal("x", padding = 0, points = FALSE) %>%
      layer_text(text:=~signif(med,digits=2), stroke:="darkgrey", align:="left",
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

  ns <- session$ns
  qcHeatmapReact %>% bind_shiny(ns("qcHeatmap"))

  ##Viol code here

  output$qcViolinUI <- renderUI({
    ns <- session$ns

    tagList(
    selectInput(ns("Marker"), "Select Marker for Violin Plots", choices=markers, selected = markers[1]),
    selectInput(ns("Color"), "Select Condition to Color", choices=colorConditions, selected=colorConditions[1])
    )
  })

  violData <- reactive({
    Marker <- input$Marker

    # If no file is selected, don't do anything
    #validate(need(input$Marker, message = FALSE))
    #validate(need(input$Population, message= FALSE))
    dataOut <- data[annotateSelect(), nomatch=0]
    #dataOut <- dataOut %>% dplyr::filter(variable %in% input$Marker) %>% arrange_(input$Order)

    dataOut[variable %in% Marker]
  })

  # Return the reactive that yields the data frame
  output$qcViolinPlot <- renderPlot({
    colors <- input$Color
    marker <- input$Marker

    #print(outData)

    qcViolinOut(violData(), marker, colors)
  })


  qcViolinOut <- function(data, marker, colors){
    plotTitle <- marker

    data$idVar <- factor(data$idVar)

    out <- ggplot(data, aes_string(x="idVar",y="value", fill=colors)) +
      geom_violin()
    #facet_grid(. ~ notation) +
    #ggtitle(plotTitle) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

    transFun <- getOption("scaleTrans")
    if(transFun == "biexp"){
      out <- out + scale_y_continuous(trans=flowTrans)
    }

    return(out)
  }


}





