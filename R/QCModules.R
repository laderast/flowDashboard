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
    #uiOutput(ns("qcHeatmapUI")),
    box(
    ggvisOutput(ns("qcHeatmap")),
      width=12),
    box(
      uiOutput(ns("qcViolinUI")),
      plotOutput(ns("qcViolinPlot"), width="1000px"),
      width = 12)#,
          #hover= hoverOpts(ns("plotHover"),
          #delay = 500, delayType = "debounce")),
    #uiOutput(ns("hoverTip"))

  )
}

#' Title
#'
#' @param id
#' @param QCO
#'
#' @return
#' @export
#'
#' @examples
qcModuleUIFromQCO <- function(QCO){

    qcModuleUI(id=QCO$objId, label = "qcViolin", QCO$markers, sortConditions=QCO$sortOptions,
                         colorConditions=QCO$sortOptions, annotation)

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param QCO
#'
#' @return
#' @export
#'
#' @examples
qcModuleFromQCO <- function(input, output, session, QCO, annotationReact){

  callModule(qcModuleOutput, id=QCO$objId, data=QCO$qcData, annotation=annotationReact,
              markers=QCO$markers,
              colorConditions=QCO$sortOptions,
              mapVar = QCO$mapVar)

  # qcModuleOutput(input=input, output=output, session=session, data=QCO$qcData, annotation=QCO$annotation,
  #                            subsetCondition=QCO$subsetOptions,
  #                            subsetChoices=QCO$subsetOptionList,
  #                            sortConditions=QCO$sortOptions,
  #                            markers=QCO$markers,
  #                            colorConditions=QCO$sortOptions, mapVar = QCO$mapVar)

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
                           #subsetCondition=NULL,
                           #subsetChoices=NULL, sortConditions,
                           markers,
                           colorConditions, mapVar = c("idVar"="FCSFiles")) {

  medData <- reactive({
    ord <- input$Order
    #subdata <- data[annotateSelect(), on=c("idVar"="FCSFiles"), nomatch=0]

    #print(class(annotation()))


    subdata <- data[annotation(), on=mapVar, nomatch=0]

    if(nrow(annotation())==0){
      subdata <- data
    }

    #levels(subdata$NewConditi) <- subsetChoices
    #print(subdata)

    medTable <- summarise(group_by(subdata,variable,idVar),
                          med = median(value), mean = mean(value),
                          firstQ = quantile(value, probs=c(0.25)),
                          thirdQ = quantile(value, probs=c(0.75)),
                          min=min(value), max=max(value)) %>%

      group_by(variable) %>%
      mutate(zscore = scale_this(med), popKey=paste0(idVar, "-", variable)) #%>%
      #arrange_(ord)
    medTable <- data.table(medTable)
    #setkey(medTable, popKey)
    #print(medTable)
    #medTable <- medTable[annotateSelect()]

    #  inner_join(y=annotateSelect(), by=c("idVar"="FCSFiles"))

    #medTable <- data.table(medTable)
    #setkey(medTable, idVar)
    #%>%
      #dplyr::filter(patientID %in% subsetVar)
      #dplyr::filter_(interp(~v==patientID, v=as.name(subsetVar)))#%>% arrange_(input$order)
    #print(medTable)

    #upd.cols = sapply(medTable, is.factor)
    #medTable[, names(medTable)[upd.cols] := lapply(.SD, factor), .SDcols = upd.cols]
    #print(head(medTable))

    medTable
  })

  qcHeatmapReact <- reactive({
    #print(medData())
    #fakeData <- data[1:10]
    qcHeatmapPlot(medData())
  })




  heatmapTooltip <- function(x,annotation){
    if(is.null(x)){return(NULL)}
    if(is.null(x$idVar)){return(NULL)}

    IDRow <- x$idVar

    outRow <- annotation()[annotation()$idVar == IDRow,]
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

  if(!is.null(qcHeatmapReact)){
  qcHeatmapReact %>% bind_shiny(ns("qcHeatmap"))
  }

  ##Viol code here

  output$qcViolinUI <- renderUI({
    ns <- session$ns

    tagList(
    selectInput(ns("Marker"), "Select Marker for Violin Plots", choices=markers, selected = markers[1]),
    selectInput(ns("Color"), "Select Condition to Color", choices=colorConditions, selected=colorConditions[1])
    )
  })

  numPatients <- reactive({length(annotation[,mapVar])})

  violData <- reactive({

    validate(need(input$Marker, "Marker not specified"))
    Marker <- input$Marker


    dataOut <- data[annotation(), nomatch=0, on=mapVar][variable %in% Marker]

    #grab sort variable (used as key)
    dataOut$idVar <- droplevels(dataOut$idVar)

    dataOut$idVar <- factor(dataOut$idVar,
                            levels=unique(as.character(annotation()[[mapVar]])))

    dataOut
  })

  # Return the reactive that yields the data frame
  output$qcViolinPlot <- renderPlot({
    colors <- input$Color
    marker <- input$Marker
    #numPatients <- unique(annotation[,names(mapVar)])

    qcViolinOut(violData(), marker, colors)
  })


  qcViolinOut <- function(data, marker, colors){
    plotTitle <- marker

    data$idVar <- factor(data$idVar)

    out <- ggplot(data, aes_string(x="idVar",y="value", fill=colors)) +
      geom_violin() + scale_y_continuous(limits = c(-2, NA), breaks=c(1,2.5,5.0,7.5)) +
      theme(axis.text.x=element_text(angle=90, hjust=1)) #+
      #scale_x_discrete(labels = notation)

    transFun <- getOption("scaleTrans")

    if(is.null(transFun))
      {transFun <- "none"}

    if(transFun == "biexp"){
      out <- out + scale_y_continuous(trans=flowTrans)
    }

    return(out)
  }


  # output$hoverTip <- renderUI({
  #
  #   #ns <- session$ns
  #
  #   hover <- input$plotHover
  #
  #   if(is.null(hover$y)){
  #     return(NULL)
  #   }
  #
  #   #print(hover)
  #   point <- violData()[floor(hover$y),]
  #   print(point)
  #
  #   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  #
  #   # create style property fot tooltip
  #   # background color is set so tooltip is a bit transparent
  #   # z-index is set so we are sure are tooltip will be on top
  #   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.70); ",
  #                   "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  #
  #   # actual tooltip created as wellPanel
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0(#"<b> Car: </b>", rownames(point), "<br/>",
  #       #"<b> mpg: </b>", point$mpg, "<br/>",
  #       "<b> Patient: </b>", point$patientID, "<br/>",
  #       "<b> Condition: </b>", point$NewCondition)))
  #   )
  # })


}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
qcHeatmapPlot <- function(data, height=NULL, width=NULL, addText=TRUE,
                          xVar="idVar", yVar="variable")
{
  #print(data)

  #namesDomX <- unique(data$notation)

  #if(is.null(data)){data <- fakeData}
  #if( nrow(data) == 0){data <- fakeData}

  domX <- unique(data[[xVar]])
  #names(domX) <- namesDomX
  domY <- unique(as.character(data[[yVar]]))
  #print(domY)

  noSamples <- length(domX)
  #print(paste0("number samples: ",noSamples))
  #noMarkers <- length(unique(MedTable()$Var1))
  noMarkers <- length(domY)
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

  if(is.null(height)){
    height <- 60 * (noMarkers)
  }

  if(is.null(width)) {
    width <- 60 * (noSamples)
  }

  #print(head(data))

  out <- data %>%
    #filter(as.character(notation) %in% domX) %>%
    #ggvis(x=~idVar,y= ~variable, fill=~factor(round(zscore))) %>%
    ggvis(fill=~factor(round(zscore)), prop("x", as.name(xVar)), prop("y", as.name(yVar))) %>%
    layer_rects(height = band(), width = band(), key:=~popKey) %>%
    scale_ordinal('fill',range = pal) %>%
    add_axis("x", properties = axis_props(labels = list(angle = 270)), orient="top",
             title_offset = 120, tick_padding=40, title="Sample/Panel") %>%
    add_axis("y", orient="left", title_offset = 80, title = "Marker") %>%
    #add_tooltip(heatmapTooltip,on="hover") %>%
    scale_nominal("y", padding = 0, points = FALSE, domain = domY) %>%
    # scale_nominal("x", padding = 0, points = FALSE, domain = namesDomX) %>%
    scale_nominal("x", padding = 0, points = FALSE, domain=domX) %>%
    set_options(width =width, height = height)

    if(addText){
    out <- out %>% layer_text(text:=~signif(med,digits=2), stroke:="darkgrey", align:="left",
               baseline:="top", dx := 10, dy:=10)

    }
  #}
    out
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
buildMedianTable <- function(data){
  medTable <- summarise(group_by(data,variable,idVar),med = median(value),
                        mean = mean(value),
                        firstQ = quantile(value, probs=c(0.25)),
                        thirdQ = quantile(value, probs=c(0.75)),
                        min=min(value), max=max(value)) %>%
    group_by(variable) %>%
    mutate(zscore = scale_this(med), popKey=paste0(idVar, "-", variable))
  medTable
}


