#require(flowWorkspace)
#require(dplyr)
#require(reshape2)


#' Plot all Populations
#'
#' Given a gatingSet, plot the provenance for each sample and each population.
#'
#'
#' @param gateSet - a gatingSet with attached populations
#' @param imagePath - directory to write population images
#'
#' @return nothing. Side effect is images written to the imagePath.
#' @export
#'
#' @examples
plotAllPopulations <- function(gateSet, nodeList, pipelineFile = "panel1",
                               imagePath= "images/"){
  if(!dir.exists(imagePath)){
    dir.create(imagePath)
  }
  require(flowWorkspace)

  #for each node in the gatingTemplate, plot complete path
  for(i in 1:length(gateSet)){
    #samp <- gateSet[[i]]
    sampName <- sampleNames(gateSet)[i]
    print(sampName)
    pD <- pData(parameters(gateSet@data[[1]]))
    if("Cell_length" %in% pD$desc){
      defaultChan <- "Cell_length"
    }
    else{defaultChan <- "SSC-A"}

    gNs <- getNodes(gateSet, path="full")
    sNodes <- getNodes(gateSet, path="auto")


    if(!is.null(nodeList)){
      matches <- which(nodeList %in% gNs)
      #print(matches)

      if(length(matches) == 0){
        stop("NodeList doesn't match nodes in GatingSet")
      }
    }


    if(is.null(nodeList)){
      nodeList <- getNodes(gateSet, path="full")
    }

    #print(nodeList)

    for(j in 1:length(nodeList)){
      node <- nodeList[j]
      print(node)
      if(node != "root"){
        outnodes <- strsplit(x = node, split="/")[[1]]

        print(outnodes)
        print(sNodes[j])

        #replace last node with the auto path
        outnodes[length(outnodes)] <- sNodes[j]
        #outnodes <- node
        outnodes <- setdiff(outnodes, c(""))
        outPop <- outnodes[length(outnodes)]
        outPop <- make.names(outPop)
        outPop <- gsub(pattern = "\\.$", replacement = "",outPop)
        #outnodes <- unlist(outnodes)
        fileId <- paste0(imagePath, sampName, "...", outPop, "...", pipelineFile, ".png")
        png(fileId, width=200*length(outnodes), height=200)

#        try(plotGate(gateSet[[i]], y=outnodes, default.y=defaultChan,checkName=FALSE,
 #                    marker.only=TRUE, raw.scale=FALSE,
  #                   gpar = list(nrow=1, ncol=length(outnodes))))
        outplot <- try(autoplot(gateSet[[i]], outnodes, y="SSC-A" ))

        if(!inherits(outplot, "try-error")){
          outplot <- outplot + scale_x_flowJo_biexp() + scale_y_flowJo_biexp()
          outplot <- ggcyto_arrange(outplot, nrow = 1)
          plot(outplot)
        }
        dev.off()
      }
    }
  }
}


#' Title
#'
#' @param gateSet
#' @param pipelineFile
#'
#' @return
#' @export
#'
#' @examples
getPopulationsAndZscores <- function(gateSet, pipelineFile="panel1"){
  popTable <- getPopStats(gateSet)
  popTable$Population <- make.names(popTable$Population)
  popTable$Parent <- make.names(popTable$Population)

  popTable$Population <- gsub(pattern = "\\.$", replacement = "", popTable$Population)
  popTable$Parent <- gsub(pattern = "\\.$", replacement = "", popTable$Parent)

  popTable <- popTable %>% mutate(idVar = paste0(name,"...",Population,"...",pipelineFile),
                                  percentPop =(Count/ParentCount)*100)
  #popMat <- acast(popTable, name~Population, value.var = "percentPop")
  popTable <- popTable %>%
    group_by(variable) %>%
    mutate(zscore = scale_this(value), uniqueID = paste0(idVar,"-",variable))

  #popScale <- scale(popMat)
  popScaleMelt <- melt(popScale,value.name="zscore")
  popScaleMelt <- popScaleMelt %>% mutate(idVar=paste0(Var1,"...",Var2,"...",pipelineFile)) %>%
    select(idVar, zscore)

  #popTable %>% inner_join(y=popScaleMelt, by=c("idVar"="idVar"))
  popTable <- merge(popTable, popScaleMelt, by="idVar")

  popTable
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
gatingModuleUI <- function(id, label = "qcViolin", sortConditions, subsetCondition, annotation){


    ns <- NS(id)
    subsetChoices <- unique(as.character(annotation[[subsetCondition]]))

    tagList(
    h4("Filter Samples"),
    # checkboxGroupInput("panelDisplayPop", label="Show Panel",
    #                    choices=c("1", "3"), selected=c("1","3")),
    selectInput(ns("subset"), paste0("Select ", subsetCondition, " to display"), choices=subsetChoices,
                selected = subsetChoices[1]),
    selectInput('colSortPop', "Order Samples", choices= sortConditions,
                selected=sortConditions[1]),
    absolutePanel(id="heatmap", h4("Population Heatmap (Click on box to see provenance)"),
                  ggvisOutput(ns("populationHeatmap")), top=250, left=0),
    # absolutePanel(id="scheme",imageOutput(ns("pipelineHierarchy")), top=250, left=650),
    absolutePanel(id="gating",draggable=TRUE,top=0, left=300,
                   fixed=FALSE,
                   style="opacity: 0.8; background-color: white",
                   height=200,width="auto",
                   h4("Gating Scheme (draggable)"),
                   imageOutput(ns("gating"))),
    br(), br(), br(), br(), br()
    )

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param imageDir - relative directory location of "images/" directory generated
#' by plotAllPopulations
#' @param popTable - populationTable generated by getPopulationsAndZscores
#' @param displayNodes - which gating populations to display in heatmap
#'
#' @return
#' @export
#'
#' @examples
gatingModuleOutput <- function(input, output, session,
                               imageDir, popTable, displayNodes, annotation, plotObj){

  ns <- session$ns

  annotateSelect <- reactive({
    subsetVar <- input$subset
    #print(subsetVar)
    annotate2 <- annotation[patientID %in% subsetVar] #%>% dplyr::filter(patientID %in% subsetVar)
    #annotate2 <- data.table(annotate2)
    #setkey(annotate2, FCSFiles)
    annotate2
  })


  # displayNodes <- reactive({
  #   if(is.null(displayNodes)){
  #     displayNodes <- levels(popTable[[Population]])
  #   }else{
  #     displayNodes <- displayNodes
  #   }
  #   displayNodes
  # })


#   PopTable <- reactive({
# #    if(is.null(input$panelDisplayPop)){
#  #     panelDisplayPop <- c("1","3")
# #    }
#  #   else{
#       #subsetVar <- input$subset
#   #  }
#
#     if(is.null(input$colSortPop)){
#       colSortPop <- "patientID"
#     }
#     else{
#       colSortPop <- input$colSortPop
#     }
#
#
#     out <- popTable[annotateSelect()] #%>%
#       #inner_join(y=SampleTable, by=c("name"="notation")) %>%
#       #dplyr::filter(patientID %in% subsetVar) #%>%
#       #filter(as.character(Population) %in% displayNodes)
#       #arrange_(colSortPop)
#     #mutate(idVar = paste0(name,"_",Population),
#     #       percentPop =(Count/ParentCount)*100)
#     #out$Population <- as.character(out$Population)
#     print(head(out))
#     out
#   })

  popTooltip <- function(x){
    if(is.null(x)) return(NULL)
    #out <- paste0("<img src='data/images/", x$idVar, ".png'></img>")
    plotObj[["gating"]] <- paste0(imageDir, x$idVar, ".png")
    #print(out)
    #out
    x$idVar
  }

  pngGraph <- reactive({
    return(plotObj[["gating"]])
  })

  output$gating <- renderImage({
    list(src = pngGraph(),
         contentType = "image/png"
    )
  },deleteFile=FALSE)


  # populationHeatmap <- reactive({
  #
  #   indata <- na.omit(popTable[annotateSelect()])
  #   #print(indata)
  #   #print(annotateSelect())
  #   populationHeatmapPlot(indata, displayNodes) #%>%
  #     #bind_shiny(ns("populationHeatmap"),session = getDefaultReactiveDomain()[["parent"]])
  #
  #   #if(length(Samples())==0){
  #   #  domX <- domXspare
  #   #}else{
  #   #  domX <- Samples()
  #   #}
  # })

  populationHeatmap <- reactive({

    data <- na.omit(popTable[annotateSelect()])
    #print(displayNodes)
    domY <- unique(as.character(data[["Population"]]))
    displayNodes <- displayNodes[displayNodes %in% domY]
    #print(displayNodes)


    #noSamples <- length(unique(data$notation))
    #print(noSamples)
    #noMarkers <- length(unique(PopTable()$Population))
    noMarkers <- length(displayNodes)
    #domX <- Samples()
    print(noMarkers)

    domX <- unique(as.character(data[["notation"]]))
    noSamples <- length(domX)
    print(noSamples)
    #print(domX)
    #popNotation <- as.character(unique(data$notation))
    #domX <- domX[domX %in% popNotation]
    #print(domX)

    Blue <- colorRampPalette(c("darkblue","lightblue"))
    Orange <- colorRampPalette(c("orange","darkorange3"))

    levs <- sort(unique(round(data$zscore)))

    #print(levs)

    belowAverage <- length(which(levs < 0))
    aboveAverage <- length(which(levs > 0))

    pal <- c(Blue(belowAverage), "#E5E5E5", Orange(aboveAverage))


    #pal <- c(Blue(3), "#E5E5E5", Orange(6))

    data[Population %in% displayNodes] %>%
      #filter(as.character(Population) %in% displayNodes) %>%
      ggvis(x=~notation,y= ~Population, fill=~factor(round(zscore))) %>%
      #ggvis(x=~name,y= ~Population, fill=~factor(round(Count))) %>%
      layer_rects(height = band(), width = band(), key:=~idVar) %>%
      scale_ordinal('fill',range = pal) %>%
      add_axis("x", properties = axis_props(labels = list(angle = 270)), orient="top",
               title_offset = 120, tick_padding=40, title="Sample") %>%
      add_axis("y", orient="left", title_offset = 100) %>%
      add_tooltip(popTooltip,on="click") %>%
      #add_tooltip(popInfoTooltip, on="hover") %>%
      scale_nominal("y", padding = 0, points = FALSE, domain = displayNodes) %>%
      scale_nominal("x", padding = 0, points = FALSE, domain = domX) %>%
      layer_text(text:=~signif(percentPop,digits=2), stroke:="darkgrey", align:="left",
                 baseline:="top", dx := 5, dy:=5) %>%
      set_options(width= 60 * (noSamples), height= 60 * (noMarkers))
  })

  populationHeatmap %>%
    bind_shiny(ns("populationHeatmap"), session=session)
  #bind_shiny("plot-plot", session = getDefaultReactiveDomain()[["parent"]])
}

padMissingValues <- function(popTable){
  populations <- unique(as.character(popTable[["Population"]]))
  samples <- unique(as.character(popTable[["notation"]]))
  expand.grid(populations, samples)

}

