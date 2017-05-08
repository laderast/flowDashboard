


popTooltip <- function(x){
  if(is.null(x)) return(NULL)
  #out <- paste0("<img src='data/images/", x$idVar, ".png'></img>")
  plotObj[["gating"]] <- paste0(imageDir, x$idVar, ".png")
  #print(out)
  #out
  #x$idVar
}

pngGraph <- reactive({
  return(plotObj[["gating"]])
})

output$gating <- renderImage({
  list(src = pngGraph(),
       contentType = "image/png"
  )
},deleteFile=FALSE)


populationHeatmap <- reactive({

  mapVars=c("name"="FCSFiles")

  data <- na.omit(GO$popTable[annotationGO(), on= GO$mapVar])

  if(nrow(annotationGO()) == 0) {data <-
    na.omit(GO$popTable[GO$annotation, on = GO$mapVar])}

  #print(displayNodes)
  domY <- unique(as.character(data[["Population"]]))
  displayNodes <- domY
  #displayNodes <- displayNodes[displayNodes %in% domY]
  #print(displayNodes)

  #noSamples <- length(unique(data$name))
  #noMarkers <- length(unique(PopTable()$Population))
  noMarkers <- length(displayNodes)
  #domX <- Samples()

  domX <- unique(as.character(data[["FCSFiles"]]))
  noSamples <- length(domX)

  #print(noMarkers)
  #print(noSamples)
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
    #mutate()
    #filter(as.character(Population) %in% displayNodes) %>%
    ggvis(x=~idVar,y= ~Population, fill=~factor(round(zscore))) %>%
    #ggvis(x=~name,y= ~Population, fill=~factor(round(Count))) %>%
    layer_rects(height = band(), width = band(), key:=~idVar) %>%
    scale_ordinal('fill',range = pal) %>%
    add_axis("x", properties = axis_props(labels = list(angle = 270)), orient="top",
             title_offset = 120, tick_padding=40, title="Sample") %>%
    add_axis("y", orient="left", title_offset = 100) %>%
    add_tooltip(popTooltip,on="click") %>%
    #add_tooltip(popInfoTooltip, on="hover") %>%
    scale_nominal("y", padding = 0, points = FALSE, domain = displayNodes) %>%
    #scale_nominal("x", padding = 0, points = FALSE, domain = domX) %>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    layer_text(text:=~signif(percentPop,digits=2), stroke:="darkgrey", align:="left",
               baseline:="top", dx := 5, dy:=5) %>%
    set_options(width= max(c(60 * (noSamples), 600)), height= 60 * (noMarkers))
})

populationHeatmap %>%
  bind_shiny("populationHeatmap")
