pngGraph <- reactive({
  return(plotObj[["gating"]])
})

output$gating <- renderImage({
  list(src = pngGraph(),
       contentType = "image/png"
  )
},deleteFile=FALSE)

populationHeatmap <- reactive({

  #need namespace reference here

  popTp <- function(x){
    if(is.null(x)) return(NULL)
    plotObj[["gating"]] <- paste0(GO$imageDir, x$idVar, ".png")
    return(NULL)
  }

  #annotationGOOut <- annotationGO()
  #annotationGOOut <- GO$annotation
  #print(head(annotationGOOut))

  #mapVars=c("name"="FCSFiles")
  flowDashboard::popHeatmap(data = GO$popTable, annotation = annotationGO(),
                            mapVar = GO$mapVar) %>%
    add_tooltip(popTp,on="click")

})

populationHeatmap  %>%
  bind_shiny("populationHeatmap")


## New GGPLot code


pngGraph2 <- reactive({
  #print(plotObj2[["gating"]])
  return(plotObj2[["gating"]])
})

output$gating2 <- renderImage({
  list(src = pngGraph2(),
       contentType = "image/png"
  )
},deleteFile=FALSE)

outDat <- reactive({
  popSubset <- input$ps
  ##need to add sort by levels
  ##popsubset is affecting the xcols here
  outDat <- GO$popTable[annotationGO4(), on=GO$mapVar][Population %in% popSubsets[[popSubset]]]#[!is.na(percentPop)]
  outDat
})

outDataXColNames <- reactive({
  #outDatSpread <- data.table::dcast(outDat(), Population~factor(name), value.var="percentPop", fill = NA)
  unique(outDat()$name)
})

outDataYColNames <- reactive({
  outDatY <- unique(outDat()$Population)
})

output$test <- renderPlot({
  popHeatmapGG(outDat())
})

output$clickTip <- renderUI({
  click <- input$clickGate

  if(is.null(click$x)){
    return(NULL)
  }

  click$x <- click$x
  click$y <- click$y

  point <- findPointsGeomTile(click, data=outDat(), xcol = outDataXColNames(), ycol=outDataYColNames())

  print(point)

  #point <- outDat()[floor(click$x),]
  outClick <- paste0(imageDir, point$idVar, ".png")
  plotObj2[["gating"]] <- outClick
  #print(outClick)
  return(NULL)
})

##need to add hovertips
output$hoverTip <- renderUI({

  hover <- input$hoverGate

  if(is.null(hover$x)){
    return(NULL)
  }

  point <- findPointsGeomTile(hover, data=outDat(), xcol = outDataXColNames(), ycol=outDataYColNames())

  #print(ncol(outDatSpread))
  print(point)

  #point <- outDat()[floor(click$x),]
  #outClick <- paste0(imageDir, point$idVar, ".png")
  #plotObj2[["gating"]] <- outClick
  #print(outClick)

  outputString <- flowDashboard:::makeOutputString(point, annotCols)

  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")

  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(outputString))
  )

  #return(NULL)
})

