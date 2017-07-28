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
  outDat <- GO$popTable[annotationGO4(), on=GO$mapVar][Population %in% popSubsets[[popSubset]]]
  outDat
})

outDatSpread <- reactive({
  outDatSpread <- data.table::dcast(outDat(), Population~name, value.var="percentPop", fill = NA)
  head(outDatSpread)
  return(outDatSpread)
})

output$test <- renderPlot({
  popHeatmapGG(outDat())
})

output$clickTip <- renderUI({
  click <- input$clickGate

  if(is.null(click$x)){
    return(NULL)
  }
  #print(hover)
  #print(click$x)
  #print(click)

  #This doesn't work - need to write new scaling function for geom_tile()
  #point <- nearPoints(outDatSpread(), click, threshold = 20, maxpoints = 1, xvar="name", yvar="Population")
  #print(point)
  click$x <- click$x - 0.5
  click$y <- click$y - 0.5

  point <- findPointsGeomTile(click, data=outDat(), spreaddata = outDatSpread())


  print(point)

  #point <- outDat()[floor(click$x),]
  outClick <- paste0(imageDir, point$idVar, ".png")
  plotObj2[["gating"]] <- outClick
  #print(outClick)
  return(NULL)
})

findPointsGeomTile <- function(point, data, spreaddata){
  numRows <- nrow(spreaddata)
  numCols <- ncol(spreaddata)
  pointXrange <- point$range$right - point$range$left
  xCellSize <- pointXrange / numRows
  #print(xCellSize)
  xCellNum <- ceiling(point$x) + 1
  pointYrange <- point$range$bottom - point$range$top
  yCellSize <- pointYrange / numRows
  #print(yCellSize)
  yCellNum <- numRows - ceiling(point$y) + 1

  print(xCellNum)
  print(yCellNum)

  ps <- popSubsets[[input$ps]]
  xName <- colnames(spreaddata)[xCellNum]
  yName <- ps[yCellNum]

  print(xName)
  print(yName)

  outLine <- data[name==xName & Population== yName]
  return(outLine)
}
