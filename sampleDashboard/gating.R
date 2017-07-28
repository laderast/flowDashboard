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
  outDat <- GO$popTable[annotationGO4(), on=GO$mapVar][Population %in% popSubsets[[popSubset]]][!is.na(percentPop)]
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

  click <- input$clickGate

  if(is.null(click$x)){
    return(NULL)
  }

  point <- findPointsGeomTile(click, data=outDat(), xcol = outDataXColNames(), ycol=outDataYColNames())

  print(ncol(outDatSpread))
  print(point)

  #point <- outDat()[floor(click$x),]
  outClick <- paste0(imageDir, point$idVar, ".png")
  plotObj2[["gating"]] <- outClick
  #print(outClick)
  return(NULL)
})

#need to fix the issue when spreaddata has more columns than plot
findPointsGeomTile <- function(point, data, xcol, ycol){
  numRows <- length(ycol)
  numCols <- length(xcol)

  #xcols <- length(spreaddata)

  print(numCols)
  print(numRows)
  pointXrange <- point$range$right - point$range$left
  xCellSize <- pointXrange / numRows
  #print(xCellSize)
  xCellNum <- ceiling(point$x - 0.5)
  pointYrange <- point$range$bottom - point$range$top
  yCellSize <- pointYrange / numRows
  #print(yCellSize)
  yCellNum <- numRows - ceiling(point$y - 0.5) + 1

  print(xCellNum)
  print(yCellNum)

  ps <- popSubsets[[input$ps]]
  xName <- xcol[xCellNum]
  #print(colnames(spreaddata))
  yName <- ps[yCellNum]

  #print(xName)
  #print(yName)

  outLine <- data[name==xName & Population== yName]
  return(outLine)
}
