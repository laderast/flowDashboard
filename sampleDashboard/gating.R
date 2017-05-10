pngGraph <- reactive({
  return(plotObj[["gating"]])
})

output$gating <- renderImage({
  list(src = pngGraph(),
       contentType = "image/png"
  )
},deleteFile=FALSE)

populationHeatmap <- reactive({

  popTp <- function(x){
    if(is.null(x)) return(NULL)
    #out <- paste0("<img src='data/images/", x$idVar, ".png'></img>")
    plotObj[["gating"]] <- paste0(GO$imageDir, x$idVar, ".png")
    #print(out)
    #out
    #x$idVar
  }

  #annotationGOOut <- annotationGO()

  #mapVars=c("name"="FCSFiles")
  flowDashboard::popHeatmap(GO$popTable, annotationGO()) %>%
    add_tooltip(popTp,on="click")

})

populationHeatmap  %>%
  bind_shiny("populationHeatmap")
