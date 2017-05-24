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
  }

  #annotationGOOut <- annotationGO()

  #mapVars=c("name"="FCSFiles")
  flowDashboard::popHeatmap(data = GO$popTable, annotation = annotationGO(),
                            mapVar = GO$mapVar) %>%
    add_tooltip(popTp,on="click")

})

populationHeatmap  %>%
  bind_shiny("populationHeatmap")
