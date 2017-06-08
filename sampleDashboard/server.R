#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  #session <- getDefaultReactiveDomain()

  #QC modules
  annotationQC <- subsetModuleDCO(input, output, dataObj = QCO)
  qcMod <- qcModuleFromQCO(input, output, session, QCO, annotationQC)

  #Gating Modules
  annotationGO <- subsetModuleDCO(input, output, dataObj = GO)
  source("gating.R",local = TRUE)

  ##Expression Modules
  #annotationPEO <- subsetModuleDCO(input, output, dataObj = PEO)
  annotationPEO <- reactive({PEO$annotation})
  violinPEO <- violinOutputFromPEO(input, output, PEO, annotationPEO)

  #violinPEO <- violinOutput(data=)

  ##Dot Plot Modules
  #annotationGO2 <- subsetModuleDCO(input, output, dataObj = GO, objId=goObjId2)
    #dotPlotMod <- dotplotOutputFromGO(input, output,
    #                                  GO, annotation = GO$annotation)

  annotationGO2 <- reactive({GO$annotation})

  dotPlotMod <- callModule(dotPlotOutput, id=goObjId2, data=GO$popTable,
                              annotation=annotationGO2,
                              facetOrderList = GO$subsetOptionList,
                              mapVar = GO$mapVar)

  #annotationGO3 <- subsetModuleDCO(input, output, dataObj = GOadam)
  annotationGO3 <- reactive({
    GO$annotation
  })

  waterfallMod <- waterfallOutputFromGO(GO, annotation=annotationGO3)

  #annotationGO4 <- subsetModuleDCO(input, output, dataObj = GOCD4CD8)
  #waterfallMod2 <- waterfallOutputFromGO(GOCD4CD8, annotation=annotationGO4)


})
