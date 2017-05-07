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
  #annotationGO <- subsetModuleDCO(input, output, session, dataObj = GO)
  #source("gating.R",local = TRUE)

  ##Expression Modules

  ##Dot Plot Modules
  annotationGO2 <- subsetModuleDCO(input, output, dataObj = GO)
  # dotPlotMod <- dotplotOutputFromGO(input, output,
  #                                  GO, annotationGO2)

   dotPlotMod <- callModule(dotPlotOutput, id=GO$objId, data=GO$popTable,
                            annotation=annotationGO2,
                            facetOrderList = GO$subsetOptionList,
                            mapVar = GO$mapVar)

})
