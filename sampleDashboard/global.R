library(flowDashboard)

annotation <- fread("data/FCSFileInfo.txt")
load("data/controlMelt.RData")
qcData <- data.table(controlMelt)
popTable <- fread("data/cleanupPopulations.txt")
load("data/expressionData.RData")

QCO <- qcFlowObj(annotation, qcData, mapVar=c("idVar"="FCSFiles"))
GO <- gatingObj$new(annotation, popTable, mapVar, reconcile=TRUE)
PEO <- populationExpressionObj$new(annotation, expressionData, mapVar)
