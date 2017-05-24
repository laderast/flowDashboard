library(flowDashboard)
plotObj <- reactiveValues(gating="")

addResourcePath("data","data")
options("scaleTrans"="none")
wd <- paste0(getwd(), "/data/")
imageDir <- paste0(wd, "/gating/image/")
plotObj <- reactiveValues(gating="")

#clinical annotation by file
annotation <- fread("../inst/extdata/FCSFileInfo.txt")
#load in qcData
load("../inst/extdata/qcData.RData")
popTable <- fread("../inst/extdata/cleanupPopulations.txt")
load("../inst/extdata/expressionData.RData")
options("scaleTrans"="none")

QCO <- qcFlowObj$new(annotation, qcData, mapVar=c("idVar"="FCSFiles"))
GO <- gatingObj$new(annotation, popTable, mapVar=c("name"="FCSFiles"),
                    reconcile=TRUE, imageDir=imageDir)
#GO2 <- gatingObj$new(annotation, popTable, mapVar=c("name"="FCSFiles"), reconcile=TRUE)

PEO <- populationExpressionObj$new(annotation, expressionData, mapVar=c("idVar"="FCSFiles"))

sortOptions <- c("BeatAMLID", "Gender", "Source")
subsetOptions <- c("BeatAMLID", "Gender", "Source")

QCO$setSubsetAndSortOptions(sortOptions=sortOptions, subsetOptions=subsetOptions)
GO$setSubsetAndSortOptions(sortOptions=sortOptions, subsetOptions=subsetOptions)
#GO2$setSubsetAndSortOptions(sortOptions=sortOptions, subsetOptions=subsetOptions)
PEO$setSubsetAndSortOptions(sortOptions=sortOptions, subsetOptions=subsetOptions)

QCO$objId <- "QCObj"
GO$objId <- "GObj"
#GO2$objId <- "GObj2"
PEO$objId <- "PEObj"

goObjId2 <- "GO2"
goObjId3 <- "GO3"
