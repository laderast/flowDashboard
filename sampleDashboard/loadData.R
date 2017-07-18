library(flowDashboard)
setwd("sampleDashboard")
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

QCO <- qcFlowObj$new(annotation, qcData, mapVar=c("idVar"="FCSFiles"))
GO <- gatingObj$new(annotation, popTable, mapVar=c("name"="FCSFiles"),
                    reconcile=TRUE, imageDir=imageDir)

subsets <- list(QC = c("singlet1", "singlet2", "live", "CD45+"),
                TCells = c("singlet2", "live", "CD45", "TCell", "CD4Helper",
                           "CD8Helper"),
                CD8Cells = c("CD8Helper","CD8TEMRA","CD8EMem","CD8CMem","CD8Naive" ),
                CD4Cells = c("CD4Helper","CD4CMem","CD4TEMRA","CD4EMem", "CD4Naive"),
                DblNeg = c("DblNegTCells","DblNegTEMRA","DblNegEMem","DblNegCMem","DblNegNaive"))

GO$setPopulationSubset(subsets)

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

load("../inst/extdata/proliferationGoObjects.rda")

varClass <- sapply(GOadam$annotation, class)
varClass <- sapply(varClass, function(x){return(x[1])})

annotation <- data.frame(GOadam$annotation)
annotation$RISK[is.na(annotation$RISK)] <- "Unknown"
annotation$Gender[is.na(annotation$Gender)] <- "Unknown"

GOadam$annotation <- data.table(annotation)
GOadam$setSubsetAndSortOptions(GOadam$subsetOptions, GOadam$subsetOptions)
#GOadam$annotation

save.image("data/objects.RData")
