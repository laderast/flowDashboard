library(testthat)
library(data.table)
library(flowWorkspace)
source("R/classes.R")
source("R/commonFunctions.R")

annotation <- fread("inst/extdata/FCSFileInfo.txt")
load("inst/extdata/qcData.RData")
qcData <- data.table(qcData)
popTable <- fread("inst/extdata/cleanupPopulations.txt")
load("inst/extdata/expressionData.RData")
gs <- load_gs("inst/extdata/gvHDgs/")


#mapVar = c("idVar"="FCSFiles")
#peo <- populationExpressionObj$new(annotation=annotation,expressionData=expressionData)
#mapVar = c("name"="FCSFiles")
#go <- gatingObj$new(annotation=annotation,popTable = popTable)

test_that("testCheckIntegrity", {

  expect_warning(checkIntegrity(annotation, qcData,
                                mapVar=c("idVar"="FCSFiles"), reconcile = TRUE))
  expect_warning(checkIntegrity(annotation, popTable,
                                mapVar=c("name"="FCSFiles"), reconcile=TRUE))

  expect_warning(checkIntegrity(annotation, expressionData, mapVar = c("idVar"="FCSFiles"),
                                reconcile = TRUE))

  test <- checkIntegrity(annotation, popTable,
                 mapVar=c("name"="FCSFiles"), reconcile=TRUE)
})

test_that("testQCO", {

  mapVar=c("idVar"="FCSFiles")

  qco <- qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=mapVar, reconcile=TRUE)
  qco2 <- qcFlowObj$new(annotation[1:5,], qcData=qcData, mapVar, reconcile=FALSE)

  expect_equal(class(qco)[1], "qcFlowObj")

  expect_warning(checkIntegrity(annotation[1:5,],qcData,
                                mapVar=c("idVar"="FCSFiles")))
  expect_error(checkIntegrity(annotation,qcData[1:10,],
                                mapVar=c("idVar"="FCSFiles")))
  expect_warning(qco2$checkIntegrity(reconcile=FALSE))
  expect_warning(qco2$checkIntegrity(reconcile=TRUE))

  expect_error(qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=c("blah"="blah")))

  expect_error(qco$setMarkers(markers=c("Not","In","Data")))
  expect_silent(qco$setMarkers(markers=c("CD4", "CD8")))

  expect_silent(QCOFromGatingSet(gs, samplePop = 100))

  #expect_error()
})


test_that("testGO",{

  mapVar <- c("name"="FCSFiles")
  go <- gatingObj$new(annotation, popTable, mapVar, reconcile=TRUE)

  expect_warning(gatingObj$new(annotation, popTable, mapVar,
                               imageDir= "blahDir",reconcile=TRUE))

  expect_silent(gatingObj$new(go$annotation, go$popTable, mapVar,
                               imageDir= ".",reconcile=FALSE))

  subsetOptions = c("patientID", "Gender", "Source")
  expect_error(go$setSubsetAndSortOptions(subsetOptions, subsetOptions))
  subsetOptions = c("fileName"="FCSFiles", "Gender", "Source")
  expect_silent(go$setSubsetAndSortOptions(subsetOptions, subsetOptions))

  #expect_silent(GOFromGatingSet(gs))

})

test_that("testPEO",{
  mapVar <- c("idVar"="FCSFiles")
  PEO <- populationExpressionObj$new(annotation, expressionData, mapVar)

  expect_warning(populationExpressionObj$new(annotation, expressionData, mapVar, reconcile=FALSE))
  expect_silent(populationExpressionObj$new(PEO$annotation, PEO$expressionData, mapVar))

  subsetOptions = c("patientID", "Gender", "Source")
  expect_error(PEO$setSubsetAndSortOptions(subsetOptions, subsetOptions))
  subsetOptions = c("fileName"="FCSFiles", "Gender", "Source")
  expect_silent(PEO$setSubsetAndSortOptions(subsetOptions, subsetOptions))

  expect_silent(PEO$setMarkers(markers=c("CD14", "OX40","CD3")))
  expect_error(PEO$setMarkers(markers=c("Not", "A","Marker")))

  pD <- pData(gs@data)

  testNodes <- getNodes(gs)[-1]

  PEOFromGatingSet(gs, populations = testNodes)

  expect_silent(PEOFromGatingSet(gs, samplePop = 100))
  expect_silent(PEOFromGatingSet(gs, annotation = pD, mapVar=c(idVar="name")))

})


test_that("setSubsetOptions",{

  mapVar=c("idVar"="FCSFiles")
  qco <- qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=mapVar)

  subsetOptions = c("Blah", "Gender", "Source")
  expect_error(qco$setSubsetAndSortOptions(subsetOptions, subsetOptions))
  subsetOptions = c("FCSFiles", "Gender", "Source")
  expect_silent(qco$setSubsetAndSortOptions(subsetOptions, subsetOptions))

})

test_that("waterfallTests",{
  mapVar <- c("name"="FCSFiles")
  pop = "CD4RA"

  GO <- gatingObj$new(annotation, popTable, mapVar, reconcile=TRUE)
  dat <- GO$popTable[GO$annotation, on=GO$mapVar, nomatch=0]
  dat <- dat[Population==pop][order(percentPop, decreasing = TRUE)][,popKey:=fct_reorder(popKey, percentPop, .desc=TRUE)]
  #dat <- dat %>% mutate(popKey=fct_reorder(popKey, percentPop))
  pl <- flowDashboard::waterfallPlot(dat, colorChoice = "Gender")
  #testthat::expect_type(pl, "ggplot")

})

test_that("dotPlotTests",{

  #dat <- GO$popTable[GO$annotation, on=GO$mapVar, nomatch=0][Population == "CD45+"]
  #flowDashboard::dotPlot(dat,xFacet = "Gender",facetOrderList = GO$sortOptionList)

})

test_that("gatingPlotTests",{

})


test_that("violinOutTests", {

  mapVar <- c("idVar"="FCSFiles")
  PEO <- populationExpressionObj$new(annotation, expressionData, mapVar)

  violinPlot(dataOut, facets, colorVar, aggregateVar,
             marker=marker, population=population)

  flowDashboard::violinPlot(PEO$expressionData[PEO$annotation, on=PEO$mapVar],population="live",
            marker="CD4", colorVar = "Gender")

  violinPlot(dataOut, facets, colorVar, aggregateVar,
             marker=marker, population=population)
})

test_that("subsetTests", {})

test_that("gatingSetTests", {
  expect_is(QCOFromGatingSet(gs), "qcFlowObj")
  expect_error(QCOFromGatingSet(gs, annotation = pData(gs@data@phenoData)))
  QCO <- QCOFromGatingSet(gs, annotation = pData(gs@data@phenoData), mapVar=c("idVar"="name"))
  expect_is(QCO, "qcFlowObj")
  QCO <- qcFlowObjFromGatingSet(gs, samplePop = 100)
  expect_is(QCO, "qcFlowObj")

  objId <- "GO"
  GO <- gatingObjFromGatingSet(gs[1:3])
  expect_null(GO$objId)
  expect_null(GO$imageDir)
  imageDir = tempdir()
  GO <- GOFromGatingSet(gs[1:3], objId = objId)
  GO <- GOFromGatingSet(gs[1:2], objId = objId, makeGraphs=TRUE, imageDir = imageDir)
  expect_equal(GO$imageDir, imageDir)
  expect_error(GOFromGatingSet(gs[1:3], makeGraphs = TRUE, imageDir = NULL))

})
