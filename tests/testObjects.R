library(testthat)
library(data.table)

annotation <- fread("data/FCSFileInfo.txt")
load("data/controlMelt.RData")
qcData <- data.table(controlMelt)
popTable <- fread("data/cleanupPopulations.txt")
expressionData
subsetOptions


peo <- populationExpressionObj$new(annotation=annotation,expressionData=expressionData)
go <- gatingObj$new(annotation=annotation,popTable = popTable)


test_that("testIntegrityQCO", {

  mapVar=c("idVar"="FCSFiles")
  expect_equal(class(qco)[1], "qcFlowObj")
  qco <- qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=mapVar)
  qco2 <- qcFlowObj$new(annotation[1:5,], qcData=qcData, mapVar, reconcile=FALSE)

  expect_warning(checkIntegrity(annotation[1:5,],qcData, mapVar=c("idVar"="FCSFiles")))
  expect_warning(checkIntegrity(annotation,qcData[1:10,], mapVar=c("idVar"="FCSFiles")))
  expect_warning(qco2$checkIntegrityObj(reconcile=FALSE))
  qco2$checkIntegrityObj()

  expect_error()
})

test_that("setSubsetOptions",{



})
