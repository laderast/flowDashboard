library(testthat)
library(data.table)
source("R/classes.R")

annotation <- fread("data/FCSFileInfo.txt")
load("data/controlMelt.RData")
qcData <- data.table(controlMelt)
popTable <- fread("data/cleanupPopulations.txt")
#expressionData

#mapVar = c("idVar"="FCSFiles")
#peo <- populationExpressionObj$new(annotation=annotation,expressionData=expressionData)
#mapVar = c("name"="FCSFiles")
#go <- gatingObj$new(annotation=annotation,popTable = popTable)

test_that("testQCO", {

  mapVar=c("idVar"="FCSFiles")

  qco <- qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=mapVar)
  qco2 <- qcFlowObj$new(annotation[1:5,], qcData=qcData, mapVar, reconcile=FALSE)

  expect_equal(class(qco)[1], "qcFlowObj")

  expect_warning(checkIntegrity(annotation[1:5,],qcData, mapVar=c("idVar"="FCSFiles")))
  expect_warning(checkIntegrity(annotation,qcData[1:10,], mapVar=c("idVar"="FCSFiles")))
  expect_warning(qco2$checkIntegrity(reconcile=FALSE))
  expect_warning(qco2$checkIntegrity(reconcile=TRUE))

  expect_error(qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=c("blah"="blah")))

  #expect_error()
})



test_that("setSubsetOptions",{

  mapVar=c("idVar"="FCSFiles")
  qco <- qcFlowObj$new(annotation=annotation, qcData=qcData, mapVar=mapVar)

  subsetOptions = c("patientID", "Gender", "Source")
  expect_error(qco$setSubsetAndSortOptions(subsetOptions, subsetOptions))
  subsetOptions = c("FCSFiles", "Gender", "Source")
  expect_silent(qco$setSubsetAndSortOptions(subsetOptions, subsetOptions))

})
