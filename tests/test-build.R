library(testthat)
library(flowCore)
library(flowWorkspace)
library(flowWorkspaceData)
library(ggplot2)
source("R/commonFunctions.R")
source("R/gatingModules.R")
source("R/violinModules.R")
options("scaleTrans"=FALSE)


testDir <- system.file("extdata",package="flowWorkspaceData")
fcsFiles <- list.files(testDir, pattern = "a2004",full.names = TRUE)

fs <- read.flowSet(fcsFiles)

#cMs <- c("FSC-A", "FSC-H","Am Cyan-H", "Am Cyan-A")

#buildSampledDataList Tests

test_that("buildSampledDataList Input", {
  cMs <- c("FSC-A", "FSC-H")
  fsSample <- buildSampledDataList(fs, cMs, controlSize = 100)

  expect_equal(nrow(fsSample[[1]]),100)
  expect_equal(ncol(fsSample[[1]]),2)
  })

test_that("buildSampleDataList wrong columns or rows",{
  cMs <- c("Nothing")
  fsSample <- buildSampledDataList(fs, cMs, controlSize = 100)
  expect_null(fsSample)
})

test_that("plotAllPopulations",{

  testGS <- load_gs(paste0(testDir,"/gs_bcell_auto"))
  nodeList <- getNodes(testGS, path="full")

  tmpDir <- tempdir()

  plotAllPopulations(testGS[1], nodeList=nodeList[1:3], pipelineFile = "test",
                     imagePath = tmpDir)

  plotAllPopulations(testGS[1], nodeList = NULL, pipelineFile = "test",
                     imagePath = tmpDir)

  expect_error(plotAllPopulations(testGS[1], nodeList = c("Blah", "nonexistent"), pipelineFile="test",
                     imagePath = tmpDir), "NodeList doesn't match nodes in GatingSet")

})


test_that("returnMeltedData tests", {
  # wsfile<-list.files(testDir,pattern="A2004Analysis.xml",full=TRUE)
  # ws <- openWorkspace(wsfile);
  # gs<-try(parseWorkspace(ws,path=testDir,name=1));
  #
  # popTable <- getPopulationsAndZscores(gs, pipelineFile = "A2004")
  #
  # nodeList <- getNodes(gs[[1]],path="full")
  # plotAllPopulations(gs, nodeList=nodeList, pipelineFile = "A2004")

})

test_that("violinModules tests", {

  fsSample <- buildSampledDataList(fs, controlSize = 100)
  meltData <- returnMeltedData(fsSample)
  meltData <- meltData[variable == "CD123"]
  expect_silent(violinOut(meltData))

})

# gs <- load_gs(paste0(testDir, "/gs_bcell_auto"))
# fs <- gs@data
#
# FCSFileInfo <- buildFileManifest(fs)
# testFS <- buildSampledDataList(fs,controlSize = 1000)
# qcMelt <- returnMeltedData(testFS, selectMarkers = c("CD3", "CD19", "IgD"),returnCellNum = FALSE)
#
# save(qcMelt, file = paste0(outputDir, "/qcData.RData"))
# popTable <- getPopulationsAndZscores(gs, pipelineFile = "A2004")
#
# nodeList <- getNodes(gs[[1]],path="full")
# plotAllPopulations(gs, nodeList=nodeList, pipelineFile = "A2004")
