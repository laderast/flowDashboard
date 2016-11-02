library(shiny)
#library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)
library(gplots)
library(data.table)
library(dtplyr)

source("../R/QCModules.R")
addResourcePath("data","data")
wd <- paste0(getwd(), "/data/")

load(paste0(wd, "qcData.RData"))
qcData <- data.table(qcMelt)
setkey(qcData, idVar)
qcMarkers <- as.character(unique(qcMelt$variable))

fileAnnotation <- data.table(read.delim(paste0(wd, "FCSFileInfo.txt")))
setkey(fileAnnotation, FCSFiles)
qcData <- qcData[fileAnnotation]

sortColumns <- c("runDate", "numCells")

