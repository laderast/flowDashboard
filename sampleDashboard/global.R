library(flowDashboard)
plotObj <- reactiveValues(gating="")
wd <- paste0(getwd(), "/data/")
imageDir <- paste0(wd, "/gating/image/")

addResourcePath("data","data")
load("data/objects.RData")
