timeDriftUI <- function(markers){


}

timeDriftUIFromQCO <- function(QCO, objId){

}

timeDriftOutputFromQCO <- function(QCO, objId){

}

timeDriftOutput <- function(){


}

timeDriftPlot <- function(data, marker, timeCol, grp){

  mat <- data.table::dcast(data)

  mat %>% ggplot(aes_string(x=timeCol, y=marker, group="idVar", col="idVar")) +  geom_point() +
    geom_smooth(method="loess")


  #library(flowDashboard)
  load("~/Code/flowDashboard/inst/extdata/qcoBeads.rda")
  library(tidyverse)
  library(data.table)

  mat <- spread(QCObeads$qcData, variable, value)

  mat2<-mat[!is.na(ICOS)]
  mat3 <- mat[QCObeads$annotation, on=QCObeads$mapVar]

  timeCol <- "TIME"
  marker <- "BEADS"

  timeCol <- "EU151"
  marker <- "LU175"

  testPatients <- unique(mat$idVar)[40:60]

  mat[idVar %in% testPatients][BEADS > 7] %>% ggplot(aes_string(x=timeCol, y=marker, color="idVar")) +
    geom_point() + theme(legend.position = "none") + facet_wrap(facets = "idVar") + geom_smooth(method="loess")


  mat3[BEADS > 7][Panel ==1] %>% ggplot(aes_string(x=timeCol, y=marker, color="idVar")) +
    geom_point() + theme(legend.position = "none") + facet_wrap(facets = "idVar") + geom_smooth(method="lm")

  mat3[BEADS > 7][Panel ==1] %>%

  timeCol <- "TIME"
  marker <- "LU175"


}



