#' Plot all Populations
#'
#' Given a gatingSet, plot the provenance for each sample and each population.
#'
#'
#' @param gateSet - a gatingSet with attached populations
#' @param imagePath - directory to write population images
#'
#' @return nothing. Side effect is images written to the imagePath.
#' @export
#'
#' @examples
plotAllPopulations <- function(gateSet, pipelineFile = "panel1",
                               imagePath= "images/"){
  if(!dir.exists(imagePath)){
    dir.create(imagePath)
  }
  require(flowWorkspace)

  #for each node in the gatingTemplate, plot complete path
  for(i in 1:length(gateSet)){
    #samp <- gateSet[[i]]
    sampName <- sampleNames(gateSet)[i]
    print(sampName)
    for(node in getNodes(gateSet[[i]], path="full")){
      print(node)
      if(node != "root"){
        outnodes <- strsplit(x = node, split="/")[[1]]
        outnodes <- setdiff(outnodes, c(""))
        outPop <- outnodes[length(outnodes)]
        #outnodes <- unlist(outnodes)
        fileId <- paste0(imagePath, sampName, "_", outPop, "_", pipelineFile, ".png")
        png(fileId, width=150*length(outnodes), height=150)
        try(plotGate(gateSet[[i]], y=outnodes, default.y="Cell_length",checkName=FALSE,
                     marker.only=TRUE,
                     gpar = list(nrow=1, ncol=length(outnodes))))
        dev.off()
      }
    }
  }
}
