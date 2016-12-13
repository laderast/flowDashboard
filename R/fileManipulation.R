#require(flowWorkspace)
#require(dplyr)
#require(reshape2)


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
plotAllPopulations <- function(gateSet, nodeList, pipelineFile = "panel1",
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
    pD <- pData(parameters(gateSet@data[[1]]))
    if("Cell_length" %in% pD$desc){
      defaultChan <- "Cell_length"
    }
    else{defaultChan <- "SSC-A"}

    gNs <- getNodes(gateSet, path="full")
    sNodes <- getNodes(gateSet, path="auto")


    if(!is.null(nodeList)){
      matches <- which(nodeList %in% gNs)
      #print(matches)

      if(length(matches) == 0){
        stop("NodeList doesn't match nodes in GatingSet")
      }
    }


    if(is.null(nodeList)){
      nodeList <- getNodes(gateSet, path="full")
    }

    #print(nodeList)

    for(j in 1:length(nodeList)){
      node <- nodeList[j]
      print(node)
      if(node != "root"){
        outnodes <- strsplit(x = node, split="/")[[1]]

        print(outnodes)
        print(sNodes[j])

        #replace last node with the auto path
        outnodes[length(outnodes)] <- sNodes[j]
        #outnodes <- node
        outnodes <- setdiff(outnodes, c(""))
        outPop <- outnodes[length(outnodes)]
        outPop <- make.names(outPop)
        outPop <- gsub(pattern = "\\.$", replacement = "",outPop)
        #outnodes <- unlist(outnodes)
        fileId <- paste0(imagePath, sampName, "...", outPop, "...", pipelineFile, ".png")
        png(fileId, width=200*length(outnodes), height=200)

        #        try(plotGate(gateSet[[i]], y=outnodes, default.y=defaultChan,checkName=FALSE,
        #                    marker.only=TRUE, raw.scale=FALSE,
        #                   gpar = list(nrow=1, ncol=length(outnodes))))
        outplot <- try(autoplot(gateSet[[i]], outnodes, y="SSC-A" ))

        if(!inherits(outplot, "try-error")){
          outplot <- outplot + scale_x_flowJo_biexp() + scale_y_flowJo_biexp()
          outplot <- ggcyto_arrange(outplot, nrow = 1)
          plot(outplot)
        }
        dev.off()
      }
    }
  }
}


#' Title
#'
#' @param gateSet
#' @param pipelineFile
#'
#' @return
#' @export
#'
#' @examples
getPopulationsAndZscores <- function(gateSet, pipelineFile="panel1"){
  popTable <- getPopStats(gateSet)
  popTable$Population <- make.names(popTable$Population)
  popTable$Parent <- make.names(popTable$Population)

  popTable$Population <- gsub(pattern = "\\.$", replacement = "", popTable$Population)
  popTable$Parent <- gsub(pattern = "\\.$", replacement = "", popTable$Parent)

  popTable <- popTable %>% mutate(idVar = paste0(name,"...",Population,"...",pipelineFile),
                                  percentPop =(Count/ParentCount)*100)
  #popMat <- acast(popTable, name~Population, value.var = "percentPop")
  popTable <- popTable %>%
    group_by(variable) %>%
    mutate(zscore = scale_this(value), uniqueID = paste0(idVar,"-",variable))

  #popScale <- scale(popMat)
  popScaleMelt <- melt(popScale,value.name="zscore")
  popScaleMelt <- popScaleMelt %>% mutate(idVar=paste0(Var1,"...",Var2,"...",pipelineFile)) %>%
    select(idVar, zscore)

  #popTable %>% inner_join(y=popScaleMelt, by=c("idVar"="idVar"))
  popTable <- merge(popTable, popScaleMelt, by="idVar")

  popTable
}

#' Title
#'
#' @param gS
#' @param population
#' @param removeMarkers
#' @param samplePopulation
#'
#' @return
#' @export
#'
#' @examples
returnMeltedDataFromGS <- function(gS, population, removeMarkers = NULL, samplePopulation = NULL){
  require(reshape2)
  amlList <- getData(gS, population)
  #amlList <- amlList[grep("D",sampleNames(amlList))]

  #exprList <- fsApply(amlList, return, use.exprs = TRUE)

  descFrame <- pData(parameters(amlList[[1]]))
  markersToInterrogate <- descFrame[!descFrame$desc %in% removeMarkers,]

  mCAList <- as(amlList, "list")

  exprList <- lapply(mCAList, exprs)

  filteredExprList <- lapply(exprList, function(x){colnames(x) <- descFrame$desc
  x <- x[,colnames(x) %in% markersToInterrogate$desc]
  return(x)
  })

  filteredExprList <- lapply(filteredExprList, function(x){
    #print(class(x))
    #print(head(x))
    if(class(x) == "numeric"){x <- as.matrix(t(x))}
    if(!is.null(samplePopulation) & nrow(x) > samplePopulation){
      sampleInd <- sample(1:nrow(x), samplePopulation)
      x <- x[sampleInd,]
    }
    return(x)
    #print(dim(out))
  })

  names(filteredExprList) <- sampleNames(amlList)

  filteredExprMeltList <- lapply(names(filteredExprList), function(y){ x <- filteredExprList[[y]]
  #rownames(x) <- 1:nrow(x)
  #print(head(x))
  if(nrow(x)==0){
    cell=NULL
    sample=NULL
    x <- data.frame(cell,sample,x)
  }
  else{
    x <- data.frame(cell=1:nrow(x), sample=y, x)
    x <- melt(x, id.vars=c("cell", "sample"))
  }
  return(x)})

  adultExprMelt <- do.call(rbind, filteredExprMeltList)

  adultExprMelt <- adultExprMelt %>% mutate(Population = population)

  return(adultExprMelt)

}
