#' @title Differentially expressed data by cytoKernel
#'
#' @description Select \code{CytoK} object according to the differentially 
#' expressed features identified by cytoKernel. Features are filtered
#' if their adjusted p values are greater than \code{CytoKalpha}.
#'
#' @param object a CytoK object from \code{CytoK}
#' @param by String specifying which adjusted p values
#' of the features to filter by. Default is "features".
#' 
#' @return A list of \code{data.frame}'s or a \code{SummarizedExperiment}. 
#' If a \code{data.frame} was originally input into the
#'  \code{CytoK} function, a list with two elements, \code{DEdata}, 
#' \code{nonDEfeatures}, will be returned. If a
#' \code{SummarizedExperiment} was originally input, output will be a 
#' \code{SummarizedExperiment} with the filtered assay with
#'  one metadata object \code{nonDEfeatures} and four row meta-data
#'  \code{EffectSize}, \code{EffectSizeSD}, \code{pvalue} and 
#'  \code{padj}.
#' 
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKDEData(data_CytoK, by = "features")
#'
#' @export
CytoKDEData <- function(object, by = c( "features")) {
  by <- match.arg(by)
  
  if (!is(object, "CytoK")) {
    stop("object must be class CytoK")
  }
  
  if (is.data.frame(object@CytoKData)) {
    return <- .dfFilter(object, by)
  }
  
  if (is(object@CytoKData, "SummarizedExperiment")) {
    return <- .seFilter(object, by)
  }
  
  return
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
.dfFilter <- function(object, by) {
  if (by == "features") {
    alphaFeatures <- object@CytoKalpha
    featuresToKeep <- object@CytoKFeatures$padj <= alphaFeatures
  } else {
    featuresToKeep <- rep(TRUE, times = nrow(object@CytoKData))
  }
  
  originalSamples <- object@CytoKData %>%
    colnames() 
  featureVars <- object@CytoKFeatureVars
  DEdata <- object@CytoKData[featuresToKeep,]
  nonDEfeatures <- object@CytoKData[!featuresToKeep, ]
  
  if(ncol(DEdata) == length(featureVars) | nrow(DEdata) == 0) { 
    DEdata <- NULL 
  }
  if(nrow(nonDEfeatures) == 0) { nonDEfeatures <- NULL }
  
  return(list("DEdata" = DEdata,
              "nonDEfeatures" = nonDEfeatures))
}

#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom S4Vectors metadata
#' @importFrom S4Vectors metadata<-
#' @importFrom SummarizedExperiment rowData<-
.seFilter <- function(object, by) {
  if (by == "features") {
    alphaFeatures <- object@CytoKalpha
    featuresToKeep <- object@CytoKFeatures$padj <= alphaFeatures
  } else {
    featuresToKeep <- rep(TRUE, times = nrow(object@CytoKData))
  }
  originalSamples <- object@CytoKData %>%
      colnames()
  DEdata <- object@CytoKData[featuresToKeep,]
  nonDEfeatures <- object@CytoKData[!featuresToKeep, ]
  if(ncol(DEdata) == 0 | nrow(DEdata) == 0) { 
    DEdata <- NULL 
  }
  if(nrow(nonDEfeatures) == 0) { nonDEfeatures <- NULL }
  
  return <- object@CytoKData[featuresToKeep,]
  EffectSize<- object@CytoKFeatures$EffectSize[featuresToKeep]
  EffectSizeSD<- object@CytoKFeatures$EffectSizeSD[featuresToKeep]
  Pval<- object@CytoKFeatures$pvalue[featuresToKeep]
  Padj<- object@CytoKFeatures$padj[featuresToKeep]
  metadata(return)$nonDEfeatures <- nonDEfeatures
  # store shrunk effectSize, shrunk effectSizeSD, p values
  #and adj. p values in row meta-data
  rowData(return)$EffectSize <- EffectSize
  rowData(return)$EffectSizeSD <- EffectSizeSD
  rowData(return)$pvalue <- Pval
  rowData(return)$padj <- Padj
  return
}

