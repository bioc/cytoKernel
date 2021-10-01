#' @title Heatmap of the differentially expressed data 
#' with features on 
#' the rows and samples (group factors) as the columns 
#' from \code{CytoK} and \code{CytoKDEData} function.
#'
#' @description This function plots a heatmap of the expression
#' matrix with features (e.g., cluster-marker combinations) on 
#' the rows and samples (group factors) as the columns.
#'
#' @param object a CytoK object from \code{CytoK}
#' @param group_factor a group level binary categorical 
#' response associated with each sample or column in the 
#' \code{object}. The order of the \code{group_factor} must 
#' match the order of the columns in \code{object}. 
#' @param topK top K differentially expressed features.
#' @param featureVars (Optional) Vector of the columns which identify
#' features. If a `SummarizedExperiment` is used for `data`,
#' row variables will be used.
#'
#' @return A heatmap will be created showing the
#' samples on the columns and features on the rows.
#'
#' @import ComplexHeatmap
#' @importFrom stats median
#' @importFrom circlize colorRamp2
#' @export
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' data("cytoHDBMW")
#' data_CytoK_HD <- CytoK(object=cytoHDBMW,
#' group_factor = rep(c(0, 1), c(4, 4)), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' plotCytoK(data_CytoK_HD,
#' group_factor = rep(c(0, 1), c(4, 4)),topK=10,
#' featureVars = NULL)
plotCytoK <- function(object, group_factor,topK,
                      featureVars = NULL) {
  plotObject <- object@CytoKData
  #featureVec<- CytoKFeatures(object)
  sortFeature<- sort(object@CytoKFeatures$padj,
                     index.return=TRUE)$ix
  if (is.data.frame(plotObject)) {
    if(!is.null(featureVars)) {
      featureColumns <- plotObject %>%
        select(featureVars)
      plotObject <- plotObject %>%
        select(-featureVars) %>%
        as.matrix()
    } else {
      plotObject <- as.matrix(plotObject)
      featureColumns <- NULL
    }
    sampleNames <- colnames(plotObject)
  } else if (is(plotObject, "SummarizedExperiment")) {
    featureColumns <- rowData(plotObject)
    sampleNames <- rownames(colData(plotObject))
    plotObject <- assay(plotObject)
  } else {
    stop("The class of the object must be a data.frame
       or SummarizedExperiment")
          }
  
  if(is.null(group_factor)){  
    stop("Must provide group_factor to specify the group 
        level information associated with each sample or 
             or column in object.")
  }
  
  if(ncol(plotObject) != length(group_factor)){
    stop("Number of columns in object does not match 
             length of group_factor.")
  }
  
  if(is.factor(group_factor) & length(levels(group_factor)) < 2){
    stop("group_factor is a factor and number of levels in 
                  group_factor is less than 2 (levels(group_factor): ",
         levels(group_factor), "). Must provide a factor with 
                  exactly 2 levels to use cytoKernel.")
  }
  
  if(is.factor(group_factor) & length(levels(group_factor)) > 2){
    stop("group_factor is a factor and number of levels in 
                  group_factor is more than 2 (levels(group_factor): ",
         levels(group_factor), "). Must provide a factor with 
                  exactly 2 levels to use cytoKernel.")
  }
  
  if (any(is.na(plotObject))) {
    stop("Object must not contains NAs.")
  }
  
  sortedPlotObject<- plotObject[sortFeature,]
  sortedPlotObjectK<- sortedPlotObject[seq_len(topK),]
  colHeat <- colorRamp2(c(min(sortedPlotObjectK),
                           median(c(sortedPlotObjectK)),
                           max(sortedPlotObjectK)),
                         c("blue", "white", "red"))
  columnAnno <- ComplexHeatmap::HeatmapAnnotation(Group = factor(group_factor))
  ComplexHeatmap::Heatmap(sortedPlotObjectK,
                          cluster_rows = FALSE,
                          cluster_columns = FALSE,
                          top_annotation = columnAnno,
                          col = colHeat)
}

