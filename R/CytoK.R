#' @title CytoK
#'
#' @description This function applies a kernel-based score
#' test for identifying differentially expressed features in
#' high-throughput experiments, called the the CytoK procedure. This
#' function also defines the CytoK class and constructor.
#'
#' @param object an object which is a \code{matrix} or
#' \code{data.frame} with features (e.g. cluster-marker
#' combinations or genes) on the rows and samples as the
#' columns. Alternatively, a user can provide a
#' \code{SummarizedExperiment} object and the
#' \code{assay(object)} will be used as input for the
#' CytoK procedure.
#' @param group_factor a group level binary categorical 
#' response associated with each sample or column in the 
#' \code{object}. The order of the \code{group_factor} must 
#' match the order of the columns in \code{object}. 
#' @param lowerRho (Optional) lower bound of the kernel parameter.
#' @param upperRho (Optional) upper bound of the kernel parameter.
#' @param gridRho (Optional) number of grid points in the interval
#' [lowerRho, upperRho].
#' @param alpha (Optional) level of significance to control
#' the False Discovery Rate (FDR).
#' Default is 0.05.
#' @param featureVars (Optional) Vector of the columns which identify
#' features. If a `SummarizedExperiment` is used for `data`,
#' row variables will be used.
#'
#' @return A object of the class \code{CytoK} that
#' contains  a data.frame of the CytoK
#' features in the \code{CytoKFeatures} slot, a data.frame of the CytoK
#' features in the \code{CytoKFeaturesOrdered} slot ordered by
#' adjusted p values from low to high, a numeric value of the CytoK
#' differentially expressed features \code{CytoKDEfeatures} slot, 
#' a data.frame or SummarizedExperiment original data objject 
#' in the \code{CytoKData} slot, a numeric value of the level of
#' significance in the \code{CytoKalpha} slot and (optional)
#' a vector of the columns which identify features in the 
#' \code{CytoKfeatureVars} slot.
#' 
#' @details
#' CytoK (Kernel-based score test in biological feature differential
#' analysis) is a nonlinear approach,
#' which identifies differentially expressed features
#' in high-dimensional biological experiments. This approach can be
#' applied across many different high-dimensional biological data
#' including Flow/Mass Cytometry data and other variety of gene
#' expression data. The CytoK procedure employs a kernel-based
#' score test to identify differentially expressed features.
#' This procedure can be easily applied to a variety of 
#' measurement types since it uses a Gaussian distance based kernel.
#'
#' This function computes the feature-wise p values and their
#' corresponding adjusted p values. Additionally,
#' it also computes the feature-wise shrunk effect sizes and
#' their corresponding shrunk effect size sd's. Further, it 
#' calculates the percent of differentially expressed features. 
#
#' See the vignette for more details.
#'
#' @aliases CytoK
#'
#' @docType methods
#' @name CytoK
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment assay
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment rowData
#' @importFrom dplyr select
#' @importFrom utils combn
#' @importFrom magrittr %>%
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
#'
#' @references
#' Liu D, Ghosh D, Lin X. Estimation and testing for the 
#' effect of a genetic pathway on a disease outcome using 
#' logistic kernel machine regression via logistic mixed
#' models. BMC Bioinf. 2008; 9(1):292.
#' 
#' Zhan X, Ghosh D. Incorporating auxiliary information for
#' improved prediction using combination of kernel machines.
#' Stat Methodol. 2015; 22:47–57.
#' 
#' Zhan, X., Patterson, A.D. & Ghosh, D.
#' Kernel approaches for differential expression analysis
#' of mass spectrometry-based metabolomics data. 
#' BMC Bioinformatics 16, 77 (2015). 
#' https://doi.org/10.1186/s12859-015-0506-3
#' 
#' Matthew Stephens, False discovery rates: a new deal,
#' Biostatistics, Volume 18, Issue 2, April 2017, 
#' Pages 275–294, https://doi.org/10.1093/biostatistics/kxw041
#' 
#' @rdname CytoK
#' @export
CytoK <- function(object,group_factor,lowerRho=2,
                  upperRho=12,gridRho=4,alpha = 0.05,
                  featureVars = NULL) {
  originalObject <- object
  if (is.data.frame(object)) {
    if(!is.null(featureVars)) {
      featureColumns <- object %>%
        select(featureVars)
      object <- object %>%
        select(-featureVars) %>%
        as.matrix()
    } else {
      object <- as.matrix(object)
      featureColumns <- NULL
    }
    sampleNames <- colnames(object)
  } else if (is(object, "SummarizedExperiment")) {
    featureColumns <- rowData(object)
    sampleNames <- rownames(colData(object))
    object <- assay(object)
  } else if (is(object, "matrix")) {
    if (!is.null(featureVars)) {
      featureColumns <- as.data.frame(object) %>%
        select(featureVars)
      object <- as.data.frame(object) %>%
        select(-featureVars) %>%
        as.matrix()
    } else {
      featureColumns <- NULL
    }
    originalObject <- as.data.frame(object)
    sampleNames <- colnames(as.data.frame(object))
  } else {
    stop("The class of the object must be a matrix,
                    data.frame or SummarizedExperiment")
  }
  
  if(is.null(group_factor)){  
    stop("Must provide group_factor to specify the group 
            level information associated with each sample or 
                 or column in object.")
  }
  
  if(ncol(object) != length(group_factor)){
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
  
  if (any(is.na(object))) {
    stop("Object must not contains NAs.")
  }
  CytoKutils <- CytoKProc(object,group_factor,
                          lowerRho, upperRho, gridRho)
  shrunkEffectSizeMean<- CytoKutils$shrunkEffectSizeMean
  shrunkEffectSizeSD<- CytoKutils$shrunkEffectSizeSD
  pValue <- CytoKutils$vec_pValue
  pValueAdj <- CytoKutils$AdjPvalue_features
  DExpfeatures <- (.DEfeatures(pValueAdj,
                               alpha) * 100)/dim(object)[1]
  
  results <- new("CytoK")
  if(is.null(featureColumns)) {
    results@CytoKFeatures <-
      data.frame(EffectSize=shrunkEffectSizeMean,
                 EffectSizeSD=shrunkEffectSizeSD, 
                 pvalue = pValue,padj = pValueAdj)
  } else {
    results@CytoKFeatures <-
      data.frame(featureColumns,
                 EffectSize=shrunkEffectSizeMean,
                 EffectSizeSD=shrunkEffectSizeSD, 
                 pvalue = pValue,padj = pValueAdj)
  }
  if(is.null(featureColumns)) {
    df_null_features <- data.frame(EffectSize=shrunkEffectSizeMean,
                                   EffectSizeSD=shrunkEffectSizeSD, 
                                   pvalue = pValue,padj = pValueAdj)
    results@CytoKFeaturesOrdered <-
      data.frame(df_null_features[order(df_null_features$padj,
                                        decreasing = FALSE),,drop=FALSE])
  } else {
    df_features <- data.frame(featureColumns,
                              EffectSize=shrunkEffectSizeMean,
                              EffectSizeSD=shrunkEffectSizeSD, 
                              pvalue = pValue,padj = pValueAdj)
    results@CytoKFeaturesOrdered <-  
      data.frame(df_features[order(df_features$padj,
                                   decreasing = FALSE),,drop=FALSE])
  }
  results@CytoKDEfeatures <- DExpfeatures
  results@CytoKData <- originalObject
  results@CytoKalpha <- alpha
  results@CytoKFeatureVars <- featureVars
  return(results)
}
