#' @title S4 Class union
#' @description Class union allowing \code{CytoKData} slot to be
#'  a data.frame or Summarized Experiment
#' @name data.frameORSummarizedExperiment-class
#' @importFrom methods setClassUnion
setClassUnion("data.frameORSummarizedExperiment",
              c("data.frame", "SummarizedExperiment")) 

#' @title S4 Class union
#' @description Class union allowing \code{CytoKFeatureVars}
#'  slot to be a vector or NULL
#' @name vectorORNull-class
#' @importFrom methods setClassUnion
setClassUnion("vectorORNull",
              c("vector", "NULL"))

#' @title the CytoK class
#'
#' @description  Objects of this class store
#' needed information to work with a
#' CytoK object
#'
#' @slot CytoKFeatures CytoK features 
#' @slot CytoKFeaturesOrdered CytoK features ordered
#'  by adjusted p values
#' @slot CytoKDEfeatures Percent of Differentially Expressed
#'  CytoK features
#' @slot CytoKData Original data object passed to \code{CytoK}
#' @slot CytoKalpha Value of \code{alpha} argument passed to \code{CytoK}
#' @slot CytoKFeatureVars Value of \code{featureVars} passed
#'  to \code{CytoK}. NULL if \code{featureVars} is left blank
#'
#' @return \code{CytoKFeatures} returns the data.frame with shrunk
#' effect size, shrunk effect size sd, unadjusted p value and 
#' adjusted p value for each feature,
#' \code{CytoKFeaturesOrdered} returns the data.frame with shrunk
#' effect size, shrunk effect size sd, unadjusted p value and 
#' adjusted p value for each feature ordered by unadjusted p value
#' from low to high,
#' \code{CytoKDEfeatures} returns the percent of differentially
#' expressed features based on alpha (level of significance),
#' \code{CytoKData} returns the original data object,
#' \code{CytoKalpha} returns the specified level of
#' significance. Default is alpha=0.05.
#' \code{CytoKFeatureVars} returns the value of featureVars.
#' Default is NULL.
#'
#' @name CytoK-class
#' @import methods
#' @importFrom dplyr mutate_if
#' @importClassesFrom SummarizedExperiment SummarizedExperiment

#' @exportClass CytoK
#' @aliases CytoK-class
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#'
setClass(Class = "CytoK", 
         slot = list(CytoKFeatures = "data.frame",
                     CytoKFeaturesOrdered = "data.frame",
                     CytoKDEfeatures = "numeric",
                     CytoKData = "data.frameORSummarizedExperiment",
                     CytoKalpha = "numeric",
                     CytoKFeatureVars = "vectorORNull"))

#' @importFrom utils head
#' @importFrom utils tail
setMethod("show", "CytoK", function(object) {
      features <- CytoKFeatures(object) %>%
        mutate_if(is.numeric, round, digits = 3)
      featuresOrdered <- CytoKFeaturesOrdered(object) %>%
        mutate_if(is.numeric, round, digits = 3)
      
      cat("CytoK: Differential expression using
           kernel-based score test\n")
      cat(c("CytoKFeatures (length =",
            nrow(features), "):", "\n"))
      print.data.frame(head(features, n = 3))
      cat("...\n")
      cat(c("CytoKFeaturesOrdered (length =",
            nrow(featuresOrdered),
            "):", "\n"))
      print.data.frame(head(featuresOrdered, n = 3))
      cat("...\n")
    })
