#' @title Accessors for the 'CytoKFeatures' slot of a CytoK object.
#'
#' @description Accessors for the 'CytoKFeatures' slot
#' of a CytoK object.
#'
#' @usage
#' \S4method{CytoKFeatures}{CytoK}(object)
#'
#' @docType methods
#' @name CytoKFeatures
#' @rdname CytoKFeatures
#' @aliases CytoKFeatures CytoKFeatures,CytoK-method
#' @param object an object of class \code{CytoK}.
#'
#' @return The data.frame with shrunk
#' effect size, shrunk effect size sd, unadjusted p value and 
#' adjusted p value for each feature.
#'
#' @export
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKFeatures(data_CytoK)
#'
setMethod(f = "CytoKFeatures", signature(object = "CytoK"),
          function(object) {
            return(object@CytoKFeatures)
          })


#' @title Accessors for the 'CytoKFeaturesOrdered' slot of a CytoK object.
#'
#' @description Accessors for the 'CytoKFeaturesOrdered' slot
#' of a CytoK object.
#'
#' @usage
#' \S4method{CytoKFeaturesOrdered}{CytoK}(object)
#'
#' @docType methods
#' @name CytoKFeaturesOrdered
#' @rdname CytoKFeaturesOrdered
#' @aliases CytoKFeaturesOrdered CytoKFeaturesOrdered,CytoK-method
#' @param object an object of class \code{CytoK}.
#'
#' @return the data.frame with shrunk
#' effect size, shrunk effect size sd, unadjusted p value and 
#' adjusted p value for each feature ordered by unadjusted p value
#' from low to high.
#'
#' @export
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKFeaturesOrdered(data_CytoK)
#'
setMethod(f = "CytoKFeaturesOrdered", signature(object = "CytoK"),
          function(object) {
            return(object@CytoKFeaturesOrdered)
          })


#' @title Accessors for the 'CytoKDEfeatures' slot of a CytoK object.
#' @description Accessors for the 'CytoKDEfeatures'
#' slot of a CytoK object.
#' @usage
#' \S4method{CytoKDEfeatures}{CytoK}(object)
#' @docType methods
#' @name CytoKDEfeatures
#' @rdname CytoKDEfeatures
#' @aliases CytoKDEfeatures CytoKDEfeatures,CytoK-method
#' @param object an object of class \code{CytoK}.
#' @return  The percent of differentially
#' expressed features based on alpha (level of significance).
#'
#' @export
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKDEfeatures(data_CytoK)
#'
setMethod(f = "CytoKDEfeatures", signature(object = "CytoK"),
          function(object) {
            return(object@CytoKDEfeatures)
          })

#' @title Accessors for the 'CytoKData' slot of a CytoK object.
#'
#' @description Accessors for the 'CytoKData'
#' slot of a CytoK object.
#'
#' @usage
#' \S4method{CytoKData}{CytoK}(object)
#'
#' @docType methods
#' @name CytoKData
#' @rdname CytoKData
#' @aliases CytoKData CytoKData,CytoK-method
#' @param object an object of class \code{CytoK}.
#'
#' @return  Original data object passed to \code{CytoK}.
#'
#' @export
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKData(data_CytoK)
#'
setMethod(f = "CytoKData", signature(object = "CytoK"),
          function(object) {
            return(object@CytoKData)
          })

#' @title Accessors for the 'CytoKalpha' slot of a CytoK object.
#'
#' @description Accessors for the 'CytoKalpha'
#' slot of a CytoK object.
#'
#' @usage
#' \S4method{CytoKalpha}{CytoK}(object)
#'
#' @docType methods
#' @name CytoKalpha
#' @rdname CytoKalpha
#' @aliases CytoKalpha CytoKalpha,CytoK-method
#' @param object an object of class \code{CytoK}.
#'
#' @return  Value of \code{CytoKalpha} argument passed 
#' to \code{CytoK}
#'
#' @export
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKalpha(data_CytoK)
#'
setMethod(f = "CytoKalpha", signature(object = "CytoK"),
          function(object) {
            return(object@CytoKalpha)
          })

#' @title Accessors for the 'CytoKFeatureVars' slot of a CytoK object.
#'
#' @description Accessors for the 'CytoKFeatureVars'
#' slot of a CytoK object.
#'
#' @usage
#' \S4method{CytoKFeatureVars}{CytoK}(object)
#'
#' @docType methods
#' @name CytoKFeatureVars
#' @rdname CytoKFeatureVars
#' @aliases CytoKFeatureVars CytoKFeatureVars,CytoK-method
#' @param object an object of class \code{CytoK}.
#'
#' @return Value of \code{featureVars} passed to \code{CytoK}. NULL
#' if \code{featureVars} was left blank
#'
#' @export
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoK <- CytoK(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4,alpha = 0.05,
#' featureVars = NULL)
#' CytoKFeatureVars(data_CytoK)
#'
setMethod(f = "CytoKFeatureVars", signature(object = "CytoK"),
          function(object) {
            return(object@CytoKFeatureVars)
          })