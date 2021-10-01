#' Generic function that returns the CytoK features
#'
#' Given a CytoK object, this function returns the
#' CytoK features
#' @rdname CytoKFeatures
setGeneric(name = "CytoKFeatures", def = function(object) {
  standardGeneric("CytoKFeatures")
})

#' Generic function that returns the ordered CytoK features
#'
#' Given a CytoK object, this function returns the
#' CytoK features ordered by adjusted p values
#' @rdname CytoKFeaturesOrdered
setGeneric(name = "CytoKFeaturesOrdered", def = function(object) {
  standardGeneric("CytoKFeaturesOrdered")
})

#' Generic function that returns the CytoK Differentially
#' Expressed (DE) features
#' Given a CytoK object, this function returns the
#' CytoK DE features
#' @rdname CytoKDEfeatures
setGeneric(name = "CytoKDEfeatures", def = function(object) {
  standardGeneric("CytoKDEfeatures")
})

#' Generic function that returns the CytoK Data
#'
#' Given a CytoK object, this function returns the
#' CytoK Data
#' @rdname CytoKData
setGeneric(name = "CytoKData", def = function(object) {
  standardGeneric("CytoKData")
})

#' Generic function that returns the CytoK level of
#' significance (alpha) 
#' Given a CytoK object, this function returns the
#' CytoK alpha
#' @rdname CytoKalpha
setGeneric(name = "CytoKalpha", def = function(object) {
  standardGeneric("CytoKalpha")
})

#' Generic function that returns the CytoK Feature Vars
#'
#' Given a CytoK object, this function returns the
#' CytoK Feature Vars
#' @rdname CytoKFeatureVars
setGeneric(name = "CytoKFeatureVars", def = function(object) {
  standardGeneric("CytoKFeatureVars")
})

