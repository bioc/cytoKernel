#' @title CytoKProc
#'
#' @description This function is a helper function that
#' computes the shrunk effective size mean, shrunk effective size
#' standard deviation (sd), p value and adjusted p value of each 
#' feature for the function \code{CytoK}.
#'
#' @param object an object which is a \code{matrix} or
#' \code{data.frame} with features (e.g. cluster-marker combinations
#'  or genes) on the rows and samples (group factors) as the columns.
#'  Alternatively, a user can provide a \code{SummarizedExperiment}
#'  object and the \code{assay(object)} will be used as input
#' for the CytoK procedure.
#' @param group_factor a group level binary categorical 
#' response associated with each sample or column in the 
#' \code{object}. The order of the \code{group_factor} must 
#' match the order of the columns in \code{object}. 
#' @param lowerRho (Optional) lower bound of the kernel parameter.
#' @param upperRho (Optional) upper bound of the kernel parameter.
#' @param gridRho (Optional) number of grid points in the interval
#' [lowerRho, upperRho].
#' 
#' @return A list of CytoK statistics including
#' \item{shrunkEffectSizeMean}{the shrunk effective size
#' posterior mean per feature}
#' \item{shrunkEffectSizeSD}{the shrunk effective size
#' posterior sd per feature}
#' \item{vec_pValue}{the unadjusted p value
#' per feature}
#' \item{AdjPvalue_features}{the adjusted p value
#' per feature using the Benjamini-Hochberg procedure.}
#'
#' @aliases CytoKProc
#' 
#' @docType methods
#' @import BiocParallel
#' @importFrom data.table rbindlist
#' @importFrom stats p.adjust
#' @import ashr
#'
#' @examples
#' data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
#' nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
#' nrow=200, ncol=6))
#' data_CytoKProc <- CytoKProc(object=data,
#' group_factor = rep(c(0,1), each=6), lowerRho=2,
#' upperRho=12,gridRho=4)
#'
#' @rdname CytoKProc
#' @export
CytoKProc <- function(object,group_factor,lowerRho=2,
                      upperRho=12, gridRho=4) {
  BPPARAM <- MulticoreParam(workers=2)
  KST_matrix <- bplapply(seq_len(dim(object)[1]),
  function(h)
  {.KST(object[h,],group_factor,lowerRho,upperRho,gridRho)},
  BPPARAM= BPPARAM)
  Q_store<- rbindlist(lapply(KST_matrix, "[[", "Q1"))
  Q_store<- matrix(Q_store$Q, nrow = dim(object)[1], byrow = TRUE)
  mu_store<- rbindlist(lapply(KST_matrix, "[[", "mu1"))
  mu_store<- matrix(mu_store$mu, nrow = dim(object)[1], byrow = TRUE)
  sigma_store<- rbindlist(lapply(KST_matrix, "[[", "sigma1"))
  sigma_store<- matrix(sigma_store$sigma, nrow = dim(object)[1], byrow = TRUE)
  postmean_EB_matrix <- bplapply(seq_len(gridRho),
                         function(l)
      {.postmean_EB(Q_store[,l],sigma_store[,l])},
                         BPPARAM= BPPARAM)
  mu_store_EB<- rbindlist(lapply(postmean_EB_matrix,
                                             "[[", "mu_post1"))
  mu_store_EB<- matrix(mu_store_EB$mu_post,
                       nrow = dim(object)[1], byrow = TRUE)
  sigma_store_EB<- rbindlist(lapply(postmean_EB_matrix,
                                 "[[", "sigma_post1"))
  sigma_store_EB<- matrix(sigma_store_EB$sigma_post,
                          nrow = dim(object)[1], byrow = TRUE)
  shrunkEffectSizeMean<- .rowMaxcpp(mu_store_EB)
  shrunkEffectSizeSD<- .rowMaxcpp(sigma_store_EB)
  pValue_features <- bplapply(seq_len(dim(object)[1]),
  function(h)
  {.p_value(Q_store[h,],mu_store[h,], sigma_store[h,])},
   BPPARAM= BPPARAM)
  vec_pValue<- unlist(pValue_features)
  AdjPvalue_features<- stats::p.adjust(vec_pValue,method=c("BH"))
  return(list(shrunkEffectSizeMean= shrunkEffectSizeMean,
              shrunkEffectSizeSD= shrunkEffectSizeSD,
              vec_pValue=vec_pValue, 
              AdjPvalue_features = AdjPvalue_features))
}
