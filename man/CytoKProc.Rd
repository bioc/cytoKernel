% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CytoKProc.R
\docType{methods}
\name{CytoKProc}
\alias{CytoKProc}
\title{CytoKProc}
\usage{
CytoKProc(object, group_factor, lowerRho = 2, upperRho = 12, gridRho = 4)
}
\arguments{
\item{object}{an object which is a \code{matrix} or
\code{data.frame} with features (e.g. cluster-marker combinations
 or genes) on the rows and samples (group factors) as the columns.
 Alternatively, a user can provide a \code{SummarizedExperiment}
 object and the \code{assay(object)} will be used as input
for the CytoK procedure.}

\item{group_factor}{a group level binary categorical 
response associated with each sample or column in the 
\code{object}. The order of the \code{group_factor} must 
match the order of the columns in \code{object}.}

\item{lowerRho}{(Optional) lower bound of the kernel parameter.}

\item{upperRho}{(Optional) upper bound of the kernel parameter.}

\item{gridRho}{(Optional) number of grid points in the interval
[lowerRho, upperRho].}
}
\value{
A list of CytoK statistics including
\item{shrunkEffectSizeMean}{the shrunk effective size
posterior mean per feature}
\item{shrunkEffectSizeSD}{the shrunk effective size
posterior sd per feature}
\item{vec_pValue}{the unadjusted p value
per feature}
\item{AdjPvalue_features}{the adjusted p value
per feature using the Benjamini-Hochberg procedure.}
}
\description{
This function is a helper function that
computes the shrunk effective size mean, shrunk effective size
standard deviation (sd), p value and adjusted p value of each 
feature for the function \code{CytoK}.
}
\examples{
data <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
nrow=200, ncol=6), matrix(rnorm(1200,mean=5, sd=1.9),
nrow=200, ncol=6))
data_CytoKProc <- CytoKProc(object=data,
group_factor = rep(c(0,1), each=6), lowerRho=2,
upperRho=12,gridRho=4)

}
