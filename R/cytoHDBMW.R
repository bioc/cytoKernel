#'The HDCytoData package is an extensible resource containing
#'a set of publicly available high-dimensional flow cytometry
#'and mass cytometry (CyTOF) benchmark datasets, which have been 
#'formatted into SummarizedExperiment and flowSet Bioconductor 
#'object formats. The data objects are hosted on Bioconductor’s 
#'ExperimentHub platform.

#'The objects each contain one or more tables of cell-level 
#'expression values, as well as all required metadata. 
#'Row metadata includes sample IDs, group IDs, patient IDs, 
#'reference cell population or cluster labels (where available), 
#'and labels identifying ‘spiked in’ cells (where available). Column metadata includes channel names, protein marker names, and protein marker classes (cell type, cell state, as well as non protein marker columns).
#'
#' @docType data
#' @format SummarizedExperiment assay object containing
#' 216 metabolites (features) of 8 subjects (samples).
#'
#' @references
#' Weber, M L, Soneson, Charlotte (2019). 
#' “HDCytoData: Collection of high-dimensional cytometry 
#' benchmark datasets in Bioconductor object formats.” 
#' F1000Research, 8(v2), 1459.
#'
#' @keywords datasets
#' @examples
#' data(cytoHDBMW)
#' @usage data(cytoHDBMW)
"cytoHDBMW"
