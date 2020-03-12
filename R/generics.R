#' Build struct report
#'
#' Builds a struct report using the input data and writes the output to the specified location.
#' @param R A struct_report object
#' @param D A DatasetExperiment object
#' @param outfile A path/filename to save the ouput to
#' @param ... additional report specific inputs
#' @return A rendered markdown file
#' @export
setGeneric("build_report",function(R,M,D,outfile,...)standardGeneric("build_report"))

#' Is model valid for report
#'
#' Checks if the provided model is valid for a specific report object by comparing the class of the model with the model_name slot of all report sections.
#' @param R A struct_report object
#' @param M a struct_model object
#' @param ... additional inputs
#' @return TRUE or throws an error
#' @export
setGeneric("is_valid",function(R,M,...)standardGeneric("is_valid"))
