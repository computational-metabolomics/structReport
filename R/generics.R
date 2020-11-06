#' Build struct report
#'
#' Builds a struct report using the input data and writes the output to the specified location.
#' @param R A struct_report object
#' @param outfile A path/filename to save the output to
#' @return A rendered markdown file
#' @export
setGeneric("build_report",function(R,outfile)standardGeneric("build_report"))


#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @param subsection A list of report sections to include as subsections.
#' @param ... additional report specific parameters
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setGeneric("as_report_section",function(obj,subsection,...)standardGeneric("as_report_section"))
