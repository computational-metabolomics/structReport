#' Constructor for PCA_section objects
#'
#' Creates a new \linkS4class{PCA_section} object and populates the slots.
#' @return a report_section object#' @include report_section_class.R
#' @export
PCA_section = function(
    factor_names,
    markdown=system.file(package='structReport','templates/PCA_section.Rmd',mustWork = TRUE),
    ...) {

    # new object
    out = new_struct('PCA_section',
        factor_names=factor_names,
        markdown=markdown,
        model_name='PCA',
        ...
    )

    return(out)
}

#' PCA section
#'
#' This report section summarises the PCA model applied to a dataset.
.PCA_section<-setClass(
    "PCA_section",
    contains='report_section',
    slots = c(
        factor_names='character'
    ),
    prototype=list(
        .params=c('factor_names','models','markdown')
    )
)
