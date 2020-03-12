#' Constructor for mean_centre_section objects
#'
#' Creates a new \linkS4class{mean_centre_section} object and populates the slots.
#' @inheritParams structToolbox::mean_centre
#' @return a report_section object
#' @include report_section_class.R
#' @export
mean_centre_section = function(
    mode="data",
    markdown=system.file(package='structReport','templates/mean_centre_section.Rmd',mustWork = TRUE),
    ...) {

    # new object
    out = new_struct('mean_centre_section',
        mode=mode,
        markdown=markdown,
        model_name='mean_centre',
        ...
    )

    return(out)
}

#' mean_centre section
#'
#' This report section summarises the mean_centre model applied to a dataset.
.mean_centre_section<-setClass(
    "mean_centre_section",
    contains='report_section',
    slots = c(
        mode='entity'
    ),
    prototype=list(
        .params=c('mode'),
        mode=param_obj(structToolbox::mean_centre(),'mode')
    )
)
