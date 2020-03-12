#' Constructor for report_section objects
#'
#' Creates a new \linkS4class{report_section} object and populates the slots.
#' @param models the model to use for this report section
#' @param markdown path and filename of markdown template to use for the section
#' @return a struct_class object
#' @export
report_section = function(
    model_name=NULL,
    markdown=system.file(package='structReport','templates/report_section.Rmd',mustWork = TRUE)) {

    # new object
    out = new_struct('report_section',
        model_name=model_name,
        markdown=markdown
    )

    return(out)
}

#' report_section object
#'
#' Struct reports contain parameters for the child sections of a struct report.
.report_section<-setClass(
    "report_section",
    contains='struct_class',
    slots = c(
        model_name='entity',
        markdown='entity'
    ),
    prototype=list(
        .params=c('model_name','markdown'),
        markdown=entity(
            name='Markdown path/file',
            description='Path and filename of the markdown template to use for the report',
            value=character(0),
            type=c('character')
        ),
        model_name=entity(
            name='Expected model',
            description='The name of the model expected for this report, or NULL if not required.',
            value=NULL,
            type=c('character',NULL)
        )
    )
)
