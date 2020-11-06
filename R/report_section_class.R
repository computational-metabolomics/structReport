#' Constructor for report_section objects
#'
#' Creates a new \linkS4class{report_section} object and populates the slots.
#' @param models the model to use for this report section
#' @param markdown path and filename of markdown template to use for the section
#' @return a struct_class object
#' @rdname report_section
#' @export
report_section = function(
    object=NULL,
    markdown=system.file(package='structReport','templates/default_object.Rmd',mustWork = TRUE),
    subsection=NULL,
    ...) {
    
    if (is(subsection,'report_section')) {
        # convert to list
        subsection=list(subsection)
    }
    
    # new object
    out = new_struct('report_section',
        object=object,
        markdown=markdown,
        subsection=subsection,
        ...
    )
    
    return(out)
}

#' report_section object
#'
#' struct reports contain parameters for the child sections of a struct report.
.report_section<-setClass(
    "report_section",
    contains='struct_class',
    slots = c(
        object='entity',
        markdown='entity',
        subsection='entity'
    ),
    prototype=list(
        .params=c('object','markdown','subsection'),
        name='Section header.',
        description='Paragraph describing this section.',
        type='section',
        libraries='rmarkdown',
        markdown=entity(
            name='Markdown path/file',
            description='Path and filename of the markdown template to use for the report',
            value=NULL,
            type=c('character','NULL')
        ),
        object=entity(
            name='Report object',
            description='The struct object being reported.',
            value=NULL,
            type=c('struct_class','NULL')
        ),
        subsection=entity(
            name='Subsections',
            description='A list of report_sections used as subsections.',
            value=NULL,
            type=c('NULL','list')
        )
    )
)

#' @rdname report_section
#' @export
#' @examples
#' # a section with 2 subsections
#' MS = report_section*(report_section()+report_section())
#' length(MS) # 2
#'
#' @return the number of sections in a struct_report
setMethod(f = 'length',
    signature = 'report_section',
    definition = function(x) {
        return(length(x$subsection))
    }
)



#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setMethod(f = 'as_report_section',
    signature = c('struct_class','missing'),
    definition = function(obj) {
        RS = report_section(
            name=obj$name,
            description=obj$description,
            object=obj
        )
        return(RS)
    }
)

#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @param subsection A report section to include as a subsection for this object.
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setMethod(f = 'as_report_section',
    signature = c('struct_class','report_section'),
    definition = function(obj,subsection) {
        RS = as_report_section(obj,subsection=list(subsection))
        return(RS)
    }
)

#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @param subsection A list of report sections to include as a subsection for this object.
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setMethod(f = 'as_report_section',
    signature = c('struct_class','list'),
    definition = function(obj,subsection) {
        RS = report_section(
            name=obj$name,
            description=obj$description,
            object=obj,
            subsection=subsection
        )
        return(RS)
    }
)

#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setMethod(f = 'as_report_section',
    signature = c('model_seq','missing'),
    definition = function(obj) {
        RS = report_section(
            name=obj$name,
            description=obj$description,
        )
        for (k in seq_len(length(obj))) {
            RS$subsection[[k]]=as_report_section(obj[k])
        }
        return(RS)
    }
)

#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @param subsection A report section to include as a subsection for the input model sequence.
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setMethod(f = 'as_report_section',
    signature = c('model_seq','report_section'),
    definition = function(obj,subsection) {
        RS = as_report_section(obj,list(subsection))
        return(RS)
    }
)

#' Creating Report sections from an object
#' 
#' This method converts the input \code{struct} object into a report section for use with
#' other reporting objects such as \code{struct_report} and methods like \code{build_report}.
#' @param obj A \code{struct_class} object
#' @param subsection A list of list report sections to include as subsections for this object.
#' @return A \code{report_section} object with pre-populated slots based on the input object.
#' @export
setMethod(f = 'as_report_section',
    signature = c('model_seq','list'),
    definition = function(obj,subsection) {
        RS = report_section(
            name=obj$name,
            description=obj$description,
            subsection=subsection
        )
        for (k in seq_len(length(obj))) {
            RS$subsection[[k]]=as_report_section(obj[k])
        }
        return(RS)
    }
)
