#' Constructor for struct_report objects
#'
#' Creates a new \linkS4class{struct_report} object and populates the slots.
#' @param title the title of the report.
#' @param author the author of the report
#' @param format the format of the report. One of 'html_document','pdf_document','word_document'. Default = 'html_document'
#' @param markdown the markdown template to use
#' @param sections a list of report_sections
#' @param model_name the name of the model required for this report section
#' @return a struct_class object
#' @export
struct_report = function(
        title,
        author='Anon',
        format='html_document',
        toc=TRUE,
        toc_depth=2,
        markdown=system.file(package='structReport','templates/struct_report.Rmd',mustWork = TRUE),
        sections=list(),
        model_name=NULL
    ) {

    # new object
    out = new_struct('struct_report',
        title=title,
        author=author,
        format=format,
        markdown=markdown,
        sections=sections,
        model_name = model_name,
        toc=toc,
        toc_depth=toc_depth
    )

    return(out)
}

#' struct_report object
#'
#' Struct reports contain parameters for populating yaml and knitr settings for rmarkdown reports.
#' @import struct
.struct_report<-setClass(
    "struct_report",
    contains='struct_class',
    slots = c(
        title='entity',
        author='entity',
        format='enum',
        sections='list',
        markdown='entity',
        model_name='entity',
        toc='entity',
        toc_depth='entity'
    ),
    prototype=list(
        name = 'Struct Report',
        description = 'Initialises a Struct Report and set yaml, markdown config etc.',
        type = 'Report frontend',
        .params=c('title','author','format','sections','markdown','model_name','toc','toc_depth'),

        title=entity(
            name='Report title',
            description='The title of the report',
            value=NULL,
            type=c('character','NULL')
        ),
        author=entity(
            name='Report author',
            description='The author of the report',
            value=NULL,
            type=c('character','NULL')
        ),
        format=enum(
            name='Report format',
            description='The format of the report. Can be any "html_document", "pdf_document" or "word_document"',
            value='html_document',
            type=c('character'),
            allowed=c('html_document','pdf_document','word_document')
        ),
        markdown=entity(
            name='Markdown path/file',
            description='Path and filename of the markdown template to use for the report',
            value=character(0),
            type=c('character')
        ),
        model_name=entity(
            name='Expected model',
            description='The class name of the model expected for this report section, or NULL if not required.',
            value=NULL,
            type=c('character','NULL')
        ),
        toc=entity(
            name='Table of contents',
            description='TRUE/FALSE to include a table of contents in the document. Default = TRUE',
            value=TRUE,
            type=c('logical')
        ),
        toc_depth=entity(
            name='Table of contents depth',
            description='The number of header levels to include in the table of contents. Default = 2.',
            value=2,
            type=c('numeric','integer')
        )
    )
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = report_section() + report_section()
#' MS[2]
#'
#' @return model for the report section at the given index
setMethod(f = "[",
    signature = "struct_report",
    definition = function(x,i) {
        return(x@sections[[i]])
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = report_section() + report_section()
#' MS[2] = model()
#'
#' @return struct_report with the model for the section at index i replaced
setMethod(f = "[<-",
    signature = "struct_report",
    definition = function(x,i,value) {
        if (!is(value,'report_section')) {
            stop('value must be a struct_report')
        }
        x@sections[[i]] = value
        return(x)
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = report_section() + report_section()
#' length(MS) # 2
#'
#' @return the number of sections in a struct_report
setMethod(f = 'length',
    signature = 'struct_report',
    definition = function(x) {
        return(length(x@sections))
    }
)

setMethod(f = 'show',
    signature = 'struct_report',
    definition = function(object) {
        cat('A struct_report object with:\n\n')
        if (length(object) == 0) {
            cat('no sections')
            return()
        }
        for (i in seq_len(length(object))) {
            cat('[',i,']\n',sep='')
            show(object[i])
        }
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = model_report() + model_report()
#' M = model_report()
#' MS = M + MS
#'
#' @return a struct_report with the additional section appended to the front of
#' the sequence
setMethod("+",
    signature(e1 = 'report_section',e2 = 'struct_report'),
    definition = function(e1,e2) {
        m = e2$sections
        m = c(e1,m)
        e2$sections = m
        return(e2)
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = model_report() + model_report()
#' M = model_report()
#' MS = MS + M
#'
#' @return a struct_report with the additional section appended to the end of the
#' sequence
setMethod("+",
    signature(e1 = 'struct_report',e2 = 'report_section'),
    definition = function(e1,e2) {
        m = e1$sections
        m = c(m,e2)
        e1$sections = m
        return(e1)
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = model_report() + model_report()
#'
#' @return a struct report containing the combined sections
setMethod("+",
    signature(e1 = 'report_section',e2 = 'report_section'),
    definition = function(e1,e2) {
        ML = struct_report()
        ML$sections= c(e1,e2)
        return(ML)
    }
)

#' @rdname struct_report
#' @export
setMethod("build_report",
    signature('struct_report','model_OR_model_seq','DatasetExperiment','character'),
    definition = function(R,M,D,outfile) {

        # check M matches expected model
        is_valid(R,M) # error if not

        # compile format options
        fmt=eval(parse(text=paste0(R$format,'(toc=',R$toc,',toc_depth=',R$toc_depth,')')))

        # render the document
        rmarkdown::render(R$markdown,output_file=outfile,quiet=TRUE,output_format = fmt)
    }
)

#' @rdname struct_report
#' @export
setMethod("is_valid",
    signature('struct_report','model'),
    definition = function(R,M) {

        valid = R$model_name

        # for each report section, get the model
        L = length(R)
        if (L>0) {
            for (k in 1:L) {
                valid = c(valid,R$sections[[k]]$model_name)
            }
        }

        # class of model
        C = class(M)

        # if all reports use the same model then its ok
        valid = unique(valid)
        if (C == valid) {
            return(TRUE)
        } else {
            stop('Model is not a valid match for this report')
        }


    }
)

#' @rdname struct_report
#' @export
setMethod("is_valid",
    signature('struct_report','model_seq'),
    definition = function(R,M) {

        valid = R$model_name

        # for each report section, get the model
        L = length(R)
        if (L>0) {
            for (k in 1:L) {
                valid = c(valid,R$sections[[k]]$model_name)
            }
        }

        # class of models in sequence
        C = NULL
        if (length(M)>0) {
            for (k in 1:length(M)) {
                C = c(C,class(M[k]))
            }
        }

        if (length(C)!=length(valid)) {
            stop('Model sequence is not a valid match for this report (length does not match).')
        }

        # must be an exact match
        if (all(C == valid)) {
            return(TRUE)
        } else {
            stop('Model sequence is not a valid match for this report (models do not match).')
        }


    }
)

#' @rdname struct_report
#' @export
setMethod("is_valid",
    signature('struct_report','iterator'),
    definition = function(R,M) {

        valid = R$model_name

        # for each report section, get the model
        L = length(R)
        if (L>0) {
            for (k in 1:L) {
                valid = c(valid,R$sections[[k]]$model_name)
            }
        }

        # class of iterator
        C = class(M)

        # if all reports use the same model then its ok
        valid = unique(valid)
        if (C == valid) {
            return(TRUE)
        } else {
            stop('Iterator is not a valid match for this report')
        }


    }
)
