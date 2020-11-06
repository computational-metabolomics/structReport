#' Constructor for struct_report objects
#'
#' Creates a new \linkS4class{struct_report} object and populates the slots.
#' @param title the title of the report.
#' @param author the author of the report
#' @param format the format of the report. One of 'html_document','pdf_document','word_document'. Default = 'html_document'
#' @param sections a list of report_sections
#' @return a struct_class object
#' @export
struct_report = function(
    title,
    author='Anon',
    format='html_document',
    toc=TRUE,
    toc_depth=2,
    sections=list(),
    ...
) {
    
    # new object
    out = new_struct('struct_report',
        title=title,
        author=author,
        format=format,
        sections=sections,
        toc=toc,
        toc_depth=toc_depth,
        markdown=system.file(package='structReport','templates/struct_report.Rmd',mustWork = TRUE),
        ...
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
        format='entity',
        sections='list',
        toc='entity',
        toc_depth='entity',
        markdown='character'
    ),
    prototype=list(
        name = 'Struct Report',
        description = 'Initialises a Struct Report and set yaml, markdown config etc.',
        type = 'Report frontend',
        .params=c('title','author','format','sections','toc','toc_depth'),
        
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
        format=entity(
            name='Report format',
            description='The format of the report.',
            value='html_document',
            type=c('character','rmarkdown_output_format'),
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
        ),
        markdown=''
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
#' MS = report_section() + report_section()
#'
#' @return a struct report containing the combined sections
setMethod("+",
    signature(e1 = 'report_section',e2 = 'report_section'),
    definition = function(e1,e2) {
        ML = struct_report(title='Title')
        ML$sections= c(e1,e2)
        return(ML)
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = struct_report() + struct_report()
#'
#' @return a struct report containing a section for each of the input reports
setMethod("+",
    signature(e1 = 'struct_report',e2 = 'struct_report'),
    definition = function(e1,e2) {
        # new report with a section for each report
        
        ML = struct_report(title='Title')
        
        S1= report_section(
                name='Section 1',
                description='Description of section 1',
                subsection=e1$sections
            )
        S2= report_section(
            name='Section 2',
            description='Description of section 2',
            subsection=e2$sections
        )
        ML$sections= c(S1,S2)
        return(ML)
    }
)


#' @rdname struct_report
#' @importFrom rmarkdown render
#' @export
setMethod("build_report",
    signature('struct_report','character'),
    definition = function(R,outfile) {
        
        # compile format options
        if (is.character(R$format)) {
            fmt=eval(parse(text=paste0(R$format,'(toc=',R$toc,',toc_depth=',R$toc_depth,')')))
        }
        
        # render the document
        rmarkdown::render(
            R@markdown,
            output_file=outfile,
            quiet=TRUE,
            output_format = fmt)
    }
)



