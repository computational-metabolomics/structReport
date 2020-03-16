#' Constructor for struct_report_list objects
#'
#' Creates a new \linkS4class{struct_report_list} object and populates the slots.
#' @param title the title of the report.
#' @param author the author of the report
#' @param format the format of the report. One of 'html_document','pdf_document','word_document'. Default = 'html_document'
#' @param markdown the markdown template to use
#' @param sections a list of report_sections
#' @param model_name the name of the model required for this report section
#' @return a struct_class object
#' @export
struct_report_list = function(
    title,
    author='Anon',
    format='html_document',
    toc=TRUE,
    toc_depth=2,
    markdown=system.file(package='structReport','templates/struct_report.Rmd',mustWork = TRUE),
    sections=list(),
    model_name=NULL,
    ...
) {
    
    # new object
    out = new_struct('struct_report_list',
        title=title,
        author=author,
        format=format,
        markdown=markdown,
        sections=sections,
        model_name = model_name,
        toc=toc,
        toc_depth=toc_depth,
        ...
    )
    
    return(out)
}

#' struct_report_list object
#'
#' Struct report lists contain parameters for populating yaml and knitr settings for combining multiple rmarkdown reports.
#' @import struct
.struct_report_list<-setClass(
    "struct_report_list",
    contains='struct_report'
)





#' @rdname struct_report_list
#' @export
#' @examples
#' MS = struct_report() + struct_report()
#' M = struct_report()
#' MS = M + MS
#'
#' @return a struct_report_list with the additional section appended to the front
setMethod("+",
    signature(e1 = 'struct_report',e2 = 'struct_report_list'),
    definition = function(e1,e2) {
        m = e2$sections
        m = c(e1,m)
        e2$sections = m
        return(e2)
    }
)

#' @rdname struct_report_list
#' @export
#' @examples
#' MS = struct_report() + struct_report()
#' M = struct_report()
#' MS = MS + M
#'
#' @return a struct_report with the additional section appended to the end
setMethod("+",
    signature(e1 = 'struct_report_list',e2 = 'struct_report'),
    definition = function(e1,e2) {
        m = e1$sections
        m = c(m,e2)
        e1$sections = m
        return(e1)
    }
)

#' @rdname struct_report_list
#' @export
#' @examples
#' MS = struct_report_list() + struct_report_list()
#'
#' @return a struct report list containing the combined sections
setMethod("+",
    signature(e1 = 'struct_report_list',e2 = 'struct_report_list'),
    definition = function(e1,e2) {
        ML = struct_report_list(title='Title')
        ML$sections= c(e1$sections,e2$sections)
        return(ML)
    }
)

#' @rdname struct_report_list
#' @export
#' @examples
#' MS = c(struct_report(), struct_report())
#'
#' @return a struct report list containing the combined sections
setMethod("c",
    signature('struct_report_list'),
    definition = function(x,...) {
        ML = struct_report_list(title='Title')
        ML$sections= list(x,...)
        return(ML)
    }
)

#' @rdname struct_report
#' @export
#' @examples
#' MS = c(struct_report(), struct_report())
#'
#' @return a struct report list containing the combined sections
setMethod("c",
    signature('struct_report'),
    definition = function(x,...) {
        ML = struct_report_list(title='Title')
        ML$sections= list(x,...)
        return(ML)
    }
)

#' @rdname struct_report
#' @export
setMethod("build_report",
    signature('struct_report_list','list','DatasetExperiment','character'),
    definition = function(R,M,D,outfile) {
        
        # check all R are struct_report
        t = lapply(R$sections,is,class2='struct_report')
        t=unlist(t)
        if (!all(t)) {
            stop('All sections of a struct_report_list must be struct_report objects')
        }
        
        # check all M are model, iterator or model_seq
        t = lapply(R,function(x) {
            a=is(x,'model')
            b=is(x,'model_seq')
            c=is(x,'iterator')
            return(any(a,b,c))
        })
        t=unlist(t)
        if (!all(t)) {
            stop('All items in the model list must be models, model_seq or iterators that match the corresponding struct_model_list section.')
        }
        
        for (k in seq_len(1,length(R))) {
            # check M matches expected model
            is_valid(R$sections[[k]],M[[k]]) # error if not
        }
        
        # compile format options
        fmt=eval(parse(text=paste0(R$format,'(toc=',R$toc,',toc_depth=',R$toc_depth,')')))
        
        # render the document
        rmarkdown::render(R$markdown,output_file=outfile,quiet=TRUE,output_format = fmt)
    }
)