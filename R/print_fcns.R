

#' @export
setMethod("print",
    signature='model',
    definition = function(x,inset='M = ') {
        
        cat(inset)

        cat(class(x), '(', sep='')
        
        n = param_ids(x)
        for (k in seq_len(length(n))) {
            if ((length(n)>1) & k>1) {
                cat('\n')
                cat(rep(' ',nchar(class(x))+5),sep='')
            }
            if (is.character(param_value(x,n[k]))) {
                cat(n[k],'="', as.character(param_value(x,n[k])),'"',sep='')
            } else {
                cat(n[k],'=', as.character(param_value(x,n[k])),sep='')
            }
            if (k<length(n)) {
                cat(', ')
            }
        }
        if (length(n)>1) {
            cat('\n',rep(' ',4),sep='')
        }
        cat(')')
    }
)


#' @export
setMethod("print",
    signature='model_seq',
    definition = function(x,inset='M = ',inset2='    ') {
        
        for (j in 1:length(x)) {
            
            if (j==1) {
                cat(inset)
                print(models(x)[[j]],'')
            } else {
                print(models(x)[[j]],inset2)
            }
            
            if (j<length(x)) {
                cat(' + \n')
            }
            
        }
        
        if(!equals) {
            cat(')')
        }
        
    }
)

#' @export
setMethod("print",
    signature='iterator',
    definition = function(x,inset='I = ') {
        
        cat(inset)
        
        cat(class(x), '(', sep='')
        
        n = param_ids(x)
        for (k in seq_len(length(n))) {
            if ((length(n)>1) & k>1) {
                cat('\n')
                cat(rep(' ',nchar(class(x))+5),sep='')
            }
            if (is.character(param_value(x,n[k]))) {
                cat(n[k],'="', as.character(param_value(x,n[k])),'"',sep='')
            } else {
                cat(n[k],'=', as.character(param_value(x,n[k])),sep='')
            }
            if (k<length(n)) {
                cat(', ')
            }
        }
        if (length(n)>1) {
            cat('\n',rep(' ',4),sep='')
        }
        cat(')')
        
        M = models(x)
        
        if (class(M)[1]=='model') {
            return()
        }
        
        cat(' * \n')
        
        if (is(M,'model_seq')) {
            cat(inset,'(',sep='')
            print(M,inset='',inset2='     ')
            cat(')')
        } else {
            print(M,inset='    ')
        }
        
    }
    
)

