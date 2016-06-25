
## problem is still that we have implicit arguments...
.vectorizeinputs <- function(e) {
    ## test function
    funcname <- e[[1]]
    e[[1]] <- NULL
    fvals <- formals(eval(funcname))
    fnames <- names(fvals)
    implicit <- setdiff(fnames, names(e))
    e <- as.list(e)
    ## e <- lapply(e, eval)
    for (i in names(e)) e[[i]] <- eval(e[[i]])
    ##    print(e)
    e <- as.data.frame(e)
    if (length(implicit) > 0) e <- cbind(e, fvals[implicit])
    ##    names(e)[names(e) == ""] <- implicit
    ## print(implicit)
    ## print(e)
    for (i in names(e)) assign(i, eval(e[[i]]),
                               envir=parent.frame())
}

.vectorizeinputs_keep <- function(x) {
    fname <- x[[1]]
    x[[1]] <- NULL
    x <- as.data.frame(as.list(x))
    for (i in names(x)) x[[i]] <- eval(x[[i]])
    for (i in names(x)) assign(i, x[[i]],
                                 envir=parent.frame())
}
