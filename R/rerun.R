









rerun_internal <- function ( .n, funcall){

    if( cl_not_available() ){
        env <-  parent.frame(2) # Create a reference to the calling environment
        x <- purrr::rerun(.n, eval(funcall$expr, envir = env))
    }
    else if ( use_block_map() ) {
        x <- block_rerun(.n, funcall)
    }
    else {
        x <- par_rerun(.n, funcall)
    }
    return(x)
}


par_rerun <- function( .n , funcall){
    x <- parallel::parLapply(
        cl = getOption("purrrcl"),
        integer(.n),
        function(...) purrr::rerun(1, eval(funcall$expr)),
        RECYCLE = T,
        SIMPLIFY = F
    )
    return(purrr::flatten(x))
}

block_rerun <- function( .n, funcall){
    nblock <- getOption("purrrnblock")
    nruns <- ceiling(.n / nblock)
    x <- parallel::clusterCall(
        cl = getOption("purrrcl"),
        function() purrr::rerun(nruns , eval(funcall$expr))
    )
    return(purrr::flatten(x)[1:.n])
}





#' rerun
#' @description
#' This is the parallelised version of rerun for full details
#' see \code{\link[purrr]{rerun}}
#' @param .n  The number of times to evaluate expr
#' @param expr An expression to evaluate .n times
#' @export
rerun <- function(.n, expr) {
    funcall <- match.call()
    rerun_internal(.n, funcall)
}

