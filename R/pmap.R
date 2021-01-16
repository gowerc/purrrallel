

pmap_internal <- function ( .l , .f,  mfun, ffun, cfun, ... ){
    if( cl_not_available() ) return( mfun( .l , .f , ...) )
    purrr::reduce( .l , is_same_length)
    if ( use_block_map() ) return( block_pmap(.l , .f, mfun, ffun, ...))
    else return( par_pmap(.l, .f , cfun, ...))
}


par_pmap <- function( .l , .f , cfun , ...){
    ARGS <- list(...)
    x <- purrr::invoke(
        parallel::clusterMap ,
        cl = getOption("purrrcl"),
        .l,
        fun = .f,
        RECYCLE = F,
        SIMPLIFY = F,
        MoreArgs = ARGS
    )
    return( cfun(x , identity) )
}


block_pmap <- function( .l , .f , mfun , ffun, ...){

    mapper <- function ( .l , .f, mfun, ...) mfun( .l , .f, ...)

    PHOLD <- list()
    for ( i in seq_along(.l)){
        PHOLD[[i]] <- get_block_list( .l[[i]] , getOption("purrrnblock"))
    }
    names(PHOLD) <- names(.l)
    HOLD <- purrr::transpose(PHOLD)

    nblock <- getOption("purrrnblock")
    options( "purrrnblock" = NULL)

    tryCatch(
        { x <- map(HOLD  , mapper , .f ,  mfun, ...) },
        error=function(e)  {
            options( "purrrnblock" = nblock)
            stop(e)
        },
        warning=function(w) {
            options( "purrrnblock" = nblock)
            warning(w)
        },
        finally= {
            options( "purrrnblock" = nblock)
        }
    )
    ffun( x)
}





#' pmap
#' @description
#' This is the parallelised version of pmap for full details
#' see \code{\link[purrr]{pmap}}
#' @param .l  A list of lists. The length of .l determines the number of arguments that .f will be called with. List names will be used if present
#' @param .f  A function, formula, or atomic vector.
#' @param ... Additional arguments passed on to .f.
#' @export
pmap <- function( .l, .f , ...){
    pmap_internal( .l,  .f , purrr::pmap , purrr::flatten, purrr::map , ...)
}



#' @rdname pmap
#' @export
pmap_dbl <- function( .l, .f , ...){
    pmap_internal( .l,  .f , purrr::pmap_dbl , purrr::flatten_dbl, purrr::map_dbl , ...)
}




#' @rdname pmap
#' @export
pmap_chr <- function( .l, .f , ...){
    pmap_internal( .l,  .f , purrr::pmap_chr , purrr::flatten_chr, purrr::map_chr , ...)
}



#' @rdname pmap
#' @export
pmap_lgl <- function( .l, .f , ...){
    pmap_internal( .l,  .f , purrr::pmap_lgl , purrr::flatten_lgl, purrr::map_lgl , ...)
}




#' @rdname pmap
#' @export
pmap_df <- function( .l, .f , ...){
    pmap_internal( .l,  .f , purrr::pmap_df , flat_df, purrr::map_df , ...)
}



