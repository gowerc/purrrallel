



map2_internal <- function ( .x, .y, .f, mfun, ffun, cfun, ... ){
    if( cl_not_available() ) return( mfun(.x , .y, .f , ...) )

    is_same_length(.x, .y)

    if ( use_block_map() ) return( block_map2(.x, .y, .f , mfun, ffun, ...))
    else return( par_map2(.x, .y, .f , cfun, ...))
}


par_map2 <- function( .x , .y, .f , cfun , ...){
    ARGS <- list(...)
    x <- parallel::clusterMap(
        cl = getOption("purrrcl"),
        fun = .f,
        .x , .y ,
        MoreArgs = ARGS ,
        RECYCLE = F,
        SIMPLIFY = F
    )

    return( cfun(x , identity) )
}


block_map2 <- function( .x , .y, .f , mfun , ffun, ...){

    mapper <- function ( .x , .y,  .f, mfun, ...) mfun( .x ,.y, .f, ...)

    HOLD.x <- get_block_list( .x , getOption("purrrnblock") )
    HOLD.y <- get_block_list( .y , getOption("purrrnblock") )

    nblock <- getOption("purrrnblock")
    options( "purrrnblock" = NULL)

    tryCatch(
        { x <- map2(HOLD.x , HOLD.y  , mapper , .f ,  mfun, ...) },
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




#' map2
#' @description
#' This is the parallelised version of map2 for full details
#' see \code{\link[purrr]{map2}}
#' @param .x  Vectors of the same length. A vector of length 1 will be recycled.
#' @param .y  Vectors of the same length. A vector of length 1 will be recycled.
#' @param .f  A function, formula, or atomic vector.
#' @param ... Additional arguments passed on to .f.
#' @export
map2 <- function( .x , .y, .f , ...) {
    map2_internal(.x , .y, .f, purrr::map2, purrr::flatten, purrr::map, ...)
}



#' @rdname map2
#' @export
map2_dbl <- function( .x , .y, .f , ...) {
    map2_internal(.x , .y, .f, purrr::map2_dbl, purrr::flatten_dbl, purrr::map_dbl, ...)
}



#' @rdname map2
#' @export
map2_chr <- function( .x , .y, .f , ...) {
    map2_internal(.x , .y, .f, purrr::map2_chr, purrr::flatten_chr, purrr::map_chr, ...)
}



#' @rdname map2
#' @export
map2_lgl <- function( .x , .y, .f , ...) {
    map2_internal(.x , .y, .f, purrr::map2_lgl, purrr::flatten_lgl, purrr::map_lgl, ...)
}



#' @rdname map2
#' @export
map2_df <- function( .x , .y, .f , ...) {
    map2_internal(.x , .y, .f, purrr::map2_df, flat_df, purrr::map_df, ...)
}



