



map_internal <- function ( .x, .f, mfun, ffun, ... ){
    if( cl_not_available() ) return( mfun(.x , .f , ...) )
    if ( use_block_map() ) return( block_map(.x,.f , mfun, ffun, ...))
    else return( par_map(.x, .f , mfun, ...))
}


par_map <- function( .x , .f , mfun , ...){
    ARGS <- list(...)
    x <- parallel::clusterMap(
        cl = getOption("purrrcl"),
        fun = .f,
        .x ,
        MoreArgs = ARGS ,
        RECYCLE = F,
        SIMPLIFY = F
    )
    return( mfun(x , identity) )
}


block_map <- function( .x , .f , mfun , ffun, ...){

    mapper <- function ( .x , .f, mfun, ...) mfun( .x , .f, ...)

    HOLD <- get_block_list( .x , getOption("purrrnblock") )

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


#' map
#' @description
#' This is the parallelised version of map for full details
#' see \code{\link[purrr]{map}}
#' @param .x  A list or atomic vector
#' @param .f  A function
#' @param ... Additional arguments passed on to .f
#' @export
map <- function( .x , .f , ...) {
    map_internal( .x , .f , purrr::map , purrr::flatten , ...)
}



#' @rdname map
#' @export
map_dbl <- function( .x , .f , ...) {
    map_internal( .x , .f , purrr::map_dbl , purrr::flatten_dbl , ...)
}




#' @rdname map
#' @export
map_chr <- function( .x , .f , ...) {
    map_internal( .x , .f , purrr::map_chr , purrr::flatten_chr , ...)
}




#' @rdname map
#' @export
map_lgl <- function( .x , .f , ...) {
    map_internal( .x , .f , purrr::map_lgl , purrr::flatten_lgl , ...)
}



#' @rdname map
#' @export
map_df <- function( .x , .f , ...) {
    map_internal( .x , .f , purrr::map_df , flat_df , ...)
}

