
#' registerCluster
#' @description
#' This function registers a cluster created by parallel::makeCluster() to be used to parallise the purrr mapping functions
#' from the parallel package. See \code{\link[parallel]{clusterApply}} for relevent documentation
#' @param cl a cluster created by parallel::makeCluster()
#' @param nblock (optional) Number of blocks to carve data into (recommend to set same as ncore)
#' @export
registerCluster <- function(cl = NULL, nblock  = NULL){
    options( "purrrcl" = cl )
    options( "purrrnblock" = nblock)
    invisible()
}


#' setClusterSeed
#' @description
#' This function sets the seeds across all your cluster processes to ensure reproducibility.
#' @param cl A cluster to set the seeds across
#' @param seeds Either a single value or a vector. If a single value the seeds for each process are automatically generated.
#' @export
setClusterSeed <- function(cl, seeds){

    if( length(seeds) == 1){
        set.seed(seeds)
        seeds <- round(stats::runif(length(cl), 1,75000))
    } else {
        if( length(seeds) != length(cl)){
            stop("Seeds should either be length 1 or the same length as the number of processes in the cluster")
        }
    }
    parallel::clusterApply(cl, seeds, set.seed)
    return(invisible())
}


cl_not_available <- function(){
    return( is.null(getOption("purrrcl") ))
}

use_block_map <- function(){
    return( !is.null(getOption("purrrnblock") ))
}



is_same_length <- function(x , y){
    if( length(x) != length(y)){
        stop("Arguments are different lengths")
    }
    invisible(y)
}



get_block_list <- function ( .x , nblock){
    .ints <- (1:length(.x) %% nblock) + 1
    .ints <- .ints[order(.ints)]
    HOLD <- list()
    for ( i in 1:nblock){
        HOLD[[i]] <- .x[which( .ints==i)]
    }
    return(HOLD)
}


flat_df <- function(x){
    if ( all(purrr::map_dbl(x , ncol) == 1 ) ){
        purrr::flatten_df(x)
    } else {
        purrr::map_df(x , function(x) x)
    }
}







