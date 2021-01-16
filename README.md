
# Purrrallel

This package aims to add parallelisation to the core functions of the [purrr package](https://github.com/tidyverse/purrr) . Essentially this package is a just wrapper around the [parallel package](https://www.rdocumentation.org/packages/parallel/versions/3.4.1) but with an api that aims to be identical to that of [purrr](https://github.com/tidyverse/purrr). The advantage of this is that you can convert your code to using parallelisation with as minimal syntax updates as possible. So far the following functions have been implemented `map_*()`, `map2_*()`, `pmap_*()` & `rerun()`

 
## Example of use

Below shows a contrivied example for creating a bootstrap confidence interval for the mean of `iris$Sepal.Length` across each `iris$Species`.

```{r}
library(purrrallel)
library(parallel)
library(dplyr)
library(tidyr)

get_sample <- function(...){
    iris %>% 
        sample_frac( 1 , replace = T) %>% 
        group_by(Species) %>% 
        summarise ( m = mean(Sepal.Length)) %>% 
        spread(Species , m)
}

cl <- makeCluster(2)
clusterEvalQ(cl, {library(dplyr); library(tidyr)})
registerCluster(cl, nblock = 2)


system.time({
    dat <- data_frame( n = 1:1200 ) %>% 
        mutate( est = map( n , get_sample)) %>% 
        unnest(est)
})

stopCluster(cl)
registerCluster()

```

Alternatively we can do the same using the `rerun()` function.

```{r}
cl <- makeCluster(2)
clusterEvalQ(cl, {library(dplyr); library(tidyr)})
clusterExport(cl, "get_sample")
registerCluster(cl, nblock = 2)


system.time({
    dat <- rerun(1200, get_sample())
})

purrr::reduce(dat, bind_rows)

stopCluster(cl)
registerCluster()

```


## Additional Functions

* `registerCluster(cl, nblock = NULL)` - Register a cluster for use in purrrallel  
* `setClusterSeed(cl, 101)` - Set seeds across all subprocesses to ensure reproducibility   


## Misc Notes

* If you call any of the purrallel functions without a socket/cluster available they will simply call the corresponding purrr function.    
* Currently purrrallel doesn't support plucking (which is supported by purrr) for example: `map_dbl( list(list(a=1), list(a=2)), "a")`    
* This is overhead involved with copying data between the master process and the subprocesses. In order to reduce this overhead use the `nblock` argument of `registerCluster()` which forces purrrallel to break your job up into blocks with each block being run in its own subprocesses (blocks <= # processes)   

## Parallel Notes

* `cl <- makeCluster(2)` - Create a cluster with n processes  
* `stopCluster(cl)` - Stop the cluster  
* `clusterEvalQ(cl, {library(dplyr); library(tidyr)})` - Run an expression in ever subprocess  
* `clusterCall(cl,  function() print("hello, world!"))` - Run a function in every subprocess  
* `clusterExport(cl, c("var1", "var2"))` - Exports specific variables to every subprocess  


























