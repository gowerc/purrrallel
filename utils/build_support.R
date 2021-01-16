devtools::document()

devtools::load_all()

devtools::test()

devtools::check()

path <- devtools::build()

getwd()
install.packages( path , repos = NULL, type="source")

vignette("purrrallel")


detach("package:purrrallel", unload=TRUE)
remove.packages( "purrrallel")



makeCluster()
getOption("purrrcl")
stopCluster()

# library(purrallel)
random_fun <- function(x){
    Sys.sleep(2)
    return( x )
}

library(dplyr)
system.time({
    data_frame(x = 1:4) %>%
        mutate( coef = map_dbl(x , random_fun))
})


system.time({
    data_frame(x = 1:4) %>%
        makeCluster(ncore = 4) %>%
        mutate( coef = map_dbl(x , random_fun)) %>%
        stopCluster()
})






