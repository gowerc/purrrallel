
library(parallel)
cl <- makeCluster(2)

is.null(cl)
stopCluster(cl)
parallel::stopCluster(cl)
cl <- parallel::makeCluster(2)
parallel::setDefaultCluster(cl)





cl <- makeCluster(2)
registerCluster(cl)


purrr::rerun(
    2,
    {
        Sys.sleep(1)
        rnorm(2)
    }
)

rerun(
    2,
    {
        Sys.sleep(1)
        rnorm(2)
    }
)


rerun(
    3,rnorm(4)
)

stopCluster(cl)
registerCluster()


cl <- makeCluster(2)
registerCluster(cl, nblock = 2)

rerun(
    6,
    {
        Sys.sleep(1)
        rnorm(2)
    }
)

purrr::rerun(
    6,
    {
        Sys.sleep(1)
        rnorm(1)
    }
)

stopCluster(cl)
registerCluster()



cl <- makeCluster(6)
registerCluster(cl, nblock = 2)
system.time({
    x <- rerun(7,{
        Sys.sleep(1)
        rnorm(2)
    })
})
stopCluster(cl)
registerCluster()



cl <- makeCluster(6)
registerCluster(cl, nblock = 6)
system.time({
    x <- rerun(7,{
        rnorm(2)
    })
})
stopCluster(cl)
registerCluster()
