context("Testing rerun")



test_that("Cluster seeding works for reproducibility",{
    cl <- makeCluster(2)
    registerCluster(cl)

    setClusterSeed(cl, 2005)
    x <- rerun(3 , rnorm(3))

    setClusterSeed(cl, 2005)
    y <- rerun(3 , rnorm(3))

    setClusterSeed(cl, 1)
    z <- rerun(3 , rnorm(3))

    setClusterSeed(cl, 1)
    w <- rerun(3 , rnorm(3))

    expect_error( setClusterSeed(cl, c(1,2,3)))

    stopCluster(cl)
    registerCluster()

    expect_equal(x, y)
    expect_equal(w, z)

    expect_false( any(x[[1]] ==  z[[1]]))
    expect_false( any(x[[2]] ==  z[[2]]))
    expect_false( any(x[[3]] ==  z[[3]]))

})




runme <- function(seed = F){

    set.seed(101)
    x <- purrr::rerun(3, rnorm(3))

    set.seed(101)
    xr <- rerun(3 , rnorm(3))

    expect_true(length(x) == length(xr))
    expect_true(length(xr[[1]]) == 3)
    expect_true(length(xr[[2]]) == 3)
    expect_true(length(xr[[3]]) == 3)
    expect_true(xr[[1]][[1]] != xr[[1]][[2]])
    expect_true(xr[[1]][[1]] != xr[[2]][[1]])

    if(seed){
        expect_equal(x, xr)
    }
}



test_that("basic usuage",{

    ### Normal run without any cluster
    runme(T)

    ### Cluster without blocking
    cl <- makeCluster(2)
    registerCluster(cl)
    setClusterSeed(cl, 2005)
    runme()
    stopCluster(cl)
    registerCluster()

    ### Cluster with blocking
    cl <- makeCluster(2)
    registerCluster(cl, nblock = 2)
    setClusterSeed(cl, 2005)
    runme()
    stopCluster(cl)
    registerCluster()
})



test_that("is running in parallel",{

    run_fun <- function(x){
        Sys.sleep(1)
        return("char")
    }


    ### Normal
    x1 <- system.time({
        r1 <- rerun(3, run_fun())
    })
    x2 <- system.time({
        r2 <- purrr::rerun(3, run_fun())
    })
    expect_true(round(x1[[3]]) == round(x2[[3]]))
    expect_equal(r1, r2)



    ### Cluster no blocking
    cl <- makeCluster(2)
    clusterExport(cl, "run_fun", envir = environment())
    registerCluster(cl)
    x1 <- system.time({
        r1 <- rerun(3, run_fun())
    })
    x2 <- system.time({
        r2 <- purrr::rerun(3, run_fun())
    })
    stopCluster(cl)
    registerCluster()
    expect_true(round(x1[[3]]) < round(x2[[3]]))
    expect_equal(r1, r2)


    ### Cluster with blocking
    cl <- makeCluster(2)
    clusterExport(cl, "run_fun", envir = environment())
    registerCluster(cl,nblock = 2)
    x1 <- system.time({
        r1 <- rerun(3, run_fun())
    })
    x2 <- system.time({
        r2 <- purrr::rerun(3, run_fun())
    })
    stopCluster(cl)
    registerCluster()
    expect_true(round(x1[[3]]) < round(x2[[3]]))
    expect_equal(r1, r2)

})


test_that("blocking works as expected",{

    run_fun <- function(x){
        Sys.sleep(1)
        return("char")
    }

    cl <- makeCluster(2)
    clusterExport(cl, "run_fun", environment())
    registerCluster(cl, nblock = 2)
    x1 <- system.time({
        r1 <- rerun(3, run_fun())
    })
    stopCluster(cl)
    registerCluster()


    cl <- makeCluster(2)
    clusterExport(cl, "run_fun", environment())
    registerCluster(cl, nblock = 1)
    x2 <- system.time({
        r2 <- rerun(3, run_fun())
    })
    stopCluster(cl)
    registerCluster()

    expect_true(round(x1[[3]]) < round(x2[[3]]))
    expect_equal(r1, r2)

})








