context("Testing PMAP")


pmap_tests <- function(){

    test_that( "Basic pmap usage works", {

        tfun <- function(x,y,z , w = 1){
            x + (y * z) + w
        }

        ### General basic calls
        expect_equal(
            pmap( list(c(1,2),  c(1,2), c(1,2)) , tfun),
            purrr::pmap( list(c(1,2),  c(1,2), c(1,2)) , tfun)
        )

        expect_equal(
            pmap( list( z = c(0,1), y =  c(1,1), x = c(2,2)) , tfun),
            purrr::pmap( list( z = c(0,1), y =  c(1,1), x = c(2,2)) , tfun)
        )

        expect_equal(
            pmap( list( z = c(0,1), y =  c(1,1), x = c(2,2)) , tfun , w = c(1,2,3)),
            purrr::pmap( list( z = c(0,1), y =  c(1,1), x = c(2,2)) , tfun , w = c(1,2,3))
        )

        expect_equal(
            pmap( list( z = c(0,1), c(1,1), c(2,2)) , tfun , c(1,2,3)),
            purrr::pmap( list( z = c(0,1), c(1,1), c(2,2)) , tfun , c(1,2,3))
        )

        ### Mis-matched dimensions error
        expect_error(
            pmap( list( z = c(0,1,1), y =  c(1,1), x = c(2,2)) , tfun),
            "length"
        )
        expect_error(
            purrr::pmap( list( z = c(0,1,1), y =  c(1,1), x = c(2,2)) , tfun),
            "length"
        )

        ### Unused arguments causes errors
        expect_error(
            pmap( list(c(1,2),  c(1,2), c(1,2)) , tfun , 1 , 2),
            "unused argument"
        )
        expect_error(
            purrr::pmap( list(c(1,2),  c(1,2), c(1,2)) , tfun , 1 , 2),
            "unused argument"
        )


    })






    test_that( "pmap_dbl yields expected results",{

        tfun <- function(x,y,z , w = 1){
            x + (y * z) + w
        }

        expect_equal(
            pmap_dbl( list( z = c(0,1), y =  c(1,1), x = c(2,2)), tfun) ,
            purrr::pmap_dbl( list( z = c(0,1), y =  c(1,1), x = c(2,2)), tfun)
        )

        expect_equal(
            pmap_dbl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , 2) ,
            purrr::pmap_dbl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , 2)
        )

        expect_error(
            pmap_dbl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , c(1,2)) ,
            "a length 1 atomic vector"
        )

        expect_error(
            purrr::pmap_dbl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , c(1,2)),
            "a length 1 atomic vector"
        )


        expect_error(
            pmap_dbl( list(1,2,3) , function(x,y,z) "a"),
            "Can't coerce element 1 from a character to a double"
        )

        expect_error(
            purrr::pmap_dbl( list(1,2,3) , function(x,y,z) "a"),
            "Can't coerce element 1 from a character to a double"
        )

    })




    test_that( "pmap_chr yields expected results",{

        tfun <- function( x, y , z, w=1){
            paste0(
                LETTERS[x],
                letters[y],
                LETTERS[z],
                letters[w]
            )
        }

        expect_equal(
            pmap_chr( list( z = c(0,1), y =  c(1,1), x = c(2,2)), tfun) ,
            purrr::pmap_chr( list( z = c(0,1), y =  c(1,1), x = c(2,2)), tfun)
        )

        expect_equal(
            pmap_chr( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , 2) ,
            purrr::pmap_chr( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , 2)
        )

        expect_error(
            pmap_chr( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , c(1,2)) ,
            "a length 1 atomic vector"
        )

        expect_error(
            purrr::pmap_chr( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , c(1,2)),
            "a length 1 atomic vector"
        )

        expect_equal(
            pmap_chr( list(1,2,3) , function(x,y,z) 1),
            purrr::pmap_chr( list(1,2,3) , function(x,y,z) 1)
        )

    })


    test_that( "pmap_lgl yields expected results", {

        tfun <- function( x, y , z, w = 0){
            c ( x < 2 & y < 2 & z < 2 & w < 2)
        }

        expect_equal(
            pmap_lgl( list( z = c(0,1), y =  c(1,1), x = c(2,2)), tfun) ,
            purrr::pmap_lgl( list( z = c(0,1), y =  c(1,1), x = c(2,2)), tfun)
        )

        expect_equal(
            pmap_lgl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , 2) ,
            purrr::pmap_lgl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , 2)
        )

        expect_error(
            pmap_lgl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , c(1,2)) ,
            "a length 1 atomic vector"
        )

        expect_error(
            purrr::pmap_lgl( list( z = c(0,1), y =  c(1,1), c(2,2)), tfun , c(1,2)),
            "a length 1 atomic vector"
        )

        expect_error(
            pmap_lgl( list(1,2,3) , function(x,y,z) 1),
            "Can't coerce element 1 from a double to a logical"
        )
        expect_error(
            purrr::pmap_lgl( list(1,2,3) , function(x,y,z) 1),
            "Can't coerce element 1 from a double to a logical"
        )

    })

    test_that( "pmap_df yields expected results",{

        tfun <- function(x , y ,z  , dat , w=NULL){
            return( dplyr::bind_rows( dat[c(x,w),] , dat[y,] , dat[z,]))
        }

        expect_equal(
            pmap_df( list( c(1,2,3), c(4,5,6) , c(7,8,9)) , tfun , dat = iris),
            purrr::pmap_df( list( c(1,2,3), c(4,5,6) , c(7,8,9)) , tfun , dat = iris)
        )

        expect_equal(
            pmap_df( list( c(1,2,3), c(4,5,6) , c(7,8,9)) , tfun , dat = iris, w = c(1,2)),
            purrr::pmap_df( list( c(1,2,3), c(4,5,6) , c(7,8,9)) , tfun , dat = iris, w = c(1,2))
        )

        expect_error(
            pmap_df( list(1, 2, 3) , function(x,y,z) x),
            "Argument 1 must have names"
        )
        expect_error(
            purrr::pmap_df( list(1, 2, 3) , function(x,y,z) x),
            "Argument 1 must have names"
        )

    })
}



#### No cluster
pmap_tests()



#### Simple Parallel Cluster
cl <- makeCluster(2)
registerCluster(cl, nblock = 2)

pmap_tests()

test_that( "parallelisation works",{
    sleepy <- function(x,y,z) Sys.sleep(x+y+z)
    x <- system.time(pmap( list(x=c(0,0), y=c(1,1), z=c(1,1)) , sleepy))
    y <- system.time(purrr::pmap( list(x=c(0,0), y=c(1,1), z=c(1,1)) , sleepy))
    expect_lt( round(x[3]) , round(y[3]) -0.5 )
})

stopCluster(cl)
registerCluster()



#### Block parallel Cluster
cl <- makeCluster(2)
registerCluster(cl, nblock = 2)

pmap_tests()

test_that( "parallelisation works",{
    sleepy <- function(x,y,z) Sys.sleep(x+y+z)
    x <- system.time(pmap( list(x=c(0,0), y=c(1,1), z=c(1,1)) , sleepy))
    y <- system.time(purrr::pmap( list(x=c(0,0), y=c(1,1), z=c(1,1)) , sleepy))
    expect_lt( round(x[3]) , round(y[3]) -0.5 )
})

stopCluster(cl)
registerCluster()





