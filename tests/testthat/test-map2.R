
context("Testing MAP2")


map2_tests <- function(){
    test_that( "map2 yields expected results",{

        tfun <- function( x, y , z=0){
            x + y + z
        }

        expect_equal(
            map2( c(1,2,3) , c(4,5,6) , tfun),
            purrr::map2( c(1,2,3), c(4,5,6) , tfun)
        )

        expect_equal(
            map2( c(1,2,3), c(4,5,6) , tfun , 1),
            purrr::map2( c(1,2,3), c(4,5,6) , tfun , 1)
        )

        expect_equal(
            map2( c(1,2,3), c(4,5,6) , tfun , z = c(1,2)),
            purrr::map2( c(1,2,3), c(4,5,6) , tfun , z = c(1,2))
        )

        expect_error(
            map2( c(1,2,3) , c(1,2) , tfun),
            "are different lengths"
        )
        expect_error(
            purrr::map2( c(1,2,3) , c(1,2) , tfun),
            "are different lengths"
        )

        ### Test that unused arugments cause errors
        expect_error(
            map2( c(1,2,3), c(4,5,6) , tfun , 1,2),
            "unused argument"
        )
        expect_error(
            purrr::map2( c(1,2,3), c(4,5,6) , tfun , 1,2),
            "unused argument"
        )

    })


    test_that( "map2_dbl yields expected results",{
        tfun <- function( x, y=0 , z=0){
            x + y + z
        }

        expect_equal(
            map2_dbl( c(1,2,3),  c(1,2,3) , tfun)  ,
            purrr::map2_dbl( c(1,2,3),  c(1,2,3) , tfun)
        )

        expect_equal(
            map2_dbl( c(1,2,3),  c(1,2,3) , tfun , z = 1)  ,
            purrr::map2_dbl( c(1,2,3),  c(1,2,3) , tfun , z = 1)
        )

        expect_equal(
            map2_dbl( c(1,2,3),  c(1,2,3) , tfun , 1 )  ,
            purrr::map2_dbl( c(1,2,3),  c(1,2,3) , tfun , 1)
        )

        expect_error(
            map2_dbl(c(1,2,3),  c(1,2,3) , tfun , z = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )
        expect_error(
            purrr::map2_dbl(c(1,2,3),  c(1,2,3) , tfun , z = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_equal(
            map2_dbl( T , T , function(x , y) x),
            purrr::map2_dbl( T , T , function(x , y) x)
        )

    })




    test_that( "map2_chr yields expected results",{

        tfun <- function( x, y , z=1){
            paste0(
                LETTERS[x],
                letters[y],
                LETTERS[z]
            )
        }

        expect_equal(
            map2_chr( c(1,2,3), c(4,5,6) , tfun),
            purrr::map2_chr( c(1,2,3), c(4,5,6) , tfun)
        )

        expect_equal(
            map2_chr( c(1,2,3), c(4,5,6) , tfun , z = 3),
            purrr::map2_chr( c(1,2,3), c(4,5,6) , tfun , z = 3)
        )

        expect_equal(
            map2_chr( c(1,2,3), c(4,5,6) , tfun , 2),
            purrr::map2_chr( c(1,2,3), c(4,5,6) , tfun , 2)
        )

        expect_error(
            map2_chr(c(1,2,3), c(4,5,6) , tfun , z = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_error(
            purrr::map2_chr(c(1,2,3), c(4,5,6) , tfun , z = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_equal(
            map2_chr( 1  , 1 , function(x, y) x),
            purrr::map2_chr( 1  , 1 , function(x, y) x)
        )
    })





    test_that( "map2_lgl yields expected results",{

        tfun <- function( x, y , z=0){
            c ( x < 2 & y < 2 & z < 2)
        }

        expect_equal(
            map2_lgl( c(1,2,3), c(1,2,3) , tfun),
            purrr::map2_lgl( c(1,2,3), c(1,2,3) , tfun)
        )

        expect_equal(
            map2_lgl(  c(1,2,3), c(1,2,3) , tfun , z = 1),
            purrr::map2_lgl( c(1,2,3), c(1,2,3) , tfun , z = 1)
        )

        expect_equal(
            map2_lgl( c(1,2,3), c(1,2,3) , tfun , 3),
            purrr::map2_lgl( c(1,2,3), c(1,2,3) , tfun , 3)
        )

        expect_error(
            map2_lgl(c(1,2,3), c(1,2,3) , tfun , z = c(1,2)) ,
            "Result 1 is not a length 1 atomic vector"
        )

        expect_error(
            purrr::map2_lgl(c(1,2,3), c(1,2,3) , tfun , y = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_error(
            map2_lgl( 1 , 2 , function(x,y) x) ,
            "Can't coerce element 1 from a double to a logical"
        )

        expect_error(
            purrr::map2_lgl( 1 , 2 , function(x,y) x) ,
            "Can't coerce element 1 from a double to a logical"
        )

    })



    test_that( "map2_df yields expected results",{

        tfun <- function(x , y  , dat , z=NULL){
            return( dplyr::bind_rows( dat[c(x,z),] , dat[y,]))
        }

        expect_equal(
            map2_df( c(1,2,3), c(4,5,6) , tfun , dat = iris),
            purrr::map2_df( c(1,2,3), c(4,5,6) , tfun , dat = iris)
        )

        expect_equal(
            map2_df( c(1,2,3), c(4,5,6) , tfun , dat = iris , z =1 ),
            purrr::map2_df( c(1,2,3), c(4,5,6) , tfun , dat = iris , z =1 )
        )

        expect_equal(
            map2_df( c(1,2,3), c(4,5,6) , tfun , dat = iris , z = c(1,2) ),
            purrr::map2_df( c(1,2,3), c(4,5,6) , tfun , dat = iris , z =c(1,2) )
        )

        expect_equal(
            map2_df( list(iris,iris,iris) , c(1,2,3) , function(x,y)x[y,]),
            purrr::map2_df( list(iris,iris,iris) , c(1,2,3) , function(x,y)x[y,])
        )

        expect_equal(
            map2_df( list(iris,iris,iris) , c(1,2,3) , function(x,y)x[,y,drop=F]),
            purrr::map2_df( list(iris,iris,iris) , c(1,2,3) , function(x,y)x[,y,drop=F])
        )

        expect_error(
            map2_df( 1 , 1 , function(x,y) x),
            "Argument 1 must have names"
        )

        expect_error(
            purrr::map2_df( 1 , 1 , function(x,y) x),
            "Argument 1 must have names"
        )

    })
}




#### No cluster
map2_tests()


#### Simple Parallel Cluster
cl <- makeCluster(2)
registerCluster(cl)

map2_tests()

test_that( "parallelisation works",{
    sleepy <- function(x,y) Sys.sleep(x+y)
    x <- system.time(map2( c(1,1),c(1,1) , sleepy))
    y <- system.time(purrr::map2( c(1,1),c(1,1) , sleepy))
    expect_lt( round(x[3]) , round(y[3]) -0.5 )
})

stopCluster(cl)
registerCluster()


#### Block parallel Cluster
cl <- makeCluster(2)
registerCluster(cl, nblock = 2)

test_that( "parallelisation works",{
    sleepy <- function(x,y) Sys.sleep(x+y)
    x <- system.time(map2( c(1,1),c(1,1) , sleepy))
    y <- system.time(purrr::map2( c(1,1),c(1,1) , sleepy))
    expect_lt( round(x[3]) , round(y[3]) -0.5 )
})

stopCluster(cl)
registerCluster()





