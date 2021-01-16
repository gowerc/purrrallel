
context("Testing MAP")


map_tests <- function(){
    test_that( "map yields expected results",{

        tfun <- function( x, y=0 , z=0){
            x + y + z
        }

        expect_equal(
            map( c(1,2,3) , tfun),
            purrr::map( c(1,2,3) , tfun)
        )

        expect_equal(
            map( c(1,2,3) , tfun , y=1),
            purrr::map( c(1,2,3) , tfun , y=1)
        )

        expect_equal(
            map( c(1,2,3) , tfun , y = c(1) , z = c(1,2,3)),
            purrr::map( c(1,2,3) , tfun , y = c(1), z = c(1,2,3))
        )

        ### Test that unused arugments cause errors
        expect_error(
            map( c(1,2,3) , tfun , 1,2,3),
            "unused argument"
        )

        expect_error(
            purrr::map( c(1,2,3) , tfun , 1,2,3),
            "unused argument"
        )

        tfun2 <- function(x , dat){
            return( dat[x,])
        }

        expect_equal(
            map( c(1,2,3) , tfun2 , iris),
            purrr::map( c(1,2,3) , tfun2 , iris)
        )


    })



    test_that( "map_dbl yields expected results",{
        tfun <- function( x, y=0 , z=0){
            x + y + z
        }

        expect_equal(
            map_dbl( c(1,2,3) , tfun),
            purrr::map_dbl( c(1,2,3) , tfun)
        )

        expect_equal(
            map_dbl( c(1,2,3) , tfun , y = 1),
            purrr::map_dbl( c(1,2,3) , tfun , y = 1)
        )

        expect_equal(
            map_dbl( c(1,2,3) , tfun , 1 , 2),
            purrr::map_dbl( c(1,2,3) , tfun , 1 , 2)
        )

        expect_error(
            map_dbl(c(1,2,3) , tfun , y = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_error(
            purrr::map_dbl(c(1,2,3) , tfun , y = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )


        expect_equal(
            map_dbl( T , function(x) x),
            purrr::map_dbl( T , function(x) x)
        )

        expect_error(
            map_dbl( "hi" , function(x) x),
            "Can't coerce element 1 from a character to a double"
        )

        expect_error(
            purrr::map_dbl( "hi" , function(x) x),
            "Can't coerce element 1 from a character to a double"
        )
    })



    test_that( "map_chr yields expected results",{

        tfun <- function( x, y=1 , z=1){
            paste0(
                LETTERS[x],
                letters[y],
                LETTERS[z]
            )
        }

        expect_equal(
            map_chr( c(1,2,3) , tfun),
            purrr::map_chr( c(1,2,3) , tfun)
        )

        expect_equal(
            map_chr( c(1,2,3) , tfun , y = 1),
            purrr::map_chr( c(1,2,3) , tfun , y = 1)
        )

        expect_equal(
            map_chr( c(1,2,3) , tfun , 1 , 2),
            purrr::map_chr( c(1,2,3) , tfun , 1 , 2)
        )

        expect_error(
            map_chr(c(1,2,3) , tfun , y = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_error(
            purrr::map_chr(c(1,2,3) , tfun , y = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )

        expect_equal(
            map_chr( 1 , function(x) x),
            purrr::map_chr( 1 , function(x) x)
        )

        expect_error(
            map_chr( list(list(a=1), list(b=2)) , identity),
            "Can't coerce element 1 from a list to a character"
        )

        expect_error(
            purrr::map_chr( list(list(a=1), list(b=2)) , identity),
            "Can't coerce element 1 from a list to a character"
        )


    })



    test_that( "map_lgl yields expected results",{

        tfun <- function( x, y=0 , z=0){
            c ( x < 2 & y < 2 & z < 2)
        }

        expect_equal(
            map_lgl( c(1,2,3) , tfun),
            purrr::map_lgl( c(1,2,3) , tfun)
        )

        expect_equal(
            map_lgl(  c(1,2,3) , tfun , y = 1),
            purrr::map_lgl( c(1,2,3) , tfun , y = 1)
        )

        expect_equal(
            map_lgl( c(1,2,3) , tfun , 0 , 3),
            purrr::map_lgl( c(1,2,3) , tfun , 0 , 3)
        )

        expect_error(
            map_lgl(c(1,2,3) , tfun , y = c(1,2)) ,
            "Result 1 is not a length 1 atomic vector"
        )

        expect_error(
            purrr::map_lgl(c(1,2,3) , tfun , y = c(1,2)),
            "Result 1 is not a length 1 atomic vector"
        )



    })


    test_that( "map_df yields expected results",{

        tfun <- function(x , dat , y = NULL){
            return( dat[c(x,y),])
        }

        expect_equal(
            map_df( c(1,2,3) , tfun , dat = iris),
            purrr::map_df( c(1,2,3) , tfun , dat = iris)
        )

        expect_equal(
            map_df( c(1,2,3) , tfun , dat = iris , y =1 ),
            purrr::map_df( c(1,2,3) , tfun , dat = iris , y =1 )
        )

        expect_equal(
            map_df( c(1,2,3) , tfun , dat = iris , y = c(1,2) ),
            purrr::map_df( c(1,2,3) , tfun , dat = iris , y =c(1,2) )
        )

        expect_equal(
            map_df( list(a= iris, b=iris) , function(x) x[,1]) ,
            purrr::map_df( list(a= iris, b=iris) , function(x) x[,1])
        )

        expect_equal(
            map_df( list(a= iris, b=iris) , function(x) x[1,]) ,
            purrr::map_df( list(a= iris, b=iris) , function(x) x[1,])
        )

    })
}






#### No cluster
map_tests()


#### Simple Parallel Cluster
cl <- makeCluster(2)
registerCluster(cl)

map_tests()
test_that ( "Parralellisation Works",{
    sleepy <- function(...) Sys.sleep(1)
    x <- system.time(map( c(1,2) , sleepy))
    y <- system.time(purrr::map( c(1,2) , sleepy))
    expect_lt( round(x[3]) , round(y[3]) - 0.5 )
})

stopCluster(cl)
registerCluster()



#### Block parallel Cluster
cl <- makeCluster(2)
registerCluster(cl, nblock = 2)

map_tests()
test_that ( "Parralellisation Works",{
    sleepy <- function(...) Sys.sleep(1)
    x <- system.time(map( c(1,2) , sleepy))
    y <- system.time(purrr::map( c(1,2) , sleepy))
    expect_lt( round(x[3]) , round(y[3]) -0.5 )
})

stopCluster(cl)
registerCluster()

















