# replace some spaces in a sentence by newlines to get a better looking
# cloud
strmultline <- function( strings, ratio= 0.2 ) {

  strings <- as.character( strings )
  n <- length( strings )
  splits <- strsplit( strings, "[[:space:]]" ) 

  for( i in 1:n ) {

    x <- splits[[i]]
    f <- family

    nw <- length( x )
    if( nw == 1 ) next

    sl <- sapply( x, nchar )
    r1 <- 1 / nchar( strings[i] )

    if( r1 > ratio ) next 

    r2s <- c()

    # try all possible splits. Surely there is a more effective approach,
    # feel free to modify the code.
    for( j in 1:(nw - 1) ) {

      str.n1 <- paste( x[1:j], collapse= " " )
      str.n2 <- paste( x[(j+1):nw], collapse= " " ) 

      r2 <- 2 / max( nchar( str.n1 ), nchar( str.n2 )  )

      r2s <- c( r2s, r2 )
    }

    j <- which.min( abs( r2s - ratio ) )

    str.n <- paste( paste( x[1:j], collapse= " " ), paste( x[(j+1):nw], collapse= " " ), sep= "\n" )
    strings[i] <- str.n 
  } 
 
  return( strings )
}
