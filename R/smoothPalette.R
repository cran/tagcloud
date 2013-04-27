




# turns a vector of numbers into a vector of colors
smoothPalette <- function( x, pal= NULL, max= NULL, min= NULL, n= 9, na.color= "white" ) {



  if( missing( pal ) ){
    pal <- colorRampPalette( c( "#cccccc", "black" ) )( n )
  } else {
    if( length( pal ) == 1 ) {
      pal <- try( brewer.pal( n, pal ), silent= TRUE )
      if( class( pal ) == "try-error" ) 
        stop( "palette is neither a vector nor a name of a RColorBrewer palette" )
    }
  }
  
  nas <- which( is.na( x ) )

  x[ nas ] <- mean( x, na.rm= T )

  if( missing( max ) ) max <- max( x )
  else x[ x >= max ] <- max

  if( missing( min ) ) min <- min( x )
  else x[ x <= min ] <- min

  ret <- findInterval( x, seq( min, max, length.out= n + 1 ), rightmost.closed= T )
  ret <- pal[ ret ]
  ret[ nas ] <- na.color
  return( ret )
}
