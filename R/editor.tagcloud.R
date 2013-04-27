# minimalistic "editor" to allow interactive moving of tags
editor.tagcloud <- function( boxes ) {

  plot.tagcloud( boxes, with.box= T )
  nstep <- 10

  while( 1 ) {
    xvec <- as.vector( sapply( 1:nrow( boxes ), function( x ) seq( boxes[x,"x"], boxes[x,"x"] + boxes[x,"w"], length.out= nstep ) )  )
    yvec <- as.vector( sapply( 1:nrow( boxes ), function( x ) seq( boxes[x,"y"], boxes[x,"y"] + boxes[x,"h"], length.out= nstep ) )  )
    catf( "Please click on the label you want to move\n" )
    catf( "(right-click to finish)\n" )
    i <- identify( xvec, yvec, n= 1, plot= F )
    if( length( i ) == 0 ) break
    i <- as.integer( i / nstep ) + 1
    if( length( i ) == 0 ) break
    catf( "Please click on the new position for:\n" )
    catf( "%s\n", boxes$tags[i] )
    xy <- locator( 1 )
    debugpr( xy )
    boxes[i,"x"] <- xy[1]
    boxes[i,"y"] <- xy[2]
    plot( boxes, with.box= T )
  }
  plot( boxes )
  return( invisible( boxes ) )
}


