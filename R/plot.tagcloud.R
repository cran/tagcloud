# plots an object of the tagcloud class (or another suitable data frame)
plot.tagcloud <- function( x, family= NULL, add= FALSE, with.box= FALSE, col= NULL, ... ) {

  boxes <- x

  xr <-c( 0, 0 )
  yr <-c( 0, 0 )

  usr <- par( "usr" )

  if( ! add ) {
    plot.new()
    old.par <- par( mar= c( 0, 0, 0, 0 ) )
    plot.window( xlim= c( 0, 1 ), ylim= c( 0, 1 ), asp= 1, ... )
  }

  if( !missing( family ) ) boxes$family <- family
  if( !missing( col )    ) boxes$color  <- col
  
  for( i in 1:nrow( boxes ) ) {
    if( with.box )
      rect( boxes[i, "x" ], boxes[i, "y" ], 
      boxes[i, "x" ] + boxes[i, "w" ], boxes[i, "y" ] + boxes[i, "h" ] ) ;

    srt <- boxes[i,"vertical"] * 90
    text( boxes[i,"x"] + boxes[i,"w"]/2, 
          boxes[i,"y"] + boxes[i,"h"]/2, 
          boxes$tags[i], cex= boxes[i,"cex"], family= boxes$family[i], 
          srt= srt, col= boxes$color[i] )
  }

  if( ! add ) par( old.par )
}


