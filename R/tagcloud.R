tagcloud.debug <- FALSE
#tagcloud.debug <- TRUE
debugprf <- function( ... ) if( tagcloud.debug ) printf( ... ) 
debugpr  <- function( ... ) if( tagcloud.debug ) print( ... )

# calculate cex from floor, ceiling and value vector
calc.cex <- function( x, floor, ceiling, wmin= NULL, wmax= NULL ) {

  if( is.null( wmin ) ) wmin <- min( x )
  if( is.null( wmax ) ) wmax <- max( x )

  ret <- (floor + (ceiling - floor) * (x - wmin) / wmax) 
  ret
}

calc.sizes <- function( tags, boxes, family ) {

  if( length( family ) < 2 ) family <- rep( family, length( tags ) )

  for( i in 1:length( tags ) ) {
    boxes[ i, "w" ] <- strwidth( paste( "X", tags[i], sep= "" ), cex= boxes[i, "cex"], family= family[ i ] )
    boxes[ i, "h" ] <- 1.35 * strheight( tags[i], cex= boxes[i, "cex"], family= family[ i ] )
  }
  boxes[ , "s" ] <- boxes[ , "w" ] * boxes[ , "h" ]

  boxes
}


.auto.scale <- function( boxes ) {

  usr <- par( "usr" )
  plot.surface <- ( usr[2] - usr[1] ) * ( usr[4] - usr[3] )
  box.surface  <- sum( boxes[,"s"] )
  scale <- sqrt( 0.4 * plot.surface / box.surface )
  if( nrow( boxes ) < 20 ) scale <- scale * 0.8
  debugprf( "box=%.2f plot=%.2f ratio=%.2f scale=%.2f",
    box.surface, plot.surface, plot.surface / box.surface, scale )
  return( scale )
}

# reduce space between non-overlapping boxes
.squeeze.boxes <- function( boxes ) {

  c <- .box.center( boxes )
  n <- nrow( boxes )

  if( tagcloud.debug ) {
    debugprf( "before: bb: %.2f, %.2f -> %.2f, %.2f", 
      min( boxes[,"x"] ), min( boxes[,"y"] ), max( boxes[,"x"] + boxes[,"w"] ), max( boxes[,"y"] + boxes[,"h"] ) )
    debugprf( "surface index before squeezing: %.3f", boxes.ratio( boxes ) )
  }

  for( nn in 1:25 ) {
    ydist <- boxes[,"y"] + boxes[,"h"] / 2 - c[2]
    ord.y <- order( abs( ydist ) )

    for( o in 2:length( ord.y ) ) {
      b <- ord.y[o]
      y.step <- ydist[b] / 15 
      
      for( i in 1:20 ) {
        if( is.overlap( c( boxes[b,"x"], boxes[b,"y"] - y.step, boxes[b,"w"], boxes[b,"h"] ), boxes[-b,,drop=F] ) ) break ;
        boxes[b,"y"] <- boxes[b,"y"] - y.step
      }
    }
  }

  for( nn in 1:25 ) {
    xdist <- boxes[,"x"] + boxes[,"w"] / 2 - c[1]
    ord.x <- order( abs( xdist ) )
    for( o in 2:length( ord.x ) ) {
      b <- ord.x[o]
      x.step <- xdist[b] / 150
      for( i in 1:20 ) {
        if( is.overlap( c( boxes[b,"x"] - x.step, boxes[b,"y"], boxes[b,"w"], boxes[b,"h"] ), boxes[-b,,drop=F] ) ) break ;
        boxes[b,"x"] <- boxes[b,"x"] - x.step
      }
    }
  }


  if( tagcloud.debug ) {
    debugprf( "surface index before squeezing: %.3f", boxes.ratio( boxes ) )
    debugprf( "after: bb: %.2f, %.2f -> %.2f, %.2f", 
      min( boxes[,"x"] ), min( boxes[,"y"] ), max( boxes[,"x"] + boxes[,"w"] ), max( boxes[,"y"] + boxes[,"h"] ) )
  }

  return( boxes )
}

.box.center <- function( boxes ) {

  x.min <- min( boxes[,"x" ] )
  y.min <- min( boxes[,"y" ] )
  x.max <- max( boxes[,"x" ] + boxes[,"w"])
  y.max <- max( boxes[,"y" ] + boxes[,"h"])
  
  xc <- x.min + ( x.max - x.min ) / 2 
  yc <- y.min + ( y.max - y.min ) / 2 

  return( c( xc, yc ) )

}

.center.boxes <- function( boxes ) {
  usr <- par( "usr" )

  x0 <- usr[1] + ( usr[2] - usr[1] ) / 2
  y0 <- usr[3] + ( usr[4] - usr[3] ) / 2

  c <- .box.center( boxes )

  boxes[,"x"] <- boxes[,"x"] - c[1] + x0
  boxes[,"y"] <- boxes[,"y"] - c[2] + y0
  return( boxes )
}

.get.min.scale <- function( boxes ) {
  
  tot.w <- max( boxes[,"x"] + boxes[,"w"] ) - min( boxes[,"x"] ) 
  tot.h <- max( boxes[,"y"] + boxes[,"h"] ) - min( boxes[,"y"] ) 

  usr <- par( "usr" )

  scale.w <- ( usr[2] - usr[1] ) / tot.w
  scale.h <- ( usr[4] - usr[3] ) / tot.h

  debugprf( "tot.w= %.2f, usr.w= %.2f, scale.w=%.2f", tot.w, usr[2] - usr[1], scale.w )
  debugprf( "tot.h= %.2f, usr.h= %.2f, scale.h=%.2f", tot.h, usr[4] - usr[3], scale.h )

  return( min( scale.w, scale.h ) )

}

.fit.boxes <- function( tags, boxes, vertical, family ) {
  stop()

  debugprf( "*********************************" )
  debugprf( "initial surface index: %.3f", boxes.ratio( boxes ) )
  c <- .box.center( boxes )
  scale <- .get.min.scale( boxes )
  debugprf( "scale=%.2f", scale )
  if( any.overlap( boxes ) ) stop( "boxes overlap" )

  max.iter <- 20
  f1 <- 1
  f2 <- 0
  f  <- 1
  scale.new <- -99
  if( scale < 1 ) stop( "not yet" )
  while( max.iter > 0 ) {

    debugprf( "iter=%d f1=%.2f f2=%.2f f=%.2f scale=%.2f", max.iter, f1, f2, f, scale.new )

    if( f2 == 0 ) f <- f * 2
    else          f <- ( f2 + f1 ) / 2
    debugprf( "        f1=%.2f f2=%.2f f=%.2f scale=%.2f", f1, f2, f, scale.new )

    boxes.tmp <- boxes
    boxes.tmp[, "cex"] <- boxes.tmp[,"cex"] * f
    boxes.tmp <- calc.sizes( tags, boxes.tmp, family )
    boxes.tmp[ vertical, c( "w", "h" ) ] <- boxes.tmp[ vertical, c( "h", "w" ) ]
    scale.new <- max( c( boxes.tmp[,"w"] / boxes[,"w"], boxes.tmp[,"h"] / boxes[,"h"] ) )
    boxes.tmp[,"x"]   <- c[1] + ( boxes.tmp[,"x"] - c[1] ) * scale.new
    boxes.tmp[,"y"]   <- c[2] + ( boxes.tmp[,"y"] - c[2] ) * scale.new

    if( scale.new > scale ) {
      debugprf( "** over scale" )
      f2 <- f
    } else {
      debugprf( "** under scale" )
      f1 <- f
    }


    max.iter <- max.iter - 1 
  }

  boxes.orig <- boxes
  boxes[, "cex"] <- boxes[,"cex"] * f1
  boxes <- calc.sizes( tags, boxes, family )
  boxes[ vertical, c( "w", "h" ) ] <- boxes[ vertical, c( "h", "w" ) ]
  scale.new <- max( c( boxes[,"w"] / boxes.orig[,"w"], boxes[,"h"] / boxes.orig[,"h"] ) )
  boxes[,"x"]   <- c[1] + ( boxes[,"x"] - c[1] ) * scale.new
  boxes[,"y"]   <- c[2] + ( boxes[,"y"] - c[2] ) * scale.new

  return( boxes )
}

.fit.boxes.old <- function( tags, boxes, vertical, family ) {

  debugprf( "*********************************" )
  debugprf( "initial surface index: %.3f", boxes.ratio( boxes ) )
  c <- .box.center( boxes )
  scale <- .get.min.scale( boxes )
  if( any.overlap( boxes ) ) stop( "boxes overlap" )

  if( scale < 1 | scale > 1.05 ) {
    if( scale < 1 ) debugprf( "downscaling %.2f", scale )
    else              debugprf( "upscaling %.2f", scale )

    boxes.orig <- boxes
    boxes[,"x"]   <- c[1] + ( boxes[,"x"] - c[1] ) * scale
    boxes[,"y"]   <- c[2] + ( boxes[,"y"] - c[2] ) * scale
    boxes <- calc.sizes( tags, boxes, family )
    boxes[ vertical, c( "w", "h" ) ] <- boxes[ vertical, c( "h", "w" ) ]

    f1 <- 0
    f2 <- 0

    boxes.tmp <- boxes

    if( any.overlap( boxes.tmp ) ) {
      f2 <- 1
    } else {
      f1 <- 1 
    }


    # find a factor for which we overlap
    if( f2 == 0 ) {
      f2 <- 1
      while( ! any.overlap( boxes.tmp ) ) {
        f2 <- f2 * 2
        boxes.tmp <- boxes
        boxes.tmp[ , "cex" ] <- f2 * boxes.tmp[ , "cex" ]
        boxes.tmp <- calc.sizes( tags, boxes.tmp, family )
        boxes.tmp[ vertical, c( "w", "h" ) ] <- boxes.tmp[ vertical, c( "h", "w" ) ]
      }
    }

    boxes.tmp <- boxes

    # find a factor for which we do not overlap
    if( f1 == 0 ) {
      f1 <- 1
      while( any.overlap( boxes.tmp ) ) {
        f1 <- f1 / 2
        boxes.tmp <- boxes
        boxes.tmp[ , "cex" ] <- f1 * boxes.tmp[ , "cex" ]
        boxes.tmp <- calc.sizes( tags, boxes.tmp, family )
        boxes.tmp[ vertical, c( "w", "h" ) ] <- boxes.tmp[ vertical, c( "h", "w" ) ]
      }
    }

    debugprf( "f1= %.2f, f2= %.2f", f1, f2 )

    # problem is, strwidth does not increase linearly with cex, so we need
    # a try and see approach to find a cex vector that fits the new boxes best

    max.tries <- 5

    while( max.tries > 0 ) {
      fn <- ( f1 + f2 ) / 2
      boxes.tmp <- boxes
      boxes.tmp[ , "cex" ] <- fn * boxes.tmp[ , "cex" ]
      boxes.tmp <- calc.sizes( tags, boxes.tmp, family )
      boxes.tmp[ vertical, c( "w", "h" ) ] <- boxes.tmp[ vertical, c( "h", "w" ) ]

      if( any.overlap( boxes.tmp ) ) f2 <- fn
      else                           f1 <- fn
      debugprf( "tries= %d, f1= %.2f, f2= %.2f", max.tries, f1, f2 )
      if( f2 - f1 < 0.05 * f1 ) break ;
      max.tries <- max.tries - 1
    }

    debugprf( "optimal f: %.2f", f1 )
    boxes[,"cex"] <- boxes[,"cex"] * f1
    boxes <- calc.sizes( tags, boxes, family )
    boxes[ vertical, c( "w", "h" ) ] <- boxes[ vertical, c( "h", "w" ) ]
    debugprf( "intermediate surface index: %.3f", boxes.ratio( boxes ) )

    print( range( boxes[,"w"] / boxes.orig[,"w"] ) )
    print( range( boxes[,"h"] / boxes.orig[,"h"] ) )

    if( scale > 1 ) { # upscaling
      scale.new <- max( c( boxes[,"w"] / boxes.orig[,"w"], boxes[,"h"] / boxes.orig[,"h"] ) )
      scale.new <- min( scale.new, scale )
      debugprf( "scale new= %.2f", scale.new )
      boxes[,"x"]   <- c[1] + ( boxes.orig[,"x"] - c[1] ) * scale.new
      boxes[,"y"]   <- c[2] + ( boxes.orig[,"y"] - c[2] ) * scale.new
    }

  }

  debugprf( "final surface index: %.3f", boxes.ratio( boxes ) )
  boxes <- .center.boxes( boxes )
  scale <- .get.min.scale( boxes )
  return( boxes )
}


boxes.width  <- function( boxes ) max( boxes[,"x"] + boxes[,"w"] ) - min( boxes[,"x"] ) 
boxes.height <- function( boxes ) max( boxes[,"y"] + boxes[,"h"] ) - min( boxes[,"y"] ) 

boxes.ratio <- function( boxes ) {

  tot.surface <- boxes.width( boxes ) * boxes.height( boxes )
  tag.surface <- sum( boxes[,"w"] * boxes[,"h"] )

  tag.surface / tot.surface
}

auto.wh <- function( boxes, mult= 2 ) {
  ds <- dev.size()
  aspect <- ds[2] / ds[1]
  w <- sum( boxes[, "w" ] ) / mult
  h <- sum( boxes[, "h" ] ) / mult
  if( h > w ) w <- h / aspect 
  else        h <- w * aspect 
  ret <- c( w, h, aspect )
  names( ret ) <- c( "w", "h", "aspect" )
  return( ret )
}


# fit using normal distribution
algorithm.normal <- function( boxes ) {

  i <- 20 
  while( i > 0 ) {
    boxes[,"x"] <- rnorm( nrow( boxes ), sd= 0.1 ) - boxes[,"w"]/2 
    boxes[,"y"] <- rnorm( nrow( boxes ), sd= 150 )
    i <- i - 1
    if( ! any.overlap( boxes ) ) {
      debugprf( "not bad!" )
      break ;
    }
  }

  boxes <- .squeeze.boxes( boxes )
  boxes <- .center.boxes( boxes )

  return( boxes )
}


# attempt a uniform distribution 
algorithm.random <- function( boxes ) {

  wh <- auto.wh( boxes )
  debugpr( wh )
  w <- wh[ "w" ]
  h <- wh[ "h" ]
  # this is our target aspect
  aspect <- wh[ "aspect" ]

  debugprf( "w=%.2f, h=%.2f, aspect= %.2f", w, h, wh[ "aspect" ] )

  boxes.new <- NULL

  for( i in 1:nrow( boxes ) ) {
    # how many tries
    n <- 100

    overlap <- TRUE
    while( overlap & n > 0 ) {
      x <- runif( 1, min= 0, max= w - boxes[i,"w"] )
      y <- runif( 1, min= 0, max= h - boxes[i,"h"] )
      if( i == 1 ) {
        overlap <- FALSE
        break 
      }

      if( ! any.overlap( rbind( boxes.new[,c( "x", "y", "w", "h")], c( x, y, boxes[i, "w"], boxes[i, "h" ] ) ) ) ) 
        overlap <- FALSE

      n <- n - 1
    }

    if( ! overlap ) {
      boxes[i,"x"] <- x
      boxes[i,"y"] <- y
      boxes.new <- rbind( boxes.new, boxes[i,c("x","y","w","h")] )
    } else {
      debugprf( "problems with word %d", i )
      stop( "Failed attempt! Try again" )
    }

  }

  boxes <- .squeeze.boxes( boxes )

  debugprf( "surface index before optimization: %.3f", boxes.ratio( boxes ) )
  debugprf( "total width before optimization: %.2f", boxes.width( boxes ) )
  debugprf( "total height before optimization: %.2f", boxes.height( boxes ) )
  debugprf( "current aspect: %.2f", boxes.height( boxes ) / boxes.width( boxes ) )

  for( loop in 1:20 ) {

    a <- boxes.height( boxes ) / boxes.width( boxes )
    debugprf( "loop= %d aspect= %.2f", loop, a )
    c <- .box.center( boxes )

    if( a < aspect ) {
      i <- which.max( sapply( 1:nrow( boxes ), function( x ) max( c[1] - boxes[x,"x"], c[1] - ( boxes[x,"x"] + boxes[x,"w"] ) ) ) )
    } else {
      i <- which.max( sapply( 1:nrow( boxes ), function( x ) max( c[2] - boxes[x,"y"], c[2] - ( boxes[x,"y"] + boxes[x,"h"] ) ) ) )
    }

    debugprf( "moving %d", i )
    c.2 <- .box.center( boxes[-i,] )
    w.2 <- boxes.width( boxes[-i,] )
    h.2 <- boxes.height( boxes[-i,] )
    a.2 <- h.2 / w.2
    debugprf( "remaining aspect: %.2f", a.2 )

    # make sure width is at least width of the i box, plus something
    if( w.2 < 1.1 * boxes[i, "w"] ) w.2 <- 1.1 * boxes[i, "w" ]
    if( h.2 < 1.1 * boxes[i, "h"] ) h.2 <- 1.1 * boxes[i, "h" ]

    # expand the area to get the correct aspect
    if( a.2 < aspect ) h.2 <- w.2 * aspect
    else               w.2 <- h.2 / aspect

    for( attempt in 1:5 ) {
      x <- runif( 1, min= c.2[1] - w.2 / 2, max= c.2[1] + w.2 / 2 )
      y <- runif( 1, min= c.2[2] - h.2 / 2, max= c.2[2] + h.2 / 2 )
      box.tmp <- rbind( boxes[-i, c( "x", "y", "w", "h" )], c( x, y, boxes[i, "w"], boxes[i, "h" ] ) )
      if( ! any.overlap( box.tmp ) ) {
        debugprf( "hah! (%d)", attempt )
        boxes[i,"x"] <- x
        boxes[i,"y"] <- y
        break ;
      }
    }
    boxes <- .squeeze.boxes( boxes )
  }

  debugprf( "surface index after optimization: %.3f", boxes.ratio( boxes ) )
  debugprf( "total width after optimization: %.2f", boxes.width( boxes ) )
  debugprf( "total height after optimization: %.2f", boxes.height( boxes ) )
  debugprf( "aspect after optimization: %.2f", boxes.height( boxes ) / boxes.width( boxes ) )

  #boxes <- .squeeze.boxes( boxes )
  boxes <- .center.boxes( boxes )
  return( boxes )
}


algorithm.list <- function( boxes, centered= F ) {

  if( centered ) {
    boxes[,"x"] <- 0 - boxes[,"w"] / 2 
  } else {
    boxes[,"x"] <- 0
  }

  boxes[1,"y"] <- 0

  for( i in 2:nrow( boxes ) ) {
    boxes[i,"y"] <- boxes[i-1,"y"] - 1.05 * boxes[i,"h"]
  }

  boxes <- .center.boxes( boxes )
  return( boxes )
}


algorithm.ulam <- function( boxes ) {

  debugprf( "*** algorithm ulam" )

  wh <- auto.wh( boxes )
  w  <- wh[ "w" ]
  h  <- wh[ "h" ]
  aspect <- wh[ "aspect" ]

  max.r <- 10 * sqrt( w^2 + h^2 ) 
  debugprf( "w=%.2f, h=%.2f, aspect=%.2f, max.r=%.2f", w, h, aspect, max.r )

  r.step <- min( boxes[,"w"] ) / 15

  boxes[1,"x"] <- 0 - boxes[1,"w"]/2
  boxes[1,"y"] <- 0 - boxes[1,"h"]/2
  boxes.new <- boxes[1,,drop=F]

  for( i in 2:nrow( boxes ) ) {
    catf( "\rCalculating, %d %% done", as.integer( 100 * i / nrow( boxes ) ) )
    r <- r.step
    x <- 0 - boxes[i,"w"]/2
    y <- 0 - boxes[i,"h"]/2

    # random initial direction
    d.r <- 0
    dir <- sample( 1:8, 1 )
    dir <- c( 0, 1, -1, 0, 0, -1, 1, 0,
              0,-1,  1, 0, 0,  1,-1, 0 )[(dir*2-1):(dir*2)]

    overlap <- TRUE

    params <- list( x=x, y=y, w=boxes[i,"w"], h=boxes[i,"h"],
                    dir1=dir[1], dir2=dir[2], rstep= r.step,
                    aspect= aspect, maxr= max.r, max.iter= 2000000 ) 

    test <- run.ulam( params, boxes[1:(i-1),c("x","y","w","h"),drop=F] ) 

    boxes[i,"x"] <- test[1]
    boxes[i,"y"] <- test[2]
    if( any.overlap( boxes[1:i,] ) ) stop() ;
  }

  catf( "\n" )
  boxes <- .center.boxes( boxes )
  return( boxes )
}



# spiral algorithm, like in wordle

algorithm.spiral <- function( boxes ) {

  debugprf( "*** algorithm spiral" )
  wh <- auto.wh( boxes )
  w  <- wh[ "w" ]
  h  <- wh[ "h" ]
  aspect <- wh[ "aspect" ]

  max.r <- 10 * sqrt( w^2 + h^2 ) 
  debugprf( "w=%.2f, h=%.2f, aspect=%.2f, max.r=%.2f", w, h, aspect, max.r )

  boxes.new <- boxes[1,]

  r.step <- min( boxes[,"h"] ) / 15
  a.step <- pi / 90

  boxes[1,"x"] <- 0 - boxes[1,"w"]/2
  boxes[1,"y"] <- 0 - boxes[1,"h"]/2
  boxes.new <- boxes[1,,drop=F]

  dir <- 1

  for( i in 2:nrow( boxes ) ) {
    if( runif( 1 ) < 0.5 ) dir <- dir * -1 
    catf( "\rCalculating, %d %% done", as.integer( 100 * i / nrow( boxes ) ) )
    angle  <- runif( 1, 0, 2 * pi )
    r <- mean( boxes[,"w"] / 3 )
    overlap <- TRUE
    x <- 0 - boxes[i,"w"]/2
    y <- 0 - boxes[i,"h"]/2

    params <- list( x=x, y=y, w=boxes[i,"w"], h=boxes[i,"h"], r= r,
                    dir=dir, rstep= r.step, angle= angle, astep= a.step,
                    aspect= aspect, maxr= max.r, max.iter= 2000000 ) 

    test <- run.spiral( params, boxes[1:(i-1),c("x","y","w","h"),drop=F] ) 

    boxes[i,"x"] <- test[1]
    boxes[i,"y"] <- test[2]
    if( any.overlap( boxes[1:i,] ) ) {
      printf( "overlap detected, i=%d", i )
      print( boxes[1:i,] ) 
      stop() 
    }
  }

  catf( "\n" )
  boxes <- .center.boxes( boxes )
  return( boxes )
}


tagcloud <- function( tags, weights= 1, 
  algorithm= "oval", scale= "auto",
  order= "size", sel= NULL,
  wmin= NULL, wmax= NULL, floor= 1, ceiling= 3, 
  family= NULL, col= NULL, 
  fvert= 0,
  plot= TRUE, add= FALSE
   ) {

  tags <- as.character( tags )
  n.tags <- length( tags )
  meta <- data.frame( tags= tags )
  meta$weights <- weights
  meta$family  <- family
  meta$colors  <- col
  boxes <- matrix( 0, nrow= n.tags, ncol= 6 )
  colnames( boxes ) <- c( "x", "y", "w", "h", "cex", "s" )

  if( ! missing( family ) && length( family ) == 1 && family == "random" ) {
    require( extrafont )
    meta$family <- sample( extrafont::fonts(), n.tags )
  }

  if( ! missing( sel ) ) {
    meta <- meta[ sel, ] 
    boxes <- boxes[ sel, ]
    n.tags <- nrow( boxes ) 
  }

  if( plot & ! add ) {
    plot.new()
    old.par <- par( mar= c( 0, 0, 0, 0 ) )
    plot.window( xlim= c( 0, 1 ), ylim= c( 0, 1 ), asp= 1 )
  }

  boxes[, "cex"] <- calc.cex( meta$weights, floor, ceiling, wmin= wmin, wmax= wmax )
  boxes <- calc.sizes( meta$tags, boxes, meta$family )

  if( scale == "auto" ) scale <- .auto.scale( boxes )

  boxes[,"cex"] <- boxes[,"cex"] * scale
  boxes <- calc.sizes( meta$tags, boxes, meta$family )

  meta$vertical <- rep( FALSE, n.tags )
  if( length( fvert ) > 1 ) {
    meta$vertical <- fvert
  } else if( fvert > 0 ) {
    n <- as.integer( n.tags * fvert )
    #meta$vertical[ order( boxes[,"w"] )[ 1:n ] ] <- TRUE 
    meta$vertical[ runif( n.tags, 0, 1 ) < fvert ] <- TRUE
  }

  order <- match.arg( order, c( "size", "keep", "random", "width", "height" ) )
  order <- switch( order,
    keep= 1:nrow( boxes ),
    random= sample( 1:nrow( boxes ) ),
    size= order( boxes[,"s"], decreasing= T ),
    width= order( boxes[,"w"], decreasing= T ),
    height= order( boxes[,"h"], decreasing= T )
     )

  meta <- meta[ order, ]
  boxes <- boxes[ order, ]

  # filter out invisible tags
  sel <- boxes[,"h"] < 1e-6 | boxes[,"w"] < 1e-6
  meta <- meta[ ! sel, ]
  boxes <- boxes[ ! sel, ]


  tmp <- boxes[ meta$vertical, "w" ]
  boxes[ meta$vertical, "w" ] <- boxes[ meta$vertical, "h" ]
  boxes[ meta$vertical, "h" ] <- tmp

  algorithm <- match.arg( algorithm, 
    c( "oval", "fill", "random", "list", "clist" ) )

  boxes <- switch( algorithm,
      oval=algorithm.spiral( boxes ),
      normal=algorithm.normal( boxes ),
      fill=algorithm.ulam( boxes ),
      random=algorithm.random( boxes ),
      clist=algorithm.list( boxes, centered= TRUE ), 
      list=algorithm.list( boxes ) )

  #boxes <- .fit.boxes( meta$tags, boxes, meta$vertical, meta$family )
  #boxes <- .squeeze.boxes( boxes )
  debugprf( "final surface index: %.3f", boxes.ratio( boxes ) )

  boxes <- as.data.frame( boxes )
  boxes <- cbind( meta, boxes )
  class( boxes ) <- c( "tagcloud", class( boxes ) )
  attr( boxes, "algorithm" ) <- algorithm
  attr( boxes, "scale" ) <- scale

  if( plot )  {
    debugprf( "plotting" )
    if( tagcloud.debug ) 
      plot( boxes, add= T, with.box= T )
    else 
      plot( boxes, add= T )
    if( ! add ) par( old.par ) 
  }

  return( invisible( boxes ))

}
