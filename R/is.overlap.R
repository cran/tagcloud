is.overlap   <- function( box, boxes ) .Call( "is_overlap", box, boxes )
row.overlap  <- function( row, boxes ) .Call( "row_overlap",  row, boxes )
any.overlap  <- function( boxes )      .Call( "any_overlap",  boxes )
all.overlaps <- function( boxes )      .Call( "all_overlaps", boxes )

run.ulam   <- function( params, boxes ) .Call( "ulam", params, boxes )
run.spiral <- function( params, boxes ) .Call( "spiral", params, boxes )



#Call down to c++ to find out if any overplotting would occur
.overlap <- function(x11,y11,sw11,sh11,boxes1){
    .Call("is_overlap",x11,y11,sw11,sh11,boxes1)
}

