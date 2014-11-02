\name{smoothPalette}
\alias{smoothPalette}
\title{
  Replace a vector of numbers by a gradient of colors
}
\description{
  Replace a vector of numbers by a vector of colors from a palette, such
  that values correspond to the colors on a smooth gradient.
}
\usage{
smoothPalette(x, pal= NULL, 
              max = NULL, min = NULL, n = 9, 
              palfunc= NULL, na.color = "white")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A numeric vector
}
  \item{max}{
  Values of \code{x} larger than \code{max} will be replaced by \code{max}
}
  \item{min}{
  Values of \code{x} smaller than \code{min} will be replaced by \code{min}
}
  \item{pal}{
  Character vector containing the color gradient onto which the numeric
  vector \code{x} will be mapped. By default, a gradient from white to
  black is generated. If it is a single character value, it will be treated
  as name of an RColorBrewer palette (see \code{\link{brewer.pal}}).
}
  \item{n}{
    Number of steps
  }
  \item{palfunc}{
    Palette function returned by colorRampPalette
  }
  \item{na.color}{
  NA values will be replaced by that color
}
}
\details{
  This function is used to map a continues numerical vector on an ordinal
  character vector, in especially a vector of colors. Color palette can be
  specified using an RColorBrewer palette name.
}
\value{
  A character vector of the same length as the numeric vector \code{x},
  containing the matching colors.
}
\author{
January Weiner <january.weiner@gmail.com>
}
\seealso{
\code{\link{tagcloud}}
}
\examples{
smoothPalette( 1:3 )
# will print:
# "#CCCCCC" "#666666" "#000000"

smoothPalette( 1:3, pal= "Blues" )
# will produce:
# "#F7FBFF" "#6BAED6" "#08306B"

x <- runif( 100 )
plot( 1:100, x, col= smoothPalette( x, pal= "BrBG" ), pch= 19 )
}
\keyword{ palette }
\keyword{ mapping }
