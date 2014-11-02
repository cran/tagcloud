\name{tagcloud}
\alias{tagcloud}
\alias{tagcloud-package}
\alias{tagcloud-class}
\alias{plot.tagcloud}
\alias{summary.tagcloud}
\alias{print.tagcloudsummary}
\alias{print.tagcloud}
\title{
Tag and Word Clouds
}
\description{
Functions to create and display plots called tag clouds, word clouds or weighted lists, in which a
usually large number of words is displayed in size correlated with a
numerical value (for example, frequency in a text or enrichment of a GO
term). This makes it easier to visualize the prominence of certain tags or
words. Also, it looks nice.
}

\usage{
tagcloud( tags, weights = 1, 
          algorithm = "oval", scale = "auto", 
          scale.multiplier = 1,
          order = "size", sel= NULL,
          wmin = NULL, wmax = NULL, floor = 1, ceiling = 3, 
          family = NULL, col = NULL, fvert = 0, 
          plot = TRUE, add = FALSE)

\method{plot}{tagcloud}( x, family = NULL, add = FALSE, with.box = FALSE, 
  col = NULL, sel = NULL, \dots )

\method{summary}{tagcloud}( object, ... )

\method{print}{tagcloud}( x, ... )

}

\arguments{
  \item{tags}{
    A character vector containing words or tags to be shown on the plot.
  }
  \item{weights}{
    A numeric vector giving the relative proportions of text size
    corresponding to the given tag.
  }
  \item{x,object}{
    An object of the type produced by tagcloud.
  }
  \item{\dots}{
  Further arguments to be passed to downstream methods.
  }
  \item{algorithm}{
    Name of the algorithm to use. Can be "oval", "fill", "random", "snake", 
    "list" or "clist". See Details.
  }
  \item{scale}{
    If "auto", text expansion will be calculated automatically to fit the
    available space. Otherwise, a numeric value used to modify the
    calculated text sizes; tune scale to achieve a better fit.
  }
  \item{scale.multiplier}{
    Multiplier for the final calculated text expansion parameter. Increase
    if there is too much empty space around the tag cloud; decrease if
    the tags go over the plot boundaries.
  }
  \item{order}{
    Determines in which order the tags will be drawn. Can be "size",
    "keep", "random", "height" or "width". See Details.
  }
  \item{sel}{
    An integer or boolean vector indicating which terms from the provided
    list will be used to plot. The vectors \code{col} and \code{weights}
    will be filtered accordingly.
  }
  \item{wmin}{
    All items in the \code{weights} vector smaller than \code{wmin} will be
    changed to \code{wmin}
  }
  \item{wmax}{
    All items in the \code{weights} vector larger than \code{wmax} will be
    changed to \code{wmax}
  }
  \item{floor}{
    Minimal text size. See Details.
  }
  \item{ceiling}{
    Maximal text size. See Details.
  }
  \item{family}{
    Font family to use, a vector containing font families to use for each
    tag. For the \code{tagcloud} function, the special keyword "random" can
    be used to assign random families (requires the extrafont package).
  }
  \item{col}{
    Color or a vector containing colors to use for drawing the tags.
  }
  \item{fvert}{
    Fraction of tags which will be rotated by 90 degrees counterclockwise.
  }
  \item{plot}{
    If FALSE, no plot will be produced. 
  }
  \item{add}{
    If TRUE, the tags will be added to the current plot instead of creating
    a new plot.
  }
  \item{with.box}{
    If TRUE, a rectangle will be plotted around each tag.
  }
}
\details{

  The package \code{tagcloud} creates and plots tag clouds (word clouds).
  The algorithms in the package have been designed specifically with long
  tags (such as GO Term descriptions) in mind.

\subsection{Term ordering}{
  The available arguments are as follows:

  \itemize{
    \item{size}{ -- tags are ordered by size, that is, their
  effective width multiplied by their effective height. Default.}
    \item{keep}{ -- keep the order from the list of words provided}
    \item{random}{ -- randomize the tag list}
    \item{width}{ -- order by effective screen width}
    \item{height}{ -- order by effective screen height}
  }

  By default, prior to plotting terms are ordered by size.

}

\subsection{Algorithms}{
  There are four algorithms for placing tags on the plot implemented in
  tagcloud.

  \itemize{
    \item{oval}{ -- creates an oval cloud.  }
    \item{fill}{ -- an attempt will be made to fill the available space}
    \item{random}{ -- randomly distribute tags over the available space.  This algorithm is slow and not very effective}
    \item{snake}{ -- tags are placed clockwise around the first tag to plot}
    \item{list}{ -- create a list, one tag directly beneath another, justified left}
    \item{clist}{ -- create a list, one tag directly beneath another,
    centered}
  }

  Algorithms \code{oval}, \code{fill} and \code{random} attempt to fill the
  available space by adjusting the scaling factor for the font sizes.
}

\subsection{Calculation of tag sizes}{
  Placing tags such that the empty space between the tags is minimized
  poses a non-trivial problem, because the effective bounding box of a
  displayed text is not linearly dependent on the \code{cex} parameter.

  In tagcloud, first a \code{cex} parameter is calculated for each tag
  separately, based on the parameters \code{floor}, \code{ceiling} and the
  vector of weights. Note that all weights smaller than \code{wmin} are
  replaced by \code{wmin} and all weights larger than \code{wmax} are
  replaced by \code{wmax}. Then, effective heights and widths of the tags
  to be displayed are calculated using the \code{\link{strwidth}} and
  \code{\link{strheight}} functions.

  Unless the argument \code{scale} is different from "auto", a scaling
  parameter for \code{cex} is automatically calculated based on the current
  area of the tags and the available plotting area. This usually results in
  a reasonable plot, but is neither guaranteed to occupy all of the
  available space without margins, nor that no tag will cross the view
  port.

}

}
\value{

  \code{tagcloud} returns an object of the \code{tagcloud-class}, which
  really is a data frame with the following columns: 

  \itemize{
    \item{\code{tags}}{ -- the tags, words or phrases shown on the plot}
    \item{\code{weights}}{ -- a numeric vector that is used to calculate the size of the plotted tags}
    \item{\code{family}}{ -- name of the font family to be used in plotting}
    \item{\code{vertical}}{ -- whether the tag should be rotated by 90 degrees counter-clockwise}
    \item{\code{x},\code{y}}{ -- coordinates of the left lower corner of the tags bounding box}
    \item{\code{w},\code{h}}{ -- width and height of the bounding box}
    \item{\code{cex}}{ -- text expansion factor, see \code{\link{par}}}
    \item{\code{s}}{ -- surface of the tag (width x height)}
  }

  The object of the \code{tagcloud} class can be manipulated using
  \code{\link{editor.tagcloud}} and displayed using \code{\link{plot}},
  \code{\link{print}} and \code{\link{summary}} functions.
}
\author{
January Weiner <january.weiner@gmail.com>
}

\note{
Care should be taken when using extra fonts loaded by the extrafont
package; not all fonts can be easily copied to a PDF file.

Some ideas in this package are based on the `wordcloud` package by Ian Fellows.
}

\seealso{
  \code{\link{editor.tagcloud}} -- interactive editing of tagcloud objects.

  \code{\link{strmultline}} -- splitting multi-word sentences into lines
  for a better cloud display.

  \code{\link{smoothPalette}} -- mapping values onto a color gradient.
}
\examples{

# a rather boring tag cloud
data( gambia )
terms <- gambia$Term
tagcloud( terms )

# tag cloud with weights relative to P value
# colors relative to odds ratio, from light
# grey to black
weights <- -log( gambia$Pvalue )
colors  <- smoothPalette( gambia$OddsRatio, max=4 )
tagcloud( terms, weights, col= colors, algorithm= "oval" )

# tag cloud filling the whole plot
tagcloud( terms, weights, col= colors, algorithm= "fill" )

# just a list of only the first ten terms
tagcloud( terms, weights, sel= 1:10,
          col= colors, algorithm= "list", order= "width" )

# oval, with line breaks in terms
terms <- strmultline( gambia$Term )
tagcloud( terms, weights, col= colors, algorithm= "oval" )

\dontrun{
# shows available font families, scaled according to
# the total disk space occupied by the fonts
require( extrafont )
ft <- fonttable()
fi <- file.info( fonttable()$fontfile )
families <- unique( ft$FamilyName )
sizes    <- sapply( families,function( x ) sum( fi[ ft$FamilyName == x, "size" ] ) )
tagcloud( families, sizes, family= families )
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ tags }
\keyword{ word cloud }
\keyword{ weighted list }
\keyword{ tag cloud }
