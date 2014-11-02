\name{editor.tagcloud}
\alias{editor.tagcloud}
\title{
Simple interactive editing of tag clouds
}
\description{
   A minimalistic editor for object of the tagcloud class. 
}
\usage{
editor.tagcloud( boxes )
}
\arguments{
  \item{boxes}{
  An object of the tagcloud class, returned by the \code{\link{tagcloud}}
  function.
}
}
\details{
  \code{tagcloud} provides a minimalistic editor for tag clouds produced by
  \code{\link{tagcloud}} function. After \code{editor.tagcloud} is called,
  the tag cloud is plotted. First click selects the tag to be moved. The
  second click sends the tag such that its left lower corner is at the
  position indicated by the mouse. Right-clicking terminates the program.
}
\value{
  An object of the \code{tagcloud class} with the modified
  positions of the tags.
}
\author{
January Weiner <january.weiner@gmail.com>
}

\seealso{
\code{\link{tagcloud}}
}
\examples{
\dontrun{
data( gambia )
terms <- gambia$Term
tagcloud( terms )
boxes <- editor.tagcloud( boxes )

}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ tags }
\keyword{ tag clouds }
\keyword{ editing }
