\name{strmultline}
\alias{strmultline}
\title{
Replace some spaces in multi-word sentences by newlines
}
\description{
  Replace a space character by a newline in a multi-word sentence to get a
  better height / width ratio

}
\usage{
strmultline(strings, ratio = 0.2)
}
\arguments{
  \item{strings}{
    a character vector containing the multi-word sentences to be split
}
  \item{ratio}{
    the desired ratio height / width
}
}
\details{
  Very long tags, for example GO Term descriptions, make a bad tag cloud.
  \code{strmultline} tries to chop up such a long sentence into multiple
  (currently two) lines, to get a better height / width ratio.

}
\value{
  A character vector containing the modified sentences.
}
\author{
January Weiner <january.weiner@gmail.com>
}
\seealso{
  \code{\link{tagcloud}}
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ strings }
\keyword{ splitting }% __ONLY ONE__ keyword per line
