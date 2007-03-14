\name{gainr}
\alias{gainr}
\title{Computes information theoretic gain ratio}
\description{
  A function that computes the information theoretic gain ratio.
}
\usage{
gainr(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{attribute values}
  \item{b}{attribute values}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns the gain ratio.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{gain}}}
\examples{
a <- round(runif(10))
b <- round(runif(10))
gain(a,b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

