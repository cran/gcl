\name{cindex}
\alias{cindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the c-index}
\description{
  The C-index is the probability of the classifier
  ranking a randomly chosen positive example as more positive than a
  randomly chosen negative example. It is related to the Wilcoxon rank
  sum statistic or Mann-Whitney U. It is equivalent to the area under
  the receiver operating curve.
}
\usage{
cindex(preds, acts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{preds}{A vector of predictions}
  \item{acts}{A vector of the actual classes. Must be 0 or 1.}
}
%\details{
%  
%}
\value{
  The c-index.
}
\references{J.A. Hanley and B.J. McNeil. The meaning and use of the area under a receiver operating characteristic (ROC) curve. Radiology, 143:29-36, 1982.}
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
\seealso{ ~~objects to See Also as \code{\link{ci.eval}}}
\examples{
preds <- runif(10)
acts <- round(runif(10))
cindex(preds,acts)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{misc}% __ONLY ONE__ keyword per line
