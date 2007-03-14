\name{loocv}
\alias{loocv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Leave one out crossvalidation}
\description{
  A function to do leave-one-out crossvalidation of a classifier function on a
  data set.
}
\usage{
loocv(df, mb, eval, seed = NULL, cv.verbose = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{Data set (frame)}
  \item{mb}{Model builder, must return a function that takes
    \code{df} as input and returns a matrix where entry (i,j) is the
    classifiers belief of case i's memebership in class j}
  \item{eval}{Evaluation function, takes as input classifier generated
    by mb above and data frame \code{df}}
  \item{seed}{Set random seed to this (for reproducibility)}
  \item{cv.verbose}{If TRUE, output info.}
  \item{\dots}{Passed on.}
}
%\details{
%
%}
\value{
  Returns \code{eval} output from a pseudo classifier that returns for
  each case the result of the leave one out cv where this case was left
  out. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
\dontrun{
loocv(df, gcl, acc.eval)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
