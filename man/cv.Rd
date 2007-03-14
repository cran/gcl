\name{cv}
\alias{cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{N-fold crossvalidation}
\description{
  A function to do n-fold crossvalidation of a classifier function on a
  data set.
}
\usage{
cv(df, mb, eval, fold = 2, seed = NULL, q.only = T, cv.verbose = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{Data set (frame)}
  \item{mb}{Model builder, must return a function that takes
    \code{df} as input and returns a matrix where entry (i,j) is the
    classifiers belief of case i's memebership in class j}
  \item{eval}{Evaluation function, takes as input classifier generated
    by mb above and data frame \code{df}}
  \item{fold}{How many folds.}
  \item{seed}{Set random seed to this (for reproducibility)}
  \item{q.only}{Return evaluation results only.}
  \item{cv.verbose}{If TRUE, output info.}
  \item{\dots}{Passed on.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  If \code{q.only} is \code{TRUE} then the function returns a vector of
  evaluation results. Otherwise it returns a list.
  \item{cf}{The classifiers generated for each fold.}
  \item{q}{The evaluation results of each classifier on \code{df}}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
\dontrun{
cv(df, gcl, acc.eval)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

