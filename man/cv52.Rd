\name{cv52}
\alias{cv52}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{5x2 fold crossvalidation}
\description{
  Function to perform 5x2-fold crossvalidation comparison of two
  predictive models on the same data.
}
\usage{
cv52(df, mb1, mb2, eval, seed = NULL, cv.verbose = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{The data frame, last column is outcome}
  \item{mb1}{Model builder 1, must return a function that takes
    \code{df} as input and returns a matrix where entry (i,j) is the
    classifiers belief of case i's memebership in class j}
  \item{mb2}{Model builder 2, must return a function that takes
    \code{df} as input and returns a matrix where entry (i,j) is the
    classifiers belief of case i's memebership in class j}
  \item{eval}{Evaluation function, takes as input classifier generated
    by mb[12] above and data frame \code{df}}
  \item{seed}{Random seed.}
  \item{cv.verbose}{If TRUE, spew info.}
  \item{\dots}{Passed on.}
}
\details{
  This function implements the 5x2 CV F statistic proposed by Alpaydin
  as an improvement of Dietterich's statistic.
}
\value{
  Returns a list.
  \item{f}{the F statistic value}
  \item{p}{p value}
  \item{m}{matrix of split/fold results}
  \item{seeds}{vector of random seeds used}
}
\references{Alpaydin: "Combined 5x2 cv F test for comparing supervised classification learning algorithms" Neural Computation 1999}
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{cv}},\code{\link{acc.eval}},\code{\link{ci.eval}}}
\examples{
\dontrun{
cv52(df, gcl, tcl, acc.eval)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

