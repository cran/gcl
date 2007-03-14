\name{cvcomp}
\alias{cvcomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compare two models using crossvalidation}
\description{
  unction to perform crossvalidation comparison of two
  predictive models on the same data.
}
\usage{
cvcomp(df, mb1, mb2, eval, folds = 5, seed = NULL, cv.verbose = F, ...)
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
  \item{folds}{Number of folds.}  
  \item{seed}{Random seed.}
  \item{cv.verbose}{If TRUE, spew info.}
  \item{\dots}{Passed on.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns a list.
  \item{tt}{T-test of cv differences}
  \item{tcis}{cv results from mb1}
  \item{lcis}{cv results from mb2}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ %

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{cv}},\code{\link{cv52}},
  \code{\link{acc.eval}},\code{\link{ci.eval}}}
\examples{
\dontrun{
cvcomp(df, gcl, tcl, acc.eval)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

