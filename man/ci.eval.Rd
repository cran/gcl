\name{ci.eval}
\alias{ci.eval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that evaluates the cindex of a classifier function on
  a data frame}
\description{
  This function evaluates a classifier function on a data set. Returns
  the c-index, aka. the area under the receiver operating curve. Works
  only on binary outcomes.
}
\usage{
ci.eval(cf, df, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cf}{Classifier function, must accept \code{df} as an argument and
    return a matrix with class labels as column names and entry (i,j)
    containing the classifiers belief that case i is of class j.}
  \item{df}{Data set (frame), last column must be outcome and encoded as
    numeric}
  \item{\dots}{ Not used. }
}
\details{
  The c-index is related to the Wilcoxon rank sum statistic or
  Mann-Whitney U. It is equivalent to the area under the receiver
  operating curve and is an estimate the probability of the classifier
  ranking a randomly chosen positive example as more positive than a
  randomly chosen negative example.
}
\value{
  The c-index.
}
\references{J.A. Hanley and B.J. McNeil. The meaning and use of the area under a receiver operating characteristic (ROC) curve. Radiology, 143:29-36, 1982.}
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{acc.eval}}, \code{\link{cindex}}}
\examples{
## The function is currently defined as
\dontrun{
function (cf, df, ...) 
{
    clsv <- unlist(df[, ncol(df)])
    clss <- unique(clsv)
    if (length(clss) != 2) 
        stop("Cannot compute CI on non-binary outcomes. Aborting.\n", 
            call. = F)
    clss <- sort(clss)
    cs <- cf(df, ...)
    if (is.null(cs)) 
        return(NULL)
    csc <- colnames(cs)
    prev <- sum(clsv == clss[2])/length(clsv)
    csn <- t(apply(cs, 1, pnormalize, c(1 - prev, prev)))
    pred <- csn[, 2]
    act <- clsv == clss[2]
    return(cindex(pred, act)[1])
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

