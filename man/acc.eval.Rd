\name{acc.eval}
\alias{acc.eval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function that evaluates the accuracy of a classifier function on
  a data frame }
\description{
  This function evaluates a classifier function on a data set. Returns
  the accuracy.
}
\usage{
acc.eval(cf, df, ...)
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
  Accuracy is computed as fraction of correct classifications to the
  number of cases.
}
\value{
  Returns the accuracy of the classifier on the data set.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
\note{
  Part of the gcl package.
}
\seealso{ ci.eval \code{\link{ci.eval}}}
\examples{
\dontrun{
## The function is currently defined as
function (cf, df, ...) 
{
    cs <- cf(df)
    if (is.null(cs)) 
        return(NA)
    return(sum(apply(paired.call(cf(df), df), 1, function(v) (v[1] == 
        v[2]) + 0), na.rm = T)/nrow(df))
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

