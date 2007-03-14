\name{hanley}
\alias{hanley}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Statistically compare C-indices}
\description{
  Compares two binary classifier outputs on the same data using the
  method by Hanley and McNeil.
}
\usage{
hanley(paref1, paref2, aaref, ctype = "pearson")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paref1}{vector of predictions from classifier 1}
  \item{paref2}{vector of predictions from classifier 2}
  \item{aaref}{actual classes, must be 0 or 1}
  \item{ctype}{type of correlation used in \code{cor.test}}
}
\details{
  Implements the method of Hanley and McNeil for comparing two c-indices
  (ROC AUC's) from the same data.
}
\value{
  Returns a list.
  \item{p}{p value}
  \item{auc1}{the AUC of classifier 1}
  \item{auc2}{the AUC of classifier 2}
  \item{se1}{standard error of AUC of classifier 1}
  \item{se2}{standard error of AUC of classifier 2}
  \item{z}{z value}
  \item{r}{Correllation factor}
  \item{corr0s}{correlation of true 0's}
  \item{corr1s}{correlation of true 1's}
}
\references{J.A. Hanley and B.J. McNeil. A method of comparing the areas
  under receiver operating characteristic curves derived from the same
  cases. Radiology, 148:839-43, 1983.} 
\author{Staal A. Vinterbo \email{staal@dsg.harvard.edu}}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{cindex}}}
\examples{
p1 <- runif(10)
p2 <- runif(10)
a <- round(runif(10))
hanley(p1,p2,a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

