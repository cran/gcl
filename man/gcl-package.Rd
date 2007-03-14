\name{gcl-package}
\alias{gcl-package}
\docType{package}
\title{
GCL: A package for Computing fuzzy rules or tree classifiers from numeric data
}
\description{\pkg{gcl} is a \R (\url{http://www.r-project.org}) package
  for computing fuzzy rules and tree classifiers given numeric input
  data.}
}
\details{
\tabular{ll}{
Package: \tab gcl\cr
Type: \tab Package\cr
Version: \tab 1.06.4\cr
License: \tab GPL Version 2\cr
URL: \tab \url{http://www.r-project.org},\cr
 \tab \url{http://www.mit.edu/~sav/fuzzy/latest/}\cr
}
Function index:
\preformatted{
acc.eval                Function that evaluates the accuracy of a
                        classifier function on a data frame
ci.eval                 Function that evaluates the cindex of a
                        classifier function on a data frame
cindex                  Compute the c-index
cv                      N-fold crossvalidation
cv52                    5x2 fold crossvalidation
cvcomp                  Compare two models using crossvalidation
gain                    Computes information theoretic gain
gainr                   Computes information theoretic gain ratio
gcl                     GCL: a fuzzy rule classifier generator
hanley                  Statistically compare C-indices
loocv                   Leave one out crossvalidation
}

\strong{gcl}

This function computes a fuzzy rules classifier given numeric input data
as the data frame or matrix mydata. 

The algorithm for doing so is described in \cite{Vinterbo et al., 2005}. 

When applied, \code{gcl} returns another \R function that implements the found
classifier. This computed classifier function takes one argument, a
vector, matrix or data frame to be classified, and outputs a vector of
class memberships for each input vector, matrix or data frame row. (See
examples section below). 

Even though the paper cited above is on classification using gene
expression data, numerical data in general can be used. For instance
\preformatted{
> library(gcl)
> library(datasets)
> data(iris)
> classifier <- gcl(iris, nlev=5)
> acc.eval(classifier, iris)
}
computes a fuzzy rule classifier for Edgar Anderson's Iris Data set and
evaluates the classifier accuracy on the same data set. 
}

\author{
Staal A. Vinterbo

Maintainer: complain to <staal at dsg fullstop harvard fullstop edu>
}
\references{
  Vinterbo, S.A.; Kim, E. and Ohno-Machado, L. \emph{Small, fuzzy and
    interpretable gene expression based classifiers}. Bioinformatics,
  2005, 21, 1964-1970.
  \url{http://bioinformatics.oxfordjournals.org/cgi/content/abstract/21/9/1964}
}
\section{Availability}{
  The gcl program should be considered as being testing
  software. It is supplied as is, and NO WARRANTY whatsoever is given.

  GCL is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License
  \url{http://www.gnu.org/copyleft/gpl.html}
  as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.
  
  GCL is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with GCL; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  The gzipped tar archive containing the latest gcl R package version
  can also be gotten here:
  \url{http://www.mit.edu/~sav/fuzzy/latest/}
}

\keyword{ package }
\seealso{
  \code{\link{gcl}}
}
\examples{
## run the demo
demo(gcldemo)

## play with the iris data set:
\dontrun{
library(gcl)
library(datasets)
data(iris)
classifier <- gcl(iris, nlev=5)
acc.eval(classifier, iris)
}

## compare performance of gcl and tcl
\dontrun{
library(gcl)
library(datasets)
data(iris)
cv52(iris, gcl, tcl, acc.eval, nlev=5, t.nlev=5)
}
}
