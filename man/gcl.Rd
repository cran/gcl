\name{gcl}

\alias{gcl}
\alias{sgcl}
\alias{tcl}

\title{GCL: a fuzzy rule classifier generator}

\description{\code{gcl} is an \R function that computes a fuzzy rules
  classifier given numeric input data as the data frame or matrix
  \code{mydata}.
  \code{gcl} returns an \R function that implements the computed
  classifier.}

\usage{
classifier <- gcl(mydata, nlev=3, filter=1.2, multi=NULL, gcl.verbose=F, ...)
classifier <- sgcl(mydata, cb=gcl, s.fold=4, s.verbose=FALSE, s.eval=acc.eval, ...) 
classifier <- tcl(mydata, t.nlev = 3, g = gainr, inf.lim = 0.5, ...)
}

\arguments{
\item{mydata}{The input data frame or matrix must have column names.  
    The last column is  taken  to  contain the class labels. All
    entries but the entries in the last column must be numerical.}
\item{nlev}{=<integer larger than 1>\cr
    Default value: 3\cr
    Sets how many fuzzy sets the values in each columns will be rep-
    resented by. The fuzzy sets have triangular shape and are deter-
    mined  by  three  numbers, the first 0 crossing, the 1 crossing,
    and the last 0 crossing. Memberships before the first and  after
    the  last  are  0.}
\item{filter}{=<positive  real  number in open unit interval>\cr
    Default value: 1.2\cr
    What data to use for empirical filtering of the rules  following
    the  rule generation stage. The objective of the filtering is to
    remove redundant rules. The data used  for  this  is  determined
    according to the following rules:
    If filter is NULL, no filtering is done.
    If filter is a matrix or data frame, this will be used.
    If  filter  is an index vector (boolean or integer), the rows in
    the data indexed by the index vector are used for filtering.
    If filter is a positive real number, a subset of  the  data  set
    will  be sampled from the data supplied such that each row has a
    probability equal to 1 minus the fractional filter value,  i.e.,
    1 - (filter - floor(filter)), to be used for construction of the
    rules. If filter < 1, then the data not used for  rule  computa-
    tion  will  be  used for rule filtering, i.e., compute redundant
    rules and remove these. If filter >= 1, then all the  data  will
    be used for filtering.}
  
\item{multi}{=<NULL or positive integer>\cr
    Default value: NULL\cr
    If  multi  is NULL, rules are created from the entire input data
    set.  If multi is not null, the input data is  partitioned  into
    multi  equally  sized  sets.  Rules are created from each of the
    (multi - 1) possibilities of forming unions of (multi  -  1)  of
    these sets. The concatenation of the resulting lists of rules is
    taken as the output of the rule generation stage.}
  
\item{gcl.verbose}{=<TRUE or FALSE>\cr
    Default value: TRUE\cr
    Make gcl output a little info while running.}
  
\item{cb}{=<classifier builder function>\cr
    Default value: gcl\cr
    Which classifier builder to use.}
  
\item{s.fold}{=<positive integer>\cr
    Default value: 4\cr How many-fold the cross validation is to be  in
    sgcl.}
  
\item{s.eval}{=<function(classifier function, data)
    returning a numeric matrix>\cr
    Default value: acc.eval\cr computing accuracy
    The evaluator used by sgcl.}
  
\item{s.verbose}{=<TRUE or FALSE>\cr
    Default value: FALSE\cr
    Make sgcl output a little info while running.}
  
\item{t.nlev}{=<integer larger than 1, or 0>\cr
    Default value: 3\cr
    Sets how many fuzzy sets the values in each columns will be rep-
    resented by. The fuzzy sets have triangular shape and are deter-
    mined  by  three  numbers, the first 0 crossing, the 1 crossing,
    and the last 0 crossing. Memberships before the first and  after
    the  last  are  0. Can be set to 0 in order to build a non-fuzzy
    classification tree.}
  
\item{g}{=<function taking two vectors of equal length returning a number>\cr
    Default value: gainr\cr
    The splitting function used by tcl.  Two implemented choices are
    gain  and  gainr.  gain is the information theoretic function of
    the same name, gainr is the gain ratio function.}
  
\item{inf.lim}{=<non-negative real number>\cr
    Default value: 0.5\cr
    If the information content in the outcome attribute is less than
    this  limit for the current partition class under consideration,
    tcl will not split further.}
  
}

\details{
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

The function \code{gcl} can also be given an optional argument \code{cfun =
function(attribute.values,outcomes,...)} that given a vector
attribute.values and a vector outcomes determines the inclusion cost
that should be associated with the attribute that has the values found
in attribute.values. An example could be
\code{function(a,b) 1/abs(cor(a,b))}
that associates less cost with an attribute that has a higher absolute
value correlation with the outcome. Note that the values given to the
function cfun are the values for the attribute after discretization. 
	

\strong{computed classifier}
	

The computed classifier is a function that takes one argument, the
numeric vector, matrix or data frame to be classified. When applied it
outputs a vector of class memberships for each input vector, matrix or
data frame row. The input data has to have (column) names compatible
with the names of the data from which the classifier function was
generated. Otherwise, the classifier function cannot operate. 

The data supplied to the computed classifier function cannot contain
non-numeric data. Specifically, if a classifier input data frame
contains a non-numeric class labels column (typically a factor), this
must be removed before application. Much like:
\preformatted{
  > classifier(inputdata[-ncol(inputdata)])
}
if the offending column is the last one. 

The computed classifier function can be \dQuote{dumped} to file by using
\R's \code{dump} function. If classifier is the name of the computed
function, 
then
\preformatted{
  > dump("classifier","classifier.r")
}
creates a file \file{classifier.r} containing the R source code of the
function classifier. This source code can then be distributed and will
work as a stand-alone program. 

If the computed classifier function is supplied with no, or a \code{NULL},
argument, it will return a documentation string. The content of this
string is decided by the value of the \code{gcl.decorate} option at the
time of the gcl call. If \code{getOption("gcl.decorate")} returns 1, the string
contains the fuzzy rules in a human readable format, if it returns 2
(default), each rule is also followed by the three numbers determining
the membership functions of each antecedent fuzzy proposition. If
returns NULL, no information about the rules is generated. This might be
used to save space and loading time. 

The computed classifier function returned has three attributes that can
be accessed by the \code{attributes()} function. They are
\code{summary.gcl.rnum},
\code{summary.gcl.amean}, \code{summary.gcl.natt} and \code{summary.gcl.nlev}. 
If \code{getOption("gcl.decorate")} returns a positive number, they contain the
number of rules in the classifier, the average number of attributes in
the rule antecedents, the number of distinct attributes found in the
rules, and the value of the \code{nlev} parameter passed to the \code{gcl}
function. The classifier function object returned by \code{tcl} has similar
attributes. 

\strong{sgcl}

The function \code{sgcl} partitions the input data \code{mydata} into
two data sets, training and holdout. It then performs a n-fold (given
by the parameter \code{s.fold}) cross validation over the training set,
using the classifier builder \code{cb} (default \code{gcl}) to generate 
classifiers. This process results in classifiers \eqn{c_i} for
\eqn{i = 1,2,\ldots,n} with associated performance measures
\eqn{p_i}. Each classifier \eqn{c_i} generated 
during the cross validation is applied to the holdout data set,
resulting in associated performance measure \eqn{q_i}.
For each classifier \eqn{c_i},
the expression
\deqn{\frac{q_i + p_i}{2} \frac{1}{(1 + |q_i - p_i|)}}{%
  (q_i + p_i)/2 * 1/(1 + |q_i - p_i|)}
is evaluated, and the
classifier that maximizes this expression is returned by \code{sgcl}. The
rationale for this is that we want the classifier with the best
consistent performance. In addition to the arguments listed above,
\code{sgcl} takes the arguments that \code{cb} and \code{cv} take. The
default performance measure used by sgcl is accuracy
as computed by \code{acc.eval}. Ties are broken arbitrarily.  

\strong{tcl}

The experimental function \code{tcl} computes a classification tree classifier
using a recursive partitioning algorithm similar to ID3. 
}

\value{
  The functions \code{gcl}, \code{tcl}, and \code{sgcl} return a
  function representing the computed classifier. 

  The computed classifier function returns a matrix with as many
  columns as the original data had class labels, \code{NULL}, or a text
  string representing a description of the classifier.
}

\note{
  If the column names do not match between the original data and the
  data to be classified by the computed function, the error 
  \code{Error in x[[ind]] : subscript out of bounds}
  is likely.

  Note that applying sgcl to small data sets is not advisable as the data
  is split repeatedly, making the learning and filtering sets even
  smaller. 
}


\references{
  Vinterbo, S.A.; Kim, E. and Ohno-Machado, L. \emph{Small, fuzzy and
    interpretable gene expression based classifiers}. Bioinformatics,
  2005, 21, 1964-1970.
  \url{http://bioinformatics.oxfordjournals.org/cgi/content/abstract/21/9/1964}
}

\author{
  Staal A. Vinterbo (C) 2007\cr
  \email{staal@dsg.harvard.edu}
}

\seealso{
  \url{http://www.r-project.org/}
}

\keyword{classif}
\keyword{tree}


\examples{
## run the demo
demo(gcldemo)

## play with the iris data set:
\dontrun{
library(datasets)
data(iris)
classifier <- gcl(iris, nlev=5)
acc.eval(classifier, iris)
}

## compare performance of gcl and tcl
\dontrun{
library(datasets)
data(iris)
cv52(iris, gcl, tcl, acc.eval, nlev=5, t.nlev=5)
}

## or a little more complex
library(gcl)
count <- matrix(c(0,0,0,1,1,0,1,1),ncol=2,byrow=TRUE)
xordata <- cbind(count, apply(count, 1, function(x) xor(x[1],x[2])))
colnames(xordata) <- c("Bit.1", "Bit.2", "XOR")
cf <- gcl(xordata,2,c())
cat(cf())
\dontrun{
# should produce something like:
Generated by gcl v1.06c Sat Nov 12 19:25:12 2005.
 nlev=2, filtering: no filtering took place
 rule generation: no subsampling.
 (c) Copyright 2005, Staal Vinterbo, all rights reserved.
Bit.1=2 & Bit.2=2 => XOR=0 [ 0 1 Inf ],[ 0 1 Inf ]
Bit.1=2 & Bit.2=1 => XOR=1 [ 0 1 Inf ],[ -Inf 0 1 ]
Bit.1=1 & Bit.2=2 => XOR=1 [ -Inf 0 1 ],[ 0 1 Inf ]
Bit.1=1 & Bit.2=1 => XOR=0 [ -Inf 0 1 ],[ -Inf 0 1 ]
}
v <- c(0,1)
names(v) <- colnames(xordata)[1:2]
cf(v)
\dontrun{
# produces:
            0 1
       [1,] 0 1
dump("cf", "cf.r")
rm(cf)
source("cf.r")
cf(v)
# produces:
            0 1
       [1,] 0 1
}
}
