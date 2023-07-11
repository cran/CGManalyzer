\name{fixMissing.fn}
\alias{fixMissing.fn}

\title{
Function to fix missing values in a vector
}
\description{
Function to fix missing values in a vector
}
\usage{
fixMissing.fn(y, x, Method = c("skip", "linearInterpolation", "loess", "dayCycle"),
OBScycle = 24 * 60 * 60/10)
}

\arguments{
  \item{y}{
a vector of data with missing values
}
  \item{x}{
a vector for a series of consecutive time indices
}
  \item{Method}{
method options for fixing missing value. "skip": skip all missing values; "loess": use local fitting by loess(); "dayCycle":a missing value is replaced by the mean of the two values one day ahead and one day behind plus the mean of the differences between the two edge points and their corresponding means of the two values one day head and one day behind in a segment with missing values in which the missing value belongs to.
}
  \item{OBScycle}{
number of observations in a full cycle
}
}
\value{
a vector of data from 'y'  but with missing values fixed
}
\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{
Xiaohua Douglas Zhang
}

\examples{
data.mat <-
   cbind( "x"=c(0,  3,  6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42),
          "signal"=c(3.930, 3.973, 4.005, NA, 4.164, 4.190, 4.205, NA, 4.186,
          4.265, NA, 4.266, 4.357, 4.503, 4.690) )
dataFixNA.mat <- fixMissing.fn( y=data.mat[,2], x=data.mat[,1], Method="linearInterpolation")
data.mat
dataFixNA.mat
}
