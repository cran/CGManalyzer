\name{equalInterval.fn}
\alias{equalInterval.fn}

\title{
Function to derive the data with equal interval
}
\description{
Function to derive the data with equal interval
}
\usage{
equalInterval.fn(x, y, Interval = NA, minGap = 4 * Interval)
}

\arguments{
  \item{x}{
time sequence
}
  \item{y}{
measured response
}
  \item{Interval}{
interval indicating equal space between two consecutive points
}
  \item{minGap}{
the length of a chain of continuous missing values in which the missing values will not be derived from the neighbor points
}
}
\details{
Function to derive the data with equal interval for a timeseries
}
\value{
a matrix with equally spaced time sequence and corresponding signal value
}
\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{
Xiaohua Douglas Zhang
}

\examples{
data.mat <-
   cbind( "timeSeries"=c(0,  3,  6, 9, 11, 21, 24, 27, 33, 38, 39, 42),
          "signal"=c(3.930, 3.973, 4.005, 4.110, 4.164, 4.165, 4.186,
          4.265, 4.266, 4.357, 4.503, 4.690) )
dataEqualSpace.mat <- equalInterval.fn(x=data.mat[,1], y=data.mat[,2], Interval=3)
data.mat
dataEqualSpace.mat
}

