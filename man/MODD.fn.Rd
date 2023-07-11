\name{MODD.fn}
\alias{MODD.fn}

\title{
Function to calculate the mean of daily differences (MODD)
}

\description{
Calculates MODD which is the absolute value of the difference between glucose values taken on two consecutive days at the same time was calculated; the MODD is the mean of these differences.
}

\usage{
MODD.fn(y, Interval = 5)
}

\arguments{
  \item{y}{
measured response, must be evenly spaced in measured time
}
  \item{Interval}{
number of minutes between two consecutive time points
}
}

\value{
a value of MODD
}

\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{
Xiaohua Douglas Zhang
}

\examples{
library(CGManalyzer)
package.name <- "CGManalyzer"
source( system.file("SPEC", "SPECexample.R", package = package.name) )
y = rnorm( 3*24*60/5, mean=5, sd=0.1)
MODD.fn(y, Interval = 5)
}

