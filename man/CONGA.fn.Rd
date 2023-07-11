\name{CONGA.fn}
\alias{CONGA.fn}

\title{
Function to calculate the continuous overlapping net glycemic action (CONGA)
}

\description{
For each observation after the first n hours of observations, the difference between the current observation and the observation n hours previous was calculated. CONGA is defined as the standard deviation of the differences.
}

\usage{
CONGA.fn(y, Interval = 5, n = 2)
}

\arguments{
  \item{y}{
measured response, must be evenly spaced in measured time
}
  \item{Interval}{
number of minutes between two consecutive time points
}
  \item{n}{
the length of a segment in CONGA
}
}

\value{
a value of CONGA
}
\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{
Xiaohua Douglas Zhang, Dandan Wang
}


\examples{
library(CGManalyzer)
package.name <- "CGManalyzer"
source( system.file("SPEC", "SPECexample.R", package = package.name) )
y = rnorm( 3*24*60/5, mean=5, sd=0.1)
CONGA.fn(y, Interval = 5, n=2)
}
