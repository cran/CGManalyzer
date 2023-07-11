\name{fac2char.fn}
\alias{fac2char.fn}

\title{
function to convert a factor to a vector
}
\description{
function to convert a factor to a vector of characters
}
\usage{
fac2char.fn(x)
}

\arguments{
  \item{x}{
a factor
}
}
\details{
function to convert a factor to a vector
}
\value{
a vector of characters
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
fac2char.fn(dataFileType.df[,1])
}

