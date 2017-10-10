\name{compileC.fn}
\alias{compileC.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compile a C program for calculating multiscale entropy (MSE)
}
\description{
A function to compile a C program for calculating multiscale entropy (MSE) of an equally spaced time series.
}
\usage{
compileC.fn(cFile = "mse.c", cFolder)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cFile}{file name for the C program for calculating MSE. The file name must have a low case 'c' after "." such as "mse.c"
}
  \item{cFolder}{
the folder to hold the .c file for the C programs
}
}
\details{
Function to compile a C program for calculating multiscale entropy (MSE) of an equally spaced time series.
}
\value{
No value returned
}
\references{
Zhang XD, Zhang Z. CGManalyzer: a R package for analyzing continuous glucose monitoring studies
}
\author{
Xiaohua Douglas Zhang
}
\examples{
library(CGManalyzer)
package.name <- "CGManalyzer"
source( system.file("SPEC", "SPECexample.R", package = package.name) )
a <- system.file("SPEC", cFile, package = package.name)
cFolder0 <- substring(a, 1, nchar(a)-nchar(cFile)-1 )
cFolder <- gsub("Program Files", "PROGRA~1", cFolder0, fixed = TRUE)
compileC.fn(cFile=cFile, cFolder=cFolder)
}
