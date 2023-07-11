\name{setSPEC.fn}
\alias{setSPEC.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load settings for the selected SPEC parameter.
}
\description{
A function to load settings for the selected SPEC parameter.
}
\usage{
setSPEC.fn(SPEC.name)
}

\arguments{
  \item{SPEC.name}{
the SPEC name to load, which is a R script name. one of "SPEC.FreestyleLibre.R", "SPEC.Glutalor.R" and "SPEC.Medtronic.R".
}
}
\details{
A function to load settings for the selected SPEC parameter.
If you want to use the example data in the package "CGManalyzer", you have to choose "SPECexample.R". If you want to run your own data, you cannot choose "SPECexample.R", instead, you need to choose one of "SPEC.FreestyleLibre.R", "SPEC.Glutalor.R" and "SPEC.Medtronic.R".
}
\value{
no value returned
}
\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{
Xiaohua Douglas Zhang
}

\examples{
# set SPEC for reading data
package.name <- "CGManalyzer"
options(scipen = 999)
mainFolder <- getwd()
SPEC.name <- "SPECexample.R"
setSPEC.fn(SPEC.name)
}

