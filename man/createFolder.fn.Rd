\name{createFolder.fn}
\alias{createFolder.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create folders for holding data and analyzing data
}
\description{
a funciton to create folders for holding data and analyzing data
}
\usage{
createFolder.fn(mainFolder)
}

\arguments{
  \item{mainFolder}{
The name and path for the main folder that you want to hold the analysis, such as    mainFolder = "C:/CGManalyzer/FilesForPackage"
}
}
\details{
a funciton to create folders for holding data and analyzing data
}
\value{
no value returned
}
\references{
Zhang XD, Zhang Z. CGManalyzer: a R package for analyzing continuous glucose monitoring studies
}
\author{
Xiaohua Douglas Zhang
}

\examples{
mainFolder <- file.path(tempdir(),"Test")
createFolder.fn(mainFolder)
}

