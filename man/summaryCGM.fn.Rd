\name{summaryCGM.fn}
\alias{summaryCGM.fn}

\title{
Function to calculate the summary statistics for each subject or sensor:  number of subjects or sensors, minimum, 1st quartile, median, mean, 2nd quartile, maximum, standard deviation, MAD
}
\description{
Function to calculate the summary statistics for each subject or sensor:  number of subjects or sensors, minimum, 1st quartile, median, mean, 2nd quartile, maximum, standard deviation, MAD
}
\usage{
summaryCGM.fn(dataFolder, dataFiles, responseNames, sensorIDs, columnNames = NULL,
skip = 0, header = TRUE, comment.char = "", sep = ",")
}

\arguments{
  \item{dataFolder}{
folder directory for holding raw CGM data
}
  \item{dataFiles}{
file names for holding raw CGM data, usually one file for one sensor
}
  \item{responseNames}{
name for the response
}
  \item{sensorIDs}{
ID's for sensors
}
  \item{columnNames}{
column names for the raw data
}
  \item{skip}{
number of lines to be skip in data file when using read.table
}
  \item{header}{
the same as in read.table()
}
  \item{comment.char}{
the same as in read.table()
}
  \item{sep}{
the same as in read.table()
}
}
\details{
Function to calculate the summary statistics for each subject or sensor:  number of subjects or sensors, minimum, 1st quartile, median, mean, 2nd quartile, maximum, standard deviation, MAD
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
summary.arr <- summaryCGM.fn(dataFolder, dataFiles, responseName, sensorIDs, columnNames,
              skip=Skip, header=Header, comment.char=Comment.char, sep=Sep)
summary.arr[1:6, ,1]
}

