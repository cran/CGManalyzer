\name{boxplotCGM.fn}
\alias{boxplotCGM.fn}
\title{Draw a boxplot for continuous glucose monitoring data sensor by sensor}
\description{
a function to draw a boxplot for continuous glucose monitoring data sensor by sensor
}
\usage{
boxplotCGM.fn(dataFolder, dataFiles, idxNA = NA, responseName, sensorIDs,
columnNames = NULL, yRange, skip = 0, header = TRUE, comment.char = "", sep = ",",
cex.axis1 = 0.75)
}

\arguments{
  \item{dataFolder}{name for the folder for holding data}
  \item{dataFiles}{names of the data files to be read in R}
  \item{idxNA}{symbol to represent a missing value, such as NA}
  \item{responseName}{name to represent the response to be analyzed, such as 'glucose'}
  \item{sensorIDs}{names of sensors or subjects}
  \item{columnNames}{names of columns of the data after reading in R}
  \item{yRange}{range of y-axis to be drawn in the boxplot}
  \item{skip}{
number of lines to be skipped in each data file when the data is read in R
}
  \item{header}{
the same meaning as in read.table()
}
  \item{comment.char}{
the same meaning as in read.table()
}
  \item{sep}{
the same meaning as in read.table()
}
  \item{cex.axis1}{cex for the x-axis}
}
\details{
a box plot for the data by each sensor or subject
}
\value{No value return; draw a boxplot}
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
yRange <- c( min(summary.arr[, "Min",responseName], na.rm=TRUE),
      max(summary.arr[, "Max",responseName], na.rm=TRUE))
boxplotCGM.fn(dataFolder, dataFiles, idxNA, responseName, sensorIDs, columnNames, yRange,
            skip=Skip, header=Header, comment.char=Comment.char, sep=Sep, cex.axis1=1)
}
