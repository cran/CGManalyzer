\name{MSEplot.fn}
\alias{MSEplot.fn}

\title{
Plot the mean and standard error or standard deviation of multiscale entropy by group
}
\description{
function to plot the mean and standard error or standard deviation of multiscale entropy by group
}
\usage{
MSEplot.fn(Scale, MSE, Name, responseName = NA, timeUnit = "", byGroup = TRUE,
MSEsd = NA, N = NA, stdError = TRUE, xRange = NA, yRange = NA, las = 2, col = NA,
pch = NA, Position = "topleft", cex.legend = 0.75, main = "")
}

\arguments{
  \item{Scale}{
a vector for scale
}
  \item{MSE}{
matrix for entropy if byGroup=FALSE, and otherwise for average entropy value in a group   at a scale. In the matrix, the row is for scale and column for individuals or groups.
}
  \item{Name}{
vector of names for groups
}
  \item{responseName}{
name to represent the response to be analyzed, such as 'glucose'
}
  \item{timeUnit}{
the time unit for scale
}
  \item{byGroup}{
If byGroup = TRUE, multiscale entropy is plotted by groups; otherwise, by individuals
}
  \item{MSEsd}{
matrix for standard deviation of entropy value in a group at a scale
}
  \item{N}{
matrix for number of subjects in a group at a scale
}
  \item{stdError}{
if it is true, the length of a vertical bar represent 2*standard error;  otherwise, the length of a vertical bar represent 2*standard deviation
}
  \item{xRange}{
range for the x-axis
}
  \item{yRange}{
range for the y-axis
}
  \item{las}{
las for the y-axis
}
  \item{col}{
vector for the colors to indicate groups or individuals
}
  \item{pch}{
vector for the point types to indicate groups or individuals
}
  \item{Position}{
position for the legend
}
  \item{cex.legend}{
cex for the legend
}
  \item{main}{
main title for title()
}
}
\details{
function to plot the mean and standard error or standard deviation of multiscale entropy by group
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
scalesInTime <- Scales*equal.interval
MSE.mat <- read.csv(file=system.file("SPEC", "MSE.csv", package = package.name), row.names=1)
Types <- unique( subjectTypes )
Types <- Types[order(Types)]
nType <-length(Types)
col.vec <- rep(NA, length(subjectTypes) )
for( i in 1:nType ) { col.vec[ subjectTypes == Types[i] ] <- i }
MSEplot.fn(scalesInTime, MSE=t(MSE.mat), Name=Types, responseName="glucose", timeUnit="minute",
          byGroup=FALSE, MSEsd=NA, N=NA, stdError=TRUE, xRange=NA, yRange=NA,
          pch=rep(1, dim(MSE.mat)[1]),las=2, col=col.vec, Position="topleft",
          cex.legend=0.0005, main="A: MSE by individual")
legend("topleft", legend=paste0(Types, "(N=", table( subjectTypes ), ")"),
      col=1:nType, cex=1, lty=1, pch=1)

}
