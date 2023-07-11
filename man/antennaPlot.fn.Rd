\name{antennaPlot.fn}
\alias{antennaPlot.fn}
\title{draw an antenna plot}
\description{function to draw an antenna plot }
\usage{
antennaPlot.fn(Mean, SSMD, Name, CIlower, CIupper, xRange = NA, yRange = NA,
col = 1:length(Mean), pch = 1:length(Mean), cex = 1, Position = "topleft", main = "")
}
\arguments{
  \item{Mean}{vector for mean difference in a comparison}
  \item{SSMD}{vector for strictly standardized mean difference (ssmd) in a comparison}
  \item{Name}{vector for name of pairs in a comparison}
  \item{CIlower}{vector for the lower bound of confidence interval}
  \item{CIupper}{vector for the upper bound of confidence interval}
  \item{xRange}{pre-defined range for the x-axis if needed}
  \item{yRange}{pre-defined range for the y-axis if needed}
  \item{col}{vector of colors for pairs in a comparison}
  \item{pch}{vector of point types for pairs in a comparison}
  \item{cex}{cex for the legend}
  \item{Position}{position indicating where to put the legend, such as 'topleft'}
  \item{main}{title name}
}
\details{
a function to draw an antenna plot, namely, plot ssmd vs. mean difference with confidence interval.
}
\value{ no return value }
\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{ Xiaohua Douglas Zhang }
\examples{
library(CGManalyzer)
package.name <- "CGManalyzer"
source( system.file("SPEC", "SPECexample.R", package = package.name) )
scalesInTime <- Scales*equal.interval
pvalSSMD.mat <- read.csv(file=system.file("SPEC", "pvalSSMD.csv", package = package.name),
              row.names=1)
outNames <- dimnames(pvalSSMD.mat)[[2]]
SSMD.mat <- as.matrix( pvalSSMD.mat[, substring(outNames, 1, 4) == "SSMD"] )
mDiff.mat <- as.matrix(pvalSSMD.mat[, substring(outNames, 1, 5) == "mDiff"])
CIlower.mat <- as.matrix(pvalSSMD.mat[, substring(outNames, 1, 7) == "CIlower"])
CIupper.mat <- as.matrix(pvalSSMD.mat[, substring(outNames, 1, 7) == "CIupper"])
pairNames <- gsub("mDiff_", "", dimnames(mDiff.mat)[[2]], fixed=TRUE)
idx = 1:4
xRange <- range( c( range( CIlower.mat[idx,], na.rm=TRUE), 0,
      range( CIupper.mat[idx,], na.rm=TRUE) )  )
yRange <- range( c(0, range( SSMD.mat[idx,], na.rm=TRUE ) ) )
par(mfrow=c(2,2))
for( i in idx ) {
    Main <- paste0("Sample entropy at a scale of ", scalesInTime[i], " ", timeUnit, "s")
    condt <- !is.na(mDiff.mat[i,]) & !is.na(SSMD.mat[i,])
	antennaPlot.fn(Mean=mDiff.mat[i,condt], SSMD=SSMD.mat[i, condt], Name = pairNames[condt],
	            CIlower=CIlower.mat[i,condt], CIupper=CIupper.mat[i,condt], xRange=xRange,
	            yRange=yRange, col=1:length(pairNames[condt]), pch=1:length(pairNames[condt]),
	            cex=0.8, Position = "topleft", main = Main)
}
}
