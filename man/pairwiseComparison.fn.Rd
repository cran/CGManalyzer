\name{pairwiseComparison.fn}
\alias{pairwiseComparison.fn}

\title{
function to calculate mean difference and its confidence interval, SSMD, p-value of t.test for pairwise comparison
}
\description{
function to calculate mean difference and its confidence interval, SSMD, p-value of t.test for pairwise comparison
}
\usage{
pairwiseComparison.fn(y, INDEX, na.rm = TRUE, conf.level = 0.95)
}

\arguments{
  \item{y}{
response value
}
  \item{INDEX}{
vector for group names
}
  \item{na.rm}{
whether to remove value for calculation
}
  \item{conf.level}{
confidence level for two-sided t-test
}
}
\details{
function to calculate mean difference and its confidence interval, SSMD, p-value of t.test for pairwise comparison
}
\value{
a vector for calculated mean difference, its upper and lower bounds of CI, SSMD and pvalue in each pairs of group comparison, along with mean, standard deviation, and sample size in each group
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
MSE.mat <- read.csv(file=system.file("SPEC", "MSE.csv", package = package.name), row.names=1)
pairwiseComparison.fn(y=MSE.mat[, 1], INDEX=subjectTypes, na.rm=TRUE)
}

