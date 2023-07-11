\name{ssmdEffect.fn}
\alias{ssmdEffect.fn}

\title{
function to derive the type of effect size based on SSMD values
}
\description{
function to derive the type of effect size based on SSMD values
}
\usage{
ssmdEffect.fn(ssmd.vec, criterion = c("mainType", "subType"))
}

\arguments{
  \item{ssmd.vec}{
 a vector for SSMD value
}
  \item{criterion}{
whether use the criterion for deriving the main effect type or the sub-type
}
}
\details{
function to derive the type of effect size based on SSMD values
}

\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).

Zhang XHD, 2011. Optimal High-Throughput Screening: Practical  Experimental Design and Data Analysis for Genome-scale RNAi Research.  Cambridge University Press, Cambridge, UK
}
\author{
Xiaohua Douglas Zhang
}

\examples{
SSMD.vec = c(-3.4, -5, 0.198, 0.055, 0.181, 2, 3, 1.5, 6, 0.25)
ssmdEffect.fn(SSMD.vec, criterion="subType")
}
