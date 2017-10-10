ssmdEffect.fn <-
function(ssmd.vec, criterion=c("mainType","subType"))
{
##**************************************************************************
## function to derive the type of effect size based on SSMD values
## Input
##   ssmd.vec:  a vector for SSMD value
##   criterion: whether use the criterion for deriving the main effect type
##              or the sub-type
## Author: Xiaohua Douglas Zhang, 2007
## Reference: 
##   Zhang XHD, 2011. Optimal High-Throughput Screening: Practical 
##   Experimental Design and Data Analysis for Genome-scale RNAi Research. 
##   Cambridge University Press, Cambridge, UK
##**************************************************************************
  effectClass.vec <- rep( NA, length(ssmd.vec) )
  if( criterion == "mainType" ) {
    effectClass.vec[ ssmd.vec >= 1.645 ] <- "extra.Large+"
    effectClass.vec[ ssmd.vec >= 1    & ssmd.vec < 1.645 ] <- "Large+"
    effectClass.vec[ ssmd.vec > 0.25  & ssmd.vec < 1     ] <- "Medium+"
    effectClass.vec[ ssmd.vec > 0     & ssmd.vec <= 0.25 ] <- "Small+"
    effectClass.vec[ ssmd.vec == 0 ] <- "ZeroEfffect"
    effectClass.vec[ ssmd.vec < 0     & ssmd.vec > -0.25 ] <- "Small-"
    effectClass.vec[ ssmd.vec <= -0.25& ssmd.vec > -1    ] <- "Medium-"
    effectClass.vec[ ssmd.vec <= -1   & ssmd.vec > -1.645] <- "Large-"
    effectClass.vec[ ssmd.vec <= -1.645 ] <- "extra.Large-"
  } else if ( criterion == "subType" ) { 
    effectClass.vec[ ssmd.vec >= 5 ] <- "ExtremelyStrong+"
    effectClass.vec[ ssmd.vec >= 3    & ssmd.vec < 5  ] <- "VeryStrong+"
    effectClass.vec[ ssmd.vec >= 2    & ssmd.vec < 3  ] <- "Strong+" 
    effectClass.vec[ ssmd.vec >= 1.645& ssmd.vec < 2  ] <- "FairlyStrong+" 
    effectClass.vec[ ssmd.vec >= 1.28 & ssmd.vec < 1.645 ] <- "Moderate+"
    effectClass.vec[ ssmd.vec >= 1    & ssmd.vec < 1.28  ] <- "FairlyModerate+"
    effectClass.vec[ ssmd.vec >  0.75  & ssmd.vec < 1    ] <- "FairlyWeak+"    
    effectClass.vec[ ssmd.vec >  0.50 & ssmd.vec <= 0.75 ] <- "Weak+" 
    effectClass.vec[ ssmd.vec >  0.25 & ssmd.vec <= 0.50 ] <- "VeryWeak+" 
    effectClass.vec[ ssmd.vec > 0     & ssmd.vec <= 0.25 ] <- "ExtremelyWeak+"
    effectClass.vec[ ssmd.vec == 0 ] <- "ZeroEffect" 
    effectClass.vec[ ssmd.vec < 0     & ssmd.vec >= -0.25] <- "ExtremelyWeak-"
    effectClass.vec[ ssmd.vec < -0.25 & ssmd.vec >= -0.50] <- "VeryWeak-"
    effectClass.vec[ ssmd.vec < -0.50 & ssmd.vec >= -0.75] <- "Weak-" 
    effectClass.vec[ ssmd.vec < -0.75  & ssmd.vec > -1   ] <- "FairlyWeak-"
    effectClass.vec[ ssmd.vec <= -1   & ssmd.vec > -1.28 ] <- "FairlyModerate-"
    effectClass.vec[ ssmd.vec <= -1.28& ssmd.vec > -1.645] <- "Moderate-" 
    effectClass.vec[ ssmd.vec <= -1.645& ssmd.vec > -2   ] <- "FairlyStrong-" 
    effectClass.vec[ ssmd.vec <= -2   & ssmd.vec > -3    ] <- "Strong-" 
    effectClass.vec[ ssmd.vec <= -3   & ssmd.vec > -5    ] <- "VeryStrong-"
    effectClass.vec[ ssmd.vec <= -5 ] <- "ExtremelyStrong-"
  } else {
    return("Error: criterion must be 'mainType' or 'subType'.")
  }
  if( sum(is.na(ssmd.vec)) > 0 ) effectClass.vec[ is.na(ssmd.vec) ] <- NA
  effectClass.vec
}
