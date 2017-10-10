\name{CGManalyzer-package}
\alias{CGManalyzer-package}
\alias{CGManalyzer}
\docType{package}
\title{
\packageTitle{CGManalyzer}
}
\description{
\packageDescription{CGManalyzer}
}
\details{
The R package CGManalyzer contains functions for analyzing data from a continuous glucose monitoring (CGM) study. It covers a complete flow of data analysis including reading a series of datasets, obtaining summary statistics of glucose levels, plotting data, transforming time stamp format, fixing missing values, calculating multiscale sample entropy (MSE), conducting pairwise comparison, displaying results using various plots including a new type of plots called an antenna plot, etc.. This package has been developed from our work in directly analyzing data from various CGM devices such as FreeStyle Libre, Glutalor, Dexcom, Medtronic CGM. Thus, this package will greatly facilitate the analysis of various CGM studies.

}
\author{
\packageAuthor{CGManalyzer}

Maintainer: \packageMaintainer{CGManalyzer}
}
\references{
Zhang XD, Zhang Z. CGManalyzer: a R package for analyzing continuous glucose monitoring studies
}

\keyword{ package }


\examples{
a <- FALSE
##set a to TRUE if you want to use the examples below

if(a == TRUE){
##################################################################################################
# The following are the example in the help file for CGManalyzer, which is also the main file for
#   using the Package
##################################################################################################

## from the Bioinformatics websiteopen
##   1. Download "CGManalyzer_1.0.tar.gz from Bioinformatics in a directory in your computer
##   2. Open Rgui and set that directory to be working direction
##   3. Run the codes below

install.packages( paste0(getwd(),"/CGManalyzer_1.0.tar.gz"), repos = NULL, type = "source" )

rm(list=ls())

library(CGManalyzer)
package.name <- "CGManalyzer"

options(scipen=999)

##################################################################################################
## Specify the name of the main folder for holding everything including data and analysis
## -----------------------------------------------------------------------------------------------

## In the first line of codes below, please replace
##    mainFolder <- "C:/CGManalyzer/FilesForPackage"
##  with the name and path for the main folder that you want to hold the analysis, such as
##    mainFolder <- "C:/yourProject"
##################################################################################################
mainFolder <- "D:/E/Paper/W.2017CGManalyzer.Bioinformatics/Submission/Demo"
#"C:/CGManalyzer/FilesForPackage"
print("Reminder: Please input the name and path for the main folder that you want to hold the
analysis if you have not do so!")

########################################################################################
# create folders for main folder and its sub-folders: Analysis and Data
########################################################################################
createFolder.fn(mainFolder)
save.image()


##########################################################################################
# set up for reading data
##########################################################################################
# Please complete the following steps before reading any data in R
#  1. Put all the data (in .csv or .txt) in the data folder and remove all .zip or other files
#  2. In the data folder, create a new .bat file named "00fetchFileNameInDirectory.bat"
#     which must have and only have "DIR *.* /B > 00filelist.csv" as its content
#  3. After create and save the file "00fetchFileNameInDirectory.bat", double click on
#     this file name to automatically create a file named "00filelist.csv" which holds
#     all the names of data files, one file for the data in one subject or sensor.
#  4. Add a second column in generated file "00filelist.csv" in which the group type for each
#     subject is specified. After the addition is completed, the first 5 lines in "00filelist.csv"
#     should be more or less as follows
#______________________________
#00fetchFileNameInDirectory.bat
#00filelist.csv
#S0001.csv	II
#S0002.csv	I
#S0003.csv	H
#_______________________________
#  5. Delete the file "00fetchFileNameInDirectory.bat" that was created in Step 2
#  6. Each data file should have the same structure with the same number of columns and
#     the same column names, the same time stamp structure, the same number of head lines
#     to be skipped for read
#  7. change the settings in the SPEC.R file so that the input for the parameters corresponds to
#     your data file and the CGM device that is used in your study.
#******************************************************************************************

###################################################################################
# specify parameters for different CGM devices
###################################################################################
# Choose one of the SPEC file that fits the CGM device that you are using for your study
# if you want to use the example data in the package "CGManalyzer", you have to
# choose "SPECexample.R"
# if you want to run your own data, you cannot choose "SPECexample.R", instead, you need to choose
# one of "SPEC.FreestyleLibre.R", "SPEC.Glutalor.R" and "SPEC.Medtronic.R". You may
# also write your own file following the format in one of them.

SPEC.name <- "SPECexample.R"
# SPEC.name <- "SPEC.FreestyleLibre.R"
# SPEC.name <- "SPEC.Glutalor.R"
# SPEC.name <- "SPEC.Medtronic.R"

source( system.file("SPEC", SPEC.name, package = package.name) )

###################################################################################
# compile C in R
###################################################################################
a <- system.file("SPEC", cFile, package = package.name)
cFolder0 <- substring(a, 1, nchar(a)-nchar(cFile)-1 )
cFolder <- gsub("Program Files", "PROGRA~1", cFolder0, fixed = TRUE)
compileC.fn(cFile=cFile, cFolder=cFolder)

cmdCore <- gsub(".c", "", cFile, fixed = TRUE)


###################################################################################
## get summary statistics: number of subjects, minimum, 1st quartile, median, mean,
##             mean, 2nd quartile, maximum, number of NA's, standard deviation, MAD
###################################################################################
if( Summary ) {
	summary.arr <- summaryCGM.fn(dataFolder, dataFiles, responseName, sensorIDs, columnNames,
	      skip=Skip, header=Header, comment.char=Comment.char, sep=Sep)
	write.csv(file ="summaryStatistics.sensor.csv", summary.arr[,,1])
}

########################################################################
## boxplot the data
########################################################################
if( Boxplot ) {
	pdf( paste0("boxplot.rawData.", responseName, ".pdf") )
	yRange <- c( min(summary.arr[, "Min",responseName], na.rm=TRUE),
            max(summary.arr[, "Max",responseName], na.rm=TRUE))
	boxplotCGM.fn(dataFolder, dataFiles, idxNA, responseName, sensorIDs, columnNames, yRange,
	    skip=Skip, header=Header, comment.char=Comment.char, sep=Sep, cex.axis1=1)
	dev.off()
}

##########################################################################################
## main analytic process including quality control, interval adjustment, MSE calculation
##########################################################################################

pdf( paste("timeSeriesPlot", responseName, ".pdf", sep=".") )
par(mfrow=c(3,2))
for( iFile in 1:nFile ) {
# iFile <- 1 #iFile <- 2 # iFile <- 19
    print( paste0("Process the ", iFile, "th file: ", dataFiles[iFile]) )
	data.df0 <- read.table(paste(dataFolder, dataFiles[iFile], sep="/"),
	    skip=Skip, header=Header, comment.char=Comment.char, sep=Sep)
	if( !Header ) {
		data.df0 <- data.df0[, 1:length(columnNames)]
	    dimnames(data.df0)[[2]] <- columnNames
	}

	if( !is.na(idxNA) ) data.df0[ data.df0[, responseName] == idxNA, responseName] <- NA
	########################################################################
	## convert time :  timeSeqConversion.fn
	########################################################################
	for( i in 1:length(timeStamp.column) ) {
	    if(i==1) { timeStamp.vec <- data.df0[, timeStamp.column[i] ] } else {
		   timeStamp.vec <- paste0(timeStamp.vec, " ", data.df0[, timeStamp.column[i] ])
		}
	}
	Time.mat <- timeSeqConversion.fn(time.stamp=timeStamp.vec, time.format=time.format,
	            timeUnit=timeUnit)
	data.df <- data.frame( timeStamp.vec, Time.mat[,1], data.df0[,responseName] )
						   # This needs to be changed if multiple responses
    dimnames(data.df)[[2]] <- c("timeStamp", "timeSeries", responseName)
	data.df <- data.df[ order(data.df[, "timeSeries"]), ]

	########################################################################
	## derive the data with equal interval:  equalInterval.fn
	########################################################################
    xx0 <- data.df[, "timeSeries"]
	yy0 <- data.df[, responseName]
	a <- table( diff(xx0) )
	if( length(a) > 1) print("the time interval is not fixed and thus may need to be adjusted.")

	dataEqualSpace.mat <- equalInterval.fn(x=xx0, y=yy0, Interval=equal.interval)

	########################################################################
	## fix missing value:  fixMissing.fn
	########################################################################
    #   fixMissing = TRUE; fixMethod = "linearInterpolation"
    fixMissing <- TRUE; fixMethod <- "skip"
	if( fixMissing == TRUE )
		dataFixNA.mat <- fixMissing.fn( y=dataEqualSpace.mat[, "signal"],
		      x=dataEqualSpace.mat[, "timeSeries"], Method=fixMethod)

	#################################################################################
	# calculate multiscale entropy
	#################################################################################
	xx1 <- dataFixNA.mat[,1]; yy1 <- dataFixNA.mat[,2]
    theMSE.mat <-
		MSEbyC.fn(yy1, scaleMax, scaleStep, mMin=m, mMax=m, mStep=1, rMin=r, rMax=r, I=I,
		          cmdCore=cmdCore, cFolder=cFolder )

	summaryAdj.vec <- c( "N.total"=length(yy0), "N.missing"=sum(is.na(yy0)), "Mean"=mean(yy1),
	                  "SD"=sd(yy1), "Median"=median(yy1), "MAD"=mad(yy1) )
	if( iFile == 1 ) {
		summaryAdj.mat <- summaryAdj.vec
		MSE.mat <- theMSE.mat[, "SampleEntropy"]
	} else {
		summaryAdj.mat <- rbind( summaryAdj.mat, summaryAdj.vec )
		MSE.mat <- rbind(MSE.mat, theMSE.mat[, "SampleEntropy"])
	}

	########################################################################
	## plot data :  plotTseries.fn
	########################################################################
	meanY <- mean(yy0, na.rm=TRUE)
	plotTseries.fn(	x=xx0, y=yy0, xAt=NA, xLab=NA, yRange=NA,  Frame=TRUE,
	      xlab=paste("Time in", timeUnit), ylab=responseName, pch=1, lty=1,
	      col.point=1, col.line=1, cex.point=0.5, lwd=1 )
	lines( range(xx0, na.rm=TRUE), rep(meanY, 2), col="grey" )
	title( main=paste0(iFile, ":", sensorIDs[iFile], ":", subjectTypes[iFile], " - Raw Data"),
	       sub=paste0("N.total=", length(yy0), ", N.noNA=", sum(!is.na(yy0)),
	          ", Mean=", round(meanY,3), ", SD=", round(sd(yy0, na.rm=TRUE),3) ) )

	plotTseries.fn(	x=xx1, y=yy1, xAt=NA, xLab=NA, yRange=NA,  Frame=TRUE,
	        xlab=paste("Time in", timeUnit), ylab=responseName, pch=1, lty=1,
					col.point=1, col.line=1, cex.point=0.5, lwd=1 )
    lines( range(data.df[, "timeSeries"], na.rm=TRUE), rep(mean(yy1, na.rm=TRUE), 2), col="grey" )
	title( main=paste0(iFile, ":", sensorIDs[iFile], ":", subjectTypes[iFile],
	      " - Adjusted Data"), sub=paste0("N.total=", round(summaryAdj.vec["N.total"],0),
	      ", Entropy=", theMSE.mat[1, "SampleEntropy"],", Mean=",
	      round(summaryAdj.vec["Mean"],3), ", SD=", round(summaryAdj.vec["SD"],3) ) )
}
dimnames(MSE.mat) <- list(sensorIDs, Scales)
dimnames(summaryAdj.mat)[[1]] <- sensorIDs
dev.off()

######################################################################################
# compare mean, median, sample entropy et al by group
######################################################################################
# Calculate the major results for group comparison among different disease statuses

Types <- unique( subjectTypes )
Types <- Types[order(Types)]
nType <- length(Types)
nPair <- nType*(nType-1)/2

# for average value in each type
resultMean.vec <- pairwiseComparison.fn(y=summaryAdj.mat[, "Mean"], INDEX=subjectTypes, na.rm=TRUE)

# for MSE in each scale and each type
for( i in 1:dim(MSE.mat)[2] ) {
  theResult.vec <- pairwiseComparison.fn(y=MSE.mat[, i], INDEX=subjectTypes, na.rm=TRUE)
  if( i==1 ) { pvalSSMD.mat <- theResult.vec } else {
    pvalSSMD.mat <-rbind( pvalSSMD.mat, theResult.vec)
  }
}
dimnames( pvalSSMD.mat)[1] <- list( Scales )

write.csv(file ="MSE.csv", MSE.mat )
write.csv(file ="pvalSSMD.csv", pvalSSMD.mat)
write.csv(file ="groupComp.mean.csv", round(resultMean.vec,5) )
write.csv(file ="groupMeanSD.MSE.csv", round(pvalSSMD.mat[, -(1:(nPair*5)) ],5) )
write.csv(file ="groupSSMDpvalue.MSE.csv", pvalSSMD.mat[, 1:(nPair*5)] )


outNames <- dimnames(pvalSSMD.mat)[[2]]
isSSMD <- substring(outNames, 1, 4) == "SSMD"
SSMD.mean.vec <- resultMean.vec[ isSSMD ]
SSMD.mat <- as.matrix( pvalSSMD.mat[, isSSMD] )
ssmdEffect.mat <- matrix( NA, nrow=nrow(SSMD.mat), ncol=ncol(SSMD.mat),
                  dimnames=dimnames(SSMD.mat) )
for( i in 1:ncol(ssmdEffect.mat) ) {
	ssmdEffect.mat[,i] <- ssmdEffect.fn(SSMD.mat[,i], criterion="subType")
}
dimnames(ssmdEffect.mat)[1] <- list(paste0("sampleEntropy", substring( Scales+100, 2,3 ) ))
write.csv(file ="groupEffect.csv",
          data.frame(t(ssmdEffect.mat),
          "glucose"=ssmdEffect.fn(SSMD.mean.vec, criterion="subType")))

######################################################################################
# plot sample entropy by individual and by group
######################################################################################
scalesInTime <- Scales*equal.interval
pdf( paste0("MSEplot.pdf") )
par( mfrow=c(1,1) )
col.vec <- rep(NA, length(subjectTypes) )
for( i in 1:nType ) { col.vec[ subjectTypes == Types[i] ] <- i }
MSEplot.fn(scalesInTime, MSE=t(MSE.mat), Name=Types, responseName="glucose", timeUnit="minute",
        byGroup=FALSE, MSEsd=NA, N=NA, stdError=TRUE, xRange=NA, yRange=NA,  pch=1,
        las=2, col=col.vec, Position="topleft", cex.legend=0.0005, main="A: MSE by individual")
legend("topleft", legend=paste0(Types, "(N=", table( subjectTypes ), ")"),
	   col=1:nType, cex=1, lty=1, pch=1)

outNames <- dimnames(pvalSSMD.mat)[[2]]
MSEmean.mat <- pvalSSMD.mat[, substring(outNames, 1, 4) == "mean"]
MSEsd.mat <- pvalSSMD.mat[, substring(outNames, 1, 2) == "SD"]
N.mat <- pvalSSMD.mat[, substring(outNames, 1, 1) == "N"]
MSEplot.fn(scalesInTime, MSE=MSEmean.mat, Name=Types, responseName="glucose", timeUnit="minute",
      byGroup=TRUE, MSEsd=MSEsd.mat, N=N.mat, stdError=TRUE, xRange=NA, yRange=NA,
      las=2, col=NA, pch=1:length(Types), Position="topleft", cex.legend=0.75,
      main="B: MSE by group")
dev.off()

######################################################################################
# plot results by pairwise comparison of groups : antenna plot
######################################################################################

pdf( paste0("antennaPlot.pdf") )
par( mfrow=c(1,1) )
# antenna plot for average glucose level
mDiff.vec <- resultMean.vec[substring(outNames, 1, 5) == "mDiff"]
CIlower.vec <- resultMean.vec[ substring(outNames, 1, 7) == "CIlower"]
CIupper.vec <- resultMean.vec[ substring(outNames, 1, 7) == "CIupper"]
pairNames <- gsub("mDiff_", "", names(mDiff.vec), fixed=TRUE)
xRange <- range( c( range( CIlower.vec, na.rm=TRUE), 0, range( CIupper.vec, na.rm=TRUE) )  )
yRange <- range( c(0, SSMD.mean.vec), na.rm=TRUE )
condt <- !is.na(mDiff.vec) & !is.na(SSMD.mean.vec)
antennaPlot.fn(Mean=mDiff.vec[condt], SSMD=SSMD.mean.vec[condt], Name = pairNames[condt],
        CIlower=CIlower.vec[condt], CIupper=CIupper.vec[condt], xRange=xRange, yRange=yRange,
        col=1:length(pairNames[condt]), pch=1:length(pairNames[condt]), cex=0.6,
        Position = "bottomright", main="Average Glucose Level")

#antenna plots for MSE at each scale
mDiff.mat <- as.matrix(pvalSSMD.mat[, substring(outNames, 1, 5) == "mDiff"])
CIlower.mat <- as.matrix(pvalSSMD.mat[, substring(outNames, 1, 7) == "CIlower"])
CIupper.mat <- as.matrix(pvalSSMD.mat[, substring(outNames, 1, 7) == "CIupper"])
xRange <- range( c( range( CIlower.mat, na.rm=TRUE), 0, range( CIupper.mat, na.rm=TRUE) )  )
yRange <- range( c(0, range( SSMD.mat, na.rm=TRUE ) ) )
for( i in 1:length(Scales) ) {
    Main <- paste0("Sample entropy at Scale = ", Scales[i], " (i.e., in ", scalesInTime[i],
    " ", timeUnit, "s)")
    condt <- !is.na(mDiff.mat[i,]) & !is.na(SSMD.mat[i,])
	antennaPlot.fn(Mean=mDiff.mat[i,condt], SSMD=SSMD.mat[i, condt], Name = pairNames[condt],
	              CIlower=CIlower.mat[i,condt], CIupper=CIupper.mat[i,condt], xRange=xRange,
	              yRange=yRange, col=1:length(pairNames[condt]), pch=1:length(pairNames[condt]),
	              cex=0.6, Position = "bottomright", main = Main)
}
dev.off()

}


}
