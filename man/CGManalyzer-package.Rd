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
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).

Costa M., Goldberger A.L., Peng C.-K. Multiscale entropy analysis of physiologic time series. Phys Rev Lett 2002; 89:062102.

Goldberger AL, Amaral LAN, Glass L, Hausdorff JM, Ivanov PCh, Mark RG, Mietus JE, Moody GB, Peng C-K, Stanley HE. PhysioBank, PhysioToolkit, and PhysioNet: Components of a New Research Resource for Complex Physiologic Signals (2003). Circulation. 101(23):e215-e220.
}

\keyword{ package }


\examples{
###################################################################################
# The following are the example in the help file for CGManalyzer, which is also
# the main file for using the Package
###################################################################################

# install from local source pacage
# install.packages(paste0(getwd(), "/CGManalyzer_1.2.tar.gz"), repos = NULL)
# or install from CRAN
# install.packages("CGManalyzer")

library(CGManalyzer)

rm(list = ls())
package.name <- "CGManalyzer"
options(scipen = 999)
mainFolder <- getwd()

###################################################################################
# use example data or your own data
###################################################################################
useExampleData <- TRUE

if (useExampleData) {
  SPEC.name <- "SPECexample.R"
} else{
  ###################################################################################
  # TODOs before creating file "00filelist.csv":
  # Create a folder named "data" in working directory
  # and copy files into it manually.
  ###################################################################################


  ###################################################################################
  # Create a file named "00filelist.csv" in "data" folder with two columns,
  # one for file name and one for group type.
  ###################################################################################
  dataFolder <-  file.path(mainFolder, "data")
  listFile <- file.path(dataFolder, "00filelist.csv")
  # create a empty file
  if(!file.exists(listFile)){
    file.create(listFile)
    # list all files and their types
    dataFiles <- list.files(dataFolder)
    if(length(dataFiles)>1){
      dataType <- rep("H",length(dataFiles))
      fileFrame <- data.frame(dataFiles, dataType)
      # save to 00filelist.csv file
      write.csv(fileFrame,listFile,row.names = FALSE,quote = F)
    }else{
      stop("No data files found in the \"data\" folder!")
    }
  }

  ###################################################################################
  # TODOS after creating file "00filelist.csv":
  # Specify the group type for each subject by changing the second
  # column of 00filelist.csv manually. The default type is "H",
  # which needs to be changed to the actual type manually.
  ###################################################################################

  ###################################################################################
  # specify parameters for different CGM devices
  ###################################################################################
  # Choose one of the SPEC file that fits the CGM device that you are using for your study
  # to use the example data in the package "CGManalyzer", you have to choose "SPECexample.R"
  # if you want to run your own data, you cannot choose "SPECexample.R", instead, you need to choose
  # one of "SPEC.FreestyleLibre.R", "SPEC.Glutalor.R" and "SPEC.Medtronic.R".
  # You may also write your own file following the format in one of them.

  SPEC.name <- "SPEC.FreestyleLibre.R"
  # SPEC.name <- "SPEC.Glutalor.R"
  # SPEC.name <- "SPEC.Medtronic.R"
}

setSPEC.fn(SPEC.name)

###################################################################################
## get summary statistics: number of subjects, minimum, 1st quartile, median, mean,
##             mean, 2nd quartile, maximum, number of NA's, standard deviation, MAD
###################################################################################
if (Summary) {
  summary.arr <-
    summaryCGM.fn(
      dataFolder,
      dataFiles,
      responseName,
      sensorIDs,
      columnNames,
      skip = Skip,
      header = Header,
      comment.char = Comment.char,
      sep = Sep
    )
  # write.csv(file = "summaryStatistics.sensor.csv",
  #           summary.arr[, , 1])
  print(summary.arr[,,1])
}

########################################################################
## boxplot the data
########################################################################
if (Boxplot) {
  # filename <- paste("boxplot.rawData", responseName, "pdf",sep=".")
  # pdf(filename)
  yRange <- c(min(summary.arr[, "Min", responseName], na.rm = TRUE),
              max(summary.arr[, "Max", responseName], na.rm = TRUE))
  boxplotCGM.fn(
    dataFolder,
    dataFiles,
    idxNA,
    responseName,
    sensorIDs,
    columnNames,
    yRange,
    skip = Skip,
    header = Header,
    comment.char = Comment.char,
    sep = Sep,
    cex.axis1 = 1
  )
  # dev.off()
}

##########################################################################################
## main analytic process including quality control, interval adjustment, MODD, CONGA, MSE
## calculation
##########################################################################################

fixMissing <- TRUE
fixMethod <- "skip"
calculateMODD <- TRUE
calculateCONGA <- TRUE
n.CONGA <- 2

# filename <- paste("timeSeriesPlot", responseName, "pdf", sep = ".")
# pdf(filename)
par(mfrow = c(3, 2))
for (iFile in 1:nFile) {
  # iFile <- 1 #iFile <- 2 # iFile <- 19
  print(paste0("Process the ", iFile, "th file: ", dataFiles[iFile]))
  data.df0 <-
    read.table(
      paste(dataFolder, dataFiles[iFile], sep = "/"),
      skip = Skip,
      header = Header,
      comment.char = Comment.char,
      sep = Sep
    )
  if (!Header) {
    data.df0 <- data.df0[, 1:length(columnNames)]
    dimnames(data.df0)[[2]] <- columnNames
  }

  if (!is.na(idxNA))
    data.df0[data.df0[, responseName] == idxNA, responseName] <- NA
  ########################################################################
  ## convert time :  timeSeqConversion.fn
  ########################################################################
  for (i in 1:length(timeStamp.column)) {
    if (i == 1) {
      timeStamp.vec <- data.df0[, timeStamp.column[i]]
    } else {
      timeStamp.vec <-
        paste0(timeStamp.vec, " ", data.df0[, timeStamp.column[i]])
    }
  }
  Time.mat <-
    timeSeqConversion.fn(time.stamp = timeStamp.vec,
                         time.format = time.format,
                         timeUnit = timeUnit)
  data.df <-
    data.frame(timeStamp.vec, Time.mat[, 1], data.df0[, responseName])
  # This needs to be changed if multiple responses
  dimnames(data.df)[[2]] <-
    c("timeStamp", "timeSeries", responseName)
  data.df <- data.df[order(data.df[, "timeSeries"]),]

  ########################################################################
  ## derive the data with equal interval:  equalInterval.fn
  ########################################################################
  xx0 <- data.df[, "timeSeries"]
  yy0 <- data.df[, responseName]
  a <- table(diff(xx0))
  if (length(a) > 1)
  {
    print("the time interval is not fixed and thus may need to be adjusted.")
  }

  dataEqualSpace.mat <-
    equalInterval.fn(x = xx0, y = yy0, Interval = equal.interval)

  ########################################################################
  ## fix missing value:  fixMissing.fn
  ########################################################################
  #fixMissing = TRUE; fixMethod = "linearInterpolation"
  if (fixMissing == TRUE) {
    dataFixNA.mat <- fixMissing.fn(y = dataEqualSpace.mat[, "signal"],
                                   x = dataEqualSpace.mat[, "timeSeries"],
                                   Method = fixMethod)
  }

  ########################################################################
  ## calculate MODD:  MODD.fn
  ########################################################################
  if (calculateMODD == TRUE) {
    theMODD <-
      MODD.fn(y = dataEqualSpace.mat[, "signal"], Interval = equal.interval)
  } else {
    theMODD <- NA
  }

  ########################################################################
  ## calculate CONGA:  CONGA.fn
  ########################################################################
  if (calculateCONGA == TRUE) {
    theCONGA <-
      CONGA.fn(y = dataEqualSpace.mat[, "signal"],
               Interval = equal.interval,
               n = n.CONGA)
  } else {
    theCONGA <- NA
  }

  ########################################################################
  # calculate multiscale entropy
  ########################################################################
  xx1 <- dataFixNA.mat[, 1]
  yy1 <- dataFixNA.mat[, 2]
  theMSE.mat <-
    MSEbyC.fn(
      yy1,
      scaleMax,
      scaleStep,
      mMin = m,
      mMax = m,
      mStep = 1,
      rMin = r,
      rMax = r,
      I = I
    )

  summaryAdj.vec <-
    c(
      "N.total" = length(yy0),
      "N.missing" = sum(is.na(yy0)),
      "Mean" = mean(yy1),
      "SD" = sd(yy1),
      "Median" = median(yy1),
      "MAD" = mad(yy1)
    )
  if (iFile == 1) {
    summaryAdj.mat <- summaryAdj.vec
    MSE.mat <- theMSE.mat[, "SampleEntropy"]
    MODD.CONGA.mat <- c(theMODD,  theCONGA)
  } else {
    summaryAdj.mat <- rbind(summaryAdj.mat, summaryAdj.vec)
    MSE.mat <- rbind(MSE.mat, theMSE.mat[, "SampleEntropy"])
    MODD.CONGA.mat <-
      rbind(MODD.CONGA.mat, c(theMODD, theCONGA))
  }

  ########################################################################
  ## plot data :  plotTseries.fn
  ########################################################################
  meanY <- mean(yy0, na.rm = TRUE)
  plotTseries.fn(
    x = xx0,
    y = yy0,
    xAt = NA,
    xLab = NA,
    yRange = NA,
    Frame = TRUE,
    xlab = paste("Time in", timeUnit),
    ylab = responseName,
    pch = 1,
    lty = 1,
    col.point = 1,
    col.line = 1,
    cex.point = 0.5,
    lwd = 1
  )
  lines(range(xx0, na.rm = TRUE), rep(meanY, 2), col = "grey")
  title(
    main = paste0(iFile, ":", sensorIDs[iFile], ":", subjectTypes[iFile],
                  " - Raw Data"),
    sub = paste0(
      "N.total=",
      length(yy0),
      ", N.noNA=",
      sum(!is.na(yy0)),
      ", Mean=",
      round(meanY, 3),
      ", SD=",
      round(sd(yy0, na.rm =
                 TRUE), 3)
    )
  )

  plotTseries.fn(
    x = xx1,
    y = yy1,
    xAt = NA,
    xLab = NA,
    yRange = NA,
    Frame = TRUE,
    xlab = paste("Time in", timeUnit),
    ylab = responseName,
    pch = 1,
    lty = 1,
    col.point = 1,
    col.line = 1,
    cex.point = 0.5,
    lwd = 1
  )
  lines(range(data.df[, "timeSeries"], na.rm = TRUE), rep(mean(yy1, na.rm =
                                                                 TRUE), 2),
        col = "grey")
  title(
    main = paste0(iFile, ":", sensorIDs[iFile], ":", subjectTypes[iFile],
                  " - Adjusted Data"),
    sub = paste0(
      "N.total=",
      round(summaryAdj.vec["N.total"], 0),
      ", Entropy=",
      theMSE.mat[1, "SampleEntropy"],
      ", Mean=",
      round(summaryAdj.vec["Mean"], 3),
      ", SD=",
      round(summaryAdj.vec["SD"], 3)
    )
  )
}
dimnames(MSE.mat) <- list(sensorIDs, Scales)
dimnames(MODD.CONGA.mat) <- list(sensorIDs, c("MODD", "CONGA"))
dimnames(summaryAdj.mat)[[1]] <- sensorIDs
# dev.off()

######################################################################################
# compare mean, median, sample entropy et al by group
# there must be at least 2 different types
# in file "00filelist.csv" by modifying manually
######################################################################################
# Calculate the major results for group comparison among different disease statuses

Types <- unique(subjectTypes)
Types <- Types[order(Types)]
nType <- length(Types)
nPair <- nType * (nType - 1) / 2

# for average value in each type
resultMean.vec <-
  pairwiseComparison.fn(y = summaryAdj.mat[, "Mean"],
                        INDEX = subjectTypes, na.rm =
                          TRUE)

# for MSE in each scale and each type
for (i in 1:dim(MSE.mat)[2]) {
  theResult.vec <-
    pairwiseComparison.fn(y = MSE.mat[, i],
                          INDEX = subjectTypes,
                          na.rm = TRUE)
  if (i == 1) {
    pvalSSMD.mat <- theResult.vec
  } else {
    pvalSSMD.mat <- rbind(pvalSSMD.mat, theResult.vec)
  }
}
dimnames(pvalSSMD.mat)[1] <- list(Scales)

# write.csv(file = "MSE.csv", MSE.mat)
# write.csv(file = "pvalSSMD.csv", pvalSSMD.mat)
# write.csv(file = "groupComp.mean.csv", round(resultMean.vec, 5))
# write.csv(file = "groupMeanSD.MSE.csv", round(pvalSSMD.mat[,-(1:(nPair *5))], 5))
# write.csv(file = "groupSSMDpvalue.MSE.csv", pvalSSMD.mat[, 1:(nPair *5)])
print(MSE.mat)
print(pvalSSMD.mat)
print(round(resultMean.vec, 5))
print(round(pvalSSMD.mat[,-(1:(nPair *5))], 5))
print(pvalSSMD.mat[, 1:(nPair *5)])


outNames <- dimnames(pvalSSMD.mat)[[2]]
isSSMD <- substring(outNames, 1, 4) == "SSMD"
SSMD.mean.vec <- resultMean.vec[isSSMD]
SSMD.mat <- as.matrix(pvalSSMD.mat[, isSSMD])
ssmdEffect.mat <-
  matrix(
    NA,
    nrow = nrow(SSMD.mat),
    ncol = ncol(SSMD.mat),
    dimnames = dimnames(SSMD.mat)
  )
for (i in 1:ncol(ssmdEffect.mat)) {
  ssmdEffect.mat[, i] <-
    ssmdEffect.fn(SSMD.mat[, i], criterion = "subType")
}
dimnames(ssmdEffect.mat)[1] <-
  list(paste0("sampleEntropy", substring(Scales + 100, 2, 3)))
# write.csv(file = "groupEffect.csv", data.frame(t(ssmdEffect.mat),
#                                               "glucose" = ssmdEffect.fn(SSMD.mean.vec, criterion =
#                                                                           "subType")
#                        ))
print(data.frame(t(ssmdEffect.mat),
                  "glucose" = ssmdEffect.fn(SSMD.mean.vec, criterion =
                                            "subType")
                       ))

######################################################################################
# plot sample entropy by individual and by group
######################################################################################
scalesInTime <- Scales * equal.interval
# filename <- "MSEplot.pdf"
# pdf(filename)
par(mfrow = c(1, 1))
col.vec <- rep(NA, length(subjectTypes))
for (i in 1:nType) {
  col.vec[subjectTypes == Types[i]] <- i
}
MSEplot.fn(
  scalesInTime,
  MSE = t(MSE.mat),
  Name = Types,
  responseName = "glucose",
  timeUnit = "minute",
  byGroup = FALSE,
  MSEsd = NA,
  N = NA,
  stdError = TRUE,
  xRange = NA,
  yRange = NA,
  pch = 1,
  las = 2,
  col = col.vec,
  Position = "topleft",
  cex.legend = 0.0005,
  main = "A: MSE by individual"
)
legend(
  "topleft",
  legend = paste0(Types, "(N=", table(subjectTypes), ")"),
  col = 1:nType,
  cex = 1,
  lty = 1,
  pch = 1
)

outNames <- dimnames(pvalSSMD.mat)[[2]]
MSEmean.mat <- pvalSSMD.mat[, substring(outNames, 1, 4) == "mean"]
MSEsd.mat <- pvalSSMD.mat[, substring(outNames, 1, 2) == "SD"]
N.mat <- pvalSSMD.mat[, substring(outNames, 1, 1) == "N"]
MSEplot.fn(
  scalesInTime,
  MSE = MSEmean.mat,
  Name = Types,
  responseName = "glucose",
  timeUnit = "minute",
  byGroup = TRUE,
  MSEsd = MSEsd.mat,
  N = N.mat,
  stdError = TRUE,
  xRange = NA,
  yRange = NA,
  las = 2,
  col = NA,
  pch = 1:length(Types),
  Position = "topleft",
  cex.legend = 0.75,
  main = "B: MSE by group"
)
# dev.off()

######################################################################################
# plot results by pairwise comparison of groups : antenna plot
######################################################################################

# filename <- "antennaPlot.pdf"
# pdf(filename)
par(mfrow = c(1, 1))
# antenna plot for average glucose level
mDiff.vec <- resultMean.vec[substring(outNames, 1, 5) == "mDiff"]
CIlower.vec <-
  resultMean.vec[substring(outNames, 1, 7) == "CIlower"]
CIupper.vec <-
  resultMean.vec[substring(outNames, 1, 7) == "CIupper"]
pairNames <- gsub("mDiff_", "", names(mDiff.vec), fixed = TRUE)
xRange <-
  range(c(
    range(CIlower.vec, na.rm = TRUE),
    0,
    range(CIupper.vec, na.rm = TRUE)
  ))
yRange <- range(c(0, SSMD.mean.vec), na.rm = TRUE)
condt <- !is.na(mDiff.vec) & !is.na(SSMD.mean.vec)
antennaPlot.fn(
  Mean = mDiff.vec[condt],
  SSMD = SSMD.mean.vec[condt],
  Name = pairNames[condt],
  CIlower = CIlower.vec[condt],
  CIupper = CIupper.vec[condt],
  xRange = xRange,
  yRange = yRange,
  col = 1:length(pairNames[condt]),
  pch = 1:length(pairNames[condt]),
  cex = 0.6,
  Position = "bottomright",
  main = "Average Glucose Level"
)

#antenna plots for MSE at each scale
mDiff.mat <-
  as.matrix(pvalSSMD.mat[, substring(outNames, 1, 5) == "mDiff"])
CIlower.mat <-
  as.matrix(pvalSSMD.mat[, substring(outNames, 1, 7) == "CIlower"])
CIupper.mat <-
  as.matrix(pvalSSMD.mat[, substring(outNames, 1, 7) == "CIupper"])
xRange <-
  range(c(
    range(CIlower.mat, na.rm = TRUE),
    0,
    range(CIupper.mat, na.rm = TRUE)
  ))
yRange <- range(c(0, range(SSMD.mat, na.rm = TRUE)))
for (i in 1:length(Scales)) {
  Main <-
    paste0("Sample entropy at Scale = ",
           Scales[i],
           " (i.e., in ",
           scalesInTime[i],
           " ",
           timeUnit,
           "s)")
  condt <- !is.na(mDiff.mat[i, ]) & !is.na(SSMD.mat[i, ])
  antennaPlot.fn(
    Mean = mDiff.mat[i, condt],
    SSMD = SSMD.mat[i, condt],
    Name = pairNames[condt],
    CIlower = CIlower.mat[i, condt],
    CIupper = CIupper.mat[i, condt],
    xRange = xRange,
    yRange = yRange,
    col = 1:length(pairNames[condt]),
    pch = 1:length(pairNames[condt]),
    cex = 0.6,
    Position = "bottomright",
    main = Main
  )
}
# dev.off()
}
