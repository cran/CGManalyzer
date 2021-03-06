# for the .csv has the following format coming from the Glutalor CGM device
#-------------------------------------------------------------------------------------------------
#ID	PatientId	SensorId	Time	Value
#1	S0002P0004	1511131055-8	2016:05:27:12:07	0
#2	S0002P0004	1511131055-8	2016:05:27:12:10	0
#3	S0002P0004	1511131055-8	2016:05:27:12:13	0
# ...
#-------------------------------------------------------------------------------------------------

##########################################################################################
# set up for reading data
##########################################################################################
# Please complete the following steps before reading any data in R
#  1. Put all the data (in .csv or .txt) in the data folder and remove all .zip or other files
#  2. Call createFileList.fn create a file named "00filelist.csv" which holds
#     all the names of data files, one file for the data in one subject or sensor.
#  3. Change the second column in generated file "00filelist.csv" in which the group type for each
#     subject is specified. After the addition is completed, the first 5 lines in "00filelist.csv"
#     should be more or less as follows
#______________________________
#dataFiles,dataType
#00filelist.csv,H
#S0003P0001T1.csv,H
#S0003P0001T2.csv,H
#S0003P0002.csv,H
#S0003P0003.csv,H
#_______________________________
#  4. Each data file should have the same structure with the same number of columns and
#     the same column names, the same time stamp structure, the same number of head lines
#     to be skipped for read
#  5. change the settings in the SPEC.R file so that the input for the parameters corresponds to
#     your data file and the CGM device that is used in your study.
#******************************************************************************************
# 'Skip' in skip=Skip
# 'timeStamp' in timeStamp = data.df0[,"timeStamp"]
# 'Glucose.reading.mmol.L.' in data.df0[,responseName]
# 00filelist.csv: a file containing the file names for time series data

#******************************************************
# SPEC for reading the data in R
#******************************************************
dataFolder = file.path(mainFolder, "data")
dataFileType.df = read.table( paste(dataFolder, "00filelist.csv", sep="/"), skip=2, sep="," ,stringsAsFactors = TRUE, as.is = FALSE)
dataFiles = fac2char.fn(dataFileType.df[,1])
subjectTypes = fac2char.fn(dataFileType.df[,2])
nFile = length(dataFiles)
dataFileSuffix = ".csv"   # suffix for the name of data files
sensorIDs = gsub(dataFileSuffix, "", dataFiles, fixed=TRUE)
Skip = 3  # number of lines to be skipped in each data file when the data is read in R
Header = FALSE  # whether to use the first line after skipping 'Skip' lines as the column names
if( !Header ) columnNames = c("Serial",	"Meter",	"Time",	"Glucose")
Comment.char=""  # in read.table()
Sep=","   # usually, Sep=',' when reading '.csv' and Sep='\t' when reading '.txt'.

timeStamp.column = "Time" # this must be corrsponding to the column names after using read.table()
responseName = "Glucose"  # this must be corrsponding to the column names after using read.table()
timeUnit = "minute"       # the smallest time unit in the time series
equal.interval = 3        # the size of interval between two consecutive points

#time.format = "yyyy:mm:\tdd:hh:nn" #"2016:08:11:09:14"  ### handle with various time formats
time.format = "yyyy:mm:dd:hh:nn" #"2016:08:11:09:14"
 # The format must have 'y' for year, 'm' for month, 'd' for day, 'h' for hour, 'n' for minute,
 #                 's' for second, 'i' for millisecond(one thousandth of a second), each uniquely
idxNA = 0   # idxNA = NA if the missing value is marked with NA correctly
              ####handle with the situation of using different symbol for missing value

#******************************************************
# SPEC for calculating multiscale sample entropy
#******************************************************
r <- 0.15
m <- 2
I <- 400000
scaleMax <- 10; scaleStep <- 1
Scales <- seq(1, scaleMax, by=scaleStep)
cFile = "mseLong.c"

#******************************************************************************************
# SPEC for whether to run calculation of summary statistics and/or boxplot for each sensor
#******************************************************************************************
Summary = TRUE
Boxplot = TRUE
