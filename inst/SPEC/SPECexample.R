
#******************************************************
# SPEC for reading the data in R
#******************************************************
a0 <- "00filelist.csv"
a <- system.file("exampleDATA", a0, package = package.name)
dataFolder <- substring(a, 1, nchar(a)-nchar(a0)-1 )

dataFileType.df = read.table( paste(dataFolder, "00filelist.csv", sep="/"), skip=2, sep="," )
dataFiles = fac2char.fn(dataFileType.df[,1])
subjectTypes = fac2char.fn(dataFileType.df[,2])
nFile = length(dataFiles)
dataFileSuffix = ".csv"   # suffix for the name of data files
sensorIDs = gsub(dataFileSuffix, "", dataFiles, fixed=TRUE)
Skip = 1  # number of lines to be skipped in each data file when the data is read in R
#Skip = 3  # number of lines to be skipped in each data file when the data is read in R
Header = FALSE  # whether to use the first line after skipping 'Skip' lines as the column names
if( !Header ) columnNames = c("Time",	"Glucose")   #must specify
#if( !Header ) columnNames = c("ID",	"PatientId",	"SensorId",	"Time",	"Glucose")
#if( !Header ) columnNames = c("serialNumber",	"SensorId",	"Time",	"Glucose", "Empty1", "Empty2")
Comment.char=""  # in read.table()
Sep=","   # usually, Sep=',' when reading '.csv' and Sep='\t' when reading '.txt'.

timeStamp.column = "Time" # this must be corrsponding to the column names after using read.table()
responseName = "Glucose"  # this must be corrsponding to the column names after using read.table()
timeUnit = "minute"       # the smallest time unit in the time series 
equal.interval = 3        # the size of interval between two consecutive points
#equal.interval = 15        # the size of interval between two consecutive points

#time.format = "yyyy:mm:\tdd:hh:nn" #"2016:08:11:09:14"  ### handle with various time formats
time.format = "yyyy:mm:dd:hh:nn" #"2016:08:11:09:14"
 # The format must have 'y' for year, 'm' for month, 'd' for day, 'h' for hour, 'n' for minute,
 #                 's' for second, 'i' for millisecond(one thousandth of a second), each uniquely
idxNA = 0   # idxNA = NA if the missing value is marked with NA correctly
              ####handle with the situation of using different symbol for missing value
#idxNA = NA   # idxNA = NA if the missing value is marked with NA correctly

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
