
compileC.fn = function(cFile="mse.c", cFolder) {
# **********************************************************************************
# Function to compile a C program for calculating multiscale entropy (MSE) 
#     of an equally spaced time series. 
#
# ARGUMENTS
#  cFile: file name for the C program for calculating MSE.
#         The file name must have a low case 'c' after "." such as "mse.c"
#  cFolder: the folder to hold the .c file for the C programs
# VERSION
#  V1:  X.H.D. Zhang, July 7, 2017
#
# Example:    
#   a <- system.file("SPEC", cFile, package = package.name)
#   cFolder0 <- substring(a, 1, nchar(a)-nchar(cFile)-1 )
#   cFolder <- gsub("Program Files", "PROGRA~1", cFolder0, fixed = TRUE)
#   compileC.fn(cFile=cFile, cFolder=cFolder) 
#******************************************************************************************
 
  cmdCore <- gsub(".c", "", cFile, fixed = TRUE)
  shell( paste0("gcc -o ", cFolder, "/", cmdCore, " -O ", cFolder, "/", cFile, " -lm -w"), 
		 wait=TRUE, intern=FALSE, invisible=TRUE )

}

