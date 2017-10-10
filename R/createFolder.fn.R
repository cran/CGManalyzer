
createFolder.fn = function(mainFolder) {
#****************************************************************************************************
# Function to create folders for main folder and its sub-folders: Analysis, Data and Functions 
# mainFolder: the main folder or directory for a study to be analyzed
# Author: Xiaohua Douglas Zhang
# Date:   2017-06-06
# example:
#   mainFolder <- paste(getwd(), "Test", sep="/")
#   createFolder.fn(mainFolder)
#*****************************************************************************************************
	analysisFolder = file.path(mainFolder, "Analysis")
	dataFolder = file.path(mainFolder, "Data")

	if( file.exists(mainFolder) ) {
		print(paste("Note:", mainFolder, "exists already, no need to create it again!") )
	} else { dir.create( mainFolder ) }

	if (file.exists(analysisFolder)){
		setwd( analysisFolder )
		print(paste("Note:", analysisFolder, "exists already, no need to create it again!") )
	} else {
		dir.create( analysisFolder )
		setwd(analysisFolder)
	}

	if( file.exists(dataFolder) ) {
		print(paste("Note:", dataFolder, "exists already, no need to create it again!") )
	} else { dir.create( dataFolder ) }
}

