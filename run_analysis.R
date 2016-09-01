# clean up memory
rm(list = ls())

# Setting the root folder
# Root folder path depends on where UCI HAR Dataset was stored
rootFolder <- "/Users/EduardoAndrade/Google Drive/Estudos/Data Science/Coursera/GettingAndCleaningData/data/Week4/UCI HAR Dataset"
setwd(rootFolder)

################# read features #################
activitiesLabelsFile <- "activity_labels.txt"
measuresTitlesFile <- "features.txt"
# activitiesLabels has the correlation between activities IDs and activities Labels
activitiesLabels <- read.table(activitiesLabelsFile)
#View(activitiesLabels)
# measuresTitles contains the descriptive name of the measures taken on the experiment
measuresTitles <- read.table(measuresTitlesFile)
vecMeasuresTitles <- measuresTitles[,2]
# vecMeasuresTitles

################# Trainning data reading ##############
trainFolder <- "./train"
trainFile <- "X_train.txt"
trainActivitiesFile <- "y_train.txt"
trainSubjectsFile <- "subject_train.txt"

### Reads trainning data
activitiesData <- read.table(
  file = paste(trainFolder, trainFile, sep = "/"))
#View(activitiesData)
#dim(activitiesData)
#object.size(activitiesData)

### Reads trainning data activities codes
activitiesCodes <- read.table(
  file = paste(trainFolder, trainActivitiesFile, sep = "/")
)
#View(activitiesCodes)
#dim(activitiesCodes)

### Reads Training Subjects

activitiesSubjects <- read.table(
  file = paste(trainFolder, trainSubjectsFile, sep = "/")
)
#View(activitiesSubjects)
#dim(activitiesSubjects)

################# read test data ################
testFolder <- "./test"
testFile <- "X_test.txt"
testActivitiesFile <- "y_test.txt"
testSubjetcsFile <- "subject_test.txt"

### Reads test data and merges it to the tail of activitiesData,
### which already have training data
### Requirement 1 will be attended whith this.
activitiesData <- rbind(
  activitiesData,
  read.table(
    paste(testFolder, testFile, sep = "/")
    )
  )
dim(activitiesData)

### Reads test activities code and merges it to the tail
### of activitiesCode, which already have training
### activities codes.
activitiesCodes <- rbind(
  activitiesCodes,
  read.table(
    paste(testFolder, testActivitiesFile, sep = "/")
  )
)
dim(activitiesCodes)

### Reads test subjects and merges it to the tail
### of activitiesSubjects, which already contains
### training subjects.
activitiesSubjects <- rbind(
  activitiesSubjects,
  read.table(
    paste(testFolder, testSubjetcsFile, sep = "/")
  )
)
colnames(activitiesSubjects) <- c("subjectID")
dim(activitiesSubjects)
#View(activitiesSubjects)

# Set the name of columns to the titles of the measures.
# With this, variable names of the data set will be
# appropriately set. Down the script variable names will be
# set even clearer for the final dataframe and compliance
# with requirement 4.
colnames(activitiesData) <- measuresTitles[,2]


### Select only the target measures to be compliant with requirement 2, thus
### Extracting the measurements on the mean and standard deviation for each measurement

### Select the indices of the mean and standard deviation of the measures
### in order to be compliant with requirement 2 of the project.
### This indices will select only the measures described on
### requirement 2 of the project, which are the mean and standard deviation of each measure
meanIndices <- grep("mean", measuresTitles[,2])
stdIndices <- grep("std", measuresTitles[,2])
targetMeasuresIndices <- c(meanIndices, stdIndices)

targetActivitiesData <- activitiesData[,targetMeasuresIndices]
View(targetActivitiesData)
dim(targetActivitiesData)
object.size(targetActivitiesData)

# dispose activitiesData in order to free memory up
rm(activitiesData)

### Create a vector with activities labels, according
### to the activities codes of measures
# Transform activities labels into a vector
activitiesLabels <- activitiesLabels[,2]
# Transform activities codes into a Vector
activitiesCodes <- activitiesCodes[,1]
activityLabel <- activitiesLabels[activitiesCodes]

### Adding a column to the targetActivitiesData to insert
### the descriptive activity name, thus being compliant
### with requirement 3
targetActivitiesData <-cbind(activityLabel, targetActivitiesData)

#targetActivitiesData$activityName <- activitiesLabels[activitiesCodes]

View(targetActivitiesData)
dim(targetActivitiesData)

### Renaming variables in order to make them clearer,
### thus being compliant with with requirement 4
varNames <- names(targetActivitiesData)
varNames <- gsub("-", "", varNames)
varNames <- gsub("\\(\\)", "", varNames)
varNames <- gsub("mean", "Mean", varNames)
varNames <- gsub("std", "Std", varNames)
names(targetActivitiesData) <- varNames

View(targetActivitiesData)

### Using dplyr package to agregate data
library(dplyr)
dim(targetActivitiesData)
dim(activitiesSubjects)
targetActivitiesData <- cbind(activitiesSubjects, targetActivitiesData)
tblExperiment
View(tblExperiment)
dim(tblExperiment)


### Grouping the experiment by subject and by activity
bySubjectActivity <- group_by(targetActivitiesData, subjectID, activityLabel)

### Calculating the mean of the 79 variables that represent
### the measurements on the mean and standard deviation for each measurement,
### thus generating the data set specified on requirement 5
meanSubjectActivity <- summarise_each(bySubjectActivity, funs(mean))

### Updating variable names to represent what they really are, thus
### making the final data fram compliant with requirement 4
varNames <- names(meanSubjectActivity)[3:length(names(meanSubjectActivity))]
names(meanSubjectActivity)[3:length(names(meanSubjectActivity))] <-
  paste("meanOf", varNames, sep = "_")

# View(meanSubjectActivity)



