# Source of data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This R script does the following:

Init <- function(workDirStr = "C:/Users/hsofoian/Desktop/DataScience/Course 3 - Getting and Cleaning data/Course3_PeerAss_RunAnalysis"){
        setwd(workDirStr)      
}

Init()

##################################################################################################

# 1. Merges the training and the test sets to create one data set.

tmp1 <- read.table("train/X_train.txt")
tmp2 <- read.table("test/X_test.txt")
x <- rbind(tmp1, tmp2) ## 561 Columns, 10299 rows   ----> to be used with features.txt

tmp1 <- read.table("train/y_train.txt")
tmp2 <- read.table("test/y_test.txt")
y <- rbind(tmp1, tmp2) ### 1 column, 10299 rows (1s,2s ... 6s)  ----> to be used with activity_labels.txt

tmp1 <- read.table("train/subject_train.txt")
tmp2 <- read.table("test/subject_test.txt")
s <- rbind(tmp1, tmp2) ### 1 column, 10299 rows (incl 1s,2s ... 6s)


##################################################################################################

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## mean(): Mean value std(): Standard deviation

features <- read.table("features.txt")   ## 2 cols , 561 rows
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2]) 
# Returns indices of mean&SD from featurees[,V2]
# Example [1]   1   2   3   4   5   6  41  42  43  44 ...[66] 

x <- x[, indices_of_good_features] ## select from X(Trani/Test) using indices related to mean/SD
names(x) <- features[indices_of_good_features, 2]
names(x) <- gsub("\\(|\\)", "", names(x))   ## replace "()" by empty using gsub
names(x) <- tolower(names(x)) 

# Result of names(x): [1] "tbodyacc-mean-x" [2] "tbodyacc-mean-y" [3] "tbodyacc-mean-z"          
#                     [4] "tbodyacc-std-x" ..... [66] "fbodybodygyrojerkmag-std"

# Result of x[1:2,1:2]:
## tbodyacc-mean-x tbodyacc-mean-y
## 1       0.2885845     -0.02029417
## 2       0.2784188     -0.01641057

##################################################################################################

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
## activities
##   V1          V2
## 1  1            WALKING
## 2  2   WALKING_UPSTAIRS
## 3  3 WALKING_DOWNSTAIRS
## 4  4            SITTING
## 5  5           STANDING
## 6  6             LAYING


activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
## V1            V2
## 1  1           walking
## 2  2   walkingupstairs
## 3  3 walkingdownstairs
## 4  4           sitting
## 5  5          standing
## 6  6            laying

y[,1] = activities[y[,1], 2]
names(y) <- "activity"
## y[1:5,]
## [1] "standing" "standing" "standing"
## [4] "standing" "standing" .......

##################################################################################################

# 4. Appropriately labels the data set with descriptive activity names.

names(s) <- "subject"
cleaned <- cbind(s, y, x)
write.table(cleaned, "Tidy1_mergedcleandata.txt") ## ## 68 Columns, 10299 rows

## cleaned[1:3,1:5]
## subject activity       tbodyacc-mean-x   tbodyacc-mean-y      tbodyacc-mean-z
## 1       1 standing       0.2885845        -0.02029417            -0.1329051
## 2       1 standing       0.2784188        -0.01641057            -0.1235202
## 3       1 standing       0.2796531        -0.01946716            -0.1134617

##################################################################################################

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(s)[,1]                         ## [1]  1  3  5 ... [30] 24
numSubjects = length(unique(s)[,1])                    ## 30 
numActivities = length(activities[,1])                 ## 6 
numCols = dim(cleaned)[2]                              ## 68
result = cleaned[1:(numSubjects*numActivities), ]      ## 180 rows

row = 1
for (s in 1:numSubjects) {
        for (a in 1:numActivities) {
                result[row, 1] = uniqueSubjects[s]
                result[row, 2] = activities[a, 2]
                tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, "Tidy2_datasetwithaverages.txt")    # row 180, col 68

##################################################################################################