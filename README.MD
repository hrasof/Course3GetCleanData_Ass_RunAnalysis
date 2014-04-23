Init <- function(workDirStr = "C:/Users/hsofoian/Desktop/DataScience/Course 3 - Getting and Cleaning data/Course3_PeerAss_RunAnalysis"){
        setwd(workDirStr)      
}

Init()

tmp1 <- read.table("train/X_train.txt")
tmp2 <- read.table("test/X_test.txt")
x <- rbind(tmp1, tmp2) 

tmp1 <- read.table("train/y_train.txt")
tmp2 <- read.table("test/y_test.txt")
y <- rbind(tmp1, tmp2) 

tmp1 <- read.table("train/subject_train.txt")
tmp2 <- read.table("test/subject_test.txt")
s <- rbind(tmp1, tmp2) 


features <- read.table("features.txt")   
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2]) 

x <- x[, indices_of_good_features] 
names(x) <- features[indices_of_good_features, 2]
names(x) <- gsub("\\(|\\)", "", names(x))   
names(x) <- tolower(names(x)) 

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))

y[,1] = activities[y[,1], 2]
names(y) <- "activity"

names(s) <- "subject"
cleaned <- cbind(s, y, x)
write.table(cleaned, "Tidy1_mergedcleandata.txt") 

uniqueSubjects = unique(s)[,1]                        
numSubjects = length(unique(s)[,1])                    
numActivities = length(activities[,1])                 
numCols = dim(cleaned)[2]                              
result = cleaned[1:(numSubjects*numActivities), ]      

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
write.table(result, "Tidy2_datasetwithaverages.txt")    