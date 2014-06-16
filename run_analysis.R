# Please ensure the following files are in your working directory:
# subject_train.txt, y_train.txt, x_Train.txt
# subject_test.txt, y_test.txt, y_test.txt
# features.txt

create_tidy_data <- function (filetype)
{
  # Which features are of interest to us?
  features <- read.table("features.txt", sep='')
  idx <- sapply(features[,2], FUN=function(x) grepl("max()", x) | grepl("std()", x))


  # read the file with activities
  if (filetype == 1) 
    filename<-"y_train.txt"
  else
    filename <- "y_test.txt"
  activities <- read.fwf(filename, sep='', 1) 
  names(activities) <- "Activities"

  # read the file with the subject id
  if (filetype == 1) 
    filename<-"subject_train.txt"
  else
    filename <- "subject_test.txt"
  
  subject<- read.fwf(filename, sep="", 1) 
  names(subject)<-"subject"

  # read the main data file
  if (filetype == 1) 
    filename<-"x_train.txt"
  else
    filename <- "x_test.txt"
  
  df <- read.table (filename, sep="")
  names(df) <- features[,2]

  # From the main data file extract the features we are interested in
  featuresidx<-which(idx)
  df2 <- subset(df, select = featuresidx)

  # Add the subjects
  df2 <- cbind(df2, subject)

  # Add the activities
  df2 <- cbind(df2, activities=factor(activities[,1], 
                         levels=c(1:6), 
                         labels = c("WALKING", "WALKING UPSTAIRS", 
                                    "WALKING DOWNSTAIRS", "SITTING", "STANDING", "LAYING")))

  # return data frame with tidy data
  df2
}


###################################
## Create the first tidy data file.
#####################################
# create tidy data for training files
dftidy <- create_tidy_data(1)
# create tidy data for test files
dftidy2 <- create_tidy_data(0)

dftidy3 <-rbind(dftidy,dftidy2)

# Write output tidy data file
write.table(dftidy3, file="tidydata.txt",sep=",",col.names=TRUE)


######################################################################
# Create the second tidy data file of averages by activity and subject
#######################################################################

# First let's do subject
nfeatures <- ncol(dftidy3) - 2
nsubjects <- unique(dftidy3$subject)
dftidy4 <- matrix(nrow=length(nsubjects),ncol=0)
for (i in seq(nfeatures))
{
  dftidy4<- cbind(dftidy4,tapply(dftidy3[,i],dftidy3$subject,mean))
}
rownames(dftidy4) <- paste("Averages for subject id ", nsubjects)

# now let's do activity
nactivity <- levels(dftidy3$activities)
dftidy5 <- matrix(nrow=length(nactivity),ncol=0)
for (i in seq(nfeatures))
{
  dftidy5<- cbind(dftidy5,tapply(dftidy3[,i],dftidy3$activities,mean))
}
rownames(dftidy5) <- paste("Averages for Activity ", nactivity)

dftidy5 <- rbind(dftidy5,dftidy4)

colnames(dftidy5)<-colnames(dftidy3)[1:nfeatures]

# Write output tidy data file
write.table(dftidy5, file="tidydata2.txt",sep=",",col.names=TRUE)
