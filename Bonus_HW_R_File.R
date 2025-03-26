
#Clear the environment
rm(list = ls())

# your info (NetID_LastName_FirstName); write to Console
name1 <- "PXY230011_Yanamandala_PrasanthChowdary"; name1

#  Assign dir1 to the folder on your computer where this excel file (HW) is. 
dir1 <- getwd()

#  set the working directory to dir1
setwd(dir1)

# load readxl library
install.packages("readxl")
library(readxl)

# read this excel file  (sheet = "Data"). Do not include the file path here, only the file name, 
# i.e. ""Bonus_HW.xlsx"" in this case.  Do not change the file name.  
var1 <- read_excel("Bonus_HW.xlsx",sheet="Data")
View(var1)

# Get all the column names
colnames(var1)

# Get the top 5 rows in the data
var1[1:5,]

# Get the bottom 10 rows in the data
tail(var1,10)

# Get the length of the column A2, store it in a variable len1
len1 <- length(var1$A2)
len1

# Get the average of the column A1, store in avg1
avg1 <- mean(var1$A1)
avg1

# Make the current dataframe as the default dataframe (Use attach command) and store the average of the column A1 in variable avg2
attach(var1)
avg2 <- mean(A1)
avg2

# Create a new third column A3 which is the  the sum of  columns 1 and 2 and add the new column to the existing data frame
A3 <- c(A1+A2)
var1 <- data.frame(var1,A3)
View(var1)

# Rename the column names A1 and A2 to "Test_Scores_1" and "Test_Scores_2"
names(var1)[1] <- "Test_Scores_1"
names(var1)[2] <- "Test_Scores_2"
View(var1)

# detach the file
detach(var1)

# Put a number between(112-999) in seed(). This will lock your random numbers.
set.seed(469)

# Create a new vector New_Scores of uniformly distributed random numbers between 77 to 100 of size len1
New_Scores <- runif(len1,77,100)
New_Scores

# Round New_Scores to 0 decimal places
New_Scores <- round(New_Scores,0)
New_Scores

# Create a table t2 which has var1 columns and also New_Scores.  Table t2 will have 4 columns.  
t2 <- data.frame(var1, New_Scores)
t2
View(t2)

# Delete  column A3
t2 <- subset(t2, select = -A3)
View(t2)

# Display Test_Scores_1.  This will give you an error message because the program doesn't recognize it.
Test_Scores_1

# Command t2$Test_Scores_1 will work because the variable is a part of t2.  Always use t2$ as a prefix for t2 columns (if you do not use attach()
t2$Test_Scores_1

# Create a new table t3 from t2 which includes every t2 row whose sum is greater than or equal to 255.  This will be a subset of t2
t3 <- subset(t2, c(t2$Test_Scores_1+t2$Test_Scores_2+t2$New_Scores)>=255)
t3
View(t3)

# Create a vector "Total" which represents the sum of each row of table t3.
Total <- apply(t3,1,sum)
Total

# Add this "Total" vector to table t3.  Table t3 now has 4 columns. 
t3 <- data.frame(t3, Total)
t3
View(t3)

# Create your output file using name1.  Variable name1 was created earlier.
csvfile <- paste(name1,"_Bonus_HW.csv",sep="");csvfile

# send the output to the csv file you just created
sink(csvfile)

# Use cat function to write your name (First  Last).  This will be Row 1 of csv file.  
cat("NAME", sep=",", "Prasanth Chowdary Yanamandala", "\n")

# Use cat function to write your netid.  This will be Row 2 of csv file.
cat("NETID", sep=",","PXY230011", "\n")

# create a divider line. This must be on Row 3 of csv file. Copy and Paste this command
cat("--------------------------------", "\n")

# write  table t3 with fields seperated by a comma
write.table(t3, sep= "," , row.names=FALSE)

# Stop writing to the CSV file.  
sink()

