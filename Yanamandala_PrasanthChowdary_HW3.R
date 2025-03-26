# Clear the Memory
rm(list = ls())

# Copy Paste This Command in your R-File. Make Sure You copy this statement as it is
dir1<-getwd()

# Copy Paste This Command in your R-File. Make Sure You copy this statement as it is
setwd(dir1)

# your info (NetID_LastName_FirstName);
name1 <- "PXY230011_Yanamandala_PrasanthChowdary" 
name1

# Create output file name using name1.
csvfile <- paste(name1,"_HW3.csv",sep="") 
csvfile

# Instruct to send all the output of your calculation  to a csv file 
sink(csvfile)

# Use cat function to write your name
cat("NAME",sep = ",","Prasanth Chowdary Yanamandala","\n")

# Use cat function to write your netid
cat("NETID",sep = ",","PXY230011","\n")

# Use this cat function to print the Problem 1 in your output
cat("Problem 1","\n")
library(readxl)
ttest <- read_excel("HW3-6359-F23.xlsx",sheet="t-test")
View(ttest)
n<-length(ttest$Weight)

#Calculate the Mean of single population
ans1<-mean(ttest$Weight)
cat("The Mean of the sample",sep= ",",ans1,"\n")

#Calculate the Std. Deviation
ans2<-sd(ttest$Weight)
cat("Standard Deviation is",sep = ",",ans2,"\n")

#Calculate the Sx-Bar
ans3<-ans2/sqrt(n)
cat("Sx-Bar is",sep = ",", ans3,"\n")

#Upper cut-off point
t<-qt(0.0125,n-1,lower.tail = FALSE)
ans4<-150+t*ans3
cat("Upper cut-off point is",sep = ",",ans4,"\n")

#Lower cut-off point
ans5<-150-t*ans3
cat("Lower cut-off point is",sep = ",",ans5,"\n")

#P-value
Tdata<-t.test(ttest$Weight,mu=150,alternative = "two.sided",conf.level = 0.975)
View(Tdata)
ans6<-Tdata$p.value
cat("P-Value is",sep = ",",ans6,"\n")

#Decision. Make Sure You Print ANY ONE of the the statement depending on your Result. Don't make Changes to the given statement.
cat("Decision",sep = ",","We Fail to reject Null Hypothesis","\n")

# Use this cat function to print the Problem 2 in your output
cat("Problem 2","\n")
Anvtest <- read_excel("HW3-6359-F23.xlsx",sheet="ANOVA")
View(Anvtest)
means<-aggregate(Anvtest$Stocks,list(Anvtest$City),FUN=mean)

#Calculate The Mean of Dallas city 
ans7<-means[2,2]
cat("The Mean of Dallas city",sep = ",",ans7,"\n")

#Calculate The Mean of Pittsburgh city 
ans8<-means[3,2]
cat("The Mean of Pittsburgh city",sep = ",",ans8,"\n")

#Calculate The Mean of Boston city 
ans9<-means[1,2]
cat("The Mean of Boston city",sep = ",",ans9,"\n")

#Calculate The Mean of Seattle city 
ans10<-means[4,2]
cat("The Mean of Seattle city",sep = ",",ans10,"\n")

#P-Value
A1<-aov(Anvtest$Stocks~Anvtest$City)
A2<-summary(A1)
cat("P-Value is",sep = ",","0.0244","\n")

#Decision. Make Sure You Print ANY ONE of the the statement depending on your Result. Don't make Changes to the given statement.
cat("Decision",sep = ",","We reject the Null Hypothesis","\n")

# Use this cat function to print the Problem 3 in your output
cat("Problem 3","\n")
Logtest <- read_excel("HW3-6359-F23.xlsx",sheet="Log")
library(moments)

#Calculate the Skewness Before Transformation
ans11<-skewness(Logtest$Radiation)
cat("The Skewness Before Log Transformation ",sep = ",",ans11,"\n")

# Calculate the Skewness After log Transformation with base e
Logtest$Logeradiation<-log(Logtest$Radiation)
ans12<-skewness(Logtest$Logeradiation)
cat("The Skewness After Log Transformation with base e",sep = ",",ans12,"\n")

# Calculate the Skewness After log Transformation with base 10
Logtest$Log10radiation<-log10(Logtest$Radiation)
ans13<-skewness(Logtest$Log10radiation)
cat("The Skewness After Log Transformation with base 10",sep = ",",ans13,"\n")

# Compare the skewness of both log Transformations is it the same? Answer 'Yes' or 'No'  Only.
cat("Similar:",sep = ",","Yes","\n")

# Stop writing to the CSV file.  
sink()

# Partition the graph area into 4 parts (2 rows and 2 columns). This will print 
#  all 4 graphs on one screen. 
par(mfrow=c(2,2))

# Plot the Histogram and QQplot Graph comparing Before and After log Transformation with base e. 
library(ggplot2)
library(car)
hist(Logtest$Radiation, main="Histogram Before Transformation", xlab="Radiation")
qqPlot(Logtest$Radiation, main="QQ Plot Before Transformation")

hist(Logtest$Logeradiation, main="Histogram After log Transformation with base e", xlab="Log_e_Radiation")
qqPlot(Logtest$Logeradiation, main="QQ Plot After log Transformation with base e")



