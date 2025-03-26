#Only use cat function to print the output.  You must label every result (in Column 1). Don’t Copy the Questions in your R file

#Once your program is running, run it many times and observe the graphs.  Submit 
#when your graphs resemble a normal distribution.  The QQ Plot will be  close
#to a straight line and the  skewness will be close to 0.  The median in the boxplot 
#will be in the center.  

#Install the Moments package on your system in R studio only once.   
#Do not install any package in any of your assignments.  Only load required libraries.
#install.packages("readxl")
#install.packages("moments")
#install.packages("dplyr")

# Do the initials steps we used to do in previous Homework
rm(list = ls())

# Get Current Working Directory and store it in dir1 variable 
dir1<-getwd()

# Set your working Directory by using the Following Command
setwd(dir1)

# your info (NetID_LastName_FirstName); write to Console
name1 <- "PXY230011_PrasanthChowdary_Yanamandala"
name1

# Load Moments library  (do not install any package in any assignments)
library(moments)

# Create output file name using name1.
csvfile <- paste(name1,"_HW2.csv",sep="")
csvfile

# Instruct to send all the output of your calculation  to a csv file 
sink(csvfile)

# Use cat function to write your name
cat("NAME",  sep = ","   ,  "Prasanth Chowdary Yanamandala", "\n")

# Use cat function to write your netid
cat("NETID" ,  sep = ","   , "PXY230011", "\n")

# Use this cat function to print the Part A in your output
cat("Part A","\n")

#PART A
#Binomial Distribution

# a) None of the LED bulbs are defective?
p<-0.10 #prob. of defective
n<-25 #sample size
x<-0 #defects
ans1<-dbinom(x,n,p)
# Use cat function to write your answer
cat("None of the LED bulbs are defective?",sep=",",ans1,"\n")

# b) Exactly two of the LED bulbs is defective?
p<-0.10
n<-25
x<-2
ans2<-dbinom(x,n,p)
# Use cat function to write your answer
cat("Exactly two of the LED bulbs is defective?",sep=",",ans2,"\n")

# c) Ten or fewer of the LED bulbs are defective?
p<-0.10
n<-25
x<-10
ans3<-pbinom(x,n,p,lower.tail=TRUE)
# Use cat function to write your answer
cat("Ten or fewer of the LED bulbs are defective?",sep=",",ans3,"\n")

# d) Three or more of the LED bulbs are defective
p<-0.10
n<-25
x<-2
ans4<-1-pbinom(x,n,p,lower.tail=TRUE)
cat("Three or more of the LED bulbs are defective",sep=",",ans4,"\n")

# Use this cat function to print the Part B in your output
cat("Part B","\n")

#PART B
#Poisson Distribution

# a) Probability that the agent sells some policies is
u<-4
x<-0
ans5<-1-dpois(x,u)
# Use cat function to write your answer
cat("Probability that the agent sells some policies is",sep=",",ans5,"\n")

# b) Agent sells 3 or more but less than 6 policies
u<-4
x<-3:5
ans6<-sum(dpois(x,u))
# Use cat function to write your answer
cat("Agent sells 3 or more but less than 6 policies",sep=",",ans6,"\n")

# Use this cat function to print the Part C in your output
cat("Part C","\n")

#PART C
# Normal Distribution 
# Calculate the probabilities and print the result and  proper label for each
# Assume  Mean = 100, Sigma = 17
Mean<-100
Sigma<-17
# Find P(X ≥ 92)
ans7<-pnorm(92,Mean,Sigma,lower.tail=FALSE)
cat("P(X >= 92)",sep=",",ans7,"\n")

# Find P(75 ≤ X ≤ 99)
ans8<-pnorm(99.5,Mean,Sigma,lower.tail=TRUE)-pnorm(74.5,Mean,Sigma,lower.tail=TRUE)
cat("P(75 <= X <= 99)",sep=",",ans8,"\n")

# Find the cut-off for top 10%.
ans9<-qnorm(0.90,Mean,Sigma)
cat("Find the cut-off for top 10%",sep=",",ans9,"\n")

# Find the cutoff for bottom 12%.
ans10<-qnorm(0.12,Mean,Sigma)
cat("Find the cutoff for bottom 12%",sep=",",ans10,"\n")

# t-distribution
# Assume  Mean = 80,  std dev = 13, sample size = 23
u<-80
s<-13
n<-23
df<-n-1

# Find the cut-off for top 10%.
ans11<-qt(0.90,df)*(s/sqrt(n))+u
cat("Find the cutoff for top 10%",sep=",",ans11,"\n")

# Find the cutoff for bottom 12%.
ans12<-qt(0.12,df)*(s/sqrt(n))+u
cat("Find the cutoff for bottom 12%",sep=",",ans12,"\n")

# Generate a normal distribution dataset with mean = 83, sigma = 27, n  = 200
# Assign it to a vector, say, nm1.  Assume nm1 is a population.
nm1<-rnorm(200,83,27)

# make all the numbers of vector nm1  integer
nm1<-as.integer(nm1)

# For nm1, find the following  and print the values and label.   
#Mean
ans13<-mean(nm1)
cat("Mean",sep=",",ans13,"\n")

#Population Std Dev (R gives sample std dev.  Convert it into population std dev)
n<-length(nm1)
ssd<-sd(nm1)
ans14<-ssd*sqrt((n-1)/n)
cat("Standard Deviation",sep=",",ans14,"\n")

#Skewness
library(moments)
ans15<-skewness(nm1)
cat("Skewness",sep=",",ans15,"\n")

# Numbers outside µ ± 2σ will be treated as outliers.  Determine the upper and lower
# cut-off numbers.  Also the number of outliers.  Label everything.
# What is the upper cut-off number?
ans16<-ans13+(2*ans14)
cat("Upper cut-off number",sep=",",ans16,"\n")

# What is the lower cutoff number? 
ans17<-ans13-(2*ans14)
cat("Lower cut-off number",sep=",",ans17,"\n")

# How many numbers are more than the upper cutoff number?  Use length function. 
ans18<-length(nm1[nm1>ans16])
cat("Numbers above upper cut-off",sep=",",ans18,"\n")

# How many numbers are below the lower cutoff number?  Use length function. 
ans19<-length(nm1[nm1<ans17])
cat("Numbers below lower cut-off",sep=",",ans19,"\n")

# create a divider line
cat("-------------------------------------------------------------","\n")

# Write summary statistics of nm1
ans20<-summary(nm1)
head<-c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
cat(head,"\n")
cat(ans20,"\n")

# create a divider line
cat("-------------------------------------------------------------","\n")

# write the vector nm1 in one column
cat(nm1, sep="\n")

# Stop writing to the CSV file.  
sink()

# Partition the graph area into 4 parts (2 rows and 2 columns). This will print 
#  all 4 graphs below on one screen. 
par(mfrow=c(2,2))

# density plot of nm1
d1<-density(nm1)  
plot(d1)

# boxplot of nm1
b1<-boxplot(nm1)

# Histogram of nm1
h1<-hist(nm1)

# qqplot of nm1
q1<-qqnorm(nm1)

