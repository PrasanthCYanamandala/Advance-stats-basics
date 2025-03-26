#  Clear the environment
rm(list = ls())

# your info (NetID_LastName_FirstName); write to Console
name1 <- "PXY230011_Yanamandala_PrasanthChowdary"
name1

#  Assign dir1 to the folder on your computer where this excel file (HW1) is. 
dir1 <- getwd();

#  set the working directory to dir1
setwd(dir1)

# Copy and paste this command into your R file. Keep it as a comment.  ------->
# setwd("C:/Users/kusum/OneDrive/Documents/TA - 6359")

#  load readxl library, if needs to be installed, once the package is installed, make it a comment by adding #
#  install.packages("readxl")
library(readxl)

# read this excel file  (sheet = Pioneer). Do not include the file path here, only the file name
table<-read_excel("HW1-6359-F23.xlsx", sheet="Pioneer")
View(table)

# rename the Units column to Quantity
names(table)[7]<-"Quantity"
View(table)

# Create a new vector Sales which is Quantity x Price 
Sales <- c(table$Quantity*table$Price)

# Create a new vector Commission which is 15% of the total sales
Commission <- c(0.15*Sales)

# add the new vectors to the excel file.  This will create two new columns.
table <- data.frame(table, Sales, Commission)
View(table)

# Create output file name using name1.  All your output will go to this file.  
csvfile <- paste(name1,"_HW1.csv",sep="")
csvfile

# send the output to the csv file you just created
sink(csvfile)

# Use the cat function to write your name (First  Last).  This must be Row 1 of your CSV file.  
cat("NAME",  sep = ","   ,  "PrasanthChowdary Yanamandala", "\n")

# Use cat function to write your netid.  This must be Row 2 of csv file.
cat("NETID" ,  sep = ","   , "PXY230011", "\n")

# write the length of the 1st column.  This must be Row 3 of csv file.
cat("--------------------------------", "\n")
len1 <- length(table$Date.Sold)
cat("LENGTH" , sep = "," , len1, "\n")

# Like above, calculate and print the following values along with the labels (as 
#  shown above for length) to the cvs file  (in the order given below)

# Average Sales
avg_sales <- mean(table$Sales)
cat("AVERAGE SALES" , sep = "," ,avg_sales, "\n")

# Median Sales
med_sales <- median(table$Sales)
cat("MEDIAN SALES" , sep = "," ,med_sales, "\n")

#Total quantity
tot_quantity <- sum(table$Quantity)
cat("TOTAL QUANTITY" , sep = "," ,tot_quantity, "\n")

#Total commission
tot_commission <- sum(table$Commission)
cat("TOTAL COMMISSION" , sep = "," ,tot_commission, "\n")

#Average commission
avg_commission <- mean(table$Commission)
cat("AVERAGE COMMISSION" , sep = "," ,avg_commission, "\n")

# How many invoices (records) have 30 or more units?
num_invoices <- length(table$Invoice[table$Quantity >= 30])
cat("INVOICES WITH 30 OR MORE UNITS" , sep = "," ,num_invoices, "\n")

# Total sales value of Skirts sold
tot_sales_skirts <- sum(table$Sales[table$Product == "Skirt"])
cat("SALES VALUE OF SKIRTS SOLD" , sep = "," ,tot_sales_skirts, "\n")

# Total quantity of T-shirts sold in Dallas
tot_quantity_tshirt <- sum(table$Quantity[table$Product == "T-Shirt" & table$City == "Dallas"])
cat("QUANTITY OF TSHIRTS SOLD IN DALLAS" , sep = "," ,tot_quantity_tshirt, "\n")

# create a divider line.  This must be Row 12 of csv file. Copy and Paste this Command 
cat("--------------------------------", "\n")

# For the Sales  data  (assume the data to be a sample)
Sales_sam <- sample(table$Sales)
# Find the following and write using cat function (as done above; see also the output)
# Mean
mean_sales <- mean(Sales_sam)
cat("SALES MEAN" , sep = "," ,mean_sales, "\n")

# Median 
median_sales <- median(Sales_sam)
cat("SALES MEDIAN" , sep = "," ,median_sales, "\n")

# Standard deviation
sd_sales <- sd(Sales_sam)
cat("SALES STANDARD DEVIATION" , sep = "," ,sd_sales, "\n")

# Skewness   (Package moments should be installed)
#install.packages("moments")
library(moments)
Skewness_Sales <- skewness(Sales_sam)
cat("SKEWNESS" , sep = "," ,Skewness_Sales, "\n")

# Lower cut-off using 2-sigma approach 
Low_Coff_Sales <- mean_sales - (2*sd_sales)
cat("LOWER CUT OFF" , sep = "," ,Low_Coff_Sales, "\n")

# Upper cut-off using 2-sigma approach
Up_Coff_Sales <- mean_sales + (2*sd_sales)
cat("UPPER CUT OFF" , sep = "," ,Up_Coff_Sales, "\n")

# Plot a Histogram of the price
hist(table$Price, xlab = "price", col = "yellow", border = "blue")

# Plot a BoxPlot of the Sales
boxplot(table$Sales, ylab = "Sales", col = "yellow", border = "blue")

# Stop writing to the CSV file.  
sink()
