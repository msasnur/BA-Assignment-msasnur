## Business Analytics - Assignment 1 ##
## Email: msasnur@kent.edu ##
## Date: 10/16/2019 ##

## Part A ##
## Descriptive Statistics & Normal Distributions ##

#Question 1A#
#The probability of obtaining a score greater than 700 on a GMAT test is
pnorm(700,494,100)

#Question 1B#
#The probability of getting a score between 350 and 450 on the same GMAT exam is
pnorm(450,494,100)-pnorm(350,494,100)

#Question 2#
#The average per diem cost in Buenos Aires is
RI<-qnorm(0.8665)
449-(RI*36)

#Question 3#
#The correlation between the temperatures of the two cities is
Kent=c(59, 68, 78, 60) 
Los_Angeles=c(90, 82, 78, 75) 
M_Kent <- mean(Kent)
M_LA <- mean(Los_Angeles)
Correlation <- (sum((Kent - M_Kent) * (Los_Angeles - M_LA))) / (sqrt(sum((Kent - M_Kent)^2) * (sum((Los_Angeles - M_LA)^2)))) #Using Mathematical formula of correlation 
print(Correlation)
#Verifying using correlation function
cor(Kent, Los_Angeles)

## PART B ##
## DATA WRANGLING ##

#Question 4#
library(readr)
library(dplyr)
onret <- read_csv("Online_Retail(2).csv")
#View(onret)
trans <- prop.table(table(onret$Country)) * 100
trans[trans>1]

#Question 5#
onret$TransactionValue <- onret$Quantity * onret$UnitPrice 
View(onret)

#Question 6#
co <- tapply(onret$TransactionValue, onret$Country, sum)
co[co>130000]

#Question 7#
Temp=strptime(onret$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')

onret$New_Invoice_Date <- as.Date(Temp)

onret$New_Invoice_Date[20000]- onret$New_Invoice_Date[10]

onret$Invoice_Day_Week= weekdays(onret$New_Invoice_Date)

onret$New_Invoice_Hour = as.numeric(format(Temp, "%H"))

onret$New_Invoice_Month = as.numeric(format(Temp, "%m"))

#Part A
Week <- tapply(onret$TransactionValue,onret$Invoice_Day_Week, sum) / sum(onret$TransactionValue) * 100
Week

#Part B
Day <- tapply(onret$TransactionValue,onret$Invoice_Day_Week, NROW) / NROW(onret$TransactionValue) * 100
Day

#Part C
Month <- tapply(onret$TransactionValue,onret$New_Invoice_Month, length) / length(onret$TransactionValue) * 100
Month

#Part D
Aus <- max(onret$TransactionValue[onret$Country == "Australia"])
onret$New_Invoice_Date[Aus]

#Question 8#
his <- hist(onret$TransactionValue[onret$Country == "Germany"], 
          ylim = c(0,10000) , xlim = c(-600,1000), breaks = 20)
text(his$mids,his$counts, label = his$counts, adj = c(0.5, -0.5))

#Question 9#
cust <- tapply(onret$TransactionValue, onret$CustomerID, length)
cust[which.max(cust)]
cust_val <- tapply(onret$TransactionValue, onret$CustomerID, sum)
cust_val[which.max(cust_val)]

#Question 10#
sum(is.na(onret))
per <- colMeans(is.na(onret[c(0:8)])) * 100
per

#Question 11#
fun1<-function(x){
  k<-sum(is.na(x))
  return(k)}
tapply(onret$CustomerID,onret$Country,fun1)

#Question 13#
NROW(onret$Quantity[onret$Quantity < 0 & onret$Country == "France"]) / NROW(onret)*100

#Question 14#
z <- tapply(onret$TransactionValue,onret$StockCode,sum)
z[which.max(z)]

#Question 15#
length(unique(onret$CustomerID))