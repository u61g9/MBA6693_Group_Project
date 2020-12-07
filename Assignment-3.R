# Report prepared for the Default of loan with below parameters
#1. Age
#2. Outstanding balance
#3. presence of open home loan
#4. presence of open auto loan
#5. Dependent
#6. Correlation between DPD for 6months, 12 Months

library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)
library(gapminder)
library(vtreat)
library(leaps)
library(MASS)
library(lattice)
library(caret)
library(boot)
library(pROC)
library(tidyverse)

demo <- read.csv("demogs.csv")
credit <- read.csv("Credit_Bureau.csv")

# Summary
summary(demo)
summary(credit)

str(demo)
str(credit)

# No of Rows
nrow(demo)
# 71295
nrow(credit)
# 71295
#===================#
#   Color palette   #
#===================#

cp_2 <- c("#FEA47F", "#F97F51")
cp_3 <- c("#2A363B", "#E84A5F", "#FF847C")
cp_5 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898")
cp_8 <- c("#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")


#==========================#
#Exploratory Data analysis #
#==========================#

# Checking for Duplicate
#dem data
length(unique(demo$Application.ID))
#71292
#credit data
length(unique(credit$Application.ID))
#71292
# Here we can say 71295 are the actual and 71292 are unique so 3 data entries are Duplicate

demo %>%
  group_by (Application.ID) %>%
  filter(n()>1)

  credit %>%
  group_by (Application.ID) %>%
  filter(n()>1)
# we can see that few Application ID duplicating for different person 
# so lets remove it

demo <- demo %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

credit <- credit %>%
  group_by(Application.ID) %>%
  filter(n() == 1)
# Total 71289

# Merging these two data set
merged_data <- merge(demo,credit,by=c("Application.ID","Performance.Tag"))

#missing data
merged_data$Performance.Tag %>% is.na() %>% sum()
#1425

# search missing
merged_data <- merged_data %>% filter(!is.na(Performance.Tag))

# Plot for Performance Tag
merged_data %>%
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=as.factor(Performance.Tag), y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar() +
  scale_fill_manual(values = cp_2) +
  labs(x="Performance Tag", y="Frequency in 1000s", fill="Performance Tag", title="Frequency of Performance Tag") +
  theme_minimal()


#==========================#
#========Default Rate======#
#==========================#

temp <- table(merged_data$Performance.Tag)

temp[1]
# 66917 non defaulters out of total 69864 of Merged data

default_count <- temp[2]
nondefault_count <- temp[1]
#2947 are defaulters out of total 69864 of Merged data
default_percent <- default_count/(default_count+nondefault_count)
default_percent
# which is 4.2182% (default rate)
nondefault_percent <- nondefault_count / (default_count + nondefault_count)
nondefault_percent
# which is 95.7818%


#==========================#
#============Age===========#
#==========================#
merged_data$Age %>% is.na()%>% sum()
# 0
# Checking for Outliers
merged_data$Age %>% quantile(seq(0,1,0.01))
merged_data$Age %>% boxplot()

# filter
merged_data[(which(merged_data$Age<18)),]$Age <- 18

#Creating bins on Age
# 0 - 20,21-25, ...61 - 65

age_bin <- function(age){
  
  if(age > 0 && age < 21)
    
    return ("18-20")
  
  else if(age > 20 && age < 26)
    
    return ("21-25")
  
  else if(age > 25 && age < 31)
    
    return ("26-30")
  
  else if(age > 30 && age < 36)
    
    return ("31-35")
  
  else if(age > 35 && age < 41)
    
    return ("36-40")
  
  else if(age > 40 && age < 46)
    
    return ("41-45")
  
  else if(age > 45 && age < 51)
    
    return ("46-50")
  
  else if(age > 50 && age < 56)
    
    return ("51-55")
  
  else if(age > 55 && age < 61)
    
    return ("56-60")
  
  else if(age > 60 && age < 66)
    
    return ("60-65")
}

merged_data$age_bin <- merged_data$Age %>%
  sapply(age_bin)%>% as.factor()
summary(merged_data$age_bin)

merged_data$age_bin
attributes(merged_data$age_bin)

library(ggplot2)
ggplot(merged_data,aes(x = age_bin, y= ..count.., fill = age_bin)) + geom_bar()
ggplot(merged_data,aes(x = age_bin, y= ..count../1000, fill = age_bin)) + geom_bar()


# We can conclude that loan holder are dense from 36 to 55 Age
# Age Bucket wise Performance Tag Frequency
merged_data %>% 
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=age_bin, y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2)
# From the above plot we can conclude that default is high between 36 to 55 Age compare to Other groups
#=============#
#   Income    #
#=============#

# checking for NA values
merged_data$Income %>%
  is.na() %>%
  sum()
# 0

# Checking for outliers
merged_data$Income %>%
  quantile(seq(0,1,0.01), na.rm = T)

merged_data$Income %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data$Income %>%
  as.factor() %>%
  levels()

# Converting Income less than 1 to 1.0
merged_data[(which(merged_data$Income < 1)), ] $Income <- 1.0

# Creating Income Bracket
# Income Bracket Function

income_bin <- function(income = 1){
  if(income >= 1 && income <=10)
    return ("1-10")
  else if(income >= 11 && income <=20)
    return ("11-20")
  else if(income >= 21 && income <=30)
    return ("21-30")
  else if(income >= 31 && income <=40)
    return ("31-40")
  else if(income >= 41 && income <=50)
    return ("41-50")
  else
    return ("51-60")
}

merged_data$Income_Bin <-  merged_data$Income %>%
  sapply(income_bin) %>%
  as.factor()



#=========================#
#   Outstanding Balance   #
#=========================#

# Checking for NA values

merged_data$Outstanding.Balance %>%
  is.na() %>%
  sum()
# 272

merged_data$Outstanding.Balance %>%
  summary()
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
#   0  208400  774242   1253410 2926250 5218801     272 

merged_data$Outstanding.Balance[which(is.na(merged_data$Outstanding.Balance))] <- 774985


# Checking for outliers
merged_data$Outstanding.Balance %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Outstanding.Balance %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")
# From the above graph we can conclude that maximum observations are skewed to the 
# below average outstanding amount which we can see Median<Mean

#=================================#
#   Presence of open home loan    #
#=================================#

# Checking for NA values
merged_data$Presence.of.open.home.loan %>%
  is.na() %>%
  sum()
# 272

merged_data$Presence.of.open.home.loan %>%
  as.factor() %>%
  summary()
# 0     1    NA's 
#51521 18071   272 

merged_data$Presence.of.open.home.loan[which(is.na(merged_data$Presence.of.open.home.loan))] <- 0

# Converting to factor type
merged_data$Presence.of.open.home.loan <- merged_data$Presence.of.open.home.loan %>%
  as.factor()

# Plot for  Presence of open home loan
ggplot(merged_data, aes(x=Presence.of.open.home.loan, y=..count../1000, fill=Presence.of.open.home.loan)) +
  geom_bar() +
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of open home loan ", y="Frequency in 1000s",fill="Presence of open home loan ", title="Frequency of Presence of open home loan ") +
  theme_minimal()

# Open Home Loan wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=Presence.of.open.home.loan, y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of Open Home Loan", y="Frequency in 1000s", fill="Performance Tag", title="Open Home Loan wise Performance Tag Frequency")

# From the above graph we can conclude that there is no vast difference between the frequency and default for Presence of open home loan or not 
# No significant Effects on the open home loan to default rate

#=================================#
#   Presence of open auto loan    #
#=================================#

# Checking for NA values
merged_data$Presence.of.open.auto.loan %>%
  is.na() %>%
  sum()

# 0

merged_data$Presence.of.open.auto.loan %>%
  as.factor() %>%
  summary()
# 0     1 
#63935  5929 

# Converting to factor type
merged_data$Presence.of.open.auto.loan <- merged_data$Presence.of.open.auto.loan %>%
  as.factor()

# Plot for  Presence of open auto loan
ggplot(merged_data, aes(x=Presence.of.open.auto.loan, y=..count../1000, fill=Presence.of.open.auto.loan)) +
  geom_bar() +
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of open auto loan ", y="Frequency in 1000s",fill="Presence of open auto loan ", title="Frequency of Presence of open auto loan ") +
  theme_minimal()

# Open Auto Loan wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=Presence.of.open.auto.loan, y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of Open Auto Loan", y="Frequency in 1000s", fill="Performance Tag", title="Open Auto Loan wise Performance Tag Frequency")

# From the above graph we can conclude that there significant difference between the frequency and default for Presence of open auto loan.
# Default rate is higher for the people who has open auto loans

#=====================#
#   No of Dependents  #
#=====================#

# Checking for NA values
merged_data$No.of.dependents %>%
  is.na() %>%
  sum()

# 3 NA's

merged_data$No.of.dependents[which(is.na(merged_data$No.of.dependents))] <- 3

merged_data$No.of.dependents %>%
  as.factor() %>%
  summary()
# 1     2     3     4     5 
# 15218 15127 15647 11997 11875 

# Checking for outliers
merged_data$No.of.dependents %>%
  quantile(seq(0,1,0.01), na.rm = T)

merged_data$No.of.dependents %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

#Converting the variable into factor type
merged_data$No.of.dependents <- merged_data$No.of.dependents %>% as.factor()

# Plot for No of Dependents Frequency
ggplot(merged_data, aes(x=as.factor(No.of.dependents), y=..count../1000, fill=as.factor(No.of.dependents))) +
  geom_bar() +
  scale_fill_manual(values=cp_5)+
  labs(x="No of Dependents", y="Frequency in 1000s", fill="No of Dependents", title="Frequency of No of Dependents") +
  theme_minimal()

# No of Dependents wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=No.of.dependents, y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="No of Dependents", y="Frequency in 1000s", fill="Performance Tag", title="No of Dependents wise Performance Tag Frequency")
# Conclusion : For Dependent 4 and 5 default rate is seen with less frequency of 12000 people.

#===================================================#
#   No of times 90 DPD or worse in last 6 months    #
#===================================================#

# Checking for NA values
merged_data$No.of.times.90.DPD.or.worse.in.last.6.months %>%
  is.na() %>%
  sum()

# 0

merged_data$No.of.times.90.DPD.or.worse.in.last.6.months %>%
  as.factor() %>%
  summary()

# 0     1     2     3 
#54662 13218  1776   208 



#===================================================#
#   No of times 60 DPD or worse in last 6 months    #
#===================================================#

# Checking for NA values
merged_data$No.of.times.60.DPD.or.worse.in.last.6.months %>%
  is.na() %>%
  sum()

# 0

merged_data$No.of.times.60.DPD.or.worse.in.last.6.months %>%
  as.factor() %>%
  summary()
#  0     1     2     3     4     5 
# 51868 11130  4916  1469   411    70 


#===================================================#
#   No of times 30 DPD or worse in last 6 months    #
#===================================================#

# Checking for NA values
merged_data$No.of.times.30.DPD.or.worse.in.last.6.months %>%
  is.na() %>%
  sum()

# 0

merged_data$No.of.times.30.DPD.or.worse.in.last.6.months %>%
  as.factor() %>%
  summary()

#   0     1     2     3     4     5     6     7 
#50096  9500  5897  2829  1045   386    96    15 


#===================================================#
#   No of times 90 DPD or worse in last 12 months   #
#===================================================#

# Checking for NA values
merged_data$No.of.times.90.DPD.or.worse.in.last.12.months %>%
  is.na() %>%
  sum()

# 0

merged_data$No.of.times.90.DPD.or.worse.in.last.12.months %>%
  as.factor() %>%
  summary()
#   0     1     2     3     4     5 
#50490 11663  6159  1244   272    36 


#===================================================#
#   No of times 60 DPD or worse in last 12 months   #
#===================================================#

# Checking for NA values
merged_data$No.of.times.60.DPD.or.worse.in.last.12.months %>%
  is.na() %>%
  sum()

# 0

merged_data$No.of.times.60.DPD.or.worse.in.last.12.months %>%
  as.factor() %>%
  summary()



#===================================================#
#   No of times 30 DPD or worse in last 12 months   #
#===================================================#

# Checking for NA values
merged_data$No.of.times.30.DPD.or.worse.in.last.12.months %>%
  is.na() %>%
  sum()
# 0

merged_data$No.of.times.30.DPD.or.worse.in.last.12.months %>%
  as.factor() %>%
  summary()

#   0     1     2     3     4     5     6     7     8     9 
#44855 11474  6116  4135  1924   853   376   107    23     1 

#===================================#
#   Correlation of DPD Variables    #
#===================================#

DPD_data_6 <- merged_data[, c(13:15)]
DPD_data_12 <- merged_data[, c(16:18)]

cor_DPD_6 <- round(cor(DPD_data_6), 2)
cor_DPD_6
melted_cor_DPD_6 <- melt(cor_DPD_6)

cor_DPD_12 <- round(cor(DPD_data_12), 2)
melted_cor_DPD_12 <- melt(cor_DPD_12)

# DPD Correlation heat map for 6 months
ggplot(melted_cor_DPD_6, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(x="", y="", title="DPD 6 months Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))

# From the above heat Map we can conclude that light blue = 1 is the relationship of variable with itself.
# Dark blue color=0.84 shows the negative relationship between the variables
# For 6 Months 30DPD and 60DPD average relationship.
# For 6 Months 30DPD and 60DPD has negative relationship and more towards 0.80 value Dark Blue



#=======================================================#
#   Average Credit Card utilisation in last 12 months   #
#=======================================================#

# Checking for NA values
merged_data$Avgas.CC.Utilization.in.last.12.months %>%
  is.na() %>%
  sum()
# 1058

merged_data$Avgas.CC.Utilization.in.last.12.months %>%
  summary()

# Replacing the NA value with the median
merged_data$Avgas.CC.Utilization.in.last.12.months[which(is.na(merged_data$Avgas.CC.Utilization.in.last.12.months))] <- 15


# Checking for outliers
merged_data$Avgas.CC.Utilization.in.last.12.months %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Avgas.CC.Utilization.in.last.12.months %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")


#==========================================#
#   No of trades opened in last 6 months   #
#==========================================#

# Checking for NA values
merged_data$No.of.trades.opened.in.last.6.months %>%
  is.na() %>%
  sum()

# 1

merged_data$No.of.trades.opened.in.last.6.months %>%
  summary()

# Replacing the NA value with the median
merged_data$No.of.trades.opened.in.last.6.months[which(is.na(merged_data$No.of.trades.opened.in.last.6.months))] <- 2

# Checking for outliers
merged_data$No.of.trades.opened.in.last.6.months %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$No.of.trades.opened.in.last.6.months %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$No.of.trades.opened.in.last.6.months > 6)),] $No.of.trades.opened.in.last.6.months <- 6



#===========================================#
#   No of trades opened in last 12 months   #
#===========================================#

# Checking for NA values
merged_data$No.of.trades.opened.in.last.12.months %>%
  is.na() %>%
  sum()

# 0

# Checking for outliers
merged_data$No.of.trades.opened.in.last.12.months %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$No.of.trades.opened.in.last.12.months %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$No.of.trades.opened.in.last.12.months > 19)),] $No.of.trades.opened.in.last.12.months <- 19

#===================================#
#   Correlation of trades opened    #
#===================================#

trades_opened <- merged_data[, c(20, 21)]

cor_trades_opened <- round(cor(trades_opened), 2)
melted_cor_trades_opened <- melt(cor_trades_opened)

#==============================================#
#   No of PL trades opened in last 6 months    #
#==============================================#

# Checking for NA values
merged_data$No.of.PL.trades.opened.in.last.6.months  %>%
  is.na() %>%
  sum()

# 0

# Checking for outliers
merged_data$No.of.PL.trades.opened.in.last.6.months   %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$No.of.PL.trades.opened.in.last.6.months   %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$No.of.PL.trades.opened.in.last.6.months   > 5)),] $No.of.PL.trades.opened.in.last.6.months   <- 5


#===============================================#
#   No of PL trades opened in last 12 months    #
#===============================================#

# Checking for NA values
merged_data$No.of.PL.trades.opened.in.last.12.months %>%
  is.na() %>%
  sum()

# 0

# Checking for outliers
merged_data$No.of.PL.trades.opened.in.last.12.months  %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$No.of.PL.trades.opened.in.last.12.months  %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$No.of.PL.trades.opened.in.last.12.months  > 10)),] $No.of.PL.trades.opened.in.last.12.months  <- 10

#===================================#
#   Correlation of PL trades opened    #
#===================================#

pl_trades_opened <- merged_data[, c(22, 23)]

cor_pl_trades_opened <- round(cor(pl_trades_opened), 2)
melted_cor_pl_trades_opened <- melt(cor_pl_trades_opened)


#===============================================================#
#   No if inquiries in last 6 months excluding home auto loan   #
#===============================================================#

# Checking for NA values
merged_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. %>%
  is.na() %>%
  sum()

# 0

# Checking for outliers
merged_data$No.of.PL.trades.opened.in.last.12.months %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$No.of.PL.trades.opened.in.last.12.months %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")


#=================================================================#
#   No if inquiries in last 12 months excluding home auto loan    #
#=================================================================#

# Checking for NA values
merged_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. %>%
  is.na() %>%
  sum()

# 0

# Checking for outliers
merged_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. > 12)),] $No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 12


#=================================#
#   Presence of open home loan    #
#=================================#

# Checking for NA values
merged_data$Presence.of.open.home.loan %>%
  is.na() %>%
  sum()

# 272

merged_data$Presence.of.open.home.loan %>%
  as.factor() %>%
  summary()

merged_data$Presence.of.open.home.loan[which(is.na(merged_data$Presence.of.open.home.loan))] <- 0

# Converting to factor type
merged_data$Presence.of.open.home.loan <- merged_data$Presence.of.open.home.loan %>%
  as.factor()

#=================================#
#   Presence of open auto loan    #
#=================================#

# Checking for NA values
merged_data$Presence.of.open.auto.loan %>%
  is.na() %>%
  sum()

# 0

merged_data$Presence.of.open.auto.loan %>%
  as.factor() %>%
  summary()

# Converting to factor type
merged_data$Presence.of.open.auto.loan <- merged_data$Presence.of.open.auto.loan %>%
  as.factor()

#=========================#
#   Outstanding Balance   #
#=========================#

# Checking for NA values
merged_data$Outstanding.Balance %>%
  is.na() %>%
  sum()
# 272

merged_data$Outstanding.Balance %>%
  summary()
# Median = 774985

merged_data$Outstanding.Balance[which(is.na(merged_data$Outstanding.Balance))] <- 774985


# Checking for outliers
merged_data$Outstanding.Balance %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Outstanding.Balance %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")


#=========================#
#   Total no of trades    #
#=========================#

# Checking for NA values
merged_data$Total.No.of.Trades %>%
  is.na() %>%
  sum()

# Checking for outliers
merged_data$Total.No.of.Trades %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Total.No.of.Trades %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Total.No.of.Trades > 20)),] $Total.No.of.Trades <- 20



names(merged_data)
#=====================================#
# Remove Rest of Missing Data 
#=====================================#

merged_data <- na.omit(merged_data)
merged_data %>% is.na %>% sum()

#==================================#
#  Data Visual.
#==================================#

# Correlation Matrix

# Income vs Performance Tag
merged_data %>%
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=Income_Bin, y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  labs(x="Income Buckets", y="Frequency in 1000s", fill="Performance Tag", title="Income Bucket wise Performance Tag Frequency")
# The Lower Income classes the more chances of default. Lower Incomes have the higher default frequency

# Income vs Credit Usage
ggplot(merged_data, aes(Income_Bin,Avgas.CC.Utilization.in.last.12.months, fill = Income_Bin)) + 
  geom_boxplot()
# Lower incomes use more Credit.

# Income vs Performance
ggplot(merged_data, aes(Performance.Tag, Income, fill = as.factor(Performance.Tag))) + 
  geom_boxplot()



#=====================================#
# Split Dataset into Train and Test   #
#=====================================#

set.seed(123)
sample_size <- floor(0.75 * nrow(merged_data))
train_ind <- sample(seq_len(nrow(merged_data)), size = sample_size)
# Train
data_train <- merged_data[train_ind,]
# Test
data_test <- merged_data[-train_ind,]

#============================#
# RMSE Function  
#============================#

rmse <- function(predcol, ycol) {
  res <- predcol - ycol
  sqrt(mean(res^2))
}

#============================#
# RSquared Function          #
#============================#
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm(formula, family = 'binomial', data=d)
  return(summary(fit)$r.square)
}

#============================#
# Cross Validation Function  
#============================#

# Function to perform cross validation over a logistic regression model
# fmla - Formula of the model
# data - dataFrame of target data
# Returns a prediction vector for the model

nFold <- 10
nRows <- nrow(merged_data)
splitPlan <- kWayCrossValidation(nRows, nFold, NULL, NULL)
myCrossValidationFunction <- function (fmla, data) {
  predictionResult <- 0  # initialize the prediction vector
  for(i in 1:nFold) {
    split <- splitPlan[[i]]
    targetModel <- glm(fmla, family = "binomial", data = data[split$train, ])
    predictionResult[split$app] <- predict(targetModel, newdata = data[split$app, ], type = 'response')
  }
  return (predictionResult)
}


#==================================#
#  Logistic Regression model
#==================================#

# Find the defaulting probability of the average
averageDefaultProspect = mean(merged_data$Performance.Tag)
#====
# Forward Stepwise Model Selection 
#====
null_model <- glm(Performance.Tag ~ 1, data = data_train, family = "binomial")
full_model <- glm(Performance.Tag ~ ., data = data_train, family = "binomial")
step_model_forward <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward", trace = 0)
summary(step_model_forward)

# 10-fold Cross Validation Sampling
# Extract step model formula
fwdSelectFormula <- step_model_forward$call[[2]]
merged_data$pred_fwdSelect <- myCrossValidationFunction(fwdSelectFormula, data = merged_data)

# Accuracy By Mean Default Prospect
modelPredBinarybyMean_fwd <- ifelse(merged_data$pred_freeStepwiseModel > averageDefaultProspect, 1, 0)
table(merged_data$Performance.Tag, modelPredBinarybyMean_fwd)
mean(merged_data$Performance.Tag == modelPredBinarybyMean_fwd)
# 0.5972175
ROC <- roc(merged_data$Performance.Tag, modelPredBinarybyMean_fwd)
auc(ROC)
# Area under the curve: 0.6251


# Accuracy By 0.5 
modelPredMidProb_fwd <- ifelse(merged_data$pred_freeStepwiseModel > 0.5, 1, 0)
table(merged_data$Performance.Tag, modelPredMidProb_fwd)
mean(merged_data$Performance.Tag == modelPredMidProb_fwd)
# 0.957818

ROC <- roc(merged_data$Performance.Tag, modelPredMidProb_fwd)
auc(ROC)
# Area under the curve: 0.5

#=========================================================#

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Performance.Tag ~., data = merged_data,
                    trControl = train.control,
                    method = "glm",
                    family=binomial())
step.model


#=====================================#
# Backward Stepwise Model Selection   #
#=====================================#

model <- glm(Performance.Tag ~ . , family = binomial(link = 'logit'), data = data_train)
step_model <- step(model, trace = 0)
summary(step_model)

# Simple Train and Test Data Sampling
data_train$predictions <- predict(step_model, type = "response")
data_train$predictions <- ifelse(data_train$predictions > mean(data_train$Performance.Tag), 1, 0)
mean(data_train$Performance.Tag == data_train$predictions)
# 0.5984198

data_test$predictions <- predict(step_model, newdata = data_test, type = "response")
data_test$predictions <- ifelse(data_test$predictions > mean(data_test$Performance.Tag), 1, 0)
mean(data_test$Performance.Tag == data_test$predictions)
# 0.6082675

# 10-fold Cross Validation Sampling
# Extract step model formula
freeStepWiseFormula <- step_model$call[[2]]
merged_data$pred_freeStepwiseModel <- myCrossValidationFunction(freeStepWiseFormula, data = merged_data)

# Accuracy By Mean Default Prospect
modelPredBinarybyMean <- ifelse(merged_data$pred_freeStepwiseModel > averageDefaultProspect, 1, 0)
table(merged_data$Performance.Tag, modelPredBinarybyMean)
mean(merged_data$Performance.Tag == modelPredBinarybyMean)
# 0.5972175

ROC <- roc(merged_data$Performance.Tag, modelPredBinarybyMean)
auc(ROC)
# Area under the curve: 0.6251

# Accuracy By 0.5 Binary
modelPredMidProb <- ifelse(merged_data$pred_freeStepwiseModel > 0.5, 1, 0)
table(merged_data$Performance.Tag, modelPredMidProb)
mean(merged_data$Performance.Tag == modelPredMidProb)
# 0.9578180
ROC <- roc(merged_data$Performance.Tag, modelPredMidProb)
auc(ROC)
# Area under the curve: 0.5


#==============================================================#
# Accuracy Limitation
# Eventhough the high accuracy, the result is misleading 
# As the rarity of the outcome being predicted.
#The model is actually performing WORSE than if it were to predict non-default for every record.
# The accuracy would have been 95.7% (the default percentage) if a model had simply predicted "no default" for each record.
#==============================================================#


#Generalized Linear Model 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 62878, 62877, 62878, 62878, 62878, 62877, ... 
# Resampling results:

# 69864 samples
# 32 predictor

#   RMSE       Rsquared    MAE       
# 0.1998038  0.01234167  0.07964126

#Function to replace continuous variables with woe values
replace_woe_continuous <-function(woetable, var_df,colname){
  
  woetable<- data.frame(woetable)
  
  if(woetable[1,1]=="NA") {group<-c(NA,seq(1, nrow(woetable)-1))}else {group <- seq(1,nrow(woetable))}
  woetable_group<- data.frame(cbind(woe=woetable$WOE, group=group))
  woetable_group$group=factor(woetable_group$group)
  
  bins<- woetable[,1] %>% str_match_all("[,][0-9]+[\\]]") %>% unlist %>% str_match_all("[0-9]+") %>% as.numeric
  if(bins[1]==0 ){ bins<-c(-1,bins)} else { bins<-c(0,bins)}
  labels<-seq(1:(length(bins)-1))
  
  var_group <- data.frame(cbind(variable=var_df, group=cut(var_df, bins, right=TRUE, include.lowest=TRUE, 
                                                           labels = labels)))
  var_group$id<-1:nrow(var_group)
  var_group<-merge(var_group,woetable_group, by="group",  all.x=TRUE ,sort=FALSE)
  var_group<-var_group[order(var_group$id), ]
  
  colnames(var_group)[4]<-colname
  
  return (var_group[4])
}

#function to prepare parameters and call replace_woe_continuous function 
callreplace<-function(index,ivtables_list,numvar_dt){
  
  #From ivtables_list, extract woetable corresponding to the numeric variable 
  param1<-ivtables_list[colnames(numvar_dt)[index]][[1]]
  #convert the numeric variable list into numeric data
  param2<-as.numeric(numvar_dt[,index])
  #create the woe column name as <numeric column name>_woe
  param3<-paste(colnames(numvar_dt)[index],"_","woe")
  
  df<-replace_woe_continuous(param1,param2,param3)
  return(df)
}

#Function to replace categorical variables with woe values
replace_woe_cat <-function(index,ivtables_list,catvar_dt){
  
  #Extract the name of the categorical variable
  colname<-colnames(catvar_dt)[index]
  #extract the categorical variable dataframe at the current index
  cat_dt<-data.frame(catvar_dt[,index])
  cat_dt$id<-1:nrow(cat_dt)
  #Set categorical variable dt column name to be same as the category name
  colnames(cat_dt)[1]<-colname
  
  #From ivtables_list, extract woetable corresponding to the categorical variable 
  woetable<-data.frame(ivtables_list[colname][[1]])
  #choose only category and woe columns
  woetable<-woetable[c(1,4)]
  
  
  #Merge woetable and category dt
  cat_list<-merge(cat_dt, woetable, by=colname, all.x=TRUE, sort=FALSE )
  cat_list<-cat_list[order(cat_list$id), ]
  colnames(cat_list)[3]<-paste(colname,"_","woe")
  
  return (cat_list[3])
  
}

# Create Woe value list for continuous variables
#Extract all numeric variables
appdatanumeric<- select_if(appdata_clean, is.numeric)
str(appdatanumeric)
#Remove application ID and Performance.Tag.x  
appdatanumeric<-data.frame(appdatanumeric[,-c(1,7 )])
#Extract woe values for each numeric variable in appdata
woe_list<-sapply(1:ncol(appdatanumeric),function(i,j,k)callreplace(i,j,k),IV$Tables,appdatanumeric)
woe_df<-as.data.frame(do.call(cbind, woe_list))

# Create Woe value list for categorical variables
#Extract all categorical variables
appdatacat<- select_if(appdata_clean, is.factor)
appdatacat<-data.frame(appdatacat)
str(appdatacat)
#Extract woe values for each categorical variable in appdata
woe_catlist<-sapply(1:ncol(appdatacat),function(i,j,k)replace_woe_cat(i,j,k),IV$Tables,appdatacat)
woe_catlist_df<-as.data.frame(do.call(cbind, woe_catlist))


# Create consolidated woe values df along with application id and the dependent variable Performance.Tag.x
woe_appdata<-data.frame(cbind(woe_df,woe_catlist_df,
                              Performance.Tag.x=appdata_clean$Performance.Tag.x ))
nonwoe_appdata<-appdata[,-c(1)]


#===============================#
#  Code for Information Value   #
#===============================#
library(Information)

# Filter the dataset for relevant variables and ensure that Performance Tag is converted to numeric value.
IV <- create_infotables(data = Filtered_Merged_DataSet, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)

# Sample: View derived Information Value for each variable
print(IV$Tables$variable_name, row.names = F)

# Sample Plot of Information Table
plot_infotables(IV, "Age") + theme_minimal()

# Checking the Information Value Summary
IV_Value[order(-IV_Value$IV), ]
# Compare the Information value of demographic data variables with credit data variables

#########################
##    Model Building   ##
#########################

library(caTools)
library(MASS)
library(car)
library(caret)

# Removing variable not considered in model building.

#=============================#
#    Demographic Data Model   #
#=============================#

# Filtering data of Demographic from merged dataframe

# Creating Dummy variables for categorical data like gender: For example

# demo_cat <- demographics_variables_Dataframe[,Categorical variables]

# dummies <- data.frame(sapply(demo_cat, function(x) data.frame(model.matrix(~x-1, data = demo_cat))[,-1]))

# demo_scaled <- demographics_variables_Dataframe[,c(2, 6, 10, 11)] %>% scale()

Performance_Tag <- demographics_variables_Dataframe$Performance_Tag

demographics_variables_Dataframe <-  cbind(Performance_Tag, demo_scaled, dummies) 

# Splitting the data
set.seed(42)

indices <- sample.split(demographics_variables_Dataframe$Performance_Tag, SplitRatio = 0.7)
train_demo <- demographics_variables_Dataframe[indices,]
test_demo <- demographics_variables_Dataframe[!(indices),]


model_demo_1 = glm(Performance_Tag~., data = train_demo, family = "binomial")
summary(model_demo_1)

# Prediction 
test_demo_pred <- predict(model_demo_1, type = "response", 
                          newdata = test_demo[,-1]) 

summary(test_demo_pred)


test_pred <- factor(ifelse(test_demo_pred >= 0.50, "Yes", "No"))
test_actual <- factor(ifelse(test_demo$Performance_Tag==1,"Yes","No"))

table(test_actual, test_pred)


# Cutoff finding function
predict_demo_cutoff <- function(cutoff) 
{
  predicted_perform <- factor(ifelse(test_demo_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_perform, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.07,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = predict_demo_cutoff(s[i])
} 

cutoff_demo <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Finding prediction with cutoff
test_cutoff_pred <- factor(ifelse(test_demo_pred >= cutoff_demo, "Yes", "No"))

table(test_actual, test_cutoff_pred)

# Creating Confusion Matrix
test_conf_demo <- confusionMatrix(test_cutoff_pred, test_actual, positive = "Yes")
test_conf_demo

# Accuracy, Sensitivity and Specificity
acc <- test_conf_demo$overall[1]
sens <- test_conf_demo$byClass[1]
spec <- test_conf_demo$byClass[2]

acc # 0.5522687 
sens #  0.5576923
spec #  0.5520299 


#==================================#
#    Data Model on combined data   #
#==================================#

# Separate the Categorical and Continuous Data
# Scale the Continuous Data
# Create Dummy variables from categorical data
dummies_merged_data <- data.frame(sapply(dF_categorical, function(x) data.frame(model.matrix(~x-1, data = dF_categorical))[,-1])) 


final_data <- cbind(Performance_Tag, DF_Continuous, dummies_merged_data)

str(final_data)
summary(final_data)

# Splitting the data
set.seed(100)

indices <- sample.split(final_data$Performance_Tag, SplitRatio = 0.7)
train_final <- final_data[indices,]
test_final <- final_data[!(indices),]

lr_model_1 = glm(Performance_Tag ~ ., data = train_final, family = "binomial")
summary(lr_model_1)

# Try several models with different variables.

# Example model 
lr_model_2 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company +
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + 
                    PL_Trades_12_months + Inquiries_6_months, 
                  family = "binomial", data = train_final)

summary(lr_model_2)

final_lr_model <- lr_model_xxx

# Predict
test_final_pred <- predict(final_lr_model, type = "response", 
                           newdata = test_final[,-1]) 

summary(test_final_pred)

test_final_predicted <- factor(ifelse(test_final_pred >= 0.5, "Yes", "No"))
test_final_actual <- factor(ifelse(test_final$Performance_Tag==1,"Yes","No"))

table(test_final_actual, test_final_predicted)

# Cutoff finding function
predict_final_cutoff <- function(cutoff) 
{
  predicted_perform <- factor(ifelse(test_final_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_perform, test_final_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.07,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = predict_final_cutoff(s[i])
} 

cutoff_final <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Prediction on cutoff
test_final_cutoff_pred <- factor(ifelse(test_final_pred >= cutoff_final, "Yes", "No"))

table(test_final_actual, test_final_cutoff_pred)

# Creating Confusion Matrix
test_conf_final <- confusionMatrix(test_final_cutoff_pred, test_final_actual, positive = "Yes")
test_conf_final

# Accuracy, Sensitivity and Specificity
acc <- test_conf_final$overall[1]
sens <- test_conf_final$byClass[1]
spec <- test_conf_final$byClass[2]

acc # 0.6170619  
sens #  0.6187783 
spec #  0.6169863
