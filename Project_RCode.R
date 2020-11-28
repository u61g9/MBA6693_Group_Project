#  MBA6693 - Group Project
#  Authors: Namrata Chintan Gala, Gautam Seghal, Darren Clark
#  2020-12-10

#==============================================#
#  Overview of Dataset                         #
#==============================================#

# Credit bureau data was provided which indicates the default of a number of 
# different borrowers.  Additional dmographic and other data is also provided
# to help determine what may be causing the defaults as well as what the 
# likelihood of default may be in the future for new borrowers.

#==============================================#
#  Read in Data, dplyr, ggplot, etc.           #
#==============================================#

library(ggplot2)
library(Metrics)
library(dplyr)
library(reshape2)
library(Information)
library(gapminder)

getwd()
credit <- read.csv("Credit_Bureau.csv", header = T)
demo <- read.csv("demogs.csv")
#View(credit)
#View(demo)

# Set color pallettes
cp_2 <- c("#FEA47F", "#F97F51")
cp_3 <- c("#2A363B", "#E84A5F", "#FF847C")
cp_5 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898")
cp_8 <- c("#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")

#================================================#
#    Exploratory Data Analysis                   #
#================================================#

# First we will chech the data for completeness, duplicates, etc. and clean up
# as needed.

# Checking for Duplicate
(nrow(demo))
(length(unique(demo$Application.ID)))
# There are 71295 rows and 71292 unique entries, so we have 3 duplicates

(nrow(credit))
(length(unique(credit$Application.ID)))
# There are 71295 rows and 71292 unique entries, so we have 3 duplicates 

# Remove duplicates

demo <- demo %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

credit <- credit %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

(nrow(credit))
(nrow(demo))
# Total 71289 rows in both datasets.  

# The next step is to merge these two data sets into one.
merged_data <- merge(demo,credit,by=c("Application.ID","Performance.Tag"))
View(merged_data)
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


# Next we will check the default rate for a number of demographics

def_rate_Gender <- merged_data  %>%
  group_by(Gender) %>%
  summarise(def_rate = mean(Performance.Tag))
def_rate_Gender 

def_rate_Marital <- merged_data  %>%
  group_by(Marital.Status..at.the.time.of.application.) %>%
  summarise(def_rate = mean(Performance.Tag))
def_rate_Marital

def_rate_Education <- merged_data  %>%
  group_by(Education) %>%
  filter(Education != "") %>%
  summarise(def_rate = mean(Performance.Tag))
def_rate_Education 

def_rate_Residence <- merged_data  %>%
  group_by(Type.of.residence) %>%
  summarise(def_rate = mean(Performance.Tag))
def_rate_Residence
ggplot(def_rate_Education, aes(reorder(Education, def_rate),def_rate)) + geom_col() + labs(x = "Education", y = "Default Rate")

# Age is continuous, but can be binned into discrete ranges
# Check for errors or outliers:
merged_data$Age %>% is.na()%>% sum()
merged_data$Age %>% boxplot()
# filter out under-age people
merged_data[(which(merged_data$Age<18)),]$Age <- 18

#Creating bins on Age
# 0 - 20,21-25, ...61 - 65

age_bin <- function(age){
  
  if(age > 0 && age < 26)
    return ("18-25")
  else if(age > 25 && age < 36)
    return ("26-35")
  else if(age > 35 && age < 46)
    return ("36-45")
  else if(age > 45 && age < 56)
    return ("46-55")
  else if(age > 55 && age < 66)
    return ("56-65")
  else if(age > 65)
    return ("66 and older")
}

merged_data$age_bin <- merged_data$Age %>%
  sapply(age_bin)%>% as.factor()
summary(merged_data$age_bin)

ggplot(merged_data,aes(x = age_bin, y= ..count.., fill = age_bin)) + geom_bar()

# We can conclude that loan holder are dense from 36 to 55 Age
# Age Bucket wise Performance Tag Frequency
merged_data %>% 
  filter(!is.na(Performance.Tag)) %>%
  ggplot(aes(x=age_bin, y=..count../1000, fill=as.factor(Performance.Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2)
# From the above plot we can conclude that default is high between 36 to 55 Age compare to Other groups


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



# DPD Correlation heat map for 12 months
ggplot(melted_cor_DPD_12, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(x="", y="", title="DPD 12 months Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))


# From the above heat Map we can conclude that light blue = 1 is the relationship of variable with itself.
# Dark blue color=0.84 shows the negative relationship between the variables
# For 12 Months 30DPD and 60DPD average relationship.
# For 12 Months 30DPD and 60DPD has significant negative relationship and more towards 0.80 value Dark Blue

#===========================================#
#  Tree-based Analysis                      #
#===========================================#

# We will examine four different tree based methods to determine which best suits
# the credit_data.  This will start with a traditional classification tree model, a
# bagged tree model, a random forest model and a gradient-boosting machine (GBM).

# We need to create a training and test set for the analysis.  This will be 
# consistent across all subsequent analyses.  First, we will filter our data to
# only include the Performance.Tag column and those we want to use in our tree.

filter_data <- merged_data
filter_data$Application.ID <- NULL
filter_data$age_bin <-NULL

set.seed(42)
gp <- runif(nrow(filter_data))
data_train <- filter_data[gp < 0.8, ]
data_test <- filter_data[gp >= 0.8, ]
(nrow(data_train))
(nrow(data_test))
(nrow(merged_data))

# Classification Tree
#------------------------

library(rpart)
library(caret)

class_model <- rpart(Performance.Tag ~ ., data_train, method = "class")
class_pred <- predict(class_model, newdata = data_test, type = "class")
View(class_pred)
str(data_test$Performance.Tag)
confusionMatrix(data = class_pred, reference = data_test$Performance.Tag)
