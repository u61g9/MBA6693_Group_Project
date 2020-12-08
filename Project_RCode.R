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
library(vtreat)
library(leaps)
library(MASS)
library(lattice)
library(caret)
library(boot)
library(pROC)
library(tidyverse)

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
filter_data[,'Performance.Tag']<-factor(filter_data[,'Performance.Tag'])

# Check for NA's in data (this will matter with some of the models)
library(bnpa)
(nrow(filter_data))
check.na(filter_data)
filter_data <- na.omit(filter_data)
(nrow(filter_data))
check.na(filter_data)

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
class_pred_train <- predict(object = class_model, newdata = data_train, type = "class")
class_pred <- predict(object = class_model, newdata = data_test, type = "class")

(mean(class_pred_train == data_train$Performance.Tag))
(mean(class_pred == data_test$Performance.Tag))
cM <- confusionMatrix(data = class_pred,reference = data_test$Performance.Tag)
cM
# The classification tree is 95.7% accurate, which seems good, but the model is 
# predicting zero in all insances.  We may need to allow for smaller min splits 
# due to the rarity of defaults.  
minsplit = 5
maxdepth = 30
cp = 0.0001
class_model <- rpart(formula = Performance.Tag~.,
      data = data_train,
      method = "class",
      minsplit = minsplit,
      maxdepth = maxdepth,
      cp = cp)
class_pred_train <- predict(object = class_model, newdata = data_train, type = "class")
class_pred <- predict(object = class_model, newdata = data_test, type = "class")
(mean(class_pred_train == data_train$Performance.Tag))
(mean(class_pred == data_test$Performance.Tag))
cM <- confusionMatrix(data = class_pred,reference = data_test$Performance.Tag)
cM
plot(class_model)
# The training accuracy has improved to 98%, but the test error is only 92.5%.
#   This is symptom of overfitting.  We can tune the hyper parameters.

splits = c(1,3,5,10,20)
depths = c(5,10,15,20,25)
cps = c(0.0001,0.001,0.01)

hyper_grid <- expand.grid(splits = splits,depths = depths,cps = cps)
accuracy = c()
hyper_grid
# Note: this for loop takes 10 minutes to run, so I have commented it out for 
# ease with double ##'s.
nrow(hyper_grid)
## for(i in 1:(nrow(hyper_grid))) {
##    class_model <- rpart(formula = Performance.Tag~.,
## data = data_train,
## method = "class",
##                         minsplit = hyper_grid$splits[i],
## maxdepth = hyper_grid$depths[i],
##                         cp = hyper_grid$cps[i])
##    class_pred <- predict(object = class_model, newdata = data_test, type = "class")
##    accuracy[i] = mean(class_pred == data_test$Performance.Tag)
##  }

accuracy
 
# Not a single tree results in higher degree of accuracy than simply guessing 
# that no one will default (accuracy = 95.777773%).  We should maybe # try a 
 # different appraoch


# Bagged Tree
#------------------------

# We will try abagged tree to see if that gives a better result.
 
library(ipred)
library(e1071)
bag_model <- bagging(Performance.Tag~.,data = data_train)
bag_pred <- predict(object = bag_model, newdata = data_test, type = "class")
print(data_test$Performance.Tag)
table(data_test$Performance.Tag)
bag_cM <- confusionMatrix(data = bag_pred, reference = data_test$Performance.Tag)
bag_cM

# The confusion matrix shows that the model doesn't predict a single defualt 
# correctly.  Sensitivity is close ot 100%, but Specificity = 0%.

# Random Forest
#--------------------------

# The random forest method shoudl allow us to produce a more useful tree as
# it will allow us to increase the weight of credit defaults in the data set

library(randomForest)
forest_model <- randomForest(Performance.Tag~.,data = data_train)
print(forest_model)

err <- forest_model$err.rate
head(err)

# Again, the tree method is predicting only that the person wil not default.
# I am goign to choose my test data set slightly differently, so that I choose
# 80% of my 'defaults' but only 5% of my non-defaults.  This should give more
# consideration for the defaults and hopefully hep build the tree in a better
# way.  The balance of the data will be my test set.

# First I will Split my data set into defaults and nondefaults.

nondef <- filter_data %>%
  filter(Performance.Tag =="0")
def <- filter_data %>%
  filter(Performance.Tag == "1")


set.seed(42)
gp1 <- runif(nrow(nondef))
gp2 <- runif(nrow(def))
nondef05 <- nondef[gp1 < 0.05, ]
nondef95 <- nondef[gp1 >= 0.05, ]
def80 <- def[gp2 < 0.8, ]
def20 <- def[gp2 >= 0.8, ]
(nrow(nondef05))
(nrow(nondef95))
(nrow(nondef))
(nrow(def20))
(nrow(def80))
(nrow(def))
(nrow(data_test))
(nrow(merged_data))

# nondef05 has 3300 entries and def80 has 2340.  These will be combined to form
# the training set

data_train <- rbind(def80,nondef05)
data_test <- rbind(def20,nondef95)

forest_model <- randomForest(Performance.Tag~.,data = data_train)
print(forest_model)

err <- forest_model$err.rate
head(err)

write.csv(err,"err.csv", row.names = FALSE)
res <- tuneRF(x = subset(data_train, select= -Performance.Tag), y = data_train$Performance.Tag,ntreeTry = 500)
print(res)

forest_model <- randomForest(Performance.Tag~.,data = data_train, mtry = 3)
print(forest_model)

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(data_train) * 0.5, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(data_train) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)
hyper_grid
# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = Performance.Tag ~ ., 
                        data = data_train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

model <- randomForest(formula = Performance.Tag ~ ., 
                      data = data_train,
                      mtry = hyper_grid$mtry[opt_i],
                      nodesize = hyper_grid$nodesize[opt_i],
                      sampsize = 3795)
print(model)

# The best we do with a random forest is an OOB accuracy of 62.8%
# sensitivity = 2608/(2608 + 1397) = 65.1%
# specificity = 943/(943 + 741) = 56.0%

# The last step will be to check the accuracy on the test set
pred <- predict(object = model,
                newdata = data_test,
                type = "prob")
head(pred)

pred1 <- ifelse(pred[,1] >= 0.58,1,0)
cF <- confusionMatrix(as.factor(pred1),data_test$Performance.Tag)
cF
# install.packages("pROC")
library("pROC")
ROC <- roc(data_test$Performance.Tag, pred1)
plot(ROC, col = "blue")
auc(ROC) 


# The specificity of the test data is much worse
# accuracy = 38.3%
# sensitivity = 38.3%
# specificity = 39.5%

#=============================================#
#  LOGISTIC REGRESSION ANALYSIS               #
#=============================================#

# The second model we will attempt to use is a logistic regression

# First we will redefine our test set and training set in order to ensure 
# everything is as we expect.

set.seed(42)
gp <- runif(nrow(filter_data))
data_train <- filter_data[gp < 0.8, ]
data_test <- filter_data[gp >= 0.8, ]
(nrow(data_train))
(nrow(data_test))
(nrow(merged_data))

glm_model <- glm(Performance.Tag ~., data = data_train, family = "binomial")
summary(glm_model)

# There are a lot of insignificant predictors.

pred <- predict(object = glm_model, newdata = data_test, type = "response")
pred03 <- if_else(pred>0.03,1,0)
pred05 <- if_else(pred>0.05,1,0)
pred10 <- if_else(pred>0.1,1,0)
pred15 <- if_else(pred>0.15,1,0)
pred20 <- if_else(pred>0.2,1,0)
pred30 <- if_else(pred>0.3,1,0)
pred40 <- if_else(pred>0.4,1,0)
pred50 <- if_else(pred>0.5,1,0)

t3 <- table(data_test$Performance.Tag, pred03)
t5 <- table(data_test$Performance.Tag, pred05)
t10 <- table(data_test$Performance.Tag, pred10)
t15 <- table(data_test$Performance.Tag, pred15)
t20 <- table(data_test$Performance.Tag, pred20)
t30 <- table(data_test$Performance.Tag, pred30)
t40 <- table(data_test$Performance.Tag, pred40)
t50 <- table(data_test$Performance.Tag, pred50)
t50
df <- data.frame(x = c(3,5,10,15,20,30,40,50), 
                 tp = c(t3[1,1],t5[1,1],t10[1,1],t15[1,1],t20[1,1],t30[1,1],t40[1,1],t50[1,1]),
                 fn = c(t3[1,2],t5[1,2],t10[1,2],t15[1,2],t20[1,2],t30[1,2],0,0),
                 fp = c(t3[2,1],t5[2,1],t10[2,1],t15[2,1],t20[2,1],t30[2,1],t40[2,1],t50[2,1]),
                 tn = c(t3[2,2],t5[2,2],t10[2,2],t15[2,2],t20[2,2],t30[2,2],0,0))
df
df$acc <- (df[,2]+df[,5])/(df[,2]+df[,3]+df[,4]+df[,5])
df$sens <- (df[,2])/(df[,2]+df[,4])
df$spec <- (df[,5])/(df[,3]+df[,5])

df
p <- ggplot(df,aes(x = x, y = acc))
# Teh fit at pretty much all ranges is terrible.  Perhaps the data is being
# overfit.  We have 44 parameters, only nine of which have any level of 
# significance.  We will cut this down to twenty parameters by eleminating the
# least significant, based on the p-value.  (All p-values over xx were removed).
# The parameters to keep are listed below with their p-value
#Age                                                             0.52639    
#No.of.dependents2                                               0.10139    
#No.of.dependents3                                               0.38565    
#No.of.dependents4                                               0.62380    
#No.of.dependents5                                               0.89680    
#Income                                                          0.02289 *  
#No.of.months.in.current.residence                               0.01830 *  
#No.of.months.in.current.company                                 0.01102 *  
#No.of.times.90.DPD.or.worse.in.last.6.months                    0.22974    
#No.of.times.60.DPD.or.worse.in.last.6.months                    0.13270    
#No.of.times.30.DPD.or.worse.in.last.6.months                    0.03471 *  
#No.of.times.90.DPD.or.worse.in.last.12.months                   0.00316 ** 
#No.of.times.30.DPD.or.worse.in.last.12.months                   0.20019    
#Avgas.CC.Utilization.in.last.12.months                          < 2e-16 ***
#No.of.trades.opened.in.last.12.months                           0.36228    
#No.of.PL.trades.opened.in.last.6.months                         0.12740    
#No.of.PL.trades.opened.in.last.12.months                        0.00433 ** 
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.  0.06521 .  
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 1.61e-05 ***
#Presence.of.open.home.loan1                                     0.21915    
#Outstanding.Balance                                             0.19399    
#Total.No.of.Trades                                              0.11014    
#Presence.of.open.auto.loan1                                     0.50095 

formula <- Performance.Tag ~ Age+No.of.dependents+Income+No.of.months.in.current.residence +
  No.of.months.in.current.company+No.of.times.90.DPD.or.worse.in.last.6.months+
  No.of.times.60.DPD.or.worse.in.last.6.months+
  No.of.times.30.DPD.or.worse.in.last.6.months+ 
  No.of.times.90.DPD.or.worse.in.last.12.months+
  No.of.times.30.DPD.or.worse.in.last.12.months+   
  Avgas.CC.Utilization.in.last.12.months+
  No.of.trades.opened.in.last.12.months+   
  No.of.PL.trades.opened.in.last.6.months+  
  No.of.PL.trades.opened.in.last.12.months+
  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+
  Presence.of.open.home.loan+
  Outstanding.Balance+
  Total.No.of.Trades+ 
  Presence.of.open.auto.loan 

formula

glm_model <- glm(formula, data = data_train, family = "binomial")
summary(glm_model)

pred <- predict(object = glm_model, newdata = data_test, type = "response")
pred03 <- if_else(pred>0.03,1,0)
pred05 <- if_else(pred>0.05,1,0)
pred10 <- if_else(pred>0.1,1,0)
pred15 <- if_else(pred>0.15,1,0)
pred20 <- if_else(pred>0.2,1,0)
pred30 <- if_else(pred>0.3,1,0)
pred40 <- if_else(pred>0.4,1,0)
pred50 <- if_else(pred>0.5,1,0)

table(data_test$Performance.Tag, pred03)
table(data_test$Performance.Tag, pred05)
table(data_test$Performance.Tag, pred10)
table(data_test$Performance.Tag, pred15)
table(data_test$Performance.Tag, pred20)
table(data_test$Performance.Tag, pred30)
table(data_test$Performance.Tag, pred40)
table(data_test$Performance.Tag, pred50)

# The model with the best specificity has a threshold of 0.1.  The specificity 
# is still only 10%.  



#=============================================#
# Support Vector Machines                     #
#=============================================#

# Use data sets set up in logistic regression section above:
# 'data_train' and 'data_test'
# Note that this hit a "reached max iterations" when I attempted to
# run 'Performance.Tag ~ .'

# We have to cut the dataset down, so we will assume that the formula used
# in the logistic regression section
# The reduced formula also couldn't solve.  I am goign to try using the 
# test set rather than the training set as it is 1/4 the size.
# The model would still not solve, so I am goign to use my much smaller 
# data set with 5% of the non defaults and 80% of the defaults.

# This still doesn't work, so we need to cut the number of coefficients down even further.

library(e1071)
formula <- Performance.Tag ~ Income+No.of.months.in.current.residence +
  No.of.months.in.current.company+
  No.of.times.30.DPD.or.worse.in.last.6.months+ 
  No.of.times.90.DPD.or.worse.in.last.12.months+
  Avgas.CC.Utilization.in.last.12.months+
  No.of.PL.trades.opened.in.last.12.months+
  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.+
  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

# Columns: 1 ~ 6 + 10 + 11 + 14 + 15 + 18 + 22 + 23 + 24


svm_model<- svm(formula,
                data = data_train,
                type = "C-classification",
                kernel = "linear",
                scale = FALSE)

# Index of support vectors in training dataset
#svm_model$index
# Support vectors
#svm_model$SV
# Negative intercept (unweighted)
#svm_model$rho

# Weighting coefficients for support vectors
#svm_model$coefs
#0.9814815

# Test accuracy
pred_test <- predict(svm_model, data_test)
mean(pred_test == data_test$Performance.Tag)
table(pred_test,data_test$Performance.Tag)
# Again, the model is not predicting any defaults, so it is of no value

# Let's try it again with a dataset with more weight on the defaults
data_train <- rbind(def80,nondef05)
data_test <- rbind(def20,nondef95)

svm_model<- svm(formula,
                data = data_train,
                type = "C-classification",
                kernel = "linear",
                scale = FALSE)

<<<<<<< HEAD
pred_test <- predict(svm_model, data_test)
mean(pred_test == data_test$Performance.Tag)
table(pred_test,data_test$Performance.Tag)
confusionMatrix(pred_test,data_test$Performance.Tag)
# Accuracy and Sensitivity are good, but The sensitivity is very 
# low (168/(168+391) = 30%
=======
# lying on a circle of radius r
circle <-
  function(x1_center, x2_center, r, npoint = 100) {
    # Angular spacing of 2*pi/npoint between points
    theta <- seq(0, 2 * pi, length.out = npoint)
    x1_circ <- x1_center + r * cos(theta)
    x2_circ <- x2_center + r * sin(theta)
    data.frame(x1c = x1_circ, x2c = x2_circ)
  }
# Generate boundary
boundary <- circle(x1_center = 0,
                   x2_center = 0,
                   r = radius)
# Add boundary to previous plot
p <- p +
  geom_path(data = boundary,
            aes(x = x1c, y = x2c),
            inherit.aes = FALSE)
# Display plot
p

#Linear SVM, cost = 1
#Partition radially separable dataset into training/test (seed = 10)
# Build default cost linear SVM on training set
svm_model <- svm(y ~ ., data = trainset, type = "C-classification", kernel = "linear")
svm_model

# Calculate accuracy on test set
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)
# 0.9210526

plot(svm_model, trainset)

#Linear SVM, cost = 100

svm_model <- svm(y ~ ., data = trainset, type = "C-classification", kernel = "linear")
svm_model
#31

# Accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)
# 0.9210526
table(pred_test,testset$y)
plot(svm_model, trainset)

#Average accuracy for default cost SVM
accuracy <- rep(NA, 100)
set.seed(10)
for (i in 1:100) {
  df[, "train"] <- ifelse(runif(nrow(df)) < 0.8, 1, 0)
  trainset <- df[df$train == 1, ]
  testset <- df[df$train == 0, ]
  trainColNum <- grep("train", names(trainset))
  trainset <- trainset[, -trainColNum]
  testset <- testset[, -trainColNum]
  svm_model<- svm(y ~ ., data = trainset, type = "C-classification", cost = 1, kernel = "linear")
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)}
mean(accuracy)
# 0.642843
sd(accuracy)
# 0.07606017
>>>>>>> 9c3c1826756deceaa514851211f67ccee975de39

# Columns: 1 ~ 6 + 10 + 11 + 14 + 15 + 18 + 22 + 23 + 24
#============================================#
#             Tuning                         #
#============================================#

# The tuning function won't solve (too many iterations), so we need to tune manually.
# Polynomial also won't solve, so we need to cut our dataset down further

set.seed(42)
gp1 <- runif(nrow(nondef))
gp2 <- runif(nrow(def))
nondef05 <- nondef[gp1 < 0.01, ]
nondef95 <- nondef[gp1 >= 0.01, ]
def80 <- def[gp2 < 0.1, ]
def20 <- def[gp2 >= 0.1, ]
(nrow(nondef05))
(nrow(nondef95))
(nrow(nondef))
(nrow(def20))
(nrow(def80))
(nrow(def))
(nrow(data_test))
(nrow(merged_data))

data_train <- rbind(def80,nondef05)
data_test <- rbind(def20,nondef95)
# Note that this data test contains only 3% of non defaults and 50% of defaults

svm_model1<- svm(formula,
                data = data_train,
                type = "C-classification",
                kernel = "polynomial", degree = 2,
                scale = FALSE)

pred_test1 <- predict(svm_model1, data_test)
mean(pred_test1 == data_test$Performance.Tag)
table(pred_test1,data_test$Performance.Tag)

# We'll try a radial kernel
nondef05 <- nondef[gp1 < 0.05, ]
nondef95 <- nondef[gp1 >= 0.05, ]
def80 <- def[gp2 < 0.8, ]
def20 <- def[gp2 >= 0.8, ]
data_train <- rbind(def80,nondef05)
data_test <- rbind(def20,nondef95)

svm_model2<- svm(formula,
                 data = data_train,
                 type = "C-classification",
                 kernel = "radial", 
                 scale = FALSE)

pred_test2 <- predict(svm_model2, data_test)
mean(pred_test2 == data_test$Performance.Tag)
table(pred_test2,data_test$Performance.Tag)


#tune_out <- tune.svm(x = data_train[,c(6,10,11,14,15,18,22,23,24)], y = data_train[,1],
#                     type = "C-classification", kernel = "radial",
#                     cost = 10^(-1:2), gamma = c(0.1,1,10), coef0 = c(0.1,1,10))
 #print out tuned parameters
#tune_out$best.parameters$cost
# 1
#tune_out$best.parameters$gamma
#10
#tune_out$best.parameters$coef0
#0.1
