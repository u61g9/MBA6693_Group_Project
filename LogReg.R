# RESTART

# Load library data
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
library(gridExtra)
library(bnpa)


# Read in data

getwd()
credit <- read.csv("Credit_Bureau.csv", header = T)
demo <- read.csv("demogs.csv")

# Remove duplicates

demo <- demo %>%
  group_by(Application.ID) %>%
  filter(n() == 1)
credit <- credit %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

# Merge datasets together
merged_data <- merge(demo,credit,by=c("Application.ID","Performance.Tag"))
merged_data <- na.omit(merged_data)
# REmove application ID
merged_data$Application.ID <- NULL


# Set all Char vaiables to factors and all integers to numeric, presence of
# loans and Performance.Tag to factors
merged_data <- merged_data %>% mutate_if(is.character,as.factor)
merged_data <- merged_data %>% mutate_if(is.integer,as.numeric)
merged_data$Performance.Tag <- as.factor(merged_data$Performance.Tag)
merged_data$Presence.of.open.home.loan  <- as.factor(merged_data$Presence.of.open.home.loan )
merged_data$Presence.of.open.auto.loan  <- as.factor(merged_data$Presence.of.open.auto.loan )

# Create dummy variables for all factors
appdatacat<- select_if(merged_data, is.factor)
appdatacat<-data.frame(appdatacat)
dummies <- data.frame(sapply(appdatacat, function(x) data.frame(model.matrix(~x-1, data = appdatacat))[,-1]))
appdatanum <- select_if(merged_data,is.numeric) %>% scale()
head(dummies)
merged_data1 <- cbind(dummies,appdatanum)

set.seed(42)
gp <- runif(nrow(merged_data1))
data_train <- merged_data1[gp < 0.8, ]
data_test <- merged_data1[gp >= 0.8, ]
(nrow(data_train))
(nrow(data_test))
(nrow(merged_data1))

# Oversample the defaults to balance the dataset
library(ROSE)
?ROSE
data_rose <- ROSE(Performance.Tag~., data=data_train, seed=42)$data
table(data_rose$Performance.Tag)

# IV Analysis to determine best data to fit to model

IV <- create_infotables(data = data_rose,y="Performance.Tag",bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)

# Sample: View derived Information Value for each variable
print(IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., row.names = F)
# Sample Plot of Information Table
plot_infotables(IV, "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") + theme_minimal()

# Checking the Information Value Summary
IV_Value[order(-IV_Value$IV), ]

# Choose subset (note that I based this completely on hte example provided)

formula <- Performance.Tag ~ Income + No.of.months.in.current.residence + 
  No.of.months.in.current.company +
  No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

glm_model <- glm(formula, family = "binomial", data = data_rose)
summary(glm_model)
pred <- predict(object = glm_model, newdata = data_test, type = "response")
pred1 <- if_else(pred>0.4,1,0)
pred2 <- if_else(pred>0.45,1,0)
pred3 <- if_else(pred>0.5,1,0)
pred4 <- if_else(pred>0.55,1,0)
pred5 <- if_else(pred>0.6,1,0)
pred6 <- if_else(pred>0.65,1,0)
pred7 <- if_else(pred>0.7,1,0)
pred8 <- if_else(pred>0.75,1,0)

t1 <- table(data_test$Performance.Tag, pred03)
t2 <- table(data_test$Performance.Tag, pred05)
t3 <- table(data_test$Performance.Tag, pred10)
t4 <- table(data_test$Performance.Tag, pred15)
t5 <- table(data_test$Performance.Tag, pred20)
t6 <- table(data_test$Performance.Tag, pred30)
t7 <- table(data_test$Performance.Tag, pred40)
t8 <- table(data_test$Performance.Tag, pred50)
df <- data.frame(x = c(1,2,3,4,5,6,7,8), 
                 tp = c(t3[1,1],t5[1,1],t10[1,1],t15[1,1],t20[1,1],t30[1,1],t40[1,1],t50[1,1]),
                 fn = c(t3[1,2],t5[1,2],t10[1,2],t15[1,2],t20[1,2],t30[1,2],t40[1,2],t50[1,2]),
                 fp = c(t3[2,1],t5[2,1],t10[2,1],t15[2,1],t20[2,1],t30[2,1],t40[2,1],t50[2,1]),
                 tn = c(t3[2,2],t5[2,2],t10[2,2],t15[2,2],t20[2,2],t30[2,2],t40[2,2],t50[2,2]))
df
df$acc <- (df[,2]+df[,5])/(df[,2]+df[,3]+df[,4]+df[,5])
df$sens <- (df[,2])/(df[,2]+df[,4])
df$spec <- (df[,5])/(df[,3]+df[,5])

df

# Specificity is still in teh 8% range at best.   
















