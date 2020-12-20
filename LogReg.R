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
# Remove application ID
merged_data$Application.ID <- NULL


# Set all Char vaiables to factors and all integers to numeric, presence of
# loans and Performance.Tag to factors
merged_data <- merged_data %>% mutate_if(is.character,as.factor)
merged_data <- merged_data %>% mutate_if(is.integer,as.numeric)
merged_data$Presence.of.open.home.loan  <- as.factor(merged_data$Presence.of.open.home.loan )
merged_data$Presence.of.open.auto.loan  <- as.factor(merged_data$Presence.of.open.auto.loan )
# 
# # Create dummy variables for all factors
# appdatacat<- select_if(merged_data, is.factor)
# appdatacat<-data.frame(appdatacat)
# dummies <- data.frame(sapply(appdatacat, function(x) data.frame(model.matrix(~x-1, data = appdatacat))[,-1]))
# appdatanum <- select_if(merged_data,is.numeric) %>% scale()
# head(dummies)
# merged_data1 <- cbind(dummies,appdatanum)
# 

# do WOE stuff.
# Load in WOE functions
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

# IV Analysis to determine best data to fit to model

IV <- create_infotables(data = merged_data,y="Performance.Tag",bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)

# Sample: View derived Information Value for each variable
print(IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., row.names = F)
# Sample Plot of Information Table
plot_infotables(IV, "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.") + theme_minimal()

# Checking the Information Value Summary
IV_Value[order(-IV_Value$IV), ]



appdatanumeric<- select_if(merged_data, is.numeric)
str(appdatanumeric)
#Remove application ID and Performance.Tag.x  
appdatanumeric<-data.frame(appdatanumeric[,-c(1,7 )])
#Extract woe values for each numeric variable in appdata
woe_list<-sapply(1:ncol(appdatanumeric),function(i,j,k)callreplace(i,j,k),IV$Tables,appdatanumeric)
woe_df<-as.data.frame(do.call(cbind, woe_list))
str(woe_df)

# Create Woe value list for categorical variables
#Extract all categorical variables
appdatacat<- select_if(merged_data, is.factor)
appdatacat<-data.frame(appdatacat)
str(appdatacat)
#Extract woe values for each categorical variable in appdata
woe_catlist<-sapply(1:ncol(appdatacat),function(i,j,k)replace_woe_cat(i,j,k),IV$Tables,appdatacat)
woe_catlist_df<-as.data.frame(do.call(cbind, woe_catlist))
str(woe_catlist_df)

# Create consolidated woe values df along with application id and the dependent variable Performance.Tag.x
woe_appdata<-data.frame(cbind(woe_df,woe_catlist_df,
                              Performance.Tag=merged_data$Performance.Tag))
str(woe_appdata)
nonwoe_appdata<-merged_data[,-c(1)]







set.seed(42)
gp <- runif(nrow(woe_appdata))
data_train <- woe_appdata[gp < 0.8, ]
data_test <- woe_appdata[gp >= 0.8, ]
(nrow(data_train))
(nrow(data_test))
(nrow(woe_appdata))

# Oversample the defaults to balance the dataset
library(ROSE)
?ROSE
data_rose <- ROSE(Performance.Tag~., data=data_train, seed=42)$data
table(data_rose$Performance.Tag)
str(data_train)

# Choose subset (note that I based this completely on hte example provided)

formula <- Performance.Tag ~ Income._.woe + No.of.months.in.current.residence._.woe + 
  No.of.months.in.current.company._.woe +
  No.of.times.30.DPD.or.worse.in.last.12.months._.woe + Avgas.CC.Utilization.in.last.12.months._.woe + 
  No.of.PL.trades.opened.in.last.12.months._.woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.._.woe

glm_model <- glm(formula, family = "binomial", data = data_rose)
summary(glm_model)
pred <- predict(object = glm_model, newdata = data_test, type = "response")
actual <- as.factor(if_else(data_test$Performance.Tag >0.5,"Yes","No"))
pred1 <- as.factor(if_else(pred>0.538,"Yes","No"))

cM <- confusionMatrix(pred1,actual)
cM

# The prediction at 0.54 gives a balanced view.  Accuracy = 0.626, 
# sensitivity = 0.626; accuracy = 0.627


# Run model with all possible inputs

formula1 <- Performance.Tag ~ .

glm_model1 <- glm(formula1, family = "binomial", data = data_rose)
summary(glm_model1)
pred <- predict(object = glm_model1, newdata = data_test, type = "response")
pred1 <- as.factor(if_else(pred>0.52,"Yes","No"))
cM <- confusionMatrix(pred1,actual)
cM


# The prediction at 0.52 gives a balanced view.  Accuracy = 0.623, 
# sensitivity = 0.623; accuracy = 0.624

# Remove all insignificant regressors at 99.9% level

formula2 <- Performance.Tag ~ Age._.woe +                                         
  No.of.dependents._.woe +                                               
  No.of.months.in.current.company._.woe +                                
  No.of.times.30.DPD.or.worse.in.last.6.months._.woe +           
  No.of.times.90.DPD.or.worse.in.last.12.months._.woe +      
  No.of.times.30.DPD.or.worse.in.last.12.months._.woe +              
  Avgas.CC.Utilization.in.last.12.months._.woe +                     
  No.of.trades.opened.in.last.12.months._.woe +                        
  No.of.PL.trades.opened.in.last.6.months._.woe +                       
  No.of.PL.trades.opened.in.last.12.months._.woe +                     
  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.._.woe +
  Profession._.woe +                     
  Presence.of.open.home.loan._.woe     

glm_model2 <- glm(formula2, family = "binomial", data = data_rose)
summary(glm_model2)
pred <- predict(object = glm_model2, newdata = data_test, type = "response")
pred1 <- as.factor(if_else(pred>0.522,"Yes","No"))
cM <- confusionMatrix(pred1,actual)
cM

# The prediction at 0.522 gives a balanced view.  Accuracy = 0.626, 
# sensitivity = 0.626; accuracy = 0.627


#################################
############### RANDOM FOREST########
################################

# The random forest method shoudl allow us to produce a more useful tree as
# it will allow us to increase the weight of credit defaults in the data set

library(randomForest)
forest_model <- randomForest(formula2,data = data_rose, do.trace = T, mtry = 5, proximity = F, ntree = 100)
print(forest_model)


# The last step will be to check the accuracy on the test set
pred <- predict(object = forest_model,
                newdata = data_test,
                type = "class")
head(pred)

pred1 <- as.factor(ifelse(pred >= 0.486,"Yes","No"))
cF <- confusionMatrix(pred1,actual)
cF
# accuracy = 62.9%
# sensitivity = 62.2%
# specificity = 62.3%
