#ALY 6015 Final Project
# Group Sigma

#install required packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("zoo")
install.packages("tidyverse")
install.packages("rstatix")
install.packages("caret")
install.packages("car")
install.packages("pROC") #plot ROC curve
install.packages("Metrics")
install.packages("glmnet")
library(dplyr)#help transform and present data
library(ggplot2)  # advanced plotting 
library(corrplot) # correlation matrix visualization
library(zoo)      # for aggregate function to fill na
library(tidyverse)#help transform and present data
library(RColorBrewer) #color palette
library(rstatix) #help with piping
library(caret) #data partition
library(car)
library(pROC) 
library(Metrics)
library(glmnet)
################### DATA CLEANING  ###########################################################################################################
# Read data set. When prompted navigate and open eastern_data.csv file
data <- read.table(file.choose(), 
                   sep = ",", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)
data <- data[1:(nrow(data) - 5), ] # last 5 records are Null records. dropping the Null records
#cleaning the data using dplyr
data_new <-  data %>%
  rename("Mortality_rate"= "Mortality.rate..under.5..per.1.000.live.births...SH.DYN.MORT.",
         "Population_0_14"= "Population.ages.0.14..total..SP.POP.0014.TO.",
         "Population_total" = "Population..total..SP.POP.TOTL.",
         "Rural_pop_pct" = "Rural.population....of.total.population...SP.RUR.TOTL.ZS.",
         "Urban_pop_pct" = "Urban.population....of.total.population...SP.URB.TOTL.IN.ZS.",
         "current_health_exp" = "Current.health.expenditure....of.GDP...SH.XPD.CHEX.GD.ZS.",
         "births_attended_by_skilled_professionals" = 'Births.attended.by.skilled.health.staff....of.total...SH.STA.BRTC.ZS.',
         "GDP_per_capita" = "GDP.per.capita..current.US....NY.GDP.PCAP.CD.",
         "Country_name" = "Country.Name") %>% #rename column names to shorter names easier to handle
  select(Mortality_rate,Population_0_14, Population_total, Rural_pop_pct, Urban_pop_pct, current_health_exp,  
         births_attended_by_skilled_professionals, GDP_per_capita, 
         Country_name,Time) %>% #we exclude columns we will not use such as country code, time code
  mutate(Mortality_rate = as.numeric(Mortality_rate),
         current_health_exp = as.numeric(current_health_exp),
         births_attended_by_skilled_professionals = as.numeric(births_attended_by_skilled_professionals),
         GDP_per_capita = as.numeric(GDP_per_capita))  %>% #adjust class to numeric if appropiate
mutate(child_pop_pct =Population_0_14/ Population_total, #create new column normalizing number of child population into percentages
       population_type = if_else(Urban_pop_pct > Rural_pop_pct, "Urban", "Rural"))  %>%#create new column classyfing population type based on rural and urban pop 
select(-c(Population_0_14,Population_total)) #drop non normalized columns

# getting the Null value count of each attribute
colSums(is.na(data_new))
########################## Mortality Rate 

#view number of na values per country for variable mortality rate
(na_mortality_country <- data_new %>%
  group_by(Country_name) %>%
  summarise(na_mortality = sum(is.na(Mortality_rate))))

#view mean value of mortality rate per country
(mortality_country <- data_new %>%
  group_by(Country_name) %>%
  summarise(mean_mortality=mean(Mortality_rate, na.rm=TRUE)))
########################## Health Expenditure 

#view number of na values per country for variable current_health_exp
(na_health_exp_country <- data_new %>%
  group_by(Country_name) %>%
  summarise(na_health_exp = sum(is.na(current_health_exp))))

#view mean value of current_health_exp per country
(health_exp_country <- data_new %>%
  group_by(Country_name) %>%
  summarise(mean_heal_exp= mean(current_health_exp, na.rm=TRUE))) 
##########Impute missing values based on means per country ##############
#impute missing values
data_complete <- data_new %>% 
  group_by(Country_name) %>%
  mutate(Mortality_rate=ifelse(is.na(Mortality_rate),mean(Mortality_rate,na.rm=TRUE),Mortality_rate), #mean mortality rate
         births_attended_by_skilled_professionals=ifelse(is.na(births_attended_by_skilled_professionals), #mean births_attended_by_skilled_professionals
                                                         mean(births_attended_by_skilled_professionals, 
                                                              na.rm=TRUE),
                                                         births_attended_by_skilled_professionals),
         current_health_exp=ifelse(is.na(current_health_exp),mean(current_health_exp,na.rm=TRUE), #mean current_health_exp
                                   current_health_exp),
         GDP_per_capita=ifelse(is.na(GDP_per_capita),mean(GDP_per_capita,na.rm=TRUE),GDP_per_capita)) #mean GDP_per_capita
# validate that there are no more null values
colSums(is.na(data_complete))
############################EDA ######################################################
############################ HIstograms and Boxplots 
#closer look into mortality rate, check outliers and summary statistics
par(mfcol=c(2,1),
    mai=c(1,1,0.2,0.4),
    mar=c(4,4.2,3,2))#fix margins to fit 2 plots on top of each other
#histogram of Mortality_ratein order to view distribution of data points
hist(data_complete$Mortality_rate,
     breaks=100,
     main="Plot 1: Histogram of Mortality Rate",
     xlab="%",
     #xlim= c(1300.0,17671.0),
     col = brewer.pal(9, "Set3"),
     las = 1) #data is skewed to the right (log transformation might have to be performed)
#boxplot of Mortality_rate in order to observe IQR and outliers
boxplot(data_complete$Mortality_rate,
        horizontal = T,
      #  ylim= c(1300.0,17671.0),
        col= "#FFCCFF",
        xlab= "%",
        main= "Plot 2: Box plot of Mortality Rate") #presence of outliers to the right of the plot

#closer look into urban GDP per capita, check outliers and summary statistics
par(mfcol=c(2,1),
    mai=c(1,1,0.2,0.4),
    mar=c(4,4.2,3,2))#fix margins to fit 2 plots on top of each other
#histogram in order to view distribution of data points
hist(data_complete$GDP_per_capita,
     breaks=100,
     main="Plot 3: Histogram of GDP per Capita",
     xlab="GDP per Capita",
     #xlim= c(1300.0,17671.0),
     col = brewer.pal(9, "Set3"),
     las = 1) 
#boxplot in order to observe IQR and outliers
boxplot(data_complete$GDP_per_capita,
        horizontal = T,
        #  ylim= c(1300.0,17671.0),
        col= "#FFCCFF",
        xlab= "GDP per Capita",
        main= "Plot 4: Box plot of GDP per capita") #Indicates presence of outliers towards the right side of the data
############################ Density Plots 
#density plot for current health expenditure
polygon(plot(density(data_complete$current_health_exp),
             main = "Plot 5: Distribution of Current Health Expenditure", col = 'red'))

#density plot for child population percentage
polygon(plot(density(data_complete$child_pop_pct),
             main = "Plot 6: Distribution of Child Population Percentage", col = 'red'))
dev.off()
################## BXPLOTS comparing population types
#create boxplots to compare GDP_per_capita for Urban/ Rural Population
ggplot(data_complete, aes(x=population_type, y=GDP_per_capita, color=population_type)) + 
  geom_boxplot() +
  labs(title="Plot 7: GDP per capita per Urban/ Rural Population",
       y = "GDP per Capita", x = "Population Type")+
  theme_classic()
#create boxplots to compare Mortality_rate for Urban/ Rural Population
ggplot(data_complete, aes(x=population_type, y=Mortality_rate, color=population_type)) + 
  geom_boxplot() +
  labs(title="Plot 8:Mortality Rate per Urban/ Rural Population",
       y =" Morality Rate", x = "Population Type")+
  theme_classic()

################# LINEAR REGRESSION ########################################
#Should we log transform the response variable?
#histogram of lot area in order to view distribution of data points
hist(log(data_complete$Mortality_rate),
     breaks=100,
     main="Plot 9: Histogram of Log(Mortality Rate)",
     xlab="Log(Mortality Rate)",
     #xlim= c(1300.0,17671.0),
     col = brewer.pal(9, "Set3"),
     las = 1) #data follows an approxiamtely normal distribution
#boxplot of lot area in order to observe IQR and outliers
# Create Train and Test set -  (70/30 split)
set.seed(100) # for repeatability of samples
#create indexing for 70% of the data 
trainIndex <- createDataPartition(data_complete$Mortality_rate, p = 0.7, list = FALSE, times = 1)
train <- data_complete[ trainIndex,] #70% of data
test <- data_complete[-trainIndex,] #30% of data
#stepwise regression:
#define intercept-only model
intercept_only <- lm(log(Mortality_rate) ~ 1, data=train)
#define model with all predictors
all <- lm(log(Mortality_rate) ~ . -Country_name -population_type, data=train)
#perform  stepwise regression
best_model <- step(intercept_only, direction='both', scope=formula(all))
summary(best_model) #view summary results of the regression
(exp(coef(best_model)) - 1) * 100 #exponent coefficients for better interpretation

#Use the "plot()" function to plot your regression model.
par(mfcol=c(2,2)) #fit 4 plots in one screen
plot(best_model)#the 4 graphs are used to analyze the linear regression assumptions. Please see report for details
dev.off() #remove plot

# multicollinearity 
vif(best_model) #all less than 5, no concern of multicollinearity

#determine performance of train set by calculating the root mean square error
preds.train.ols <- predict(best_model, new=train)
rmse(train$Mortality_rate,preds.train.ols)
#determine performance of test set by calculating the root mean square error
preds.test.ols <- predict(best_model, new=test)
rmse(test$Mortality_rate,preds.test.ols)

################# LASSO REGRESSION ########################################
# Convert Training data into matrix
train_X <- model.matrix(Mortality_rate~.-Country_name-population_type, train)[,-1] #exclude Mortality_rate from matrix and country names
# Convert Testing data into matrix
test_X <- model.matrix(Mortality_rate~.-Country_name-population_type,test)[,-1] #exclude Mortality_rate from matrix and country names
class(train_X) #check class is matrix and not dataframe
class(test_X) #check class is matrix and not dataframe
#assign Mortality_rate values to train and test
train_y <- train$Mortality_rate
test_y <- test$Mortality_rate

set.seed(100) # for repeatability of samples
# Run cross validation for different lambda values using K-Fold CV where K=10. 
cv.lasso <- cv.glmnet(train_X, train_y, nfolds = 10)
# Check the min and max lambda values
cv.lasso$lambda.min # MIN  lambda
cv.lasso$lambda.1se # MAX  lambda within 1 standard error from MIN
# Visualize cross-validation data.
plot(cv.lasso) # The vertical lines are the confidence interval for the error.
title("LASSO Regression Cross-Validation Results", line=3) #add title to plot
log(cv.lasso$lambda.min) # MIN log lambda
log(cv.lasso$lambda.1se) # MAX log lambda within 1 standard error from MIN
#fit final model on the training data using lambda.min
model.fit.lasso <- glmnet(train_X, train_y, alpha =1, lambda = cv.lasso$lambda.1se)
#display regresion coef
coef(model.fit.lasso)

#determine performance of train set by calculating the root mean square error
preds.train.lasso <- predict(model.fit.lasso, newx  = train_X)
(train.rmse <- rmse(train_y, preds.train.lasso))
#determine performance of test set by calculating the root mean square error
preds.test.lasso <- predict(model.fit.lasso, newx = test_X)
(test.rmse <- rmse(test_y, preds.test.lasso))

#compare test LASSO and OLS RMSE
rmse(test$Mortality_rate,preds.test.ols) #linear regression RMSE
(test.rmse <- rmse(test_y, preds.test.lasso)) #LASSO RMSE
################# LOGISTIC REGRESSION ########################################
#fit a logistic regression model
#stepwise regression:
#define intercept-only model
intercept_only <- glm(as.factor(population_type) ~ 1, 
                      data = train, 
                      family= binomial(link= "logit"))
#define model with all predictors
all <- glm(as.factor(population_type) ~ . -Country_name -Rural_pop_pct -Urban_pop_pct, 
           data = train, 
           family= binomial(link= "logit"))

#stepwise regression:
log_model <- step(intercept_only, direction='both', scope=formula(all)) # save the preferred model 
summary(log_model) #view summary of logistic regression
#view final model
exp(coef(log_model)) #display regression coef ( odds) to interpret coeffs

######## Confusion matrix- train 
#make predictions in train data
probabilities.train <- predict(log_model, newdata = train, type ="response")
predicted.min <- as.factor(ifelse(probabilities.train>=0.5, "Urban", "Rural")) 
#confusion matrix to measure model accuracy
confusionMatrix(predicted.min, as.factor(train$population_type), positive= "Urban")

######## Confusion matrix- test 
#make predictions in test data
probabilities.test <- predict(log_model, newdata = test, type ="response")
predicted.min <- as.factor(ifelse(probabilities.test>=0.5, "Urban", "Rural")) 
#confusion matrix to measure model accuracy
confusionMatrix(predicted.min, as.factor(test$population_type), positive= "Urban")

######## ROC Curve and AUC 
#plot the ROC curve
ROC <- roc(as.factor(test$population_type), probabilities.test)
plot(ROC, col="red", ylab = "Sensitivity - TP Rate",
     xlab = "Specificity - FP Rate",
     main= "ROC curve")
#calculate area under the ROC curve (AUC)
#(AUC <- auc(ROC))
ROC # observe the AUC value here as well
