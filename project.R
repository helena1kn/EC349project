##Data Import 
library(jsonlite)

#Clear
cat("\014")  
rm(list=ls())

#Set Directory as appropriate
setwd("C:/Users/helen/R/EC349 assignment")
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)

#Loading small datasets 
load(file='yelp_user_small.Rda')
load(file='yelp_review_small.Rda')

#Loading packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library("scales")
library(rpart)
library(glmnet) 
library(VGAM)
library(nnet)
library(caret)

# Default ggplot2 theme
theme_set(theme_minimal(base_size = 14))

##Data Exploration

head(review_data_small)
head(user_data_small)

View(user_data_small)
View(review_data_small)
View(business_data)
View(checkin_data)
View(tip_data)

summary(review_data_small)
summary(user_data_small)

#Replacing strings by numbers in friends column
user_data_small <- user_data_small %>% 
  mutate(friends = ifelse(friends == "None", 0, sapply(strsplit(friends, ", "), length))) 

#Plotting star distribution among reviews 
options(scipen=999)
ggplot(review_data_small, aes(stars)) +
  geom_bar(fill = "lightblue", colour ="black") +
  scale_y_continuous(labels = comma)

#Plotting stars distribution among businesses
options(scipen=999)
ggplot(business_data, aes(stars)) +
  geom_bar(fill = "salmon", colour ="black") +
  scale_y_continuous(labels = comma)

#How many businesses does the review data set include? 
df_uniq <- unique(review_data_small$business_id)
length(df_uniq)

##Modelling
#Merging data frames based on user_id
final_df <- merge(review_data_small, user_data_small, by = "user_id")

#Merging average business rating from business_data into final_df
final_df <- merge(final_df, business_data[, c("business_id", "stars")], by = "business_id")

#Dropping unnecessary columns
drop_col <- c(5,6,7,8,9,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,30)
final_df <- final_df[, -drop_col]

#stars.x = review rating, stars.y = business rating

#Convert rating variable into factor
final_df$stars.x <- as.factor(final_df$stars.x)

###Train-Test Split
set.seed(1) 
train <- sample(1:nrow(final_df), 3*nrow(final_df)/4) #split 3/4 and 1/4
final_df_train <-final_df[train,]
final_dfx_train <-final_df_train[,-1]
final_dfy_train <-final_df_train[,1]

#Test data is the one not in train
final_df_test<-final_df[-train,]
final_dfx_test<-final_df_test[,-1]
final_dfy_test <-final_df_test[,1]

model <- multinom(stars.x ~ average_stars + stars.y, data = final_df_train)
summary(model)

###Evaluation
final_df_predict<-predict(model, newdata = final_df_test[,-1])

actual_class <- final_df_test[, 4]
predicted_class <- final_df_predict

conf_matrix <- table(actual_class, predicted_class)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))
#Accuracy: 0.5459