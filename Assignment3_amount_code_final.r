# Load the package
library(RODBC)
# install.packages('dataPreparation')
library('dataPreparation')
library(tidyverse)
library(dplyr)
library(nnet)
# install.packages("SDMTools")
#install.packages('caret')
library('caret')
#install.packages('xgboost')
library('xgboost')
# install.packages('caret')
library(caret)

# Connect to MySQL (my credentials are mysql_server_64/root/.)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_full")


query = "select * from  ass3_train_my_dataset_latest2"     # this is my training_set
data = sqlQuery(db, query)

query2 = 'select ass3_train_my_dataset_latest2.contact_id, avg(acts.amount) as avg_amount
  from ass3_train_my_dataset_latest2  ass3_train_my_dataset_latest2
  left 
  join acts   acts
    on ass3_train_my_dataset_latest2.contact_id = acts.contact_id
  and year(acts.act_date) = 2016
  group by ass3_train_my_dataset_latest2.contact_id
  order by avg(acts.amount)  ;  '
target = sqlQuery(db, query2)

query3 =  "select * from  ass3_test_my_dataset_latest2"
test_data = sqlQuery(db, query3)

query4 =  "select ass3_test_my_dataset_latest2.contact_id, avg(acts.amount) as avg_amount
  from ass3_test_my_dataset_latest2  ass3_test_my_dataset_latest2
  left 
  join acts   acts
    on ass3_test_my_dataset_latest2.contact_id = acts.contact_id
  where year(act_date) = 2017
  group by ass3_test_my_dataset_latest2.contact_id
  order by ass3_test_my_dataset_latest2.contact_id asc ;"

test_data_target = sqlQuery(db, query4)

# Close the connection
odbcClose(db)



remove_large_donations <- function(df, limit) {
  df <- subset(df, !df$avg_amount > limit)
  
  return(df)
}



merged_training <- merge(data, target, by="contact_id", all=TRUE) 
merged_test     <- merge(test_data, test_data_target, by="contact_id", all=TRUE) 

merged_test = remove_large_donations(merged_test, 100)

label_regr = merged_training$avg_amount


train = merged_training[ , -which(names(merged_training) %in% c("contact_id", "avg_amount"))]
test = merged_test[ , -which(names(merged_test) %in% c("contact_id", "avg_amount"))]


params2 <- list(booster = "gbtree", objective = "reg:squarederror", 
                eta=0.1, gamma=0, max_depth=7, min_child_weight=1, 
                subsample=1, colsample_bytree=1)




data_regression <- xgb.DMatrix(data = as.matrix(train),label = label_regr)
model_regr <- xgb.train (params = params2, data = data_regression, nrounds = 42,  
                         print_every_n = 5,maximize = T , eval_metric = "auc")

amounts2 = predict (model_regr,xgb.DMatrix(data = as.matrix(test)))



mad(merged_test$avg_amount, amounts2)   # 43.7367, 6.965647(filtered), 7.4

RMSE(merged_test$avg_amount, amounts2)  # 1 30.78035(filtered), 89,72


######################## Visualisation ################################
out = data.frame(contact_id = merged_test$contact_id)
out$true_values =  merged_test$avg_amount
out$predictions = amounts2


ggplot(out, aes(x=true_values, y=predictions)) + geom_point() +xlim(0,100) + ylim(0,100) +
  xlab('True value') + 
  ylab('Predictions') + 
  ggtitle('Prediction Results') + 
  geom_smooth(method = lm)



mat <- xgb.importance (feature_names = colnames(test),model = model_regr)
xgb.plot.importance (importance_matrix = mat[1:30]) 


ggplot(test_data_target, aes(x=avg_amount)) + geom_histogram(bins = 20) + xlim(0,100)