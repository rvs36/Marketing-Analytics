library(RODBC)

# Connect to you new data source
db = odbcConnect("mysql_server_64", uid="root", pwd="raghav123")

# Load the complete dataset
sqlQuery(db, "USE ma_charity_full")

# Extract data from database
query = "SELECT * from assignment2"

data_assignment2 = sqlQuery(db, query)

query = "SELECT contact_id, DATEDIFF(20180626, MAX(act_date)) / 365 AS 'days_last_donation', 
max(amount) as max_donation,
COUNT(amount) AS 'frequency',
avg(amount) as avg_donation
from acts
where act_type_id = 'DO'
group by contact_id;"

data_acts = sqlQuery(db, query)

query = "Select id as contact_id, zip_code/1000 as area_code, active
from contacts;"

data_contacts = sqlQuery(db, query)

# dropping the actions table

# query = "Select contact_id, DATEDIFF(20180626, MAX(action_date)) / 365 as 'days_last_contact'
# from actions
# group by contact_id;"
# 
# data_actions = sqlQuery(db, query)

# once we have the data it is a good practice to close the connection
# Close the connection
odbcClose(db)

# drop extra columns from all the tables and create new columns

library(tidyverse)


# keeping unique in conctacts, just in case.....

data_contacts = unique(data_contacts)


#merging all the tables to get the desired final train and test data.

tab1 = left_join(data_assignment2, data_acts, by= "contact_id")

tab2 = left_join(tab1, data_contacts, by= "contact_id")


#exporting data to Rdata

saveRDS(tab2, file="dF1.Rda")

#save as CSV

write.csv(tab2,'DF1.csv')
#____________________________PART 2____________________________________________

data = read.csv("df1.csv")

# Splitting into test and training data

library(tidyverse)

train = filter(data, calibration ==1)
test = filter(data, calibration ==0)

#logistic modelling to decide the amount

# we are only bothered about the people making donation, so we filter donation = 1

train_donor = filter(train, donation ==1)

train_donor = train_donor[complete.cases(train_donor), ] 

apply(train_donor, 2, function(x) any(is.na(x)))


# now we do regression

model1 = lm(formula = log(amount) ~  log(max_donation) + log(avg_donation), data = train_donor)

print(summary(model1))

pred1 <- predict(model1, newdata = test)

exp(pred1)

# so we have amount for all the test data points

test_clean = select(test, contact_id)
result = add_column(test_clean, exp(pred1))
result = rename(result, amount_pred = 'exp(pred1)')

apply(result, 2, function(x) any(is.na(x)))
# test_fil = test_clean[complete.cases(test_clean), ] 
result[is.na(result)] = 0

#--------------------CARET PACKAGE-----CROSS VALIDATION  -------------
# used for cross validation of regression type models
library(caret)

data_ctrl <- trainControl(method = "cv", number = 5)

# model1 = lm(formula = log(amount) ~ log(days_last_donation)+ log(max_donation) + log(min_donation) + 
#               log(avg_donation), data = train_donor)

# the above code becomes this:

model_caret <- train(log(amount) ~ log(days_last_donation)+ log(max_donation) + log(min_donation) + 
                       log(avg_donation), data = train_donor,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)           # pass missing data to model- will give Na back

model_caret
model_caret$finalModel
model_caret$resample                 # they should be homogenous
sd(model_caret$resample$Rsquared)     # this should be closer to zero

#----------------------STEP 2 - to predict if that person will donate or not--------

library(nnet)

#checking for class imbalance

table(train$donation)

# so there is a very high class imbalance

# we shall use downsampling to overcome it


'%ni%' <- Negate('%in%')         # define 'not in' func

train$donation = factor(train$donation)
up_train <- downSample(x = train[, colnames(train) %ni% "Donation"],
                         y = train$donation)

table(down_train$donation)


# clean the dataframe

tr = select(train, -c(amount, act_date))
tr = tr[complete.cases(tr), ]           

# Logit model
model2 = multinom(formula = donation ~ log(days_last_donation) + log(frequency) + days_last_donation*frequency, data = tr)


pred2 <- predict(model2, newdata = test, type = "probs")
predictd <- predict(model2, newdata = test)

r2 = add_column(result, pred2)

r2[is.na(r2)] = 0

r3  = r2 %>%
  mutate(Final_amount = amount_pred*pred2)
filter(r3, Final_amount >2)

apply(r3, 2, function(x) any(is.na(x)))


# exporting the solution 

out = r3 %>% filter(Final_amount >2) %>% mutate(result = 1) %>% select(contact_id, result)

file1 = test %>% select(contact_id) %>% mutate(result = 0)

export = left_join(file1, out, by = "contact_id")

export[is.na(export)] = 0

ans = export %>% select(contact_id, result.y)
  
write.table(ans, "Out_2.txt", sep="\t", col.names = F, row.names=FALSE)


#***********************
#**** VALIDATING ANSWERS *************
#**********************

#-------------------------CROSS VALIDATION  -------------

# Run a nfold cross-validation
nfold = 5
nobs  = nrow(tr)
index = rep(1:nfold, length.out = nobs)
probs = rep(0, nobs)
for (i in 1:nfold)
  {
  
  # Assign in-sample and out-of-sample observations
  insample  = which(index != i)
  outsample = which(index == i)
  formula = "donation ~ days_last_donation+ max_donation+ 
                    min_donation + avg_donation + area_code + active"
  # Run model on in-sample data only
  submodel = multinom(formula, tr[insample, ])
  
  # Obtain predicted probabilities on out-of-sample data
  probs[outsample] = predict(object = submodel, newdata = tr[outsample, ])
}

results <- ifelse(probs > 0.5,1,0)
answers <- tr[outsample, ]$donation

misClasificError <- mean(answers != results)
