# Load the package
library(RODBC)
library(BTYD)
library(ggplot2)


# Connect to MySQL (my credentials are mysql_server_64/root/root)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_small")

# Extract data from database
query = "SELECT a.contact_id,
                DATEDIFF(20180626, MIN(act_date)) / 365 AS 'T.cal',
                DATEDIFF(MAX(act_date), MIN(act_date)) / 365 AS 't.x',
                COUNT(amount) - 1 AS 'x'
                FROM acts a
                inner join(select acts.contact_id, count(*) as 'n' from acts group by contact_id) c
                on c.contact_id=a.contact_id
                WHERE act_type_id LIKE 'DO'
                GROUP BY 1 
                limit 20000;"
data = sqlQuery(db, query)
print(head(data))


# Estimate parameters for the Pareto-NBD process
params = bgnbd.EstimateParameters(data)
print(params)
params = bgnbd.EstimateParameters(data, params)
print(params)

# Estimate probabilities that customers are still alive
p.alives <- bgnbd.PAlive(params, data[,"x"], data[,"t.x"], data[,"T.cal"])
plot(p.alives)
plot(density(p.alives))
hist(p.alives, c='firebrick2')

ggplot(as.data.frame(p.alives), aes(x=p.alives)) + geom_histogram(bins=200, color="black", fill="firebrick2")
#print(p.alives)

# Keep only the transitioning customers at high weight
p.alives[p.alives<0.2]<-0.1
p.alives[p.alives>0.8]<-0.1
print(length(p.alives[p.alives>0.1]))
#print(p.alives)


# Estimate the frequency of donating
# Test for one period
freq <- bgnbd.ConditionalExpectedTransactions(params, T.star = 100 ,data[,"x"], data[,"t.x"], data[,"T.cal"])-bgnbd.ConditionalExpectedTransactions(params, T.star = 99 ,data[,"x"], data[,"t.x"], data[,"T.cal"])
plot(freq)
print(freq)

# For all periods, calculate the expected amount of time each person will donate:
frequencies<- vector(mode = "list")
frequencies[["contact_id"]]=data[,"contact_id"]
for (i in seq(1,10,1)){
frequencies[[toString(i)]] <- bgnbd.ConditionalExpectedTransactions(params, T.star = i ,data[,"x"], data[,"t.x"], data[,"T.cal"])-bgnbd.ConditionalExpectedTransactions(params, T.star = i-1 ,data[,"x"], data[,"t.x"], data[,"T.cal"])
}
plot(frequencies[[3]])
plot(frequencies[[10]])
plot(density(frequencies[[1]]))
frequencies[["probabilities_alive"]] <- p.alives
print(head(frequencies))
write.csv(frequencies, file = "C:/Users/nicol/Documents/Essec/Marketing analytics/Assignment 3/frequencies.csv")


# Add the expected amount each person will donate per donation
query = "SELECT a.contact_id,
                avg(amount) as avg
                FROM acts a
                inner join(select acts.contact_id, count(*) as 'n' from acts group by contact_id) c
                on c.contact_id=a.contact_id
                WHERE act_type_id LIKE 'DO' 
                GROUP BY 1
                limit 20000;"
data2= sqlQuery(db, query)
print(head(data2))

preds<-load(file="C:/Users/nicol/Documents/Essec/Marketing analytics/Assignment 3/predictions_for_Nicolas2.Rdata")
preds <- res
frequencies[["amount"]] <- preds[, "amounts2"]

# Discount factor r:
r=0.1
# Discount future cashflows:
frequencies[["discounted_amount"]] <- 0
for (i in seq(1,10,1)){
  frequencies[["discounted_amount"]] <- frequencies[["discounted_amount"]]+frequencies[["amount"]]*frequencies[[i+1]]/(1+r)^i
}

# Compute the score
frequencies[["score"]] <- frequencies[["discounted_amount"]]*frequencies[["probabilities_alive"]]
frequencies[["solicit"]]<- frequencies[["score"]]
frequencies[["solicit"]][frequencies[["solicit"]]<2]<- 0
frequencies[["solicit"]][frequencies[["solicit"]]>=2]<- 1


print(length(frequencies[["solicit"]][frequencies[["solicit"]]==1]))
write.csv(frequencies, file = "C:/Users/nicol/Documents/Essec/Marketing analytics/Assignment 3/results_test10.csv")


Worth=sum(frequencies[["solicit"]]*frequencies[["score"]])
print(Worth-)
# Close the connection
odbcClose(db)
