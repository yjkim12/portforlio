# Preliminaries
bike = read.table("bikes14.txt", head = T)

# construct a variable named 'Profit'
profit = ifelse(bike$N_bikes >= 20000, 1, 0)
bike = cbind(profit, bike)
bike1 = bike[-2][-2]

# continuous variable impute the mean
# continuous : temperature, feels_like, humidity (43NAs), wind_speed
impute = function(a, a.impute){ 
  ifelse(is.na(a), a.impute, a)}
mean(bike1$humidity, na.rm = T)
humi.imp.mean = impute(bike1$humidity, mean(bike1$humidity, na.rm = T))
sum(is.na(humi.imp.mean))
bike1$humidity = humi.imp.mean

# categorical values missing
# categorical : N_bikes (73NAs), holiday (20NAs), weekend, season
bike1$holiday = as.factor(bike1$holiday)
bike1$weekend = as.factor(bike1$weekend)
bike1$season = as.factor(bike1$season)
bike2 = bike1[complete.cases(bike1), ]

# Variable selection (Best subset selection)
library(leaps)
subsetsel = regsubsets(profit ~ ., data = bike2)
subset.summary = summary(subsetsel)
names(subset.summary)
which.max(subset.summary$adjr2)
coef(subsetsel, which.max(subset.summary$adjr2))

holiday1 = ifelse(bike2$holiday == "1", 1, 0)
weekend1 = ifelse(bike2$weekend == "1", 1, 0)
season3 = ifelse(bike2$season == "3", 1, 0)
bike3 = data.frame(bike2$profit, bike2$temperature, bike2$feels_like, bike2$humidity, bike2$wind_speed, holiday1, weekend1, season3)
names(bike3) = c("profit", "temperature", "feels_like", "humidity", "wind_speed", "holiday1", "weekend1", "season3")

bike3$profit = as.factor(bike3$profit)
bike3$holiday1 = as.factor(bike3$holiday1)
bike3$weekend1 = as.factor(bike3$weekend1)
bike3$season3 = as.factor(bike3$season3)


# Main analysis to be implemented
set.seed(1234)
n = nrow(bike3)
index = sample(seq(1:n), size = round(0.7 * n))
train = bike3[index, ]
test = bike3[-index, ]

y.train = as.numeric(train$profit) - 1
y.test = as.numeric(test$profit) - 1

#Logistic Regresion to predict Profit
logistic = glm(profit ~ ., family = "binomial", data = train)
summary(logistic)

pi.logit.test = predict(logistic, newdata = test, type = "response")
y.logit.test = ifelse(pi.logit.test > 0.5, 1, 0)

table(test$profit, y.logit.test)
ER.logit.test = mean((y.test - y.logit.test)^2)
ER.logit.test

Pearson = sum(residuals(logistic, type = "pearson")^2)
pchisq(Pearson, 281, lower.tail = F)

# Classification to predict Profit
# KNN
normalize = function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

bike.norm = as.data.frame((lapply(bike3[2:5], normalize)))
bike.k = data.frame(bike3$profit, bike.norm, bike3$holiday1, bike3$weekend1, bike3$season3)
names(bike.k) = c("profit", "temperature", "feels_like", "humidity", "wind_speed", "holiday1", "weekend1", "season3")

library(class)
set.seed(1234)
n = nrow(bike.k)
index = sample(seq(1:n), size = round(0.7 * n))
train.k = bike.k[index, ]
test.k = bike.k[-index, ]

train.knn = train.k[, -1]
test.knn = test.k[, -1]

y.test.k = as.numeric(test.k$profit) - 1
  
calc.class.err = function(actual, predicted){
  mean(actual != predicted)
}

set.seed(1234)
k = 1:100
err.k = rep(x = 0, times = length(k))

for (i in seq_along(k)){
  pred = knn(train.knn, test.knn, train.k[, 1], k = k[i])
  err.k[i] = calc.class.err(y.test.k, pred)
}

plot(err.k, type = "b", col = "dodgerblue", cex = 1, pch = 20, main = "KNN Test Error Rate")
abline(h = min(err.k), col = "darkorange", lty = 3)
abline(h = mean(y.test.k == 1), col = "grey", lty = 2)

which(err.k == min(err.k))

profit.pi.knn.test = as.numeric(attr(knn(train.knn, test.knn, train.k[, 1], k = 5, prob = T), "prob"))
profit.class = knn(train.knn, test.knn, train.k[, 1], k = 5)
pi.knn.test = ifelse(profit.class == "1", profit.pi.knn.test, 1 - profit.pi.knn.test)
y.knn.test = ifelse(pi.knn.test > 0.5, 1, 0)

table(test.k$profit, y.knn.test)
ER.knn.test = mean((y.test.k - y.knn.test)^2)
ER.knn.test


# Construct a decision tree and implement at least one between random forest, bagging or boosting to predict profit
# tree
library(tree)
tree1 = tree(profit ~ ., data = train)
plot(tree1)
text(tree1, cex = 0.75, col = "blue")

pi.tree.test = predict(tree1, test, type = "class")
pi.tree.test1 = as.numeric(pi.tree.test) - 1
y.tree.test = ifelse(as.numeric(pi.tree.test1) > 0.5, 1, 0)

table(test$profit, y.tree.test)
ER.tree.test = mean((y.test - y.tree.test)^2)
ER.tree.test

cv.tree(tree1, FUN = prune.misclass)
prune = prune.misclass(tree1, best = 5)
plot(prune)
text(prune, cex = 0.75, col = "blue")

pi.prun.test =  predict(prune, test, type = "class")
pi.prun.test1 = as.numeric(pi.prun.test) - 1
y.prun.test = ifelse(as.numeric(pi.prun.test1) > 0.5, 1, 0)

table(test$profit, y.prun.test)
ER.prun.test = mean((y.test - y.prun.test)^2)
ER.prun.test

# Bagging, Random Forest and Boosting
# Bagging
library(randomForest)
bagging0 = randomForest(profit ~ ., data = bike3, mtry = 7, importance = T)
print(bagging0)

bagging1 = randomForest(profit ~ ., data = bike3, subset = index, mtry = 7, importance = T)
print(bagging1)

pi.bagg.test = predict(bagging1, test, type = "class")
pi.bagg.test1 = as.numeric(pi.bagg.test) - 1
y.bagg.test = ifelse(as.numeric(pi.bagg.test1) > 0.5, 1, 0)

table(test$profit, y.bagg.test)
ER.bagg.test = mean((y.test - y.bagg.test)^2)
ER.bagg.test

importance(bagging1)
varImpPlot(bagging1)

# Random Forest
forest1 = randomForest(profit ~ ., data = bike3, mrty = 3, subset = index, importance = T)
print(forest1)

pi.rand.test = predict(forest1, test, type = "class")
pi.rand.test1 = as.numeric(pi.rand.test) - 1
y.rand.test = ifelse(as.numeric(pi.rand.test1) > 0.5, 1, 0)

table(test$profit, y.rand.test)
ER.rand.test = mean((y.test - y.rand.test)^2)
ER.rand.test

importance(forest1)
varImpPlot(forest1)

#ROC and AUC
library(ROCR)

par(mfrow = c(2, 3))
# Logistic
pred.logit.test = prediction(pi.logit.test, y.test)
perf.logit.test = performance(pred.logit.test, "tpr", "fpr")
plot(perf.logit.test, colorize = T, main = "Logistic Test")
AUC.logit.test = performance(pred.logit.test, "auc")@y.values[[1]]
AUC.logit.test

# KNN
pred.knn.test = prediction(pi.knn.test, y.test)
perf.knn.test = performance(pred.knn.test, "tpr", "fpr")
plot(perf.knn.test, colorize = T, main = "KNN Test")
AUC.knn.test = performance(pred.knn.test, "auc")@y.values[[1]]
AUC.knn.test

# Tree
pred.tree.test = prediction(pi.tree.test1, y.test)
perf.tree.test = performance(pred.tree.test, "tpr", "fpr")
plot(perf.tree.test, colorize = T, main = "Tree Test")
AUC.tree.test = performance(pred.tree.test, "auc")@y.values[[1]]
AUC.tree.test

pred.prun.test = prediction(pi.prun.test1, y.test)
perf.prun.test = performance(pred.prun.test, "tpr", "fpr")
plot(perf.prun.test, colorize = T, main = "Prune Test")
AUC.prun.test = performance(pred.prun.test, "auc")@y.values[[1]]
AUC.prun.test

pred.bagg.test = prediction(pi.bagg.test1, y.test)
perf.bagg.test = performance(pred.bagg.test, "tpr", "fpr")
plot(perf.bagg.test, colorize = T, main = "Bagging Test")
AUC.bagg.test = performance(pred.bagg.test, "auc")@y.values[[1]]
AUC.bagg.test

#pred.rand.test = prediction(pi.rand.test1, y.test)
#perf.rand.test = performance(pred.rand.test, "tpr", "fpr")
#plot(perf.rand.test, colorize = T, main = "RandomForest Test")
#AUC.rand.test = performance(pred.rand.test, "auc")@y.values[[1]]
#AUC.rand.test

final = glm(profit ~ ., family = "binomial", data = bike3)
summary(final)

final2 = glm(profit ~ humidity + wind_speed + holiday1 + weekend1 + season3, family = "binomial", data = train)
summary(final2)

# Regularization (Lasso)
library(glmnet)
grid = 10^seq(10, -2, length = 100)
X = model.matrix(profit ~ ., data = train)[, -1]
Y = train$profit

lasso = glmnet(y = Y, x = X, alpha = 1, lambda = grid)
set.seed(1234)
cv.fit = cv.glmnet(y = Y, x = X, alpha = 1, nfolds = 10, lambda = grid)
opt.lambda = cv.fit$lambda.min
lasso2 = glmnet(y = Y, x = X, alpha = 1, lambda = opt.lambda)$beta
lasso2




