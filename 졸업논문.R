data = read.csv("testdata.csv")

data$GFovGA = round(data$goal/data$allowed, 2)
attach(data)

# Logistic Regression
data$champ = ifelse(data$champ == "Y", 1, 0)

model1 = glm(champ ~ GFovGA + market_value + shots + passes, data = data, family = binomial(link = "logit"))
summary(model1)

# Newral Network Analysis
new = data.frame(year_id, champ, GFovGA, market_value, shots, passes)
new = na.omit(new)

set.seed(3800)
# Create Vector of Column Max and Min Value
maxs = apply(new[, 3:6], 2, max)
mins = apply(new[, 3:6], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.data = as.data.frame(scale(new[, 3:6], center = mins, scale = maxs - mins))

# Convert Champ column from Yes/No to 1/0
champ = as.numeric(new$champ) - 1
new2 = cbind(year_id, champ, scaled.data)
head(new2)

test = subset(new2, !(year_id < 2015 & year_id > 2005))
train = subset(new2, year_id < 2015 & year_id > 2005)

# Neural Network
library(neuralnet)
new2$advance = new2$champ == "Y"
new2$not_advance = new2$champ == "N"

d = neuralnet(advance + not_advance ~ GFovGA + market_value + shots + passes, new2, hidden = 3, stepmax = 1e6)
head(d$result.matrix)

plot(d)

#
library(nnet)
train$id_a = class.ind(train$champ)
test$id_b = class.ind(test$champ)

fitnn = nnet(id_a ~ GFovGA + market_value + shots + passes, train, size = 3, softmax = TRUE)
fitnn

summary(fitnn)

c = table(data.frame(predicted = predict(fitnn, test)[, 2] > 0.5, actual = test$id_b [, 2] > 0.5))
c

(c[1, 1] + c[2, 2]) / (c[1, 1] + c[1, 2] + c[2, 1] + c[2, 2]) 
