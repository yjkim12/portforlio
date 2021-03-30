data = read.csv("data.csv")
data$Gender = ifelse(data$gender == "F", 1, 0)
data$Group = ifelse(data$group == "Control", 0, 1)

attach(data)
qqnorm(pre.IQ)
qqline(pre.IQ, col = "red")

# Demographic Table
library(plyr)
table(group)
table(gender, group)
table(island, group)

ddply(data, c("group"), summarise, N = length(pre.IQ), mean = round(mean(pre.IQ), 2), sd = round(sd(pre.IQ), 2), se = round(sd / sqrt(N), 2))

# Data analysis
data = data[-c(56), ] # Remove not usual islander
data = na.omit(data) # Remove withdrew or died islanders

data$post.IQ = as.numeric(data$post.IQ)
data$diff = data$post.IQ - data$pre.IQ

attach(data)


boxplot(diff ~ Group, data = data, names = c("Control","Piano"), ylab="Difference", main = "Sitting vs Piano", col = c("red", "blue"))


# Check Normality
qqnorm(diff)
qqline(diff, col = "red")

data = data[-c(7, 62), ] # remove outliers (difference is -20, -15)

attach(data)
# Check Normality again
qqnorm(diff)
qqline(diff, col = "red")

#New Demographic Table
table(group)
table(gender, group)
table(island, group)

ddply(data, c("group"), summarise, N = length(pre.IQ), mean = round(mean(pre.IQ), 2), sd = round(sd(pre.IQ), 2), se = round(sd / sqrt(N), 2))
ddply(data, c("group"), summarise, N = length(post.IQ), mean = round(mean(post.IQ), 2), sd = round(sd(post.IQ), 2), se = round(sd / sqrt(N), 2))
ddply(data, c("group"), summarise, N = length(diff), mean = round(mean(diff), 2), sd = round(sd(diff), 2), se = round(sd / sqrt(N), 2))


boxplot(diff ~ Group, data = data, names = c("Control","Piano"), ylab="Difference", main = "Sitting vs Piano", col = c("red", "blue"))


# T-test
new = data.frame(id, island, village, Gender, Group, pre.IQ, post.IQ, diff)

t.test(diff ~ Group, data = new, alternative = "two.sided", conf.level = 0.95, var.equal = T) 


boxplot(diff ~ Group + Gender, data = data, names = c("Sitting Male", "Piano Male", "Sitting Female", "Piano Female"), main = "Gender and IQ", ylab = "Difference", col = c("red", "blue"))
boxplot(diff ~ Group + island, data = data, names = c("Sitting #1", "Piano #1", "Sitting #2", "Piano #2", "Sitting #3", "Piano #3"), main = "Island and IQ", ylab = "Difference", col = c("red", "blue"))

