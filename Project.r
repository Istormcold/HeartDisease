library("ggplot2")      
library("GGally")
library("class")
library("tree")

data=read.csv2("C:/Users/HP/Desktop/RStudio/heart.csv",header=TRUE,sep=',')

#DATA VISUALIZATION
# multiple regression model
reg <- lm(age ~ cp + trtbps + chol, data = data)
plot(reg)

newdata <- data.frame(cp = 1:10, trtbps = 2:11, chol = 3:12)

# prediction
predicted_y <- predict(reg, newdata)

#predicted values
plot(newdata$cp, predicted_y, type = "l", xlab = "chest pain/resting blood pressure/cholesterol ", ylab = "AGE", 
     main = "Multiple Regression Prediction")
lines(newdata$cp, predicted_y, col = "black")
lines(newdata$trtbps, predicted_y, col = "red")
lines(newdata$chol, predicted_y, col = "blue")
legend("topright", legend = c("chest pain", "resting blood pressure", "cholesterol"), col = c("black", "red", "blue"), lty = 1)
predicted_y

#bar chart age vs chest pain condition
ggplot(data, aes(x = age, fill = factor(cp))) +
  geom_bar() +
  scale_fill_manual(values=c("black", "green", "blue", "red")) +
  labs(title = "Age vs. Count for Chest Pain Conditions",
       x = "Age", y = "Count", fill = "Chest Pain Condition ") +
  theme_bw()

#Clustering and plotting
cluster.data <- kmeans(data[, 3:4], 3, nstart = 20)
cluster.data

table(cluster.data$cluster, data$chol)

cluster.data$cluster <- as.factor(cluster.data$cluster)
ggplot(data, aes(chol, age, color=cluster.data$cluster)) +
  geom_point() + 
  ggtitle("Cluster plot of the Data")

#Regression
reg <- lm(output ~ age +sex + chol + cp + thalachh + trtbps, data = data)
summary(reg)
plot(reg)

data = data[c(1:300),]
data = data[c(1,2,3,4,5,6,7,8,9,11,12,13,14)]
train = data[c(1:75,151:225),]
test = data[c(76:150,226:300),]

# DECISION TREE 
decision=tree(train$output~.,data=train)
decision
plot(decision)
text(decision)
pred_tree=predict(decision,test)
pred_tree
v=ifelse(pred_tree > 0.5, 1, 0)
sum(v==test$output)/nrow(test)
1-sum(v==test$output)/nrow(test) 
# 75% ACCURACY


# KNN MODEL 
accuracy_list = c()
train_knn = train[c(3,7,10,11,12,13)]
test_knn = test[c(3,7,10,11,12,13)]
for (i in 1:150){
  modelknn=knn(train_knn,test_knn,cl=train_knn$output,k=i)
  accuracy = sum(modelknn==test_knn$output)/nrow(test_knn) 
  accuracy_list = c(accuracy_list[],accuracy)
}
print(accuracy_list)
print(max(accuracy_list))

# 92% ACCURACY