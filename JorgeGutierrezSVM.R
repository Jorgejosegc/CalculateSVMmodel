library(readxl)
library(e1071)
library(openxlsx)
library(kernlab)

#Jorge Gutierrez Projects

setwd("C:/Users/jorge/Desktop/Projects/CalculateSVMmodel")
JorgeProject <- read.xlsx("HW6data.xlsx")
JorgeProject


#QUESTION 1 -   All the parts are bellow

o <-JorgeProject$Class
o

class(sample(c("N"),100, replace = TRUE))
split_Data<-split(JorgeProject,JorgeProject$Class)
print(split_Data)
op <- as.numeric(as.character(JorgeProject$Class))
class(op)

Mydata <- data.frame(values1 = JorgeProject$x1,
                     values2 = JorgeProject$x2,
                     o = as.factor(o))

plot(Mydata[,-3],col=(3)/2, pch=19); abline(h=0,v=0,lty=3)

svm.model = svm(o ~ ., 
             data = Mydata, 
             kernel = "linear",
             cost = 10,                   
             type = 'C-classification', 
             scale = FALSE)   

summary(svm.model)

str(Mydata)

points(Mydata[svm.model$index, c(1,2)], col = "blue", cex=4)

p<- t(svm.model$coefs)   %*% svm.model$SV
w<- -svm.model$rho

abline(a= -w/p[1,2], b = -p[1,1]/p[1,2], col = "red", lty=3)

predicted_classes <- predict(svm.model, Mydata)

accuracy <- mean(predict(svm.model,Mydata)==o) * 100


#The accuracy is a 100% if we use the cost of 10. Otherwise, it will drop it and be 90%
#Moreover, the graph will be change if we remove the cost part in the SVM formula

print(accuracy)



#QUESTION 2    -     All the parts are bellow

setwd("C:/Users/jorge/Desktop/Projects/CalculateSVMmodel")
JorgeProject1 <- read.xlsx("HW6data1.xlsx")
JorgeProject1

features<- JorgeProject1[, c("x1","x2")]
class_label<- JorgeProject1$Class

predicted_labels <- predict(svm.model, data = features)

accuracy_test <- sum(predicted_labels == class_label) / length(class_label) * 100

accuracy_test

plot(features, col = ifelse(predicted_labels == "Y", "blue", "red"), pch = 16, xlab = "x1", ylab = "x2")
legend("topright", legend = c("Y", "N"), col = c("blue", "red"), pch = 16)

p1<- t(svm.model$coefs)   %*% svm.model$SV
w1<- -svm.model$rho

abline(a= -w/p[1,2], b = -p[1,1]/p[1,2], col = "black", lty=6)


#Question 3   -   All the parts are bellow
library(mlbench)
library(e1071)
library(naivebayes)

data("Ionosphere")

Ionosphere <- na.omit(Ionosphere)

set.seed(123)
train_indices <- sample(1:nrow(Ionosphere), nrow(Ionosphere) * 2/3)

#sample and traning data
train_data <- Ionosphere[train_indices, ]
test_data <- Ionosphere[-train_indices, ]


#Remove Columns 1 and 2
train_data <- train_data[, -c(1, 2)]
test_data <- test_data[, -c(1, 2)]

#Doing SVM Model
svm_model <- svm(Class ~ ., data = train_data)
svm_pred <- predict(svm_model, newdata = test_data)
svm_model
svm_pred

#Doing Naive Bayes
nb_model <- naiveBayes(Class ~ ., data = train_data)
nb_pred <- predict(nb_model, newdata = test_data)
nb_model
nb_pred

svm_accuracy <- (sum(svm_pred == test_data$Class) / nrow(test_data))*100
nb_accuracy <- (sum(nb_pred == test_data$Class) / nrow(test_data))*100
invisible(
            {
              message <- "\n\nThe accuracy working with SVM model is ------> %"
              formatted_message <- sprintf("\033[1m%s\033[0m", message) #For Bold Text
              cat(formatted_message,svm_accuracy )
              message1 <- "\n\nThe accuracy working with Naïve Bayes model is ------> %"
              formatted_message <- sprintf("\033[1m%s\033[0m", message1) #For Bold Text
              cat(formatted_message,nb_accuracy )
             }
          )

if (svm_accuracy > nb_accuracy) {
  result <- "\n\nSVM performs better accuraccy than Naïve Bayes on the dataset with accuracy of ----> %"
  formatted_message3 <- sprintf("\033[1m%s\033[0m", result) #For Bold Text
  a<- "\nThe diference of accurancy is---->"
  p <- svm_accuracy - nb_accuracy
  cat(formatted_message3,svm_accuracy)
  cat(a,p)

} else if (svm_accuracy < nb_accuracy) {
  result <- "\n\nNaïve Bayes performs better accuracy than SVM on the dataset with accuracy of -----> %"
  formatted_message3 <- sprintf("\033[1m%s\033[0m", result) #For Bold Text
  a<- "\nThe diference of accurancy is---->"
  p <- nb_accuracy - svm_accuracy
  cat(formatted_message3,nb_accuracy)
  cat(a,p)
  
} else {
  result <- "SVM and Naïve Bayes perform equal accuraccy"
}

