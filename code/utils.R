library(MASS)
library(e1071)

#These two functions split the entire data into folds and choose some folds 
#to be training, validataion and test sets respectively.
#===
#inputs: 
# *data - the data set which needs to be split
# *k - number of folds
#===
#output: a list of lists including the index
splitMethod1 <- function(data, k){
  data$label1 <- cut(data$x, k, labels = F)
  toReturn  <- list()
  for (i in 1:k) {
    toReturn[i] <- list(which(data$label1==i))
  }
  sample(toReturn)
}

splitMethod2 <- function(data, k){
  data$label2 <- cut(data$y, k, labels = F)
  toReturn  <- list()
  for (i in 1:k) {
    toReturn[i] <- list(which(data$label2==i))
  }
  sample(toReturn)
}

#loss function 
misclassification <- function(truthLabel, predLabel) {
  return(mean(truthLabel != predLabel))
}

#The following functions fits different model on the data 
#===================
#inputs:
# *data - the data set we fit model on
# *features - a vector contains list of feature names
# *response - the response variable name
#===================
#outpus:
# *model - the fitted model


#This function fits a logistic regression on the data
fitLogistic <- function(data, features, response) {
  currFormula <-  as.formula(
    paste(response, "~", paste(features, collapse = "+"), sep = ""))
  model <- glm(formula = currFormula, data = data, family = binomial)
  return(model)
}

#This function fits LDA on the data
fitLDA <- function(data, features, response) {
  currFormula <-  as.formula(
    paste(response, "~", paste(features, collapse = "+"), sep = ""))
  model <- lda(formula = currFormula, data = data)
  return(model)
}


#This function fits QDA on the data
fitQDA <- function(data, features, response) {
  currFormula <-  as.formula(
    paste(response, "~", paste(features, collapse = "+"), sep = ""))
  model <- qda(formula = currFormula, data = data)
  return(model)
}


#This function fits SVM on the data
fitSVM <- function(data, features, response) {
  currFormula <-  as.formula(
    paste(response, "~", paste(features, collapse = "+"), sep = ""))
  model <- svm(formula = currFormula, data = data, kernel = "linear")
  return(model)
}

#This function fits KNN on the data
#===
#inputs:
# *train - the training dataset
# *test - the test set
# *features - list of features to use
# *kNeighbors - the number of nerghbors 
#===
#output
# the fitted result
fitKNN <- function(train, test, features, kNeighbors) {
  model <- knn(train[,features], test[,features], train[,"label"], kNeighbors)
  return(model)
}


#This function calculates the CV-training_error of a classifier.
#===
#inputs: 
# *classifier - name for a generic classifier from ["logistic", "LDA", "QDA", "SVM"]
# *splitMethod - a split function to split the data into k folds
# *data - a dataframe contains all the data we need
# *features - colnames of all features we used to fit model
# *label - a colname for the label in the dataframe
# *k - number of folds 
# *loss - a loss function to calculate the error
#===
#output:
# *cv_loss - a vector of length k with the training loss in each k folds using
CVgeneric <- function(classifier, splitMethod, data, features, label, k, loss) {
  
  folds <- splitMethod(data, k)
  cv_loss <- c()
  for(i in 1:k){
    train <- data[-folds[[i]], ]
    val <- data[folds[[i]], ]
    
    if (classifier == "logistic") {
      model = fitLogistic(train, features, label)
      y_pred <- predict(model, val[,features], type = "response")
      y_pred <- round(y_pred)
      cv_loss <- c(cv_loss, loss(val[,label], y_pred))
      
    } else if (classifier == "LDA") {
      model = fitLDA(train, features, label)
      y_pred <- predict(model, val[,features], type = "response")
      y_pred <- y_pred$class
      cv_loss <- c(cv_loss, loss(val[,label], y_pred))
      
    } else if (classifier == "QDA") {
      model = fitQDA(train, features, label)
      y_pred <- predict(model, val[,features], type = "response")
      y_pred <- y_pred$class
      cv_loss <- c(cv_loss, loss(val[,label], y_pred))
      
    } else if (classifier == "SVM") {
      model = fitSVM(train, features, label)
      y_pred <- predict(model, val[,features], type = "response")
      cv_loss <- c(cv_loss, loss(val[,label], y_pred))
      
    } else if (classifier == "KNN") {
      kNeighbors <- seq(3,20)
      for (j in kNeighbors) {
        y_pred <- fitKNN(train, val, features, j)
        cv_loss <- c(cv_loss, loss(val[,label], y_pred))
      }
    }
  }
  
  if (classifier == "KNN") {
    cv_loss <- as.matrix(cv_loss, nrow = length(kNeighbors), ncol = k)
  }
  
  return(cv_loss)
}
