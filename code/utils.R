#This function create a m*n grid on the image.
#===
#inputs: 
# *data - the whole data frame
# *m - the dimension on y-axis
# *n - the dimension on x-axis
#=== 
#outputs:
# *data_with_grid - a dataframe with the grid_label column, which indicates the 
#                   grid that each data point is in 
createGrid <- function(data, m, n) {
  grids <- matrix(seq(1,m*n), nrow = m, byrow = T)
  xPos <- cut(data$x, n, labels = F)
  yPos <- cut(data$y, m, labels = F)
  data$grid_label <- mapply(function(x,y){grids[m+1-y,x]}, xPos, yPos)
  return(data)
}


#These two functions choose around 10% test data, 10% validation data and 
#80% training data. 
#===
#inputs: 
# *data - a dataframe with the grid label
#===
#output: a list that contains 3 lists in the order of [train, val, test]
splitMethod1 <- function(data){
  uniqueLabel <- unique(data$grid_label)
  numTest <- ceiling(length(uniqueLabel) * 0.1)
  uniqueLabel <- sample(uniqueLabel)
  
  test <- c()
  val <- c()
  train <- c()
  for (i in uniqueLabel[1:numTest]) {
    test <- c(test, which(data$grid_label == i))
  }
  for (i in uniqueLabel[(numTest+1):numTest*2]) {
    val <- c(val, which(data$grid_label == i))
  }
  for (i in uniqueLabel[(numTest*2 + 1):length(uniqueLabel)]) {
    train <- c(train, which(data$grid_label == i))
  }
  return(list(train, val, test))
}

splitMethod2 <- function(data){
  test <- c()
  val <- c()
  train <- c()
  
  for (i in unique(data$grid_label)) {
    currFold <- which(data$grid_label == i)
    currFold <- sample(currFold)
    numTest <- ceiling(length(currFold)*0.1)
    
    test <- c(test, currFold[1:numTest])
    val <- c(val, currFold[(numTest+1):numTest*2])
    train <- c(train, currFold[(numTest*2+1):length(currFold)])
  }
  return(list(train, val, test))
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
  train_features <- scale(train[,features])
  test_features <- scale(test[,features])
  model <- knn(train_features, test_features, train[,"label"], kNeighbors)
  return(model)
}

#This function create k folds
#===
#inputs:
# *data - a dataframe with the grid_label column
# *k - number of cv folds
#=== 
#output: a list of k elements, each contains the index in that fold
createCVFolds <- function(data, k) {
  uniqueLabel <- unique(data$grid_label) 
  uniqueLabel <- sample(uniqueLabel)
  numInCVFold <- unname(table(cut(1:length(uniqueLabel), k)))
  prev <- 1
  
  toReturn <- list() 
  for (i in 1:k) {
    toReturn[i] <- list(which(data$grid_label %in% 
                                uniqueLabel[prev:(prev+numInCVFold[i]-1)]))
    prev <- prev + numInCVFold[i]
  }
  return(toReturn)
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
CVgeneric <- function(classifier, data, features, label, k, loss) {
  
  folds <- createCVFolds(data, k)
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
      kNeighbors <- seq(3,20,2)
      for (j in kNeighbors) {
        y_pred <- fitKNN(train, val, features, j)
        cv_loss <- c(cv_loss, loss(val[,label], y_pred))
      }
    }
  }
  
  if (classifier == "KNN") {
    cv_loss <- matrix(cv_loss, nrow = length(kNeighbors), ncol = k)
  }
  
  return(cv_loss)
}
