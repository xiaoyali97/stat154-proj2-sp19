#These functions split the entire data into folds and choose some folds to be training, validataion and test sets respectively.
#===
#inputs: 
# *data - the data set which needs to be split
# *k - number of folds

#===
#output: a list of lists including the index.
# *

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
