library(dplyr)
library(ggplot2)
library(caret)

show_number <- function(m, i, oriented=T)
{
  im <- matrix(mtrain[i,], byrow=T, nrow=28)
  
  if (oriented) {
    im_orient <- matrix(0, nrow=28, ncol=28)
    for (i in 1:28)
      im_orient[i,] <- rev(im[,i])
    
    im <- im_orient
  }
  image(im)
}

prediction_errors <- function(filename, t_out)
{
  d <- read.csv(filename, header=T)
  d$y <- factor(d$y, levels=c(0, 1))
  
  x <- cbind(d$x1, d$x2)
  colnames(x) <- c("x1", "x2")
  
  true_y <- d$y
  pred_y <- predict(t_out, x)
  
  n_samples <- nrow(x)
  error <- sum(true_y != pred_y)/n_samples # sum(log vector)=# of T's
  return (error)
}


# get the training datasets
if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header=F) %>% as.matrix
  train_classification <- mtrain[,1] #numbers to be made into images/y values
  mtrain <- mtrain[,-1]/256 # x matrix
  # y must be a factor for caret!!!
  
  colnames(mtrain) <- 1:(28)^2
  x <- mtrain[1:1000,]
  
  #colnames(mtrain) <- NULL
  rownames(mtrain) <- NULL
}

y <- rep(NA, length(train_classification))

# look at a sample
show_number(mtrain, 100)

# label whether 3 or not

for (i in 1:length(train_classification)) {
  cn <- train_classification[i]
  if (cn==3) {
   cn <- 1
  }
  else {
    cn <-0
  }
  y[i] <- cn
}

y <- factor(y, levels=c(0,1))
y <- y[1:1000]



if (!exists("mtrain2")) {
  mtrain2 <- read.csv("mnist_test.csv", header=F) %>% as.matrix
  train_classification2 <- mtrain2[,1] #Go through this vector, set equal to zero if not 3 and 1 if 3. Gives y values
  mtrain2 <- mtrain2[,-1]/256 #x values
  
  
  colnames(mtrain2) <- 1:(28^2)
  rownames(mtrain2) <- NULL
  
  x2 <- mtrain2[1:1000,]
}
y2 <- rep(NA, length(train_classification))

#Converting all threes to one and all other numbers to zero
for (i in 1:length(train_classification2)){
  cn <- train_classification2[i]
  
  if (cn==3){
    cn <- 1
  } else {
    cn <- 0
  }
  y2[i] <- cn
}

y2 <- factor(y, levels=c(0,1))
y2 <- y[1:1000]



######

tuning_df <- data.frame(size=8:12, decay=c(0,0.1,0.5,1,2))

# fitControl <- trainControl(method="none")
fitControl <- trainControl(## 2-fold CV
  method = "repeatedcv",
  number = 2,
  repeats = 2)

t_out <- caret::train(x=x, y=y, method="nnet",
                      trControl = fitControl,
                      tuneGrid=tuning_df, maxit=1000, MaxNWts=10000)


true_y <- y
pred_y <- predict(t_out, x)

n_samples <- nrow(x)
error <- sum(true_y != pred_y)/n_samples

pred_error <- error
cat("train1 prediction error", pred_error, "\n")


true_y2 <- y2
pred_y2 <- predict(t_out, x2)

n_samples2 <- nrow(x2)
error2 <- sum(true_y2 != pred_y2)/n_samples2

pred_error2 <- error2
cat("test prediction error", pred_error2, "\n")


# > cat("train1 prediction error", pred_error, "\n")
# train1 prediction error 0.002 


# > cat("test prediction error", pred_error2, "\n")
# test prediction error 0.158 



