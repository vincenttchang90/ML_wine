#Reading command line arguments
args = commandArgs(trailingOnly=TRUE)

#Checking for a train and test arguments
if (length(args) < 2) {
  stop("Please specify a train set and a test set", call.=FALSE)
}

#Loading libraries
library(caret)
library(tibble)

#Reading arguments and loading data
train_data <- read.csv(args[1])
test_data <- read.csv(args[2])

#Function for data preparation 
data_prep <- function(data){
  #Feature engineering. Creating a sulfur ratio variable.
  wine <- add_column(data, sulfur.ratio = data$free.sulfur.dioxide/data$total.sulfur.dioxide, .before = 'quality')
  #Feature selection. Dropping redundant variables.
  wine$total.sulfur.dioxide <- wine$free.sulfur.dioxide <- wine$density <- NULL
  #Normalizing data using log1p transformation.
  log_data <- log1p(wine)
  log_data$quality <- wine$quality
  #Normalizing variable by subtracting mean and taking the absolute value.
  log_data$residual.sugar <- abs(log_data$residual.sugar - mean(log_data$residual.sugar))
  log_data
}

#Function for outlier detection and removal
remove_outliers <- function(data){
  #Standardizing to mean 0
  scaled_data <- data.frame(scale(data))
  scaled_data$quality <- data$quality
  outliers <- data.frame()
  #Looping over data for outliers
  for (i in 1:nrow(scaled_data)){
    for (x in scaled_data[i,1:length(scaled_data)-1]){
      if (x > 4 | x < -4){
        outliers <- rbind(outliers, i)
      }
    }
  }
  outliers <- data.frame(table(outliers))
  L <- outliers$Freq > 0
  indexs <- array(outliers$outliers[L])
  #Dropping rows with outliers
  no_outliers <- data[!rownames(data) %in% indexs, ]
  no_outliers
}

#Setting training parameters
train_control <- trainControl(method="cv", number=10)
#Setting model specific parameters
grid <- expand.grid(.nt=length(data_prep(train_data))-1, .alpha.pvals.expli=.05)
#Training
model <- train(quality~., data=data_prep(train_data), trControl=train_control, method="plsRglm", tuneGrid=grid, verbose=FALSE)
#Predicting
pred <- predict(model, newdata = data_prep(test_data))
#Return MAE
MAE(pred, data_prep(test_data)$quality)