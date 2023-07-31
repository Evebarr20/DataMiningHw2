# Data preprocessing functions

# Function to handle missing values by replacing them with the rounded median
handle_NA <- function(data) {
  data[is.na(data)] <- round(median(data, na.rm = TRUE), digits = 0)
  data <- as.numeric(data)  # Convert data to numeric
  return(data)
}

# Function to remove the 'Ticket' column from the data
handle_ticket <- function(data) {
  data$Ticket <- NULL
  return(data)
}
# Function to remove the 'Cabin' column from the data
handle_Cabin <- function(data) {
  data$Cabin <- NULL
  return(data)
}

# Function to convert 'Sex' column to numeric values (0 for 'male' and 1 for 'female')
handle_sex <- function(data) {
  data$Sex <- ifelse(data$Sex == "male", 0, 1)
  return(data)
}

# Min-max normalization function to scale the data between 0 and 1
normalize <- function(data) {
  x <- min(data, na.rm = TRUE)
  y <- max(data, na.rm = TRUE)
  
  if (x == y) {
    normal_x <- numeric(length(data))
  } else {
    normal_x <- round((data - x) / (y - x), 1)
  }
  
  return(normal_x)
}

# Euclidean distance function to calculate the distance between two vectors
distance <- function(x, y) {
  if (length(x) == length(y)) {
    sqrt(sum((x - y)^2))
  } else {
    stop("Vectors not same length")
  }
}

# Function to find K nearest neighbors for a test instance in the training data
calculate_neighbor <- function(data1, data2, k) {
  d <- apply(data2[, c("Age", "Fare", "Sex", "Pclass", "SibSp", "Parch")], 1, function(row) distance(data1, unlist(row)))
  n <- data2[order(d)[1:k], ]
  return(n)
}

# Function to make predictions
make_prediction <- function(n) {
  gender <- ifelse(mean(n$Sex) >= 0.5, "female", "male")
  surviving <- as.numeric(mean(n$Survived) >= 0.5)
  return(data.frame(Sex = gender, Survived = surviving))
}
# Function to predict labels
knn_classifier <- function(data2, data1, k) {
  p <- list()
  
  for (i in 1:nrow(data1)) {
    test <- data1[i, c("Age", "Fare", "Sex","Pclass","SibSp", "Parch")]
    n <- calculate_neighbor(test, data2, k)
    predict <- make_prediction(n)
    data_frame <- data.frame(data1[i, ], predict)
    p[[i]] <- data_frame
  }
  
  make_prediction <- do.call(rbind, p)
  return(make_prediction)
}

# Read the training and test data from CSV files (provide the correct file paths)
train <- read.csv('/Users/evelynbarragan/documents/train.csv', stringsAsFactors = FALSE)
test <- read.csv('/Users/evelynbarragan/documents/test.csv', stringsAsFactors = FALSE)

# Data preprocessing for the training data
train$Age <- handle_NA(train$Age)
train$Embarked[train$Embarked == ""] <- "Unknown"
train <- handle_Cabin(train)
train <- handle_ticket(train)
train$Age <- normalize(train$Age)
train$Fare <- normalize(train$Fare)
train <- handle_sex(train)

# Data preprocessing for the test data
test$Age <- handle_NA(test$Age)
test <- handle_Cabin(test)
test <- handle_ticket(test)
test$Age <- normalize(test$Age)
test$Fare <- normalize(test$Fare)
test <- handle_sex(test)

k <- 3
prediction_result <- knn_classifier(train, test, k)

print(prediction_result)

result_data <- data.frame(prediction_result$PassengerId, Predicted_Survived = prediction_result$Survived, Actual_Survived = prediction_result$Survived.1)

# Add a column to check if the predicted label matches the actual label
result_data$matches <- ifelse(result_data$Predicted_Survived == result_data$Actual_Survived, TRUE, FALSE)


correct <- sum(result_data$matches == TRUE)
cover <- sum(result_data$matches == FALSE)
nrows <- nrow(result_data)


accuracy <- correct / nrows
# Print the accuracy percentage
print(paste0(accuracy * 100, "% accuracy"))

# Provide the file path where you want to save the CSV file
output_file <- "/Users/evelynbarragan/documents/output_file.csv"

# Export 'result_data' dataframe to a CSV file
write.csv(result_data, file = output_file, row.names = FALSE)

# Print a message indicating the successful export
cat("Results have been exported to", output_file, "\n")




