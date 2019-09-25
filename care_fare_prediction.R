# Remove variables
rm(list = ls())

train_df <- read.csv("train_cab.csv")
test_df <- read.csv("test.csv")

head(train_df)
str(train_df)

# Find and rplace NAs with median value
train_df[!complete.cases(train_df),]
test_df[!complete.cases(test_df),]

str(train_df[!complete.cases(train_df),])

str(train_df[is.na(train_df$passenger_count),])

med_pc <- median(train_df$passenger_count, na.rm = TRUE)
med_pc

train_df[is.na(train_df$passenger_count),"passenger_count"] <- med_pc


# Change data type of fare_Amount to numeric
train_df$fare_amount <- as.numeric(train_df$fare_amount)

train_df[train_df$fare_amount==430,]

# Removing rows with passenger_count < 1 and greater than 7: genreally cars have max seating capacity of 7
f <- (train_df$passenger_count < 1) | (train_df$passenger_count > 7)
f
train_df[f,]
train_df <- train_df[!f,]

train_df[train_df$passenger_count < 1,]
train_df[train_df$passenger_count > 7,]

summary(train_df)

# Outlier Analysis

boxplot(train_df$fare_amount)
boxplot(train_df$pickup_longitude)
boxplot(train_df$pickup_latitude)
boxplot(train_df$dropoff_longitude)
boxplot(train_df$dropoff_latitude)


# assign the outlier values into a vector

out_pickup_longitude <- boxplot(train_df$pickup_longitude, plot=FALSE)$out
out_pickup_latitude <- boxplot(train_df$pickup_latitude, plot=FALSE)$out
out_dropoff_longitude <- boxplot(train_df$dropoff_longitude, plot=FALSE)$out
out_dropoff_latitude <- boxplot(train_df$dropoff_latitude, plot=FALSE)$out

# Check the results
print(out_pickup_latitude)
print(out_pickup_longitude)
print(out_dropoff_latitude)
print(out_dropoff_longitude)

outlier_rows <- which(train_df$pickup_longitude %in% out_pickup_longitude) |
  which(train_df$pickup_latitude %in% out_pickup_latitude) |
  which(train_df$dropoff_longitude %in% out_dropoff_longitude) |
  which(train_df$dropoff_latitude %in% out_dropoff_latitude)
  
# Removing outliers

train_df <- train_df[-which(train_df$pickup_longitude %in% out_pickup_longitude),]
train_df <- train_df[-which(train_df$pickup_latitude %in% out_pickup_latitude),]
train_df <- train_df[-which(train_df$dropoff_longitude %in% out_dropoff_longitude),]
train_df <- train_df[-which(train_df$dropoff_latitude %in% out_dropoff_latitude),]

####################################################################################################################

# Preprocessing Data
library(pracma)
library(dbscan)

### function that splits datetime into categorical data
add_datetime <- function(train_df){
  # Converting pickup_datetime to POSIXlt time format
  train_df$pickup_datetime <- as.POSIXlt(train_df$pickup_datetime, format="%Y-%m-%d %H:%M:%S")
  
  # Adding  new columns to dataframe
  train_df$year <- train_df$pickup_datetime$year + 1900 # year since 1900
  train_df$month <- train_df$pickup_datetime$mon
  train_df$day <- train_df$pickup_datetime$mday
  train_df$dayOfWeek <- train_df$pickup_datetime$wday
  train_df$hour <- train_df$pickup_datetime$hour
  
  train_df
}

### function that calcuates distance between two locations
getDistance <- function(lat1,lon1,lat2,lon2){
  r <- 6373 # earth's radius
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- r*c
  
  distance
}
 
### function that calculates distance between pickup location and dropoff location
add_distance <- function(df){
  df['distance'] <- getDistance(df$pickup_latitude, df$pickup_longitude, 
                               df$dropoff_latitude, df$dropoff_longitude)
  df
}
  
### function that convert latitudes and longtitudes to radians format
convert_to_radians <- function(df){
  df['pickup_latitude'] <- deg2rad(df$pickup_latitude)
  df['pickup_longitude'] <- deg2rad(df$pickup_longitude)
  df['dropoff_latitude'] <- deg2rad(df$dropoff_latitude)
  df['dropoff_longitude'] <- deg2rad(df$dropoff_longitude)
  df
}

### function that get a clusterer, using DBScan technique
add_cluster <- function(df){
  Z <- df[c("pickup_latitude", "pickup_longitude")]
  Y <- df[c("dropoff_latitude", "dropoff_longitude")]
  colnames(Y) <- c("pickup_latitude", "pickup_longitude")
  
  EPS <- 0.0009
  clusters <- dbscan(Z, eps = EPS)
  df$pickup_area <- clusters$cluster
  df$dropoff_area <- predict(clusters, Y, data=Z)
 
  df
  
  # km <- kmeans(Z, centers = 10)
  # km$cluster
}

str(train_df)

# Do all pre-processing steps
preprocess_data <- function(df){
  df <- add_cluster(df)
  df <- add_datetime(df)
  df <- add_distance(df)
  df <- convert_to_radians(df)
  df
}
train_df <- preprocess_data(train_df)
test_df <- preprocess_data(test_df)

str(train_df)

# Drop pickup_datetime variable
train_df$pickup_datetime <- NULL
test_df$pickup_datetime <- NULL

str(train_df)
class(train_df)


#train_df$year <- as.factor(train_df$year)
#train_df$hour <- as.factor(train_df$hour)
#train_df$month <- as.factor(train_df$month)
#train_df$day <- as.factor(train_df$day)
#train_df$dayOfWeek <- as.factor(train_df$dayOfWeek)
#train_df$pickup_area <- as.factor(train_df$pickup_area)
#train_df$dropoff_area <- as.factor(train_df$dropoff_area)

#####################################################################################################################

# Modeling 
# Splitting test and training set
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(train_df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_set =subset(train_df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_set =subset(train_df, sample==FALSE)

head(training_set)


# Multiple Linear Regression
# Fitting Multiple Linear Regressor to training set
regressor <- lm(formula = fare_amount ~ .,
                data = training_set)
summary(regressor)

# Predicting the test set results
y_pred <- predict(regressor, newdata = test_set)


#----------------------------------------------------------------------------------
# Random Forest Regression
library(randomForest)

# Fitting the Regression Model to the training_set
regressor <- randomForest(x = training_set[2:14],
                          y = training_set$fare_amount,
                          ntree = 100)

summary(regressor)
# Predicting a new result
y_pred = predict(regressor, training_set)


#---------------------------------------------------------------------------------------
# XGBoost 
library(xgboost)

regressor = xgboost(data = as.matrix(training_set[2:14]),
                    label = training_set$fare_amount, nrounds = 2000)

# Predicting the Test set results
y_pred = predict(regressor, newdata = as.matrix(test_set[2:14]))

head(y_pred)

## Using XGBoost for submission
test_predict = predict(regressor, newdata = as.matrix(test_df))
head(test_predict)

test_key <- read.csv("test.csv")

submission_df <- data.frame(key= test_key$pickup_datetime, predicted_fare_amount=test_predict)

# Writing Submission file:
write.csv(submission_df, "submission_R.csv")


######################################################################################################################