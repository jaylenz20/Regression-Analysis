#Importing necessary libraries
library(caTools)
library(ggplot2)

# Importing the dataset
dataset <- read.csv('../input/salary/Salary.csv')
dim(dataset)

# Split the dataset into the Training set and Test set
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

print(split)

cat(' Dimension of traing data:', dim(training_set), "\n",'Dimension of testing data:', dim(test_set))

# Fitting Simple Linear Regression to the Training set
regressor <- lm(formula <- Salary ~ YearsExperience, data <- training_set)

#Interpreting the model
summary(regressor)

# Predicting the Test set results
y_pred <- predict(regressor, test_set)

#Training set results
ggplot() + 
geom_point(aes(x <- training_set$YearsExperience, y <- training_set$Salary), colour = 'red') +
geom_line(aes(x <- training_set$YearsExperience, y <- predict(regressor, training_set)), colour = 'blue') +
ggtitle('Salary vs Experience (Training set)') +
xlab('Years of experience') +
ylab('Salary')

# Test set results
ggplot() +
geom_point(aes(x <- test_set$YearsExperience, y <- test_set$Salary), colour = 'red') +
geom_line(aes(x <- training_set$YearsExperience, y <- predict(regressor, training_set)), colour = 'blue') +
ggtitle('Salary vs Experience (Test set)') +
xlab('Years of experience') +
ylab('Salary')

# Evaluation
original <- test_set$Salary
predicted <- y_pred
d <- original - predicted
MAE <- mean(abs(d))
MSE <- mean((d)^2)
RMSE <- sqrt(MSE)
R2 <- 1 - (sum((d)^2) / sum((original - mean(original))^2))

cat(" Mean Absolute Error:", MAE, "\n", "Mean Square Error:", MSE, "\n", 
    "Root Mean Square Error:", RMSE, "\n", "R-squared:", R2)
