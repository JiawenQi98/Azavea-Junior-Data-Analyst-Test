##### Import the dataset ###
cars_mileage <- read.csv("Cars_mileage.csv", header = TRUE, stringsAsFactors = FALSE) # import tha dataset
head(cars_mileage) # look at the first 6 lines to have a taste
summary(cars_mileage) # have an overview

## a. Create a binary variable mpg_binary
cars_mileage$mpg_binary <- 0
cars_mileage$mpg_binary[cars_mileage$mpg > median(cars_mileage$mpg)] <- 1
cars_mileage$mpg_binary <- as.factor(cars_mileage$mpg_binary)

## b. Which of the other variables seem most likely to be useful in predicting whether a car's mpg is above or below its median? 
### Dig into each variable

##### cylinders #####
summary(cars_mileage$cylinders)
table(cars_mileage$cylinders)
cars_mileage$cylinders <- as.factor(cars_mileage$cylinders)

##### displacement #####
summary(cars_mileage$displacement)
table(cars_mileage$displacement)

##### horsepower #####
summary(cars_mileage$horsepower)
table(cars_mileage$horsepower)
##### Imputing missing value #####
cars_mileage$horsepower.new = cars_mileage$horsepower
cars_mileage$horsepower.new[cars_mileage$horsepower.new == '?'] <- NA
cars_mileage$horsepower.new <- as.integer(cars_mileage$horsepower.new)
library(Hmisc)
cars_mileage$imputed_horsepower <- as.integer(with(cars_mileage, impute(horsepower.new, mean)))
summary(cars_mileage$imputed_horsepower)
table(cars_mileage$imputed_horsepower)

##### weight #####
summary(cars_mileage$weight)

##### acceleration #####
summary(cars_mileage$acceleration)

##### year #####
summary(cars_mileage$year)
table(cars_mileage$year)
cars_mileage$age <- 117- cars_mileage$year

##### origin #####
summary(cars_mileage$origin)
table(cars_mileage$origin)
cars_mileage$origin <- as.factor(cars_mileage$origin)

##### name #####
summary(cars_mileage$name)
cars_mileage$brands <- gsub( " .*$", "", cars_mileage$name)
table(cars_mileage$brands)
cars_mileage$brands[cars_mileage$brands == 'capri'] <- 'mercury'
cars_mileage$brands[cars_mileage$brands == 'chevroelt'] <- 'chevrolet'
cars_mileage$brands[cars_mileage$brands == 'chevy'] <- 'chevrolet'
cars_mileage$brands[cars_mileage$brands == 'maxda'] <- 'mazda'
cars_mileage$brands[cars_mileage$brands == 'mercedes'] <- 'mercedes-benz'
cars_mileage$brands[cars_mileage$brands == 'toyouta'] <- 'toyota'
cars_mileage$brands[cars_mileage$brands == 'vokswagen'] <- 'volkswagen'
cars_mileage$brands[cars_mileage$brands == 'vw'] <- 'volkswagen'
table(cars_mileage$brands)
cars_mileage$brands <- as.factor(cars_mileage$brands)

### Correlation Matrix
dataset <- cars_mileage[,c(10, 2, 3, 5, 6, 8, 12:14)]
names(dataset)
colnames(dataset) <- c("mpg_binary", "cyl", "dis", "wei", "acc", "ori", "hor", "age", "bra")
library(corrplot)
dataset.num <- lapply(dataset, function(x) as.numeric(as.character(x)))## brands are changed into NA
dataset.num$bra <- as.numeric(dataset$bra)
dataset.num <- as.data.frame(dataset.num)
M <- cor(dataset.num)
corrplot.mixed(M)

## c. Split the data into a training set and a test set.
set.seed(66666) # my favorite seed
trainingIndex <- sample(nrow(dataset)*0.8)
testingIndex <- setdiff(seq(1, nrow(dataset)), trainingIndex)
training <- dataset[trainingIndex, -9]
testing <- dataset[testingIndex, -9]

## d. Perform two of the following in order to predict mpg_binary:

### Logistic Regression
LGmodel <- glm(mpg_binary ~.,family=binomial(link='logit'), data=training)
summary(LGmodel)
fitted.lg <- predict(LGmodel,newdata=testing, type='response')
fitted.lg <- ifelse(fitted.lg > 0.5, 1, 0)
confusion.matrix <- table(testing$mpg_binary, fitted.lg)
accuracy <- (confusion.matrix[1,1]+confusion.matrix[2,2])/sum(confusion.matrix)
error <- 1 - accuracy
recall <- confusion.matrix[1,1]/sum(confusion.matrix[1,])
precision <- confusion.matrix[1,1]/sum(confusion.matrix[,1])
f1 <- 2*confusion.matrix[1,1]/(2*confusion.matrix[1,1]+confusion.matrix[1,2]+confusion.matrix[2,1])
paste("accuracy:", accuracy,
      "error:", error,
      "precision:", precision,
      "recall:", recall,
      "f1score:", f1)

### Random Forest
library(randomForest)
library(reprtree)
set.seed(66666)
modelRF <- randomForest(mpg_binary ~ ., data=training, importance=TRUE, ntree = 2000)
print(modelRF) 
varImpPlot(modelRF)
reprtree:::plot.getTree(modelRF)
prediction <- predict(modelRF, testing)
confusion.matrix <- table(testing$mpg_binary, prediction)
accuracy <- (confusion.matrix[1,1]+confusion.matrix[2,2])/sum(confusion.matrix)
error <- 1 - accuracy
recall <- confusion.matrix[1,1]/sum(confusion.matrix[1,])
precision <- confusion.matrix[1,1]/sum(confusion.matrix[,1])
f1 <- 2*confusion.matrix[1,1]/(2*confusion.matrix[1,1]+confusion.matrix[1,2]+confusion.matrix[2,1])
paste("accuracy:", accuracy,
      "error:", error,
      "precision:", precision,
      "recall:", recall,
      "f1score:", f1)
