library(plyr)
library(foreign)

train <- read.csv('./train.csv', stringsAsFactors = FALSE)
test <- read.csv('./test.csv', stringsAsFactors = FALSE)

# Create Survived column in Test
test$Survived <- 0

#Convert cateogrical Varaibles to factors
train$Survived <- factor(train$Survived)
train$Sex <- factor(train$Sex)
train$Pclass <- factor(train$Pclass)
test$Survived <- factor(test$Survived)
test$Sex <- factor(test$Sex)
test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)


# Fixing missing values
# combining the data sets for age/fate modeling 
full <- join(test, train, type = 'full')

# Create LM models for predicting missing values in Age and Fare
age.mod <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = full)

fare.mod <- lm(Fare ~ Pclass + Sex + SibSp + Parch + Age, data =full)


# Replacing missing values in Age and Fare with prediction
train$Age[is.na(train$Age)] <- predict(age.mod, train)[is.na(train$Age)]
test$Age[is.na(test$Age)] <- predict(age.mod, test)[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- predict(fare.mod, test)[is.na(test$Fare)]

# Replace missing values in Embarekd with most popular
train$Embarked[train$Embarked == ''] <-'S'
train$Embarked <- factor(train$Embarked)

###### Create 'Sex.name" variable
library(stringr)
train$sex.name <- 0
test$sex.name <- 0
train$sex.name[!is.na(str_extract(train$Name, "Mr"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Mrs"))] <- "Mrs"
train$sex.name[!is.na(str_extract(train$Name, "Mme"))] <- "Mrs"
train$sex.name[!is.na(str_extract(train$Name, "Miss"))] <- "Miss"
train$sex.name[!is.na(str_extract(train$Name, "Ms"))] <- "Miss"
train$sex.name[!is.na(str_extract(train$Name, "Mlle"))] <- "Miss"
train$sex.name[!is.na(str_extract(train$Name, "Capt"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Major"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Col"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Master"))] <- "Mast"
train$sex.name[!is.na(str_extract(train$Name, "Rev"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Dr"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Don"))] <- "Mr"
train$sex.name[!is.na(str_extract(train$Name, "Countess"))] <- "Mrs"
train$sex.name[!is.na(str_extract(train$Name, "Jonkheer"))] <- "Mr"

test$sex.name[!is.na(str_extract(test$Name, "Mr"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Mrs"))] <- "Mrs"
test$sex.name[!is.na(str_extract(test$Name, "Mme"))] <- "Mrs"
test$sex.name[!is.na(str_extract(test$Name, "Miss"))] <- "Miss"
test$sex.name[!is.na(str_extract(test$Name, "Ms"))] <- "Miss"
test$sex.name[!is.na(str_extract(test$Name, "Mlle"))] <- "Miss"
test$sex.name[!is.na(str_extract(test$Name, "Capt"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Major"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Col"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Master"))] <- "Mast"
test$sex.name[!is.na(str_extract(test$Name, "Rev"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Dr"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Don"))] <- "Mr"
test$sex.name[!is.na(str_extract(test$Name, "Countess"))] <- "Mrs"
test$sex.name[!is.na(str_extract(test$Name, "Jonkheer"))] <- "Mr"

test$Name[test$sex.name == 0]
train$Name[train$sex.name == 0]

train$sex.name <- factor(train$sex.name)
test$sex.name <- factor(test$sex.name)


# Create 'Fare Distance ' varaibel


# Find the mean fare for each Pclass
class1 <- subset(full, Pclass == 1)
class2 <- subset(full, Pclass == 2)
class3 <- subset(full, Pclass == 3)
fare1 <- mean(class1$Fare, na.rm = TRUE)
fare2 <- mean(class2$Fare, na.rm = TRUE)
fare3 <- mean(class3$Fare, na.rm = TRUE)

# Create fare_Avg column
train$fare_avg[train$Pclass == 1] <- fare1
train$fare_avg[train$Pclass == 2] <- fare2
train$fare_avg[train$Pclass == 3] <- fare3

test$fare_avg[test$Pclass == 1] <- fare1
test$fare_avg[test$Pclass == 2] <- fare2
test$fare_avg[test$Pclass == 3] <- fare3

# Create fare-distance metric for trian
train <- transform(train, fare.distance = Fare - fare_avg)
train <- train[, !names(train) %in% c("fare_avg")]



# Create fare-distance metric for Test
test <- transform(test, fare.distance = Fare - fare_avg)
test <- test[, !names(test) %in% c("fare_avg")]


### Add family column
train$family <- NA
test$family <- NA
train$family[which(train$SibSp != 0 | train$Parch != 0)] <- 1
train$family[which(train$SibSp == 0 & train$Parch == 0)] <- 0
test$family[which(test$SibSp != 0 | test$Parch != 0)] <- 1
test$family[which(test$SibSp == 0 & test$Parch == 0)] <- 0
test$family <- factor(test$family)
train$family <- factor(train$family)
test$familySize <- test$SibSp + test$Parch + 1
train$familySize <- train$SibSp + train$Parch + 1

### Scale the non factors

train$age_scale <- (train$Age-min(train$Age)) / (max(train$Age - min(train$Age)))
train$fare_scale <- (train$Fare - min(train$Fare)) / (max(train$Fare - min(train$Fare)))

test$age_scale <- (test$Age-min(test$Age)) / (max(test$Age - min(test$Age)))
test$fare_scale <- (test$Fare-min(test$Fare)) / (max(test$Fare - min(test$Fare)))

# Saving the new Data Sets

# save files as RData in order to preserve data structures
# open .RDdata with load()
save("test", file = "./Data/test_clean.RData")
save("train", file = "./Data/train_clean.RData")

# save the empty age data
save('test', file = './Data/test_clean_age.RData')
save('train', file = './Data/train_clean_age.RData')

# Save as ARFF for WEKA using foreign
write.arff(test, file = "./Data/test_clean.ARFF")
write.arff(train, file = "./Data/train_clean.ARFF")

# Also save .csv's just in case. These do not preserve data structures,
# so don't use them in the analysis! 
write.csv(test, "./Data/CSV/test_clean.csv", row.names = FALSE)
write.csv(train, "./Data/CSV/train_clean.csv", row.names = FALSE)
write.csv(full, './Data/CSV/full.csv', row.names = FALSE)