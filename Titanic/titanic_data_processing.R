##########################################
# Kaggle.com - Titanic Survival Analysis
# Data Exploration & Transformation
# Sep 11 2017
# Xiaoyu Yang
##########################################

# Load data
train.orig <- read.csv("./github/titanic/data/train.csv", header = TRUE)
test.orig <- read.csv("./github/titanic/data/test.csv", header = TRUE)

# Packages
library(data.table)
library(ggplot2)
library(dplyr)

# Data Quality Assessment
# Check NA or blank values
col.names <- colnames(train.orig)[2:length(colnames(train.orig))]
missing.values <- vector()
for (i in 1:length(col.names)) {
  
  temp <- select_(train.orig, col.names[i])
  temp[which(is.na(temp)),] <- ''
  cnt <- sum(ifelse(temp == '', 1, 0))
  missing.values <- rbind(missing.values, cnt)
  
}

missing.values.result <- data.frame(as.data.frame(col.names), cnt = missing.values)
missing.values.result

# Embarked has 2 missing values that can be excluded
# Now focus on Age and Cabin
explore.cabin <- select(train.orig, Pclass, Fare, Cabin, Survived) %>%
  mutate(have_cabin = ifelse(Cabin == '', "0", "1"), Survived = as.factor(Survived))

table(explore.cabin$Pclass, explore.cabin$have_cabin)
table(explore.cabin$Survived, explore.cabin$have_cabin)
table(explore.cabin$Survived, explore.cabin$Pclass)
boxplot(explore.cabin$Fare ~ explore.cabin$have_cabin)
boxplot(explore.cabin$Fare ~ explore.cabin$Pclass)
