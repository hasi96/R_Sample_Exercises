install.packages('tidyverse')
library('tidyverse')
test <- read_csv("test.csv")
train <- read_csv("train.csv")
colSums(is.na(train))
colSums(is.na(test))
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Combine data sets
combined <- rbind(train, test.survived)
na.omit(combined) %>%
  group_by(Pclass, Sex) %>%
  summarise(Mean = mean(Age), Median = median(Age), SD = sd(Age), Total=n())
combined <- combined %>% mutate(Title = str_extract(Name, "[a-zA-Z]+\\."))
na.omit(combined) %>%
  group_by(Pclass, Sex, Title) %>%
  summarise(Mean = mean(Age), Median = median(Age), SD = sd(Age), Total=n()) %>%
  filter(Sex == 'male')
na.omit(combined) %>%
  group_by(Pclass, Sex, Title) %>%
  summarise(Mean = mean(Age), Median = median(Age), SD = sd(Age), Total=n()) %>%
  filter(Sex == 'female')
combined <- mutate(combined, Title2 =
                     ifelse((Sex=='male'), ifelse(Title != 'Master.', 'Mr', 'Master'),
                            ifelse(Title != 'Miss.', 'Mrs', 'Miss')))
missingAge <- combined %>%
  group_by(Title2) %>%
  summarize(meanAge = mean(na.omit(Age)))
View(missingAge)
combined <- combined %>%
  left_join(missingAge, by = c("Title2")) %>%
  mutate(Age = ifelse(is.na(Age), meanAge, Age)) %>%
  select(-meanAge)
table(combined$Embarked)
table(combined$Embarked)
#Now let's find out where these missing values are located in our dataset
which(is.na(combined$Embarked), arr.ind=TRUE)
#Replace those missing values with 'S' because it was the most used out of them all
combined$Embarked[c(62,830)] <- 'S'
na.omit(combined) %>% summarize(Median = median(Fare))
#Find where the missing value is located
which(is.na(combined$Fare), arr.ind=TRUE)
#The median turned out to be 57, so we'll use that value to replace it
combined$Fare[c(1044)] <- 57
colSums(is.na(combined))
plot(combined)
combined <- combined %>% select(-Cabin)
combined
na.omit(combined) %>%
  group_by(Pclass, Sex, Title) %>%
  summarise(Mean = mean(Age), Median = median(Age), SD = sd(Age), Total=n()) %>%
  filter(Sex == 'male')
plot(combined)
which(is.na(combined$Fare), arr.ind=TRUE)
which(is.na(combined), arr.ind=TRUE)
ggplot(combined[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram(bins=30) +
  xlab("Age") +
  ylab("Count") +
  facet_grid(.~Sex)+
  scale_fill_discrete(name = "Survived") +
  ggtitle("Age vs Sex vs Survived")

combined$Title2[combined$Title2 == "Mr"] <- 0
combined$Title2[combined$Title2 == "Miss"] <- 1
combined$Title2[combined$Title2 == "Master"] <- 2
combined$Title2[combined$Title2 == "Mrs"] <- 3
combined
combined$Sex[combined$Sex == "male"] <- 1
combined$Sex[combined$Sex == "female"] <- 0
combined
combined$Embarked[combined$Embarked == "S"] <- 0
combined$Embarked[combined$Embarked == "C"] <- 1
combined$Embarked[combined$Embarked == "Q"] <- 2
combined
combined$Sex  <- as.factor(combined$Sex)
combined$Embarked  <- as.factor(combined$Embarked)
combined$Title2  <- as.factor(combined$Title2)
combined$Pclass  <- as.factor(combined$Pclass)
train <- combined[1:891,]
test <- combined[892:1309,]
combined
factor(combined)
install.packages("randomForest")
library('randomForest')
set.seed(1234)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title2 + Age, data = train[1:600,])
varImpPlot(rf_model, main = "RF_MODEL")
prediction = predict(rf_model,train[601:891,],"response")
misClasificError <- mean(prediction != actual[601:891])
print(paste("RandomForest",misClasificError))


Survived.data = glm(formula = factor(Survived) ~  + Pclass + Title2+Sex+Fare+Embarked, data = input, family = binomial)
#TO CHECK DEVIATION - TO GET FEATURE ORDER & BETTER FEATURES
anova(Survived.data, test="Chisq")
Survived.data = glm(formula = factor(Survived) ~  +Title2+ Pclass +Fare+Sex, data = input, family = binomial)
print(summary(Survived.data))
prediction = predict(Survived.data,train[601:891,],"response")
prediction <- ifelse(prediction > 0.5,1,0)
misClasificError <- mean(prediction != actual[601:891])
print(paste("Logistic Regression",misClasificError))


install.packages("party")
output.tree <- party::ctree(factor(Survived) ~ Pclass + Sex + Fare  +Embarked + Title2, 
                            data = train[1:600,])
plot(output.tree)
actual <- train$Survived
prediction = predict(output.tree,train[601:891,],"response")
misClasificError <- mean(prediction != actual[601:891])
print(paste("Decision tree",misClasificError))
