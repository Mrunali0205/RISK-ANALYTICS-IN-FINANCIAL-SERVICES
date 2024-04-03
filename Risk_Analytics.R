# Load necessary libraries
library(readr)  
library(dplyr)     
library(ggplot2)
library(caret)
library(party)
library(e1071)
library(ipred)
library(pROC)
library(zoo)
library(forcats)


# Loading the datasets
#TRAINING SET
loan.train <-  read.csv("/Users/mrunalipatil/Downloads/loan_train1.csv")
head(loan.train)

col_types = cols(Credit_History = col_factor(levels = c("0", "1", "Missing")),
                 CNT_FAM_MEMBERS = col_factor(levels = c("0", "1", "2", "3+", "Missing")),
                 NAME_EDUCATION_TYPE = col_factor(levels = c("Graduate", "Not Graduate", "Missing")),
                 CODE_GENDER = col_factor(levels = c("Male", "Female", "Missing")),
                 Loan_Status = col_factor(levels = c("Y", "N")),
                 NAME_FAMILY_STATUS = col_factor(levels = c("Yes", "No", "Missing")),
                 LANDAREA_MEDI = col_factor(levels = c("Urban", "Semiurban", "Rural", "Missing")),
                 NAME_INCOME_TYPE = col_factor(levels = c("No", "Yes", "Missing")),
                 MONTHS_LAST_DUE = col_factor(levels = c('6', '12',  '36',  '60',  '84', '120', '180', '240', '300', '350', '360', '480', "Missing")))

str(loan.train)

#TEST SET
loan.test <-  read.csv("/Users/mrunalipatil/Downloads/loan_test1.csv")
col_types = cols(Credit_History = col_factor(levels = c("0", "1", "Missing")),
                 CNT_FAM_MEMBERS = col_factor(levels = c("0", "1", "2", "3+", "Missing")),
                 NAME_EDUCATION_TYPE = col_factor(levels = c("Graduate", "Not Graduate", "Missing")),
                 CODE_GENDER = col_factor(levels = c("Male", "Female", "Missing")),
                 NAME_FAMILY_STATUS = col_factor(levels = c("Yes", "No", "Missing")),
                 LANDAREA_MEDI = col_factor(levels = c("Urban", "Semiurban", "Rural", "Missing")),
                 NAME_INCOME_TYPE = col_factor(levels = c("No", "Yes", "Missing")),
                 MONTHS_LAST_DUE = col_factor(levels = c('6', '12',  '36',  '60',  '84', '120', '180', '240', '300', '350', '360', '480', "Missing")))
str(loan.test)

# Data cleaning and preparation steps
loan.train <- loan.train %>%
  mutate(
    CODE_GENDER = fct_explicit_na(CODE_GENDER, na_level = 'Missing'),
    NAME_FAMILY_STATUS = fct_explicit_na(NAME_FAMILY_STATUS, na_level = 'Missing'),
    CNT_FAM_MEMBERS = fct_explicit_na(CNT_FAM_MEMBERS, na_level = 'Missing'),
    NAME_EDUCATION_TYPE = fct_explicit_na(NAME_EDUCATION_TYPE, na_level = 'Missing'),
    NAME_INCOME_TYPE = fct_explicit_na(NAME_INCOME_TYPE, na_level = 'Missing'),
    LANDAREA_MEDI = fct_explicit_na(LANDAREA_MEDI, na_level = 'Missing'),
    AMT_CREDIT = na.aggregate(AMT_CREDIT) # Handling missing values in AMT_CREDIT
  )

loan.test <- loan.test %>%
  mutate(
    CODE_GENDER = fct_explicit_na(CODE_GENDER, na_level = 'Missing'),
    NAME_FAMILY_STATUS = fct_explicit_na(NAME_FAMILY_STATUS, na_level = 'Missing'),
    CNT_FAM_MEMBERS = fct_explicit_na(CNT_FAM_MEMBERS, na_level = 'Missing'),
    NAME_EDUCATION_TYPE = fct_explicit_na(NAME_EDUCATION_TYPE, na_level = 'Missing'),
    NAME_INCOME_TYPE = fct_explicit_na(NAME_INCOME_TYPE, na_level = 'Missing'),
    LANDAREA_MEDI = fct_explicit_na(LANDAREA_MEDI, na_level = 'Missing'),
   )

str(loan.train.train)
str(loan.train.test)

#Summary 
summary(loan.train)
summary(loan.test)

#EXPLORATORY DATA ANALYSIS
library(ggplot2)

Loan_Status.labs <- c("approved", "not approved")
names(Loan_Status.labs) <- c('Y', 'N')

#Defining the Custom ggplot Theme
design <- theme_minimal() +                   
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 8, angle = 45),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5))

#Gender Distribution by Loan Status
plot.CODE_GENDER <- ggplot(loan.train, aes(x = CODE_GENDER,fill = CODE_GENDER)) +
  geom_bar() +
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Gender', subtitle = 'Credit') +
  design
plot(plot.CODE_GENDER)

#Dependent Count by Loan Status
plot.CNT_FAM_MEMBERS <- ggplot(loan.train, aes(x = CNT_FAM_MEMBERS, fill = CNT_FAM_MEMBERS)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Dependents', subtitle = 'Credit') +
  design
plot(plot.CNT_FAM_MEMBERS)

#Educational Background by Loan Status
plot.NAME_EDUCATION_TYPE <- ggplot(loan.train, aes(x = NAME_EDUCATION_TYPE, fill = NAME_EDUCATION_TYPE)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Education', subtitle = 'Credit') +
  design
plot(plot.NAME_EDUCATION_TYPE)

#Marital Status by Loan Status
plot.NAME_FAMILY_STATUS <- ggplot(loan.train, aes(x = NAME_FAMILY_STATUS, fill = NAME_FAMILY_STATUS)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Married', subtitle = 'Credit') +
  design
plot(plot.NAME_FAMILY_STATUS)

#Property Area Type by Loan Status
plot.LANDAREA_MEDI <- ggplot(loan.train, aes(x = LANDAREA_MEDI, fill = LANDAREA_MEDI)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Property area', subtitle = 'Credit') +
  design
plot(plot.LANDAREA_MEDI)

#Employment Type by Loan Status
plot.NAME_INCOME_TYPE <- ggplot(loan.train, aes(x =NAME_INCOME_TYPE, fill = NAME_INCOME_TYPE)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Self employeed', subtitle = 'credit') +
  design
plot(plot.NAME_INCOME_TYPE)

#Credit History by Loan Status
plot.credit <- ggplot(loan.train, aes(x = Credit_History, fill = Credit_History)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', breaks = c(0, 1), labels = c('No', 'Yes'), name = '') +
  ggtitle('Credit history', subtitle = 'Credit') +
  design
plot(plot.credit)

#Applicant Income Distribution by Loan Status
plot.AMT_INCOME_TOTAL <- ggplot(loan.train, aes(x = AMT_INCOME_TOTAL, fill = Loan_Status)) +
  geom_histogram(bins=30) + 
  scale_fill_brewer(palette = 'Dark2', labels = c('Yes', 'No'), name = 'Credit') +
  theme_minimal() +
  ggtitle('Applicant income') +
  labs(x = '$') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))

plot(plot.AMT_INCOME_TOTAL)

#Loan Amount Term by Loan Status
plot.MONTHS_LAST_DUE <- ggplot(loan.train, aes(x = MONTHS_LAST_DUE, fill = Loan_Status)) +
  geom_bar() +
  scale_fill_brewer(palette = 'Dark2', labels = c('Yes', 'No'), name = 'Credit') +
  ggtitle('Loan amount term') +
  labs(x = 'Amount of months') +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))
plot(plot.MONTHS_LAST_DUE)

#Coapplicant Income Distribution by Loan Status
plot.AMT_ANNUITY <- ggplot(loan.train, aes(x = AMT_ANNUITY, fill = Loan_Status)) +
  geom_histogram(bins=30) + 
  scale_fill_brewer(palette = 'Dark2', labels = c('Yes', 'No'), name = 'Credit') +
  theme_minimal() +
  ggtitle('Coapplicant income') +
  labs(x = '$') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(angle = 45))
plot(plot.AMT_ANNUITY)

#Loan Amount Distribution by Loan Status
plot.AMT_CREDIT <- ggplot(loan.train, aes(x = AMT_CREDIT, fill = Loan_Status)) +
  geom_histogram(bins=30) + 
  scale_fill_brewer(palette = 'Dark2', labels = c('Yes', 'No'), name = 'Credit') +
  theme_minimal() +
  ggtitle('Amount of loan') +
  labs(x = 'K $') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))
plot(plot.AMT_CREDIT)

library(gridExtra)

#The Influence of Marital Status on Loan Approval"
plot.NAME_FAMILY_STATUS <- ggplot(loan.train, aes(x = NAME_FAMILY_STATUS, fill = NAME_FAMILY_STATUS)) +
  geom_bar() + 
  facet_grid(~Loan_Status, labeller = labeller(Loan_Status = Loan_Status.labs)) +
  scale_fill_brewer(palette = 'Dark2', name = '') +
  ggtitle('Married', subtitle = 'Credit') +
  design
plot(plot.NAME_FAMILY_STATUS )

#First Grid Arrangement: "Demographic and Family Insights"
grid.arrange(plot.CODE_GENDER, plot.NAME_FAMILY_STATUS, plot.NAME_EDUCATION_TYPE, plot.CNT_FAM_MEMBERS, ncol = 2)

#Second Grid Arrangement: "Financial Factors and Loan Terms"
grid.arrange(plot.AMT_INCOME_TOTAL, plot.AMT_ANNUITY, plot.AMT_CREDIT, plot.MONTHS_LAST_DUE, ncol = 2)

#Third Grid Arrangement: "Employment, Property, and Creditworthiness"
grid.arrange(plot.LANDAREA_MEDI, plot.NAME_INCOME_TYPE, plot.credit, ncol = 2)

#Data Splitting into Training and Testing Sets
library(caret)
library(ggplot2)
library(lattice)
set.seed(111)
divide = sample(2, nrow(loan.train), replace = TRUE, prob = c(0.75, 0.25))
loan.train.train <- loan.train[divide == 1, ] 
loan.train.test <- loan.train[divide == 2, ] 

head(loan.train.train)

#Converting Loan Status to Factor for Training Data
loan.train.train$Loan_Status <- as.factor(loan.train.train$Loan_Status)
head(loan.train.train$Loan_Status)

#Building and Plotting a Conditional Inference Tree
library(party)
tree <- ctree(Loan_Status ~ ., data = loan.train.train)
plot(tree)

#Evaluating the Tree Model on Training Data
confusionMatrix(predict(tree), loan.train.train$Loan_Status, positive = "Y")

#Evaluating the Tree Model on Testing Data
pred_tree = predict(tree, newdata = loan.train.test)
confusionMatrix(pred_tree, as.factor(loan.train.test$Loan_Status), positive = "Y")



# Ensure loan.train.test$Loan_Status is a factor with explicit levels
loan.train.test$Loan_Status <- factor(loan.train.test$Loan_Status, levels = c("Y", "N"))

test_score <- function(x, loop = FALSE) {
  pred_x <- predict(x, newdata = loan.train.test)
  # Set the factor levels of predictions to match the actual outcomes
  pred_x <- factor(pred_x, levels = levels(loan.train.test$Loan_Status))
  
  # Compute confusion matrix
  cf.2 <- confusionMatrix(pred_x, loan.train.test$Loan_Status, positive = "Y")
  
  score <- data.frame(
    acc = cf.2$overall['Accuracy'],
    sens = cf.2$byClass['Sensitivity'],
    spec = cf.2$byClass['Specificity']
  )
  
  # Add loop variables to the dataframe if loop is TRUE
  if(loop) {
    score$i <- i
    score$j <- j
  }
  
  return(score)
}

df <- data.frame(acc = NULL, sens = NULL, spec = NULL, i = NULL, j = NULL)  # Initialize empty dataframe

for (i in 1:20) {
  for (j in 1:20) {
    tree_w <- ctree(Loan_Status ~ ., data = loan.train.train, weights = ifelse(loan.train.train$Loan_Status == 'Y', i, j))
    df <- rbind(df, test_score(tree_w, loop = TRUE))
  }
}



df[df$acc == max(df$acc),]

#Evaluating Weighted Tree Model on Testing Data
tree_w <- ctree(Loan_Status ~ ., data = loan.train.train, weights = ifelse(loan.train.train$Loan_Status == 'Y', 3, 4))
pred_tree_w <- predict(tree_w, newdata = loan.train.test)
confusionMatrix(pred_tree_w, as.factor(loan.train.test$Loan_Status), positive = "Y")

#Training a Random Forest Model
forest <- cforest(Loan_Status ~ ., data = loan.train.train, control = cforest_unbiased(ntree = 200))
confusionMatrix(predict(forest), loan.train.train$Loan_Status)

#Random Forest Model Evaluation on Testing Data
pred_forest = predict(forest, newdata = loan.train.test)
confusionMatrix(pred_forest, as.factor(loan.train.test$Loan_Status), positive = "Y")

#Optimizing Random Forest Model with Weight Adjustments
df <- data.frame(acc = NULL,sens = NULL,spec = NULL, i = NULL,j = NULL)
for (i in 1:10) {
  for (j in 1:10) {
    forest_w <- cforest(Loan_Status ~., data = loan.train.train, control = cforest_unbiased(ntree = 200), weights = ifelse(loan.train.train$Loan_Status == 'Y', i, j))
    df <- rbind(df, test_score(forest_w, TRUE))
  }
}
head(df[df$acc == max(df$acc),])

#Bagging Model Training and Evaluation
bag <- bagging(Loan_Status ~ ., data = loan.train.train, nbagg = 2000)
pred_bag <- predict(bag, newdata = loan.train.test)
confusionMatrix(pred_bag, as.factor(loan.train.test$Loan_Status), positive = "Y")

#Bagging with Out-of-Box Error Estimation
bag2 <- bagging(Loan_Status ~ ., data = loan.train.train, coob = TRUE, nbagg = 2000) 
pred_bag2 <- predict(bag2, newdata = loan.train.test)
confusionMatrix(pred_bag2, as.factor(loan.train.test$Loan_Status), positive = "Y")

library(e1071)

test_score <- function(model, newdata, actual) {
  pred <- predict(model, newdata = newdata)
  
  # Ensure pred is a factor with levels matching the actual outcomes
  if (!all(levels(pred) %in% levels(actual))) {
    pred <- factor(pred, levels = levels(actual))
  }
  
  # Recast pred to have exactly the same levels as actual, in the same order
  pred <- factor(pred, levels = levels(actual))
  
  # Compute the confusion matrix
  cm <- confusionMatrix(pred, actual, positive = levels(actual)[2]) # Adjust the 'positive' parameter as needed
  
  score <- data.frame(
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity']
  )
  
  return(score)
}

# Defining SVM models
svm_radial <- svm(Loan_Status ~ ., data = loan.train.train, type = 'C-classification', kernel = "radial") 
svm_sigmoid <- svm(Loan_Status ~ ., data = loan.train.train, type = 'C-classification', kernel = "sigmoid")
svm_polynomial <- svm(Loan_Status ~ ., data = loan.train.train, type = 'C-classification', kernel = "polynomial")

# Evaluating SVM models using the test_score function
svm_radial_scores <- test_score(svm_radial, loan.train.test, loan.train.test$Loan_Status)
svm_sigmoid_scores <- test_score(svm_sigmoid, loan.train.test, loan.train.test$Loan_Status)
svm_polynomial_scores <- test_score(svm_polynomial, loan.train.test, loan.train.test$Loan_Status)

# Print the scores for each model
print(svm_radial_scores)
print(svm_sigmoid_scores)
print(svm_polynomial_scores)


#Support Vector Machine (SVM) Models Training
cc1 <- svm(Loan_Status ~ ., data = loan.train.train, type = 'C-classification', kernel = "radial") 
cc2 <- svm(Loan_Status ~ ., data = loan.train.train, type = 'C-classification', kernel = "sigmoid") 
cc3 <- svm(Loan_Status ~ ., data = loan.train.train, type = 'C-classification', kernel = "polynomial") 

test_score(cc1)
test_score(cc2)
test_score(cc3)

#Nu-Support Vector Classification
nuc1 <- svm(Loan_Status ~ ., data = loan.train.train, type = 'nu-classification', kernel = "radial") 
nuc2 <- svm(Loan_Status ~ ., data = loan.train.train, type = 'nu-classification', kernel = "sigmoid") 
nuc3 <- svm(Loan_Status ~ ., data = loan.train.train, type = 'nu-classification', kernel = "polynomial")
test_score(nuc1)
test_score(nuc2)
test_score(nuc3)

#Evaluating SVM Model on Testing Data
pred_svm <- predict(nuc1, newdata = loan.train.test)
confusionMatrix(pred_svm, as.factor(loan.train.test$Loan_Status), positive = "Y")

#Naive Bayes Classifier Training and Laplace Adjustment
bayes <- naiveBayes(Loan_Status ~ ., data = loan.train.train)
test_score(bayes)
df <- data.frame(acc = NULL,sens = NULL,spec = NULL, i = NULL,j = NULL)
for (i in 0:20) {
  bay <- naiveBayes(Loan_Status ~ ., data = loan.train.train, laplace = i)
  df <- rbind(df, test_score(bay, TRUE))
}

#Visualizing Naive Bayes Model Accuracy Across Laplace Adjustments
ggplot(df, aes(x = i, y = acc)) +
  geom_line(color = '#1B9E77') +
  theme_minimal() +
  scale_x_continuous(breaks = c(0:20)) +
  geom_vline(xintercept = df[df$acc == max(df$acc),]$i, lty = 1, color = "#D95F02")

#Final Model Evaluation and ROC Curve Comparison
library(pROC)
plot(roc(as.ordered(pred_tree_w), as.ordered(loan.train.test$Loan_Status)), col = "#1B9E77", main = 'ROC curve')
roc(as.ordered(pred_forest), as.ordered(loan.train.test$Loan_Status), plot = TRUE, add = TRUE, col = "#D95F02")
roc(as.ordered(pred_bag2), as.ordered(loan.train.test$Loan_Status), plot = TRUE, add = TRUE, col ="#7570B3")
roc(as.ordered(pred_svm), as.ordered(loan.train.test$Loan_Status), plot = TRUE, add = TRUE, col = "#E7298A")
roc(as.ordered(pred_bayes), as.ordered(loan.train.test$Loan_Status), plot = TRUE, add = TRUE, col = "#66A61E")
legend("bottomright", legend = c('Weighted tree classifier', 'Random forest', 'Bagging', 'SVM method', 'Naive Bayes'),
       col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"),
       lty = 1,
       lwd = 2,
       box.lty = 0)

#Comprehensive Model Evaluation Comparison
df <- rbind(test_score(tree_w),
            test_score(forest),
            test_score(bag2),
            test_score(nuc1),
            test_score(bayes))
df

#Generating Predictions for Loan Test Data
library(caret)
# Generate predictions for the test dataset using different models
pred_tree_w <- predict(tree_w, newdata = loan.test)
pred_forest <- predict(forest, newdata = loan.test)
pred_bag <- predict(bag2, newdata = loan.test)
pred_svm <- predict(nuc1, newdata = loan.test)
pred_bayes <- predict(bayes, newdata = loan.test)

#Preparing Data Frames for Model Predictions
# Create separate data frames for each model's predictions along with the Loan_ID
tree <- data.frame('Loan_ID' = loan.test[,1], 'Loan_Status' = pred_tree_w)
forest <- data.frame('Loan_ID' = loan.test[,1], 'Loan_Status' = pred_forest)
bag <- data.frame('Loan_ID' = loan.test[,1], 'Loan_Status' = pred_bag)
svm <- data.frame('Loan_ID' = loan.test[,1], 'Loan_Status' = pred_svm)
bayes <- data.frame('Loan_ID' = loan.test[,1], 'Loan_Status' = pred_bayes)

#Exporting Model Predictions to CSV Files
# Export each model's predictions to separate CSV files for further analysis or reporting
write.csv(tree, file = 'tree.csv', row.names = FALSE)
write.csv(forest, file = 'forest.csv', row.names = FALSE)
write.csv(bag, file = 'bagging.csv', row.names = FALSE)
write.csv(svm, file = 'svm.csv', row.names = FALSE)
write.csv(bayes, file = 'bayes.csv', row.names = FALSE)


#Summarizing Model Performance Results
# Summarize the performance (e.g., accuracy) of each model in a single data frame for easy comparison
results <- data.frame('Model' = c('Weighted Tree Classifier', 'Random Forest', 'Bagging', 'SVM Method', 'Naive Bayes'),
                      'Accuracy' = c(0.75, 0.78, 0.76, 0.77, 0.75))
results







































