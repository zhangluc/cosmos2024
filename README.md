# COSMOS Project on Juvenile Myopia

**Lucia Zhang, Giang Tran, Luke Owyang, April Pan**

## Data Introduction

## Data Exploration via Correlation Matrix

``` r
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load & Clean data
data <- read.csv('myopia_og.csv', sep=";", header=TRUE)

#data$MYOPIC <- as.numeric(data$MYOPIC)
xtabs(~data$AGE)

# Excluding 6 subjects at 9 year old
data <- subset(data, AGE != 9)

# Select only numeric columns for correlation analysis
numeric_data <- data[sapply(data, is.numeric)]

# Check for any possible missing values
anyNA(numeric_data)

# Calculate the correlation matrix
#correlation_matrix <- cor(numeric_data, use = "complete.obs")
correlation_matrix <- cor(numeric_data)

# Print the correlation matrix
print(correlation_matrix)

# Melt the correlation matrix to a long format for ggplot2
correlation_melted <- melt(correlation_matrix)

# Plot the heatmap
ggplot(data = correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Matrix Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

which(abs(correlation_matrix-diag(1,18,18))>0.90)
cor(numeric_data$VCD,numeric_data$AL)
```

**Conclusion:**

-   There are only 6 subjects at 9 year old, all others are younger than 9 with ages ranging from 5 to 8;
-   No missing values and all variables coded in numeric values;
-   There is only one pair of variables, AL and VCD, which are correlated over with scale of correlation coefficient over 0.9. In fact their correlation coefficient is 0.94.

## Hypothesis Tests on Association of Hour-related Variables to Myiopia

``` r
#install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)

#setwd('data')
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
#myopia
head(myopia)
summary(myopia)

hours <- select(myopia, contains("hr"), contains("MYOPIC"))

hours <- hours %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

# Summary statistics
summary_stats <- hours %>%
  group_by(MYOPIC) %>%
  summarise(across(c(SPORTHR, TVHR, COMPHR, READHR, STUDYHR, DIOPTERHR), list(mean = mean, sd = sd, median = median, IQR = IQR), .names = "{col}_{fn}"))

print(t(summary_stats))


# Box plots: ALL 6 HOURS
pivot_longer(hours,
  contains('HR'),
  names_to = "category",
  values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

# Too many outliers so consider log-transformation: log(hours+1)
loghours <- log(hours+1)
loghours$MYOPIC <- hours$MYOPIC
pivot_longer(loghours,
  contains('HR'),
  names_to = "category",
  values_to = "loghours"
) %>%
  ggplot(aes(x = category, y = loghours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

# Observe difference in READHR & SPORTHR between healthy vs. myopic subjects

# t-test on log(hours+1):
t_test_logres <- list(
  SPORTHR = t.test(SPORTHR ~ MYOPIC, data = loghours), # p-value = 0.004127 -- Significant
  TVHR = t.test(TVHR ~ MYOPIC, data = loghours), # p-value = 0.8125
  COMPHR = t.test(COMPHR ~ MYOPIC, data = loghours), # p-value = 0.9077
  READHR = t.test(READHR ~ MYOPIC, data = loghours), # p-value = 0.09062 -- <0.1
  STUDYHR = t.test(STUDYHR ~ MYOPIC, data = loghours), # p-value = 0.7465
  DIOPTERHR = t.test(DIOPTERHR ~ MYOPIC, data = loghours) # p-value = 0.5428
)

t_test_logres

# Mann-Whitney U Test (Wilcoxon Rank-Sum Test) directly on hours:
wilcox_test_results <- list(
  SPORTHR = wilcox.test(SPORTHR ~ MYOPIC, data = hours), # p-val = 0.004804 -- Significant
  TVHR = wilcox.test(TVHR ~ MYOPIC, data = hours), #p-val = 0.8204
  COMPHR = wilcox.test(COMPHR ~ MYOPIC, data = hours), #p-val = 0.8927
  READHR = wilcox.test(READHR ~ MYOPIC, data = hours), #p-val = 0.102 -- Around 0.1
  STUDYHR = wilcox.test(STUDYHR ~ MYOPIC, data = hours), #p-val = 0.9676
  DIOPTERHR = wilcox.test(DIOPTERHR ~ MYOPIC, data = hours) #p-val = 0.3169
)

wilcox_test_results


# Explore READHR+SPORTHR on myopia

ggplot(hours,
       aes(x = READHR,
           y = SPORTHR,
           color = MYOPIC)) +
  geom_point()

#logistic regression: sports vs read hours (both seem differ but differently)
sportread.model <- glm(MYOPIC ~ SPORTHR+READHR, data = hours, family = binomial)
summary(sportread.model) # SPORTHR: 0.00729; READHR: 0.02689 -- Both significant (alpha=0.05)

ggplot(hours, aes(x = SPORTHR, y = READHR, color = MYOPIC)) +
  geom_point(size = 2) +
  labs(x = "Hours Spent on Sports", y = "Hours Spent on Reading", title = "Logistic Regression Decision Boundary") +
  theme_minimal()
```

**Conclusion:**

-   Hours spent on different activities are right-skewed, implying that, for each activity, some subjects might overspend their time;
-   After log-transformation, we observed significantly different times on sports between healthy and myopic subjects, which is also evident in Mann-Whitney U Test;
-   After log-transformation, we observed slightly significant (p-value=0.09062) difference of times on reading between healthy and myopic subjects, which is also evident in Mann-Whitney U Test (with p-value=0.102);
-   Using the logistic regression model including both SPORTHR and READHR, both times are significantly associated with MYOPIC (with p-values at 0.00729 and 0.02689 respectively), with SPORTHR significantly decreasing the chance to have myopia and READHR significantly increasing the chance to have myopia;
-   The logistic regression model suggests that, every additional hour spent on sports, the log odds of MYOPIC decreases by 0.04747($\pm$ 0.01769) and every additional hour spent on reading, the log odds of MYOPIC increases by 0.08030 ($\pm$ 0.03629).

## Predicting Myopia

### Predicting Myopia Using Sport Vector Machines

``` r
# Load necessary packages
library(e1071)
library(caret)
library(ggplot2)
library(kernlab)
library(pROC)

#load & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL
myopia$MYOPIC <- as.factor(myopia$MYOPIC)

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.7, list = FALSE)
train_data <- myopia[trainIndex, ]
test_data <- myopia[-trainIndex, ]

# Set up the parameter grid with correct column names
tune_grid <- expand.grid(
  C = 2^(-5:5),      # Range of values for C
  sigma = 2^(-15:3)  # Range of values for sigma (gamma)
)

# Set up train control
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 5,           # Number of folds
  search = "grid",      # Grid search
)

# Train the model with parameter tuning
svm_tuned <- train(
  MYOPIC ~ SPHEQ + SPORTHR,
  data = train_data,
  method = "svmRadial",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Extract the best model
best_svm_model_rbf <- svm_tuned$finalModel

# Function to plot the decision boundary with RBF kernel and support vectors
plot_svm_rbf_with_support_vectors <- function(model, myopia) {
  # Create a grid of values
  x_range <- seq(min(myopia$SPHEQ) - 1, max(myopia$SPHEQ) + 1, length.out = 100)
  y_range <- seq(min(myopia$SPORTHR) - 1, max(myopia$SPORTHR) + 1, length.out = 100)
  grid <- expand.grid(SPHEQ = x_range, SPORTHR = y_range)
  
  # Predict the class for each point in the grid
  grid$Pred <- predict(model, newdata = grid)
  
  # Extract support vectors
  support_vectors_indices <- model@SVindex
  support_vectors <- myopia[support_vectors_indices, ]
}

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.7, list = FALSE)
train_data <- myopia[trainIndex, ]
test_data <- myopia[-trainIndex, ]

# Train the SVM model with probability enabled
svm_model <- svm(MYOPIC ~ SPHEQ + SPORTHR, 
                 data = train_data, 
                 type = "C-classification", 
                 kernel = "radial", 
                 probability = TRUE)

# Predict probabilities for test data
test_pred <- predict(svm_model, newdata = test_data, probability = TRUE)

# Extract probabilities
probabilities <- attr(test_pred, "probabilities")

# Compute ROC curve
roc_svm <- roc(test_data$MYOPIC, probabilities[, 2])

# Plot ROC curve
plot.roc(roc_svm, col = "blue", main = "ROC Curve for SVM Model")

# Print AUC
text(0.5, 0.2, paste("AUC =", round(auc(roc_svm), 2)), col = "blue", cex = 1)
```

### Predicting Myopia Using Random Forests

#### Without considering of unbalanced data

``` r
#initial exploration: random forest
library(ggplot2)
library(tidyverse)
library(cowplot) #improve ggplot's default settings
library(randomForest)

#read & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL

#for missing vals
#set.seed(42)
#myopia.imputed <- rfImpute(myopic ~ ., data = myopia, iter = 6)
#myopic = var we want to predict
#iter = how many forests should be built to estimate missing vals (usually 4-6)

model <- randomForest(MYOPIC ~ ., data = myopia, proximity = TRUE)
model
#524 not myopic correctly classified as "not myopic"
#13 not myopic incorrectly classified as "myopic"
#57 myopic incorrectly classified as "not myopic"
#24 myopic correctly classified as "myopic"

#error rates at diff stages of RF
#1st row: error rates after making 1st tree
oob.error.myopia <- data.frame(
  trees = rep(1:nrow(model$err.rate), times = 3),
  type = as.factor(rep(c('OOB', '0', '1'), each = nrow(model$err.rate))),
  error = c(model$err.rate[,'OOB'],
  model$err.rate[,'0'],
  model$err.rate[,'1']))

#graph of error rates (0, 1, OOB)
ggplot(data = oob.error.myopia, 
       aes(x = trees, y = error)) +
  geom_line(aes(color = type,group = type))

#improving tree
model <- randomForest(MYOPIC ~ ., data = myopia, ntree = 2000, proximity = TRUE)
model

oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(MYOPIC ~ ., data = myopia, mtry = i, ntree = 2000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #mtry = 4/5 seems to be most optimal

distance.matrix <- dist(1-model$proximity) #conver RF proximity matrix into distance matrix
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE) #find lower-dimensional rep of data
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1) #computes % of var

#format data for ggplot
mds.values <- mds.stuff$points
mds.data <- data.frame(sample = rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2],
                       status = myopia$MYOPIC)

#graph on ggplot
ggplot(data = mds.data, aes(x=X, y=Y, label=sample)) +
  geom_text(aes(color = status)) +
  theme_bw() +
  xlab(paste('MDS1 - ', mds.var.per[1], '%', sep = '')) +
  ylab(paste('MDS2 - ', mds.var.per[2], '%', sep = '')) +
  ggtitle("MDS plot using (1- Random Forest Proximities)")

#improved MDS plot
ggplot(data = mds.data, aes(x = X, y = Y, color = status, label = sample)) +
  geom_point(size = 3, alpha = 0.7) +  # Use points instead of text for clarity
  scale_color_manual(values = c('0' = 'blue', '1' = 'red')) +
  theme_minimal() +
  xlab(paste('MDS1 - ', mds.var.per[1], '% variance', sep = '')) +
  ylab(paste('MDS2 - ', mds.var.per[2], '% variance', sep = '')) +
  ggtitle("MDS Plot using (1 - Random Forest Proximities)") +
  theme(plot.title = element_text(hjust = 0.5))

#calculate feature importance
importance_scores <- importance(model)
importance_scores

par(cex = 0.7) #change font size
varImpPlot(model, main = "Feature Importance: Mean Decrease in Gini")
par(cex = 1)


#convert to data frame for ggplot
importance_df <- data.frame(Feature = rownames(importance_scores), 
                            Importance = importance_scores[, 1])

#plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab('Features') +
  ylab('Importance') +
  ggtitle('Feature Importance in Random Forest Model') +
  theme_bw()

#ROC curve
install.packages("pROC")
library(pROC)

#use the model's OOB predictions
oob_predictions <- predict(model, type = "response", predict.all = TRUE)  # probabilities for the positive class (MYOPIC = 1)

#create the ROC curve
roc_curve <- roc(myopia$MYOPIC, oob_predictions)
auc_value <- auc(roc_curve)
print(auc_value)

#plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Random Forest Model")

text(0.5, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1)
```

#### Using SMOTE for unbalanced data

``` r
#random forest tuned
# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(caret)
library(cowplot)
library(randomForest)
library(pROC)
library(smotefamily)

# Read & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia$MYOPIC <- as.factor(myopia$MYOPIC)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL

# Apply SMOTE
smote_output <- SMOTE(X = myopia[, -which(names(myopia) == "MYOPIC")],
                      target = myopia$MYOPIC, 
                      K = 5, 
                      dup_size = 1)

# Combine the resulting data
myopia_balanced <- smote_output$data
names(myopia_balanced)[names(myopia_balanced) == "class"] <- "MYOPIC"

# Check the class distribution
print(table(myopia_balanced$MYOPIC))

# Define the control for cross-validation
control <- trainControl(method = "cv", number = 5, search = "grid")

# Define the grid of hyperparameters to search
tunegrid <- expand.grid(.mtry = c(1:7))

# Perform the grid search with the balanced data
set.seed(42)
rf_gridsearch <- train(MYOPIC ~ ., data = myopia_balanced, 
                       method = "rf", 
                       metric = "Accuracy", 
                       tuneGrid = tunegrid, 
                       trControl = control)

# Print the best model
print(rf_gridsearch)

# Train the model using the best parameters
best_params <- rf_gridsearch$bestTune
best_ntree <- ifelse(!is.null(best_params$.ntree), best_params$.ntree, 2000)
best_nodesize <- ifelse(!is.null(best_params$.nodesize), best_params$.nodesize, 1)
best_maxnodes <- ifelse(!is.null(best_params$.maxnodes), best_params$.maxnodes, NULL)

set.seed(42)
myopia_balanced$MYOPIC <- as.factor(myopia_balanced$MYOPIC)
model <- randomForest(MYOPIC ~ ., data = myopia_balanced, ntree = 2000, proximity = TRUE)
model

# Evaluate the model
oob_predictions <- predict(model, type = "prob")[, 2] # Probabilities for the positive class

# Create the ROC curve
roc_curve <- roc(myopia_balanced$MYOPIC, as.numeric(oob_predictions))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Random Forest Model")
text(0.5, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1)
```

### Predicting Myopia Using Boosting Trees

``` r
library(xgboost)
library(tidyverse)
library(caret)
library(data.table)
library(pROC)
library(ggplot2)
library(dplyr)

#load & clean data
myopia <- read.csv("myopia_og.csv", head = TRUE, sep = ";")

myopia <- myopia %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

myopia <- myopia %>% 
  filter(AGE != 9)

myopia$ID <- NULL
myopia$STUDYYEAR <- NULL


#XG BOOST
# Setting the seed for reproducibility
set.seed(0)

# Split into training and testing sets
my_trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.8, list = FALSE)
trainData <- myopia[my_trainIndex, ]
testData <- myopia[-my_trainIndex, ]

# Convert data.table to data.frame to avoid data.table-specific issues
trainData <- as.data.frame(trainData)
testData <- as.data.frame(testData)

# Detect character columns and convert them to factors, then to numeric
char_cols <- sapply(trainData, is.character)
for (col in names(char_cols[char_cols])) {
  trainData[[col]] <- as.numeric(as.factor(trainData[[col]]))
  testData[[col]] <- as.numeric(as.factor(testData[[col]]))
}

# Ensure all columns are numeric. We use sapply to convert any residual non-numeric columns
trainData <- as.data.frame(sapply(trainData, as.numeric))
testData <- as.data.frame(sapply(testData, as.numeric))

# Separate features and target
trainLabel <- trainData$MYOPIC
testLabel <- testData$MYOPIC
trainData <- trainData[, setdiff(names(trainData), "MYOPIC")]
testData <- testData[, setdiff(names(testData), "MYOPIC")]

# Convert features to numeric matrices
train_feats <- as.matrix(trainData)
test_feats <- as.matrix(testData)

# Convert to XGBoost DMatrix format
dtrain <- xgb.DMatrix(data = train_feats, label = as.numeric(trainLabel) - 1)
dtest <- xgb.DMatrix(data = test_feats, label = as.numeric(testLabel) - 1)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,  # Depth of the tree
  eta = 0.3,      # Learning rate
  nthread = 2
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,  # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10  # Early stopping if no improvement
)

# Predict on test data
pred <- predict(xgb_model, newdata = dtest)

#ROC CURVE 
roc_boost <- roc(testLabel, pred)

plot(roc_boost, main = "ROC Curve for XGBoost Model", col = "green", lwd = 2)

# Add AUC (Area Under the Curve) to the plot
text(0.75, 0.2, paste("AUC =", round(auc(roc_boost), 2)), col = "green", cex = 1)
```

#### Important Features

``` r
library(xgboost)
library(tidyverse)
library(caret)
library(data.table)
library(pROC)
library(ggplot2)
library(dplyr)

#load & clean data
my_data <- read.csv("myopia_og.csv", head = TRUE, sep = ";")

my_data <- my_data %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

my_data <- my_data %>% 
  filter(AGE != 9)

my_data <- my_data[, !colnames(myopia)]


glimpse(my_data)


#XG BOOST

# Setting the seed for reproducibility
set.seed(0)

# Split into training and testing sets
my_trainIndex <- createDataPartition(my_data$MYOPIC, p = 0.8, list = FALSE)
trainData <- my_data[my_trainIndex, ]
testData <- my_data[-my_trainIndex, ]

# Convert data.table to data.frame to avoid data.table-specific issues
trainData <- as.data.frame(trainData)
testData <- as.data.frame(testData)

# Detect character columns and convert them to factors, then to numeric
char_cols <- sapply(trainData, is.character)
for (col in names(char_cols[char_cols])) {
  trainData[[col]] <- as.numeric(as.factor(trainData[[col]]))
  testData[[col]] <- as.numeric(as.factor(testData[[col]]))
}

# Ensure all columns are numeric. We use sapply to convert any residual non-numeric columns
trainData <- as.data.frame(sapply(trainData, as.numeric))
testData <- as.data.frame(sapply(testData, as.numeric))

# Separate features and target
trainLabel <- trainData$MYOPIC
testLabel <- testData$MYOPIC
trainData <- trainData[, setdiff(names(trainData), "MYOPIC")]
testData <- testData[, setdiff(names(testData), "MYOPIC")]

# Convert features to numeric matrices
train_feats <- as.matrix(trainData)
test_feats <- as.matrix(testData)

# Convert to XGBoost DMatrix format
dtrain <- xgb.DMatrix(data = train_feats, label = as.numeric(trainLabel) - 1)
dtest <- xgb.DMatrix(data = test_feats, label = as.numeric(testLabel) - 1)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,  # Depth of the tree
  eta = 0.3,      # Learning rate
  nthread = 2
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,  # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10  # Early stopping if no improvement
)

# Predict on test data
pred <- predict(xgb_model, newdata = dtest)

# Convert probabilities to class labels
pred_labels <- ifelse(pred > 0.3, 1, 0)
# adjusting threshold in predictions to raise spec: pred_labels <- ifelse(pred > 0.3, 1, 0) 

confusionMatrix(factor(pred_labels), factor(as.numeric(testLabel) - 1))


#TREE MODEL
xgb.plot.tree(model = xgb_model, trees = 1)



#FEATURE IMPORTANCE
# Calculate feature importance
importance <- xgb.importance(model = xgb_model)

# Plot feature importance
xgb.plot.importance(importance_matrix = importance)


#CONFUSION MATRIX HEATMAP
# Predicted labels
pred_labels <- ifelse(pred > 0.3, 1, 0)

# Create confusion matrix
cm <- confusionMatrix(factor(pred_labels), factor(as.numeric(testLabel) - 1))

# Extract confusion matrix as a table
cm_table <- as.matrix(cm$table)

# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
cm_df$predicted <- rownames(cm_table)
cm_df <- gather(cm_df, actual, value, -predicted)

# Ensure 'value' is treated as factor
cm_df$value <- as.factor(cm_df$value)

# Get unique levels of 'value'
value_levels <- unique(cm_df$value)

# Determine number of unique levels
num_levels <- length(value_levels)

# Define colors for the heatmap
heat_colors <- colorRampPalette(c("lightblue", "darkblue"))(num_levels)

# Plot confusion matrix as heatmap
ggplot(cm_df, aes(x = actual, y = predicted, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = heat_colors,
                    breaks = value_levels,
                    labels = value_levels) +
  labs(title = "Confusion Matrix Heatmap",
       x = "Actual",
       y = "Predicted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#BUILDING ROC CURVE 

# Compute ROC curve
roc_curve <- roc(testLabel, pred)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model", col = "blue", lwd = 2)

# Add diagonal reference line (random classifier)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add AUC (Area Under the Curve) to the plot
text(0.75, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1.5)
```

**Conclusion:**

-   

-   

-   

-   

-   
