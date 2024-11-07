# COSMOS Project on Juvenile Myopia

**Lucia Zhang (Captain), Giang Tran, Luke Owyang, April Pan**

## Data Introduction

The dataset `myopia_og.csv` includes a total of 618 observations, each reporting the following information,

-   `ID`: Subject identification;
-   `STUDYYEAR`: Year of the subject entering the study;
-   `MYOPIC`: Myopia within the first five years of followup (1=Yes, 0=No);
-   `AGE`: Age at first visit;
-   `GENDER`: Gender of the subject;
-   `SPHEQ`: Spherical equivalent refraction;
-   `AL`: Axial length (mm);
-   `ACD`: Anterior chamber depth (mm);
-   `LT`: Lens thickness (mm);
-   `VCD`: Vitreous champer depth (mm);
-   `SPORTHR`: Time spent in sports/outdoor activities (hours/week);
-   `READHR`: Time spent for reading (hours/week);
-   `COMPHR`: Time spent in using computer (hours/week);
-   `STUDYHR`: Time spent in studying for school assignments (hours/week);
-   `TVHR`: Time spent in watching television (hours/week);
-   `DIOPTERHR`: Composite of near-work activities (hours/week);
-   `MOMMY`: Whether subject's mother was myopic (1=Yes, 0=No);
-   `DADMY`: Whether subject's father was myopic (1=Yes, 0=No);

``` r
data <- read.csv('myopia_og.csv', sep=";", header=TRUE)
summary(data)

xtabs(~data$MYOPIC)
```

Since there are 537 healthy subjects and only 81 myopic subjects, the dataset is unbalanced with slight over 13% subjecgts as myopia cases.

## Data Exploration via Correlation Matrix

We first explore the correlation between different pairs of variables, in particular, the correlation of `MYOPIC` to other variables in terms of Pearson's correlation coefficient.

``` r
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load & Clean data
data <- read.csv('myopia_og.csv', sep=";", header=TRUE)

xtabs(~data$AGE)

# Select only numeric columns for correlation analysis
numeric_data <- data[sapply(data, is.numeric)]

# Check for any possible missing values
anyNA(numeric_data)

# Calculate the correlation matrix
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

correlation_matrix[3,]
```

**Conclusion:**

-   There are no missing values and all variables are coded in numeric values;
-   There is only one pair of variables, AL and VCD, which are correlated over with scale of correlation coefficient over 0.9. In fact their correlation coefficient is 0.94;
-   Whether the subject is myopic is slightly correlated with Gender (with Pearson's correlation coefficient $\rho=0.0616$), ACD ($\rho=0.1079$), SPORTHR ($\rho=-0.0983$), READHR ($\rho=0.0727$), MOMMY ($\rho=0.1340$), DADMY ($\rho=0.1499$) but rather strongly related to SPHEQ ($\rho=-0.3736$).

## Hypothesis Tests on Association of Different Activity Times with Myopia

We will first explore the association of different activity times with myopia using t-tests.

``` r
library(tidyverse)
library(ggplot2)
library(dplyr)

# Reload the data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
head(myopia)
summary(myopia)

# Extract only activity times
hours <- select(myopia, contains("hr"), contains("MYOPIC"))

hours <- hours %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

# Summary statistics
summary_stats <- hours %>%
  group_by(MYOPIC) %>%
  summarise(across(c(SPORTHR, TVHR, COMPHR, READHR, STUDYHR, DIOPTERHR), list(mean = mean, sd = sd, median = median, IQR = IQR), .names = "{col}_{fn}"))

print(t(summary_stats))
```

We then use boxplots to visualize the difference of these activity times between healthy and myopic subjects.

``` r
# Boxplots: ALL 6 Activit Times in hours/week
pivot_longer(hours,
  contains('HR'),
  names_to = "category",
  values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")
```

There are too many outliers shown in each boxplot, implying right-skewed distributions of activity times. So we first consider a log-transformation on each activity time, i.e., `log(hours+1)`, then retake the t-tests.

``` r
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
```

After the log-transformation, we observe much less number of outliers shown in the boxplots, although there are still some. We further consider Mann-Whitney U test, aka Wilcoxon Rank-Sum test, on the activity hours.

``` r
wilcox_test_results <- list(
  SPORTHR = wilcox.test(SPORTHR ~ MYOPIC, data = hours), # p-val = 0.004804 -- Significant
  TVHR = wilcox.test(TVHR ~ MYOPIC, data = hours), #p-val = 0.8204
  COMPHR = wilcox.test(COMPHR ~ MYOPIC, data = hours), #p-val = 0.8927
  READHR = wilcox.test(READHR ~ MYOPIC, data = hours), #p-val = 0.102 -- Around 0.1
  STUDYHR = wilcox.test(STUDYHR ~ MYOPIC, data = hours), #p-val = 0.9676
  DIOPTERHR = wilcox.test(DIOPTERHR ~ MYOPIC, data = hours) #p-val = 0.3169
)

wilcox_test_results
```

As the hypothesis tests suggest possible association of `READHR` and `SPORTHR` with `MYOPIC`, we further explore the association with the plot.

``` r
ggplot(hours,
       aes(x = READHR,
           y = SPORTHR,
           color = MYOPIC)) +
  geom_point()
```

It seems that less `SPROTHR` and more `READHR` are associated with `MYOPIC`. So we resort to logistic regression include both `SPROTHR` and `READHR` in the model and study their association iwht `MYOPIC`.

``` r
sportread.model <- glm(MYOPIC ~ SPORTHR+READHR, data = hours, family = binomial)
summary(sportread.model) # SPORTHR: 0.00729; READHR: 0.02689 -- Both significant (alpha=0.05)
```

Aware of possible confounding effects by age and gender, we apply the logistic regression to check the association of each activity time with `MYOPIC`.

``` r
sporthr.glm <- glm(MYOPIC ~ AGE*GENDER*SPORTHR, data = myopia, family = binomial)
summary(sporthr.glm) # SPORTHR: 0.00341; AGE:SPORTHR: 0.00792; GENDER:SPORTHR:0.01248; AGE:GENDER:SPORTHR: 0.02307

readhr.glm <- glm(MYOPIC ~ AGE*GENDER*READHR, data = myopia, family = binomial)
summary(readhr.glm) # None significant

comphr.glm <- glm(MYOPIC ~ AGE*GENDER*COMPHR, data = myopia, family = binomial)
summary(comphr.glm) # None significant

studyhr.glm <- glm(MYOPIC ~ AGE*GENDER*STUDYHR, data = myopia, family = binomial)
summary(studyhr.glm) # None significant

tvhr.glm <- glm(MYOPIC ~ AGE*GENDER*TVHR, data = myopia, family = binomial)
summary(tvhr.glm) # None significant

diopterhr.glm <- glm(MYOPIC ~ AGE*GENDER*DIOPTERHR, data = myopia, family = binomial)
summary(diopterhr.glm) # None significant
```

**Conclusion:**

-   Hours spent on different activities are right-skewed, implying that, for each activity, some subjects might overspend their time;
-   After log-transformation, we observed significantly different times on sports between healthy and myopic subjects, which is also evident in Mann-Whitney U Test;
-   After log-transformation, we observed slightly significant (p-value=0.09062) difference of times on reading between healthy and myopic subjects, which is also evident in Mann-Whitney U Test (with p-value=0.102);
-   Using the logistic regression model including both SPORTHR and READHR, both times are significantly associated with MYOPIC (with p-values at 0.00729 and 0.02689 respectively), with SPORTHR significantly decreasing the chance to have myopia and READHR significantly increasing the chance to have myopia;
-   The logistic regression model suggests that, every additional hour spent on sports, the log odds of MYOPIC decreases by 0.04747($\pm$ 0.01769) and every additional hour spent on reading, the log odds of MYOPIC increases by 0.08030 ($\pm$ 0.03629);
-   After controlling the possible confounding effects of age and gender, only `SPORTHR` is associated with 'MYOPIC\`, and the association IS confounded by age and gender.

## Predicting Myopia

Shown in above hypothesis tests, different variables may complicated relation to `MYOPIC`. Here we will apply different types of machine learning methods to build predictive models for `MYOPIC`, using all available models. As there are 6 subjects (4 healthy and 2 myopic subjects) with age at 9 year old, we will exclude them for the purpose of building more reliable models including age.

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
myopia <- myopia[myopia$AGE != 9, -c(1,2)]   # Exclude: ID, STUDYYEAR, subjects with AGE==9 
myopia$MYOPIC <- as.factor(myopia$MYOPIC)

# Setting the seed for reproducibility and same splits
set.seed(0)

# Split into training and testing sets
my_trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.8, list = FALSE)
trainData <- myopia[my_trainIndex, ]
testData <- myopia[-my_trainIndex, ]

# Convert data.table to data.frame to avoid data.table-specific issues
trainData <- as.data.frame(trainData)
testData <- as.data.frame(testData)

rf.model <- randomForest(MYOPIC~., data = trainData, ntree = 2000, proximity = TRUE)
rf.model
#Confusion matrix:
#    0    1   class.error
# 0 419   8   0.01873536
# 1  55   9   0.85937500
```

We observe that the built random forest cannot predict myopia well even in the training set, with only 9 myopic subjects classified correctly but 55 myopic subjects wrongly classified as healthy subjects. This is due to the unbalanced data, i.e., 64 myopic subjects out of a total of 519 subjects in the training set. So we need apply certain methods to manage the unbalanced data before we built the random forest.


#### Using SMOTE for unbalanced data

Here we will apply the SMOTE technique to help build the random forest for our unbalanced data.

``` r
library(ggplot2)
library(tidyverse)
library(caret)
library(cowplot)
library(randomForest)
library(pROC)
library(smotefamily)

# Read & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, -c(1,2)]   # Exclude: ID, STUDYYEAR, subjects with AGE==9 
myopia$MYOPIC <- as.factor(myopia$MYOPIC)

# Setting the seed for reproducibility and same splits
set.seed(0)

# Split into training and testing sets
my_trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.8, list = FALSE)
trainData <- myopia[my_trainIndex, ]
testData <- myopia[-my_trainIndex, ]

# Convert data.table to data.frame to avoid data.table-specific issues
trainData <- as.data.frame(trainData)
testData <- as.data.frame(testData)

# Apply SMOTE
smote_output <- SMOTE(X = trainData[, -which(names(myopia) == "MYOPIC")],
                      target = trainData$MYOPIC, 
                      K = 8, 
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
best_maxnodes <- best_params$.maxnodes

set.seed(42)
myopia_balanced$MYOPIC <- as.factor(myopia_balanced$MYOPIC)
model <- randomForest(MYOPIC ~ ., data = myopia_balanced, ntree = 2000, proximity = TRUE)
model

# Evaluate the model
oob_predictions <- predict(model, newdata=testData, type = "prob")[, 2] # Probabilities for the positive class

# Create the ROC curve
roc_curve <- roc(testData$MYOPIC, as.numeric(oob_predictions))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Random Forest Model")
text(0.5, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1)
```

Achieved AUC at 0.89.


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
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, -c(1,2)]   # Exclude: ID, STUDYYEAR, subjects with AGE==9 
myopia$MYOPIC <- as.factor(myopia$MYOPIC)

# Setting the seed for reproducibility and same splits
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
library(DiagrammeR)

#load & clean data
my_data <- read.csv('myopia_og.csv', sep=";", header=TRUE)
my_data <- my_data[my_data$AGE != 9, -c(1,2)]   # Exclude: ID, STUDYYEAR, subjects with AGE==9 
my_data$MYOPIC <- as.factor(my_data$MYOPIC)

#my_data <- my_data[, !colnames(myopia)]

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

### Predicting Myopia Using Sport Vector Machines

``` r
library(e1071)
library(caret)
library(ggplot2)
library(kernlab)
library(pROC)

#load & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, -c(1,2)]   # Exclude: ID, STUDYYEAR, subjects with AGE==9 
myopia$MYOPIC <- as.factor(myopia$MYOPIC)

# Split the data into training (80%) and testing (20%) sets, stratified under MYOPIC
set.seed(42)
trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.8, list = FALSE)
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
  #MYOPIC ~ SPHEQ + SPORTHR,
  MYOPIC ~ SPHEQ + SPORTHR + AGE + GENDER,
  data = train_data,
  method = "svmRadial",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Print the best parameters and model
print(svm_tuned)

# Extract the best model
best_svm_model_rbf <- svm_tuned$finalModel

# Function to plot the decision boundary with RBF kernel and support vectors
plot_svm_rbf_with_support_vectors <- function(model, data, age, gender) {
  # Create a grid of values
  x_range <- seq(min(data$SPHEQ) - 1, max(data$SPHEQ) + 1, length.out = 100)
  y_range <- seq(min(data$SPORTHR) - 1, max(data$SPORTHR) + 1, length.out = 100)
  grid <- expand.grid(SPHEQ = x_range, SPORTHR = y_range, AGE=age, GENDER=gender)
  
  # Predict the class for each point in the grid
  grid$Pred <- predict(model, newdata = grid)
  
  # Extract support vectors
  support_vectors_indices <- model@SVindex
  support_vectors <- data[support_vectors_indices, ]
  
  # Check if support_vectors is empty
  if (nrow(support_vectors) == 0) {
    stop("No support vectors found.")
  }
  
  # Plot the decision boundary and support vectors
  ggplot() +
    geom_tile(data = grid, aes(x = SPHEQ, y = SPORTHR, fill = Pred), alpha = 0.3) +
    geom_point(data = data, aes(x = SPHEQ, y = SPORTHR, color = MYOPIC), size = 2) +
    geom_point(data = support_vectors, aes(x = SPHEQ, y = SPORTHR), 
               color = 'black', shape = 17, size = 2, stroke = 1.5) +
    scale_fill_manual(values = c('lightblue', 'lightcoral'), guide = "none") +
    labs(title = "SVM with RBF Kernel Decision Boundary and Support Vectors",
         x = "SPHEQ",
         y = "SPORTHR") +
    theme_minimal()
}

# Plot using the training data
plot_svm_rbf_with_support_vectors(best_svm_model_rbf, train_data,age=6, gender=1)

# Train the SVM model with probability enabled
svm_model <- svm(MYOPIC ~ SPHEQ + SPORTHR + AGE + GENDER, 
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

# Print AUC: 0.87
text(0.5, 0.2, paste("AUC =", round(auc(roc_svm), 2)), col = "blue", cex = 1)
```

 
