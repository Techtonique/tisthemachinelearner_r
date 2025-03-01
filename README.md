# tisthemachinelearner 

R interface to scikit-learn through (through Python tisthemachinelearner) with S3 and R6 interfaces.

```R
remotes::install_github("Techtonique/tisthemachinelearner")
```

```R
library(tisthemachinelearner)
```

```R
# Load data
data(mtcars)
head(mtcars)

# Split features and target
X <- as.matrix(mtcars[, -1])  # all columns except mpg
y <- mtcars[, 1]              # mpg column

# Create train/test split
set.seed(42)
train_idx <- sample(nrow(mtcars), size = floor(0.8 * nrow(mtcars)))
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# R6 interface
model <- Regressor$new(model_name = "LinearRegression")
model$fit(X_train, y_train)
preds <- model$predict(X_test)
print(preds)

# S3 interface
model <- regressor(X_train, y_train, model_name = "LinearRegression")
preds <- predict(model, X_test)
print(preds)
```


