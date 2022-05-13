df.train <- df.training
df.test <- df.testing

# Install packages if necessary
library(tidyverse)
library(dplyr)
library(lmtest)
library(corrplot)
library(car)

#### QUESTION 1 ####
# build tree
tree.train <- rpart(y ~ ., data = df.train, method = "class", maxdepth = 5)
summary(tree.train)
# visualize tree
rpart.plot(tree.train)

# alternative
df.test %>% count(y)

#### QUESTION 2 ####
# build tree
tree.test <- rpart(y ~ ., data = df.test, method = "class", maxdepth = 5)
summary(tree.test)
# visualize tree
rpart.plot(tree.test)

# alternative
df.test %>% count(y)

#### QUESTION 4 ####
mod1 <- glm(y~., data = df.train, family = "binomial")
summary(mod1)

#### QUESTION 5 ####
vif(mod1)

#### QUESTION 7 + 8 ####
fit_to_test <- predict(mod1, newdata = df.train)
fit_preds <- exp(fit_to_test) / (1 + exp(fit_to_test))

# set cutoff
cutoff <- 0.5
factor_levels <- c(0,1)

# create predictions
df.train$pred <- factor_levels[(fit_preds > cutoff)+1]

# build matrix
conf_mat <- xtabs(~ y + pred, data=df.train)
conf_mat

# metric calculations
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy
recall <- conf_mat["1","1"] / (conf_mat["1","1"] + conf_mat["1","0"])
recall

#### QUESTION 9 + 10 ####
fit_to_test <- predict(mod1, newdata = df.test)
fit_preds <- exp(fit_to_test) / (1 + exp(fit_to_test))

# set cutoff
cutoff <- 0.5
factor_levels <- c(0,1)

# create predictions
df.test$pred <- factor_levels[(fit_preds > cutoff)+1]

# build matrix
conf_mat <- xtabs(~ y + pred, data=df.test)
conf_mat

# metric calculations
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy
recall <- conf_mat["1","1"] / (conf_mat["1","1"] + conf_mat["1","0"])
recall

#### QUESTION 12 ####
residual <- resid(mod1)

plot(df.train$x1, residual, xlab="X", ylab="Residuals", main = "Residuals against X")
plot(df.train$x2, residual, xlab="X", ylab="Residuals", main = "Residuals against X")
plot(df.train$x3, residual, xlab="X", ylab="Residuals", main = "Residuals against X")
plot(df.train$x4, residual, xlab="X", ylab="Residuals", main = "Residuals against X")
plot(df.train$x5, residual, xlab="X", ylab="Residuals", main = "Residuals against X")

abline(0, 0, col = "blue")

#### QUESTION 14 ####
df_train_under <- subset(df.train, x2 < 0)
df_train_over <- subset(df.train, x2 > 0)

mod1a <- glm(y ~., data = df_train_under, family = "binomial")
summary(mod1a)

#### QUESTION 15 ####
cutoff <- 0.5
factor_levels <- levels(df_train_under$y)

fit_preds <- predict(mod1, newdata = df_train_under, type = "response")
df_train_under$y = as.factor(df_train_under$y)
df_train_under$preds <- factor_levels[(fit_preds > cutoff) + 1]

conf_mat <- xtabs(~y + preds, data=df_train_under)
conf_mat

accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy

#### QUESTION 16 ####
cutoff <- 0.5
factor_levels <- levels(df_train_under$y)

fit_preds <- predict(mod1a, newdata = df_train_under, type = "response")
df_train_under$y = as.factor(df_train_under$y)
df_train_under$preds <- factor_levels[(fit_preds > cutoff) + 1]

conf_mat <- xtabs(~y + preds, data=df_train_under)
conf_mat

accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy

#### QUESTION 18 ####
mod1b <- glm(y ~., data = df_train_over, family = "binomial")
summary(mod1b)

#### QUESTION 19 ####
cutoff <- 0.5
factor_levels <- levels(df_train_over$y)

fit_preds <- predict(mod1b, newdata = df_train_over, type = "response")
df_train_over$y = as.factor(df_train_over$y)
df_train_over$preds <- factor_levels[(fit_preds > cutoff) + 1]

conf_mat <- xtabs(~y + preds, data = df_train_over)
conf_mat

accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy

#### QUESTION 21 ####
tree.train <- rpart(y ~ ., data = df.train, method = "class", maxdepth = 3)
summary(tree.train)
# visualize tree
rpart.plot(tree.train)

#### QUESTION 23 ####
df.test$preds <- predict(tree.train, newdata = df.test, type = "class")
conf_mat <- xtabs(~y + preds, data = df.test)
conf_mat

accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy

#### QUESTION 24 ####
# build tree
tree.train2 <- rpart(y ~ ., data = df.train, method = "class", maxdepth = 6)
summary(tree.train2)
# visualize tree
rpart.plot(tree.train2)

df.test$preds <- predict(tree.train2, newdata = df.test, type = "class")
conf_mat <- xtabs(~y + preds, data = df.test)
conf_mat

accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy

#### QUESTION 25 ####
# build tree
tree.train3 <- rpart(y ~ ., data = df.train, method = "class", maxdepth = 10)
summary(tree.train3)
# visualize tree
rpart.plot(tree.train3)

df.test$preds <- predict(tree.train3, newdata = df.test, type = "class")
conf_mat <- xtabs(~y + preds, data = df.test)
conf_mat

accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
accuracy
