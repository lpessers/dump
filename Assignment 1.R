# setwd("~/Documents/Erasmus University/IBA Year 3/Data Modelling & Analysis/Assignment 1")
# Instructions: Please fill in your student ID below.
studentID <- 520791

# Install packages if necessary
library(writexl)
library(readxl)
library(car)
library(lmtest)
library(sandwich)

#######################
### START OF PART 1 ###
#######################
set.seed(studentID %% (37*27))
n <- 100
d <- 7
cov_vec <- seq(1,7)
cov_vec <- cov_vec / sqrt(sum(cov_vec^2))
cov_mat <- cov_vec %*% t(cov_vec)
df1 <- as.data.frame(rmvnorm(n = n, mean = rep(0,d), sigma = cov_mat) + 0.25*rmvnorm(n = n, mean = rep(0,d)))
colnames(df1) <- paste("x", seq(1,7), sep = "")
true_coeff <- c(4,5,1,3,2,7,6)/7
df1$y <- as.matrix(df1) %*% true_coeff + rnorm(n = n, sd = 0.4)
df1_stu <- df1

#####################
### END OF PART 1 ###
#####################

## QUESTION 1
mean(df1_stu$y)

## QUESTION 2
corr_q2 <- cor(df1_stu[,-1])
library(corrplot)
corrplot(corr_q2, method = 'number', type = 'upper')

## QUESTION 3
model_q3 <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x7, data = df1_stu)
summary(model_q3)

## QUESTION 7
residual <- resid(model_q3)

hist(residual, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
lines(density(residual), col = "red")

qqnorm(residual)
qqline(residual) 

#######################
### START OF PART 2 ###
#######################
set.seed(studentID %% (37*27))
base_df <- data.frame(matrix(ncol = 0, nrow = n))
base_df$x1 <- rnorm(n)
base_df$x2 <- base_df$x1 * 0.9 + rnorm(n, sd = 0.2)
base_df$x3 <- rnorm(n)
base_df$x4 <- rnorm(n)
base_df$x5 <- rnorm(n, mean = 0.5, sd = 0.25)
y_hold <- with(base_df, x2 + x3 * x4 + x5^2)

shuffler <- sample(1:5, 5, replace = FALSE)
df2 <- base_df[,shuffler]
colnames(df2) <- paste(rep('x',5), 1:5, sep = "")
df2$y <- y_hold

df2_stu <- df2

#####################
### END OF PART 2 ###
#####################

## QUESTION 8
model_q8 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = df2_stu)
summary(model_q8)

## QUESTION 9
vif(model_q8)

## QUESTION 10
model_q8 <- lm(y ~ x1 + x3 + x4 + x5, data = df2_stu)
summary(model_q8)
vif(model_q8)

## QUESTION 11
model_q11 <- lm(y ~ x1 * x2 * x3 * x4 * x5, data = df2_stu)
summary(model_q11)

## QUESTION 14
model_q14 <- lm(y ~ x1 + x3 + x4 * x5, data = df2_stu)
summary(model_q14)

plot(df2_stu$x3, residual, xlab="x3", ylab="Residuals", main = "Residuals against x3") 
abline(0, 0, col = "blue")

## QUESTION 15
df2_stu$x_squared <- df2_stu$x3 * df2_stu$x3

model_q15 <- lm(y ~ x1 + x3 + x4 * x5 + x_squared, data = df2_stu)
summary(model_q15)

residual <- resid(model_q15)

plot(df2_stu$x_squared, residual, xlab="x3", ylab="Residuals", main = "Residuals against x3") 
abline(0, 0, col = "blue")

anova(model_q14, model_q15)

## QUESTION 16
plot(fitted(model_q15), residuals(model_q15), main = "Residuals vs. Fitted")
abline(0, 0, col = "red")

bptest(model_q15)

