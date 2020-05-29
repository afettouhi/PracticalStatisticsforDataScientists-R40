library(dplyr)
library(ggplot2)
library(FNN)
library(rpart)
library(randomForest)
library(xgboost)

loan200 <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/loan200.csv")
loan200$outcome <- ordered(loan200$outcome, levels=c('paid off', 'default'))

loan_data <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/loan_data.csv")
loan_data <- select(loan_data,-X, -status)

loan3000 <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/loan3000.csv")
loan3000$outcome <- ordered(loan3000$outcome, levels=c('paid off', 'default'))

newloan <- loan200[1, 2:3, drop=FALSE]
knn_pred <- knn(train=loan200[-1,2:3], test=newloan, cl=loan200[-1,1], k=20)
knn_pred == 'paid off'

newloan

loan_df <- model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal +
                           revol_util, data=loan_data)
newloan = loan_df[1,, drop=FALSE]
loan_df = loan_df[-1,]
outcome <- loan_data[-1,1]
knn_pred <- knn(train=loan_df, test=newloan, cl=outcome, k=5)
loan_df[attr(knn_pred,"nn.index"),]

loan_df <- model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal +
                           revol_util, data=loan_data)
loan_std <- scale(loan_df)
target_std = loan_std[1,, drop=FALSE]
loan_std = loan_std[-1,]
outcome <- loan_data[-1,1]
knn_pred <- knn(train=loan_std, test=target_std, cl=outcome, k=5)
loan_df[attr(knn_pred,"nn.index"),]

borrow_df <- model.matrix(~ -1 + dti + revol_bal + revol_util + open_acc +
                            delinq_2yrs_zero + pub_rec_zero, data=loan_data)
borrow_knn <- knn(borrow_df, test=borrow_df, cl=loan_data[, 'outcome'],
                  prob=TRUE, k=20)
prob <- attr(borrow_knn, "prob")
borrow_feature <- ifelse(borrow_knn=='default', 1-prob, prob)
summary(borrow_feature)

loan_tree <- rpart(outcome ~ borrower_score + payment_inc_ratio,
                   data=loan_data, control = rpart.control(cp=.005))
plot(loan_tree, uniform=TRUE, margin=.05)
text(loan_tree)

loan_tree

rf <- randomForest(outcome ~ borrower_score + payment_inc_ratio,
                    data=loan3000)
rf

error_df = data.frame(error_rate = rf$err.rate[,'OOB'],
                      num_trees = 1:rf$ntree)
ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()

pred <- predict(rf, prob=TRUE)
rf_df <- cbind(loan3000, pred = pred)
ggplot(data=rf_df, aes(x=borrower_score, y=payment_inc_ratio,
                       shape=pred, color=pred)) +
  geom_point(alpha=.6, size=2) +
  scale_shape_manual( values=c( 46, 4))

rf_all <- randomForest(outcome ~ ., data=loan_data, importance=TRUE)
rf_all

varImpPlot(rf_all, type=1)
varImpPlot(rf_all, type=2)

predictors <- data.matrix(loan3000[, c('borrower_score', 'payment_inc_ratio')])
label <- as.numeric(loan3000[,'outcome'])-1
xgb <- xgboost(data=predictors, label=label, objective = "binary:logistic",
               params=list(subsample=.63, eta=0.1), nrounds=100)

pred <- predict(xgb, newdata=predictors)
xgb_df <- cbind(loan3000, pred_default=pred>.5, prob_default=pred)
ggplot(data=xgb_df, aes(x=borrower_score, y=payment_inc_ratio,
                       color=pred_default, shape=pred_default)) +
       geom_point(alpha=.6, size=2)

seed <- 400820
predictors <- data.matrix(loan_data[,-which(names(loan_data) %in%
                                       'outcome')])
label <- as.numeric(loan_data$outcome)-1
test_idx <- sample(nrow(loan_data), 10000)

xgb_default <- xgboost(data=predictors[-test_idx,],
                       label=label[-test_idx],
                       objective = "binary:logistic", nrounds=250, verbose=0)
pred_default <- predict(xgb_default, predictors[test_idx,])
error_default <- abs(label[test_idx] - pred_default) > 0.5
xgb_default$evaluation_log[250,]
mean(error_default)

xgb_penalty <- xgboost(data=predictors[-test_idx,],
                       label=label[-test_idx],
                       params=list(eta=.1, subsample=.63, lambda=1000),
                       objective = "binary:logistic", nrounds=250, verbose=0)
pred_penalty <- predict(xgb_penalty, predictors[test_idx,])
error_penalty <- abs(label[test_idx] - pred_penalty) > 0.5
xgb_penalty$evaluation_log[250,]
mean(error_penalty)

error_default <- rep(0, 250)
error_penalty <- rep(0, 250)
for(i in 1:250){
  pred_def <- predict(xgb_default, predictors[test_idx,], ntreelimit=i)
  error_default[i] <- mean(abs(label[test_idx] - pred_def) >= 0.5)
  pred_pen <- predict(xgb_penalty, predictors[test_idx,], ntreelimit = i)
  error_penalty[i] <- mean(abs(label[test_idx] - pred_pen) >= 0.5)
}

errors <- rbind(xgb_default$evaluation_log,
                xgb_penalty$evaluation_log,
                data.frame(iter=1:250, train_error=error_default),
                data.frame(iter=1:250, train_error=error_penalty))
errors$type <- rep(c('default train', 'penalty train',
                     'default test', 'penalty test'), rep(250, 4))
ggplot(errors, aes(x=iter, y=train_error, group=type)) +
  geom_line(aes(linetype=type, color=type))

N <- nrow(loan_data)
fold_number <- sample(1:5, N, replace = TRUE)
params <- data.frame(eta = rep(c(.1, .5, .9), 3),
                     max_depth = rep(c(3, 6, 12), rep(3,3)))

error <- matrix(0, nrow=9, ncol=5)
for(i in 1:nrow(params)){
  for(k in 1:5){
    fold_idx <- (1:N)[fold_number == k]
    xgb <- xgboost(data=predictors[-fold_idx,], label=label[-fold_idx],
                     params = list(eta = params[i, 'eta'],
                                   max_depth = params[i, 'max_depth']),
                   objective = "binary:logistic", nrounds=100, verbose=0)
    pred <- predict(xgb, predictors[fold_idx,])
    error[i, k] <- mean(abs(label[fold_idx] - pred) >= 0.5)
  }
}

avg_error <- 100 * round(rowMeans(error), 4)
cbind(params, avg_error)
