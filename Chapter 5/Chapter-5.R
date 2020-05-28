library(MASS)
library(dplyr)
library(ggplot2)
library(mgcv)
library(rpart)
library(klaR)

loan3000 <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/loan3000.csv")
loan_data <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/loan_data.csv")
full_train_set <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/full_train_set.csv")

loan_data$outcome <- as.factor(loan_data$outcome)

naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_,
                          data = na.omit(loan_data))
naive_model$table

new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan) <- NULL
new_loan

predict(naive_model, new_loan)

loan_lda <- lda(outcome ~ borrower_score + payment_inc_ratio,
                     data=loan3000)
loan_lda$scaling

pred <- predict(loan_lda)
head(pred$posterior)

lda_df <- cbind(loan3000, prob_default=pred$posterior[,'default'])

x <- seq(from=.33, to=.73, length=100)
y <- seq(from=0, to=20, length=100)
newdata <- data.frame(borrower_score=x, payment_inc_ratio=y)
pred <- predict(loan_lda, newdata=newdata)
lda_df0 <- cbind(newdata, outcome=pred$class)

ggplot(data=lda_df, aes(x=borrower_score, y=payment_inc_ratio, color=prob_default)) +
  geom_point(alpha=.6) +
  scale_color_gradient2(low='white', high='blue') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), lim=c(0, 20)) +
  geom_line(data=lda_df0, col='green', size=2, alpha=.8)

logistic_model <- glm(outcome ~ payment_inc_ratio + purpose_ +
                        home_ + emp_len_ + borrower_score,
                      data=loan_data, family='binomial')
logistic_model

pred <- predict(logistic_model)
summary(pred)

prob <- 1/(1 + exp(-pred))
summary(prob)

summary(logistic_model)

logistic_gam <- gam(outcome ~ s(payment_inc_ratio) + purpose_ +
                        home_ + emp_len_ + s(borrower_score),
                      data=loan_data, family='binomial')

terms <- predict(logistic_gam, type='terms')
partial_resid <- resid(logistic_model) + terms
df <- data.frame(payment_inc_ratio = loan_data[, 'payment_inc_ratio'],
                 terms = terms[, 's(payment_inc_ratio)'],
                 partial_resid = partial_resid[, 's(payment_inc_ratio)'])
ggplot(df, aes(x=payment_inc_ratio, y=partial_resid, solid = FALSE)) +
  geom_point(shape=46, alpha=.4) +
  geom_line(aes(x=payment_inc_ratio, y=terms),
            color='red', alpha=.5, size=1.5) +
  labs(y='Partial Residual')

pred <- predict(logistic_gam, newdata=loan_data)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(loan_data$outcome=='default')
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)
conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)
colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat) <- c('Y = 1', 'Y = 0')
conf_mat

# precision
conf_mat[1,1]/sum(conf_mat[,1])
# recall
conf_mat[1,1]/sum(conf_mat[1,])
# specificity
conf_mat[2,2]/sum(conf_mat[2,])

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='blue') +
  scale_x_reverse(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x),
            linetype='dotted', color='red')

sum(roc_df$recall[-1] * diff(1-roc_df$specificity))

mean(full_train_set$outcome=='default')

full_model <- glm(outcome ~ payment_inc_ratio + purpose_ +
                   home_ + emp_len_+ dti + revol_bal + revol_util,
                 data=full_train_set, family='binomial')
pred <- predict(full_model)
mean(pred > 0)

wt <- ifelse(full_train_set$outcome=='default',
              1/mean(full_train_set$outcome == 'default'), 1)
full_model <- glm(outcome ~ payment_inc_ratio + purpose_ +
                    home_ + emp_len_+ dti + revol_bal + revol_util,
                  data=full_train_set, weight=wt, family='quasibinomial')
pred <- predict(full_model)
mean(pred > 0)
