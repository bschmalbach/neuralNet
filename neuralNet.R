library(ggplot2)
library(neuralnet)

df <- read.csv("bank_note_data.csv")

#plot
res <- data.frame(matrix(NA, 2, 4))
res[1,1] <- mean(df[df$Class==0,1])
res[1,2] <- mean(df[df$Class==0,2])
res[1,3] <- mean(df[df$Class==0,3])
res[1,4] <- mean(df[df$Class==0,4])
res[2,1] <- mean(df[df$Class==1,1])
res[2,2] <- mean(df[df$Class==1,2])
res[2,3] <- mean(df[df$Class==1,3])
res[2,4] <- mean(df[df$Class==1,4])
names(res) <- names(df)[1:4]
rownames(res) <- c("0", "1")

barplot(as.matrix(res), col = c("grey","blue"), beside = T)
legend("topleft", c("Fake","Real"), fill = c("grey","blue"))


set.seed(1)
#randomly split data
split <- sample(c(T,F), size=nrow(df), prob = c(0.7,0.3), replace=T)
df_train <- df[split,]
df_test <- df[!split,]


model <- neuralnet(Class ~ ., df_train, hidden=c(5,3), linear.output = F)
plot(model)


pred_train <- compute(model, df_train[1:4])
table(df_train$Class, round(pred_train$net.result)) # perfect accuracy in training

pred_test <- compute(model, df_test[1:4])
table(df_test$Class, round(pred_test$net.result)) # also perfect accuracy in training
