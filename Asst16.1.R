read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Training/Features_Variant_1.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_Variant_1)
FV1 <- Features_Variant_1
load("E:/kamagyana/Computing/DARET/R-code-submissions/Asst15.1.new.RData")
library(glmnet)
install.packages("glmnet")
library(glmnet)
install.packages("glmnet")
xFV1 <- model.matrix(ncH~., FV1)[,-54]
yFV1 <- FV1$ncH
ridge.FV1 <- glmnet(xFV1, yFV1,family="gaussian",alpha=0,lambda=grid)
pred.ridge = predict(ridge.FV1,xFV1,type="link")
mse.ridge <- mean((yFV1 - pred.ridge)^2)
mse.ridge
lasso.FV1 <- glmnet(xFV1, yFV1, family = "gaussian",alpha=1,lambda=grid)
pred.lasso = predict(lasso.FV1,xFV1,type="link")
mse.lasso <- mean((yFV1 - pred.lasso)^2)
mse.lasso
elastic.FV1 <- glmnet(xFV1, yFV1, family = "gaussian", alpha=0.5, lambda=grid)
pred.elastic <- predict(elastic.FV1,XFV1, type = "link")
pred.elastic <- predict(elastic.FV1,xFV1, type = "link")
mse.elastic <- mean((yFV1 - pred.elastic)^2)
mse.elastic
ncol(test)
colnames(test)[,53:55]
colnames(test)[c(53:55)]
test <- test[,-55]
ncol(test)
xtest <- model.matrix(ncH~., test)[,-54]
ytest <- test$ncH
testpred.ridge <- predict(ridge.FV1,xtest, type="link")
testpred.lasso <- predict(lasso.FV1,xtest, type = "link")
testpred.elastic <- predict(elastic.FV1, xtest, type = "link")
mse.testpred.ridge <-mean((ytest - testpred.ridge)^2)
mse.testpred.lasso <-mean((ytest - testpred.lasso)^2)
mse.testpred.elastic <-mean((ytest - testpred.elastic)^2)
msemodel1
msemodel2
msemodel3
msemodel4
msemodel6
msemodel5
msetab <- c(mse.ridge,mse.lasso, mse.elastic,mse.testpred.ridge,mse.testpred.lasso,mse.testpred.elastic,msemodel1,msemodel2,msemodel3,msemodel4,msemodel5)
plot(msetab)
msetab <- as.data.frame(msetab)
class(msetab)
ncol(msetab)
msetab <- as.matrix(msetab,byrow=FALSE,NROW=1)
msetab
nrow(msetab)
ncol(msetab)
rownames(msetab) <-c ("mse.ridge","mse.lasso","mse.elastic","mse.testpred.ridge","mse.testpred.lasso","mse.testpred.elastic","msemodel","msemodel2","msemodel3","msemodel4","msemodel5")
msetab
colnames(msetab) <- c("mse")
msetab
plot(msetab)
msetab <- as.data.frame(msetab)
ncol(msetab)
msetab
str(msetab)
nrow(msetab)
plot(msetab)
plot(msetab, x=rownames, y=mse)
msetab <- as.matrix(msetab)
msetab
colnames(msetab)
rownames(msetab)
plot(msetab, xlab = "model", ylab = "MSE", type = "b", main =  "MSE of various Models",xaxt = "n",ylim = c(700,14000));axis(1, at=1:11, labels=rownames(msetab),las=2)


