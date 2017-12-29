model=read.csv("c:/users/ly/desktop/model.csv",header = T)
sum(train$status)
length(model$status)-sum(model$status)
str(model)
model$status=factor(model$status)
model$R=factor(model$R)
str(train)
table(train$status)

library(caTools)
set.seed(11)
split=sample.split(model$status,SplitRatio = 0.75)
train=subset(model,split==T)
test=subset(model,split==F)

fit=glm(status~.,family = binomial,data=train)
summary(fit)
fitted(fit)

predict=predict(fit,type = "response")

table(train$status,predict>0.5)

library(ROCR)
pred=prediction(predict,train$status)
performance=performance(pred,'tpr','fpr')
plot(performance,colorize=T)

help(colorize)

data=cbind(train$status,predict)

stat=as.numeric(as.character(train$status))
predict1=as.numeric(predict)
str(data)
data=as.data.frame(cbind(stat,predict1))
#KS
score=unique(predict1)
score1=sort(score,decreasing = F)
t.gr=numeric(length=length(score1))
t.br=numeric(length=length(score1))
for (i in 1:length(score1))
{
  t.br[i]=sum(data$stat[which(data$predict1<=score1[i])])/sum(data$stat)
  t.gr[i]=(length(data$stat[which(data$predict1<=score1[i])])-sum(data$stat[which(data$predict1<=score1[i])]))/(length(data$predict1)-sum(data$stat))
  
}
par(mfrow=c(1,1))
plot(score1,t.gr,xlab = "score", ylab = "用户比例")
points(score1,t.br)
text(550,0.6,"KS值为0.288")
text(550,0.4,"拒绝33.4%好用户同时，拒绝62.2%坏用户")
abline(v=387)
t.ks=max(t.gr-t.br)
co=score1[which(t.gr-t.br==t.ks)]
t.gr[which(t.gr-t.br==t.ks)]
t.br[which(t.gr-t.br==t.ks)]
table(train$status,predict>co)

test.predict=predict.glm(fit,newdata = test,type = "response")
table(test$status,test.predict>co)

pred1=prediction(test.predict,test$status)
performance1=performance(pred1,'tpr','fpr')
plot(performance1,colorize=T)

help("performance")
