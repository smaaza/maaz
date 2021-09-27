read.csv("redwine.csv")
redwine = read.csv("redwine.csv", sep = ';')
attach(redwine)
names(redwine)
head(redwine)
dim(redwine)
summary(redwine)
stat.desc(redwine)
summary(redwine$quality)
table(redwine$quality)
redwine1=redwine[,1:12]
redwine2=log(redwine1+1)
boxplot(redwine2,xlab="Value",ylab="Parameters",main="Boxplot Presentation of different Parameters")

model1 = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine)
summary(model1)
summary(model1)$coefficient


#now considering the elements who have high t-statistic values

model2 = lm(quality~alcohol+volatile.acidity+sulphates, data = redwine)
summary(model2)
summary(model2)$coefficient
par(mfrow=c(2,2))
plot(model2)
anova(model2)
confint(model2)
qf(0.95,1,198)

#Equation ---> Quality = 2.610 +0.309alcohol +0.679sulphates -1.221volatile acidity

sigma(model2)/mean(redwine$quality)


pairs(redwine,panel=panel.smooth)
#covariance
cov(redwine, method = "p")
#correlation
cor(redwine, method = "p")
cor(alcohol, quality)
cor(sulphates, quality)

#continuous variable is alcohol

model2$residuals
plot(predict(model2),model2$residuals)
hist(model2$residuals)
predict(model2)
predict(model2,interval='confidence')
predict(model2,interval='confidence')



boxplot(alcohol~quality, ylab= "alcohol")



#validation set approach
dim(redwine)
set.seed(3)
tr = sample(1:1599,800)

train=redwine[tr , ]
test=redwine[-tr,]
dim(train)
#build the model
modelTr = lm(quality~alcohol,
            data=redwine, subset = tr)
coef(modelTr)
mean((quality -predict (modelTr ,redwine))[-tr ]^2)

library(boot)
modelTr1 = glm(quality~alcohol,
             data=redwine, subset = tr)
coef(modelTr1)

#decision trees
#changing quality variable from continuous to categorical
Highquality=ifelse(quality<=6,"No","Yes")
redwine=data.frame(redwine,Highquality)
View(redwine)
head(redwine)
str(Highquality)

#Remove Quality variable and create new data frame with Highquality Variable
redwinenew=redwine[,-12]
names(redwinenew)

library(ISLR)
library(tree)
tree_model=tree(Highquality~.,redwinenew)
plot(tree_model)
text(tree_model,pretty=0)
summary(tree_model)
#model check

tree_pred=predict(tree_model,redwinenew,type="class")
table(tree_pred,Highquality)
#misclassification error rate = 0.09944

#Cross Validation using Training and Testing data Sets

dim(redwinenew)
set.seed(3)
train=sample(1:nrow(redwinenew), 800)
redwine.train=redwinenew[train,]
redwine.test=redwinenew[-train,]
dim(redwine.train)
dim(redwine.test)
head(redwine.train)
head(redwine.test)

#Set the Highquality variable (Target Variable) in training and testing data sets

Highquality.train=Highquality[train]
Highquality.test=Highquality[-train]
list(Highquality.train)

#Build the tree model for training data

tree_model1=tree(Highquality~.,redwine.train)
plot(tree_model1)
text(tree_model1,pretty=0)
summary(tree_model1)

#testing the model accuracy 
tree_pred1=predict(tree_model1,redwine.test,type="class")


table(tree_pred1,Highquality.test)
#missclassification error rate is 0.0512


#cross validation
set.seed(3)
cv.redwine=cv.tree(tree_model1,FUN=prune.misclass)
names(cv.redwine)
cv.redwine
par(mfrow=c(1,1))
plot(cv.redwine$size, cv.redwine$dev, type = "b")

#pruning the tree
prune.redwine=prune.misclass(tree_model1,best=4)
plot(prune.redwine)
text(prune.redwine,pretty=0)
table(tree_pred1,Highquality.test)


yhat<-predict(prune.redwine, newdata = redwine[-train,])
redwine_test<-redwine[-train,"quality"]
MSE<-mean((yhat-redwine_test)^2)
MSE
sqrt(MSE)
#PCA
apply(redwine[,1:11], 2, mean)
apply(redwine[,1:11], 2, var)
obj = prcomp(redwine[,1:11]) # perform PCA
par(mfrow=c(1,1))
plot(obj$x[,1:11], col=unclass(redwine$quality)+1,
     pch=16, asp=1)
obj
names(obj)
obj$sdev
obj$rotation
obj$center
obj$scale
dim(obj$x)
obj$x
screeplot(obj)
biplot(obj)

obj1 = prcomp(redwine[,1:11], scale. = TRUE)
obj1$rotation
plot(obj1$x[,1:11], col=unclass(redwine$quality)+1,
     pch=16, asp=1)
biplot(obj1)
summary(obj1)
obj1$sdev
obj1.var=obj1$sdev^2
obj1.var
pve = obj1.var/sum(obj1.var)
pve

plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance",
     ylim = c(0,1), type = 'b')
