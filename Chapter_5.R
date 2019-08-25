library(ISLR)
library(boot)
set.seed(1)
#Problem 5-5
attach(Default)

err_total <- 0
for (i in seq(0, 10))
  {
  print(i)
  train=sample(length(Default[,"income"]),0.8*length(Default[,"income"]))
  glm.fit=glm(default~income+balance, data=Default ,subset=train, family=binomial)
  glm.probs=predict(glm.fit,type="response")
  
  glm.pred=rep("No",length(glm.probs))
  glm.pred[glm.probs >.5]="Yes"
  err.train <- mean(glm.pred!=default[train])
  
  # test error
  glm.probs=predict(glm.fit, Default[-train,], type="response")
  glm.pred=rep("No",length(glm.probs))
  glm.pred[glm.probs >.5]="Yes"
  err.test <- mean(glm.pred!=default[-train])
  err_total = err_total + err.test
}
(mean_error <- err_total/10)

err_total <- 0
for (i in seq(0, 10))
{
  print(i)
  train=sample(length(Default[,"income"]),0.8*length(Default[,"income"]))
  glm.fit=glm(default~income+balance+student, data=Default ,subset=train, family=binomial)
  glm.probs=predict(glm.fit,type="response")
  
  glm.pred=rep("No",length(glm.probs))
  glm.pred[glm.probs >.5]="Yes"
  err.train <- mean(glm.pred!=default[train])
  
  # test error
  glm.probs=predict(glm.fit, Default[-train,], type="response")
  glm.pred=rep("No",length(glm.probs))
  glm.pred[glm.probs >.5]="Yes"
  err.test <- mean(glm.pred!=default[-train])
  err_total = err_total + err.test
}
mean_error <- err_total/10
mean_error

# Problem 5-6

boot.fn <-function(data.scr, idx)
  {
  glm.fit=glm(data.scr$default~data.scr$income+data.scr$balance, data=data.scr ,subset=idx, family=binomial)
  dev.coeff <- summary.glm(glm.fit)$coefficient[,2]
  return (dev.coeff)
  #return (coef(glm.fit))
}

train=sample(length(Default[,"income"]),length(Default[,"income"]), replace=T)
boot.fn(Default,train)
boot(Default ,boot.fn ,1000)

#Problem 5-7
attach(Weekly)
glm.fit=glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)

err.test = 0
for (i in seq(1, length(Direction)))
{
train = seq(1,length(Direction))[-i]
glm.fit=glm(Direction~Lag1+Lag2, data=Weekly, subset = train, family=binomial)
glm.probs=predict(glm.fit, Weekly[-train,], type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs >.5]="Up"
err.test <- err.test+mean(glm.pred!=Direction[-train])
}
err.test/length(Direction)
#Problem 5-8
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

glm.fit=glm(y~x+I(x^2) ,data=data.frame(x,y))
plot(x,y)
cv.err=cv.glm(data.frame(x,y), glm.fit)
cv.err$delta

glm.fit=glm(y~x ,data=data.frame(x,y))
cv.err=cv.glm(data.frame(x,y), glm.fit)
cv.err$delta

glm.fit=glm(y~x+I(x^2) ,data=data.frame(x,y))
cv.err=cv.glm(data.frame(x,y), glm.fit)
cv.err$delta

glm.fit=glm(y~x+I(x^2)+I(x^3) ,data=data.frame(x,y))
cv.err=cv.glm(data.frame(x,y), glm.fit)
cv.err$delta

glm.fit=glm(y~x+I(x^2)+I(x^3)+I(x^4) ,data=data.frame(x,y))
cv.err=cv.glm(data.frame(x,y), glm.fit)
cv.err$delta

#Problem 5-9
library(MASS)






