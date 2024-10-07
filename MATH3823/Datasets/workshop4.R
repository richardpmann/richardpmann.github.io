## Workshop 4, 24/3/22 - Examples of logistic regression

#######################################################################
## 1. Prediction of premature birth, using data from the 'UsingR' library

## Does mother's BMI or smoking affect likelihood of premature birth?

babies = read.table("babies.txt", header=T)

summary(babies) # Note strange maxima

## Prepare data set for analysis
babies2 = subset(babies, 
              subset = gestation < 999 & wt1 < 999 & ht < 99 & smoke < 9,
              select = c("gestation", "smoke", "wt1", "ht"))

summary(babies2)

## Premature if gestation < 37 full weeks
babies2$prem = as.numeric(babies2$gestation < 7*37)

## BMI = weight (kg) / height (cm) squared
babies2$bmi = (babies2$wt1/2.2) / (babies2$ht*2.54/100)^2

babies2$smoke = as.factor(babies2$smoke)

## Eploratory data analysis

summary(babies2)

attach(babies2)
table(prem)

## BMI

lapply(split(bmi, prem), summary)

par(mfrow=c(1,2))
hist(bmi[prem==0], xlab="BMI", main="Full term", xlim=c(15,42))
hist(bmi[prem==1], xlab="BMI", main="Premature", xlim=c(15,42))
par(mfrow=c(1,1))

## Comment on any differences

## (Extension: Kernel density estimates)
kdef = density(bmi[prem==0]); kdep = density(bmi[prem==1])
plot(kdef, xlim=range(kdef$x, kdep$x), ylim = range(kdef$y, kdep$y), xlab="BMI", main="")
lines(kdep, col="blue", lty=2)
legend(x="topright", lty=c(1,2), col=c("black", "blue"), legend = c("Full term", "Premature"))

## Smoking status: does mother smoke? Coding is
##  0 = never,
##  1 = smokes now,
##  2 = until current pregnancy,
##  3 = once did, not now,
##  9 = unknown

table(prem, smoke)
round(prop.table(table(prem, smoke), 1), 3)*100
round(prop.table(table(prem, smoke), 2), 3)*100

## Comment on any differences

fit.0 = glm(prem ~ 1, family=binomial)
fit.b = glm(prem ~ bmi, family=binomial)
fit.s = glm(prem ~ smoke, family=binomial)
fit.bs = glm(prem ~ bmi + smoke, family=binomial)
fit.bsi = glm(prem ~ bmi * smoke, family=binomial)

devs = c(deviance(fit.0), deviance(fit.b), deviance(fit.s), deviance(fit.bs), deviance(fit.bsi))
degs = c(df.residual(fit.0), df.residual(fit.b), df.residual(fit.s), df.residual(fit.bs), df.residual(fit.bsi))

tmp = cbind(devs, degs)
rownames(tmp) = c("Null", "B", "S", "B+S", "B*S")
tmp
qchisq(0.05, df=1:5, lower.tail=F)

## Which model do you prefer?

## Visualise main effects model

summary(fit.bs)

b = seq(from = min(bmi), to = max(bmi), length=201)
nd = data.frame(bmi = b, smoke = as.factor(rep("0", 201)))
fv0 = predict(fit.bs, type="response",
              data.frame(bmi = b, smoke = as.factor(rep("0", 201))))
fv1 = predict(fit.bs, type="response",
              data.frame(bmi = b, smoke = as.factor(rep("1", 201))))
fv2 = predict(fit.bs, type="response",
              data.frame(bmi = b, smoke = as.factor(rep("2", 201))))
fv3 = predict(fit.bs, type="response",
              data.frame(bmi = b, smoke = as.factor(rep("3", 201))))
plot(b, fv0, type="l", ylim=range(fv0, fv1, fv2, fv3), xlab = "BMI", ylab = "Pr(premature)", lwd=3)
lines(b, fv1, lty=2, col=2, lwd=2)
lines(b, fv2, lty=3, col=3, lwd=2)
lines(b, fv3, lty=4, col=4, lwd=2)
legend(x="topleft", lty=1:4, col=1:4, lwd=2, 
       legend = c("smoke = 0", "smoke = 1", "smoke = 2", "smoke = 3"))

detach(babies2)
rm(list=ls())

#######################################################################
## Spam emails

## 5,000 'spam' emails sent with either first name or offer in
## subject.  Number of opened emails were tracked.  Do these factors
## affect the chances of a spam email being opened?

y = c(120, 45, 34, 8)
m = c(1250, 1250, 1250, 1250)
fname = factor(c(1, 1, 0, 0))
offer = factor(c(1, 0, 1, 0))

spam = data.frame(y, m, fname, offer)
spam

ym = cbind(y, m-y)
fit.fo = glm(ym ~ fname + offer, family=binomial)
summary(fit.fo)
fitted(fit.fo, type="response")

spam.data = matrix(y/m, byrow=T, ncol=2)
rownames(spam.data) = c("fname T", "fname F")
colnames(spam.data) = c("offer T", "offer F")
spam.data

spam.fv = matrix(fitted(fit.fo, type="response"), byrow=T, ncol=2)
rownames(spam.fv) = c("fname T", "fname F")
colnames(spam.fv) = c("offer T", "offer F")
round(spam.fv, 4)
