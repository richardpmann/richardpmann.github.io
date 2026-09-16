## Load data and collapse exposure to 0/1

gs = read.table("genestudy.txt", header=T)
gs$exp = as.numeric(gs$exp>0)
table(read.table("genestudy.txt", header=T)$exp, gs$exp)

## Create alternative version with factor variables
gsf = gs
gsf$agecat = as.factor(gsf$agecat)
gsf$gene = as.factor(gsf$gene)
gsf$exp = as.factor(gsf$exp)

## Look at deviances and degrees of freedom
attach(gsf)
fit.age = glm(case ~ agecat + gene + exp, family=binomial)
fit.ge = glm(case ~ gene + exp, family=binomial)
fit.ae = glm(case ~ agecat + exp, family=binomial)
fit.ag = glm(case ~ agecat + gene, family=binomial)
fit.a = glm(case ~ agecat, family=binomial)
fit.g = glm(case ~ gene, family=binomial)
fit.e = glm(case ~ exp, family=binomial)
fit.0 = glm(case ~ 1, family=binomial)

dev = c(deviance(fit.age), deviance(fit.ge), deviance(fit.ae), deviance(fit.ag), deviance(fit.a), deviance(fit.g), deviance(fit.e), deviance(fit.0))

deg = c(df.residual(fit.age), df.residual(fit.ge), df.residual(fit.ae), df.residual(fit.ag), df.residual(fit.a), df.residual(fit.g), df.residual(fit.e), df.residual(fit.0))

p.gof = pchisq(dev, df=deg, lower.tail=F)

tmp = cbind(dev, deg, p.gof)
rownames(tmp) = c("age", "ge", "ae", "ag", "a", "g", "e", "null")
tmp
qchisq(0.05, 1:5, lower.tail=F)


## Backwards selection

dev[4] - dev[1] # < 3.84; drop exposure
dev[5] - dev[4] # < 3.84; drop  gene
dev[8] - dev[5] # > 9.49; can't drop age

## Forwards selection will give the same result here.

## Should really investigate interaction terms.

summary(fit.a)
plot(fit.a)

## Drop levels?  

agev2 = gs$age
agev2[agev2 %in% c(1,2)] = 0
agev2[agev2 == 4] = 3
table(agev2, gs$age)
agev2f = as.factor(agev2)

fit.a2 =  glm(case ~ agev2f, family=binomial)
summary(fit.a2)
anova(fit.a, fit.a2, test="Chisq")


## Interpret fit.a

summary(fit.a)
beta = coef(fit.a)
eta = beta[1] + c(0, beta[-1]) # lin pred values for each group
pred = exp(eta) / (1 + exp(eta))
names(pred)[1] = "agecat0"
pred ## same as %ages in data:
tab = table(case, agecat)
tab2 = rbind(pred, tab[2,] / apply(tab, 2, sum))
rownames(tab2) = c("modelled", "observed")
tab2
detach(gsf)


## Grouped form, numeric
gs2 = data.frame(NULL)
attach(gs)
for(a in 0:4){
    for(g in 0:1){
        for(e in 0:1){
            tmp = gs$case[agecat==a & gene == g & exp==e]
            gs2 = rbind(gs2, c(sum(tmp), length(tmp), a, g, e))
        }
    }
}
names(gs2) = c("y", "m", "agecat", "gene", "exp")
gs2
ym = cbind(gs2$y, gs2$m-gs2$y)
detach(gs)

## Create alternative version with factor variables
gs2f = gs2
gs2f$agecat = as.factor(gs2f$agecat)
gs2f$gene = as.factor(gs2f$gene)
gs2f$exp = as.factor(gs2f$exp)

## Two models, are they the same?
summary(glm(case ~ agecat + gene + exp, family=binomial, data=gsf))
summary(glm(ym ~ agecat + gene + exp, family=binomial, data=gs2f))

pchisq(725.79, df=764, lower.tail=F)
pchisq(31.078, df=13, lower.tail=F)
742.48 - 725.79
47.775 - 31.078
