library(tableone)
library(ipw)
library(sandwich) #for robust variance estimation
library(survey)
library(MatchIt)
data(lalonde)

logit <- function(p) {log(p)-log(1-p)}
xvars<-c("age","educ","black","hispan","married","nodegree",
         "re74","re75")

psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree +
                 re74 + re75, data = lalonde, family = binomial(link = "logit"))

ps <- predict(psmodel, type = "response")

weight<-ifelse(lalonde$treat==1,1/(ps),1/(1-ps))
weighteddata<-svydesign(ids = ~ 1, data =lalonde, weights = ~ weight)

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treatment", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

#############################
#alternative: use ipw package
#############################

#first fit propensity score model to get weights
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~  age + educ + black + hispan + married + 
                        nodegree + re74 + re75, data=lalonde)
#numeric summary of weights
summary(weightmodel$ipw.weights)
lalonde$wt<-weightmodel$ipw.weights
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                    data =lalonde)))
coef(msm)
confint(msm)

#first fit propensity score model to get weights with trunc
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~  age + educ + black + hispan + married + 
                        nodegree + re74 + re75, data=lalonde, trunc = 0.01)

summary(weightmodel$weights.trun)
lalonde$wt<-weightmodel$weights.trunc
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                data =lalonde)))
coef(msm)
confint(msm)
