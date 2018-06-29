library(tableone)
library(Matching)
library(MatchIt)
data(lalonde)

# The potential confounding variables are: age, educ, black, hispan, married, 
# nodegree, re74, re75.
xvars <- c("age", "educ", "black", "hispan", "married" , "nodegree",
           "re74", "re75" )

#q1
table1 <- CreateTableOne(vars = xvars, strata = "treat", data = lalonde,
                         test = FALSE)
print(table1, smd = TRUE)

#q2
mean(lalonde[lalonde$treat == 1,]$re78) -mean(lalonde[lalonde$treat == 0,]$re78)

# fit propensity score model

psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree +
                 re74 + re75, family = binomial(), data = lalonde)
summary(psmodel)
pscore <- psmodel$fitted.values
#q3
summary(pscore)

# propensity score matching
#q4, q5
set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
xvars <- c("age", "educ", "black", "hispan", "married" , "nodegree",
           "re74", "re75" )

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#q6
set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE, caliper = 0.1)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
xvars <- c("age", "educ", "black", "hispan", "married" , "nodegree",
           "re74", "re75" )

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#q7
mean(matched[matched$treat == 1,]$re78) -mean(matched[matched$treat == 0,]$re78)

#q8
#outcome analysis
y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)

