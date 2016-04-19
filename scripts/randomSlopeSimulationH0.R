#----
library(MASS)
library(boot)
library(lme4)
res1 = NULL
res2 = NULL

for(k in 1:100){
# number of subjects
nsubjects = 30

# This defines two random samples, one would be slope the other would be the mean
RE = mvrnorm(nsubjects, mu=c(0,0), Sigma=rbind(c(1.0, 0.6),
                                               c(0.6, 1.0) ))

colnames(RE) = c("intercept","slope");  t(round(RE,2))
#RE = sweep(RE,2,apply(RE,2,mean),FUN='-')

ntrials   = 60                              # number of repetitions / trials
data = data.frame(subject   = rep(1:nsubjects,   each=ntrials), 
                  trial = rep(1:ntrials,   times=nsubjects),
                  factor1 = as.factor(rep(c(0,1),length.out = ntrials*nsubjects)),
                  factor2 = as.factor(rep(c(0,1,2),length.out = ntrials*nsubjects)),
                  RE.i = rep(RE[,1], each=ntrials),
                  RE.s = rep(RE[,2], each=ntrials)
)

factor1Beta = c(-0.3,0.3)
factor2Beta = c(-3,0,1)
data$y       = with(data,
                    (-1 + RE.i) +
                      (0+1*RE.s)*factor1Beta[factor1]+ 
                      factor2Beta[factor2] +
                      0#rnorm(n=nsubjects*nj, mean=0, sd=0.1)
)
data$y = sapply(inv.logit(data$y),FUN=function(x){rbinom(1,1,x)})
#data$y       = data$y >=0


#----

mres1  = glmer(y~factor1+factor2+(1|subject),family = binomial,data=data)
mres2 = glmer(y~factor1+factor2+(1+factor1|subject),family = binomial,data=data)
res1 = rbind(res1,coef(summary(mres1)))
res2 = rbind(res2,coef(summary(mres2)))
}

sum(res1[seq(2,length(res1)/4,4),4]> 0.05)
sum(res2[seq(2,length(res2)/4,4),4]> 0.05)
