#----
library(MASS)
library(boot)
# number of subjects
nsubjects = 30

# This defines two random samples, one would be slope the other would be the mean
RE = mvrnorm(nsubjects, mu=c(0,0,0), Sigma=rbind(c(1.0, 0.7,    0),
                                                 c(0.7  , 1,  -0.5),
                                                 c(0  , -0.5  ,  1)))

colnames(RE) = c("intercept","facLow",'facHigh')  
RE = t(round(RE,2))
RE[1,] = RE[1,] - mean(RE[1,])
RE[2,] = RE[2,] - mean(RE[2,])
RE[3,] = RE[3,] - mean(RE[3,])

RE[2,] = RE[2,]*5

ntrials   = 120                              # number of repetitions / trials
data = data.frame(subject   = rep(1:nsubjects,   each=ntrials), 
                  trial = rep(1:ntrials,   times=nsubjects),
                  factor = as.factor(rep(c(0,1),length.out = ntrials*nsubjects)),
                  #factorH = as.factor(rep(c(0,1),length.out = ntrials*nsubjects)),
                  RE.i = rep(RE[,1], each=ntrials),
                  RE.sL = rep(RE[,2], each=ntrials),
                  RE.sH = rep(RE[,3], each=ntrials)
                  )

factor1Beta = c(0,1)
factor2Beta = c(0,1)

data$y       = with(data,
                      (0 + RE.i) +
                      (1+1*RE.sH)*factor1Beta[factor]+ 
                      (0+1*RE.sL)*(1-factor1Beta[factor])+
                      rnorm(n=nsubjects*ntrials, mean=0, sd=0.1)
              )
data$y = sapply(inv.logit(data$y),FUN=function(x){rbinom(1,1,x)})
#data$y       = data$y >=0


#----
library(lme4)


mres1  = glmer(y~factor+(1+factor|subject),family = binomial,data=data)
mres2  = glmer(y~factor+(1|subject)+(0+factor||subject),family = binomial,data=data)
mres3  = glmer(y~factor+(0+factor|subject),family = binomial,data=data)
