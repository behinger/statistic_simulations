#----
library(MASS)
library(boot)
# number of subjects
nsubjects = 20

# This defines two random samples, one would be slope the other would be the mean
RE = mvrnorm(nsubjects, mu=c(0,0,0), Sigma=rbind(c(1.0, 0.7,    0),
                                                 c(0.7  , 1,  -0.5),
                                                 c(0  , -0.5  ,  1)))

colnames(RE) = c("intercept","facLow",'facHigh')  
RE = t(round(RE,2))
RE[1,] = RE[1,] - mean(RE[1,])
RE[2,] = RE[2,] - mean(RE[2,])
RE[3,] = RE[3,] - mean(RE[3,])

#RE[2,] = RE[2,]*5

ntrials   =200                              # number of repetitions / trials
data = data.frame(subject   = rep(1:nsubjects,   each=ntrials), 
                  trial = rep(1:ntrials,   times=nsubjects),
                  factor = as.factor(rep(c(0,1),length.out = ntrials*nsubjects)),
                  #factorH = as.factor(rep(c(0,1),length.out = ntrials*nsubjects)),
                  RE.i = rep(RE[1,], each=ntrials),
                  RE.sL = rep(RE[2,], each=ntrials),
                  RE.sH = rep(RE[3,], each=ntrials)
)


factor1Beta = c(0,0)
#factor2Beta = c(-.5,.5)

data$y       = with(data,
                    (95 + RE.i) +
                      (10+1*RE.sH)*factor1Beta[factor]+ 
                      #(0+1*RE.sL)*(1-factor1Beta[factor])+
                      rnorm(n=nsubjects*ntrials, mean=0, sd=.1)
)
#data$y = sapply(inv.logit(data$y),FUN=function(x){rbinom(1,1,x)})
#data$y       = data$y >=0

contrasts(data$factor) = contr.sum(2)

#----
library(lme4)
library(ggplot2)

mres_1  = lmer(y~1+factor+(1+factor|subject),data=data)
mres_2  = lmer(y~1+factor+(1+factor||subject),data=data)
mres_3  = lmer(y~1+factor+(0+factor|subject),data=data)
mres_4  = lmer(y~1+factor+(1|subject),data=data)

library(plyr)
data_un = ddply(data,.(subject),function(x){
  
  howmany = rank(RE[1,])[x$subject[1]] # dependent on intercept rank
 #howmany = x$subject[1] # dependent on subject number
  howmany = max(4,howmany)
  remove = sample(seq(1,length(x$y),2),min(length(x$y),howmany))#runif(x$subject[1],min=2,max=max(2,length(x$y)))
  howmany = (nsubjects+1)-rank(RE[1,])[x$subject[1]] # dependent on intercept rank
  #howmany = x$subject[1] # dependent on subject number
  #howmany = max(4,howmany)
  #remove2 = sample(seq(2,length(x$y),2),min(length(x$y),howmany))
  remove2 = remove+1
  
  
  x = x[c(remove,remove2),]
})

mres_un_1  = lmer(y~1+factor+(1+factor|subject),data=data_un)
mres_un_2  = lmer(y~1+factor+(1+factor||subject),data=data_un)
mres_un_3  = lmer(y~1+factor+(0+factor|subject),data=data_un)
mres_un_4  = lmer(y~1+factor+(1|subject),data=data_un)

rbind(fixef(mres_1),fixef(mres_2),fixef(mres_3),fixef(mres_4),fixef(mres_un_1),fixef(mres_un_2),fixef(mres_un_3),fixef(mres_un_4))
ggplot(data_un,aes(x=interaction(subject,factor),y=y,color=factor(subject),group=subject))+geom_point(position=position_jitter())

mean(data$y)
mean(data_un$y)