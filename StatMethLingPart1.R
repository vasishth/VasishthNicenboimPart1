## ----setup,include=FALSE,cache=FALSE-------------------------------------
library(knitr)
library(coda)

# set global chunk options, put figures into folder
options(replace.assign=TRUE,show.signif.stars=FALSE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=75)
#opts_chunk$set(dev='postscript')
opts_chunk$set(dev='pdf')

options(digits = 2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(9991)
library(dplyr)

# save workspace image, if you want
#the.date <- format(Sys.time(), "%b%d%Y")
#save.image(file=paste0("homework01-",the.date,".RData")

# knit_hooks$set(source = function(x, options) {
#     paste("\\begin{lstlisting}[numbers=left]\n", x, 
#         "\\end{lstlisting}\n", sep = "")
# })

## ----echo=FALSE----------------------------------------------------------
set.seed(987654321)
ndraws <- 1000
mean1 <- 6
mean2 <- 5.99
SD <- 0.8
x<-rlnorm(ndraws,mean=mean1,sd=SD) -  rlnorm(ndraws,mean=mean2,sd=SD)
sim_diff<-data.frame(dat=x)

## ----fig01histogramsimRTs,echo=FALSE,fig.height=4,fig.width=5------------
#seed(987654321)
library(ggplot2)
ggplot(sim_diff,aes(x=dat)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth=200, fill="white", colour="black") +
  xlab("difference in reading times (ms)")+
  ylab("density")+theme_bw() 

## ----echo=FALSE----------------------------------------------------------
barx<-mean(x)
sdx<-sd(x)
obs_t<-barx/(sdx/sqrt(1000))

## ----echo=FALSE,include=FALSE--------------------------------------------
pval<-pt(obs_t,df=999,lower.tail=FALSE)+
  pt(-obs_t,df=999,lower.tail=TRUE)
## with n> 20 the t-distribution and normal are 
## effectively identical:
#pnorm(obs_t,lower.tail=FALSE)+
#  pnorm(-obs_t,lower.tail=TRUE)

## ----fig02tdistrn,echo=FALSE,fig.height=4,fig.width=5--------------------
xseq<-seq(-4,4,by=0.01)
p<-qplot(xseq, dt(xseq,df=1000-1), geom="line")
obst<-data.frame(x=obs_t,y=0)
p+geom_point(data=obst,aes(x,y),colour="black",size=4)+geom_point(data=obst,aes(-x,y),colour="gray",size=4) + xlab("t") + ylab("density")+theme_bw() + scale_x_continuous(minor_breaks=seq(-4,4,1),breaks=seq(-4,4,1))


## ----echo=FALSE----------------------------------------------------------
# library(reshape)
## expt 2
CP1<-read.table("husainetalexpt2critdata.txt",header=T)
#head(CP1)
#xtabs(~cond+exp,CP1)  # strong weak
#xtabs(~cond+dist,CP1) # short long

# data.rs <- melt(CP1, id=c("cond","subj"), measure=c("rt"),na.rm=TRUE)

# data.id <- data.frame(cast(data.rs, subj + cond ~ ., function(x) c(rt=mean(x), N=length(x) ) ))

data.id <- summarise(group_by(CP1, subj,cond),rt=mean(rt), N=length(rt))

conda<-subset(data.id,cond=="a")
condb<-subset(data.id,cond=="b")
condc<-subset(data.id,cond=="c")
condd<-subset(data.id,cond=="d")

## first significant
abtest<-t.test(log(conda$rt)-log(condb$rt))
# second not significant
cdtest<-t.test(log(condc$rt)-log(condd$rt))

ab<-log(conda$rt)-log(condb$rt)
ab_diff<-mean(log(conda$rt))-mean(log(condb$rt))
se_ab<-sd(ab)/sqrt(60)
cd<-log(condc$rt)-log(condd$rt)
cd_diff<-mean(log(condc$rt))-mean(log(condd$rt))
se_cd<-sd(cd)/sqrt(60)

vcovmat<-var(cbind(ab,cd))
covar<-vcovmat[1,2]
#se_ab^2 + se_cd^2 - 2*covar
  
int<-log(conda$rt)-log(condb$rt)-(log(condc$rt)-log(condd$rt))
se_int<-sd(int)/sqrt(60)

inttest<-t.test(int)

## for plotting:
rt<-c(mean(conda$rt),mean(condb$rt),mean(condc$rt),mean(condd$rt))
se<-c(sd(conda$rt)/sqrt(60),sd(condb$rt)/sqrt(60),sd(condc$rt)/sqrt(60),sd(condd$rt)/sqrt(60))
Distance<-factor(c(rep(c("long","short"),2)),levels=c("short",
                                                   "long"))
Predicate_Type<-factor(c(rep(c("complex","simple"),each=2)),levels=c("complex","simple"))

dat<-data.frame(Predicate_Type=Predicate_Type,Distance=Distance,rt=rt,se=se)


## ----fig03husainetal,echo=FALSE,fig.height=4,fig.width=5-----------------
ggplot(dat, aes(x=Predicate_Type, y=rt, fill=Distance)) +
        geom_bar(position="dodge", stat="identity")+
geom_errorbar(aes(ymin=rt-se, ymax=rt+se), 
              position=position_dodge(0.9),width=.2)+
annotate("text", x=1, y=900, label="*", family="serif",
                 fontface="italic", colour="black", size=10)+
annotate("text", x=2, y=1000, label="n.s.", family="serif",
                 fontface="italic", colour="black", size=5)+
  xlab("Predicate Type")+ylab("Reading time (ms)")+theme_bw()+
  scale_fill_grey(start = 0, end = .9)


## ----fig04typeItypeII,echo=FALSE,fig.height=4----------------------------
library(ggplot2)
null <- with(density(rnorm(10000,0,1),adjust=2,from=-6, to=6),data.frame(mu=x,density=y,dist="null"))
two <- with(density(rnorm(10000,2,1),adjust=2,from=-6, to=6),data.frame(mu=x,density=y,dist="two"))

dens <- rbind(two,null)


ggplot(data=dens,aes(x=mu,y=density,group=dist)) +geom_line()+
  geom_ribbon(data=subset(dens, mu > 2 & dist=="null" ),aes(ymax=density,fill=dist),ymin=0,
              fill="black",colour=NA,alpha=0.7)+
  geom_ribbon(data=subset(dens, mu < -2 & dist=="null" ),aes(ymax=density,fill=dist),ymin=0,
              fill="black",colour=NA,alpha=0.7)+
  geom_ribbon(data=subset(dens, mu < 2 & dist=="two" ),aes(ymax=density,fill=dist),ymin=0,
              fill="gray",colour=NA,alpha=1)+ theme_bw() + geom_vline(xintercept = c(-2,2),linetype="dashed")+
  ggtitle("Type I, II Error")+ scale_x_continuous(limits=c(-6,6),breaks=seq(-6,6,2))+xlab("")+ylab("Density") +
  geom_line(data=subset(dens, mu > 2 & dist=="two" ),color="gray") +
  geom_line(data=subset(dens, mu < -2 & dist=="two" ),color="gray")


## ----fig05powerfunction,echo=FALSE,fig.height=4,fig.width=5--------------
Ds<-seq(-100,100,by=0.01)
Power<-power.t.test(d=Ds,sd=40,n=10)$power
dpower <- data.frame(Ds=Ds,Power= Power)
ggplot(dpower ,aes(Ds, Power)) +geom_line() + theme_bw()+xlab(expression(mu))

## ----fig06powerfunction2,echo=FALSE,fig.height=4,fig.width=5-------------
N<-seq(10,500,by=1)
Power<-power.t.test(d=10,sd=40,n=N)$power
dpower <- data.frame(N=N,Power= Power)
ggplot(dpower ,aes(N, Power)) +geom_line() + theme_bw()

## ----dgw,cache=TRUE,include=FALSE----------------------------------------
dgw <- read.table("gibsonwu2012data.txt")
length(unique(dgw$item))
length(unique(dgw$subj))
dgw_hn <- subset(dgw,subset=region=="headnoun")

mean(dgw_hn$rt)
log(550 +22) - log(550)
length(unique(dgw_hn$subj))
N_subj <- 40
N_item <- 16


## ----typesandm,cache=TRUE,include=FALSE----------------------------------

source("sim_data.R")

#characterstics of the data mimic Gibson and Wu's data (except for the correlation for r.e.):
contrasts(dgw_hn$type) <- contr.sum(2)
library(lme4)
summary(mgw<- lmer(log(rt)~ type + (type|subj)+(type|item),data=dgw_hn
    ))
            

alpha <- coef(summary(mgw))["(Intercept)","Estimate"]
 #intercept
beta <- 0.01
sdev_subj <-c(attributes(VarCorr(mgw)$subj)$stddev[1],attributes(VarCorr(mgw)$subj)$stddev[2]) 
sdev_item <- c(attributes(VarCorr(mgw)$item)$stddev[1],attributes(VarCorr(mgw)$item)$stddev[2]) 
sdev <- sigma(mgw)               
rho_subjs <- .3
rho_items <- .03



## Here I generate nsim datasets
#nsim<-10000
nsim<-1000
drep <- plyr::llply(1:nsim,function(x) data_2cond(N_subj,N_item,alpha,beta,sdev_subj,sdev_item,sdev,rho_subjs,rho_items))

 
coefs <- plyr::ldply(drep, function(d){
    # m <- lmer(log(rt) ~ c + (1 + code|subj) +(1 + code|item), d)
    m <- lmer(log(rt) ~ c + (1 |subj) +(1|item), d)
    t <- coef(summary(m))["c1","t value"]
    beta <- coef(summary(m))["c1","Estimate"]
    #did it converge?
    conv <- ifelse(is.null(m@optinfo$conv$lme4$code),1,0)
    data.frame(t=t,beta=beta,converged=conv)
} )

# How many models didn't converge?
sum(coefs$converged==0)

(pow<- mean(abs(coefs$t)>2))

## which cells in drep are significant at alpha=0.05?
signif<-which(abs(coefs$t)>2)

(types_sig<-mean(coefs[signif,]$t<0))

(types_nonsig<-mean(coefs[-signif,]$t<0))
## Type S error rate | non-signif

(typem_sig<-mean(abs(coefs[signif,]$beta)/beta))
## Type M error rate | signif

(typem_nonsig<-mean(abs(coefs[-signif,]$beta)/beta))
## Type M error rate | not-signif


## ----fig07funnelplot,echo=FALSE,,fig.height=4,fig.width=5----------------
data<-read.table("MScDissData.txt",header=T)

## reorder by increasing y:
data<-data[with(data,order(y)),]

## add sd:
data$sd<-data$se*sqrt(data$n)
meansd<-mean(data$sd,na.rm=T)

## just "imputing" values that are missing:
which.na<-which(is.na(data$se))

data$se[which.na]<-meansd/sqrt(data$n[which.na])

## no. of studies:
n<-dim(data)[1]

# sd increases with sample size
data.xtab<-data[,c(3,4,5,7,8,9,10,11)]
rownames(data.xtab)<-1:n

data2<-data.frame(rt=data$y,se=data$se)

ggplot(data2, aes(x=rt, y=1/se^2)) + geom_point(size=4, shape=21,colour="gray", fill="gray")+geom_vline(xintercept=mean(data2$rt)) + ylab("precision")+xlab("reading time (ms)")  +theme_bw()+scale_fill_grey(start = 0, end = .9)

## ----betas,cache=FALSE,include=FALSE-------------------------------------
betas <- c(0.01,0.02,0.03,.05,.1)
showbetas <- paste(betas,collapse=", ")
N_subj_s <- 30
N_item_s <- 16

N_subj_m <-80
N_item_m <- 40

nsim<- 200

## ----fig08betasim,cache=TRUE,include=FALSE,eval=TRUE---------------------
#eval is set to FALSE, so this won't run. The results are saved.

#nsim<- 20 #BRUNO: remove

M_s <- plyr::ldply(betas, function(b){
    drep <- plyr::llply(1:nsim,function(x) data_2cond(N_subj_s,N_item_s,alpha,beta=b,sdev_subj,sdev_item,sdev,rho_subjs,rho_items))
    
    coefs <- plyr::ldply(drep, function(d){
        m <- lmer(log(rt) ~ c + (1 |subj) +(1|item), d)
        t <- coef(summary(m))["c1","t value"]
        beta <- coef(summary(m))["c1","Estimate"]
        conv <- ifelse(is.null(m@optinfo$conv$lme4$code),1,0)
        data.frame(t=t,beta_hat=beta,converged=conv,beta=b)
    } )
    return(coefs)
})
M_s$size <- "small"

M_m <- plyr::ldply(betas, function(b){
    drep <- plyr::llply(1:nsim,function(x) data_2cond(N_subj_m,N_item_m,alpha,beta=b,sdev_subj,sdev_item,sdev,rho_subjs,rho_items))
    
    coefs <- plyr::ldply(drep, function(d){
        m <- lmer(log(rt) ~ c + (1 |subj) +(1|item), d)
        t <- coef(summary(m))["c1","t value"]
        beta <- coef(summary(m))["c1","Estimate"]
        conv <- ifelse(is.null(m@optinfo$conv$lme4$code),1,0)
        data.frame(t=t,beta_hat=beta,converged=conv,beta=b)
    } )
    return(coefs)
})
M_m$size <- "medium"

M <- rbind(M_s,M_m)
print(M_s)
print(M_m)
print(M)

# save(M,file="sim_2expt.Rda")


 # groupedM<-group_by(M,beta,size)
 # head(groupedM)
 # powers<-c(with(subset(groupedM,size=="small"),mean(abs(t)>2)),
 # with(subset(groupedM,size=="medium"),mean(abs(t)>2)))
 # powers<-data.frame(size=factor(c("small","medium")), app_power=powers)


## bruno's code is broken:
powers <- dplyr::summarise(dplyr::group_by(M,beta,size),app_power =mean(abs(t)>2))


M_sig<- M[abs(M$t)>2,]

M_sig$size <- factor(M_sig$size,levels=c("small","medium"))

betas_x_axis <- paste(betas, paste("(",round(exp(6+betas)-exp(6),0)," ms)",sep=""),sep="\n")
y_breaks <- seq(-.1,.2,.02)
beta_hat_y_axis <- paste(y_breaks, paste("(",round(exp(6+y_breaks)-exp(6),0)," ms)",sep=""),sep=" ")

p <- ggplot(data=M_sig, aes(y=beta_hat,x=beta,color=size)) +
scale_x_continuous(breaks=betas,labels=betas_x_axis,name="True effect size")+
scale_y_continuous(breaks=y_breaks ,labels=beta_hat_y_axis,name="Estimate") +
 geom_jitter(position = position_jitter(height = 0,width=0.005))+
 geom_abline(intercept = 0, slope = 1,linetype="dashed")+
  scale_color_manual(values=c("black","gray"),name="Experiment" )+
 #scale_color_manual(values=c("#5e3c99","#e66101"),name="Experiment" )+
theme(text = element_text(size=12)) +theme_bw()

#annotation of the power:
#p <- p+annotate("text", x =  betas, y = c(rep(.12,4),.18) , label = round(powers[powers$size=="small",]$app_power,2),color="#5e3c99",size=5)

p <- p+annotate("text", x =  betas, y = c(rep(.12,3),.14,.18) , label = round(powers[powers$size=="small",]$app_power,2),color="black",size=5)

#p <- p+annotate("text", x =  betas, y = c(rep(.005,3),.008,.03) , label = round(powers[powers$size=="medium",]$app_power,2),color="#e66101",size=5)


p <- p+annotate("text", x =  betas, y = c(rep(.005,4),.03) , label = round(powers[powers$size=="medium",]$app_power,2),color="gray",size=5)



ggsave(p,file="power.pdf",dpi=1200,width= 17.6,height= 17.6 ,unit="cm")

## ----fig09observedpower,echo=FALSE---------------------------------------
D<-seq(1,250,by=1)
## SE from a study:
se<-46
## sample size
n<-37
## typical SD:
stddev<-se*sqrt(n)

## rejection value (absolute) under null:
qnull<-abs(qnorm(0.025,mean=0,sd=se))

## sample repeatedly to get an estimated d:
nsim<-10000

pii<-true_power<-rep(NA,length(D))
pii_CI<-matrix(rep(NA,length(D)*2),ncol=2)

## for each D:
for(j in 1:length(D)){
  currentD<-D[j]
  ## repeatedly sample to compute obs power:
  drep<-rep(NA,nsim)
  for(i in 1:nsim){
     drep[i]<-mean(rnorm(n,mean=currentD,sd=stddev))
  }

  ## actual power for currentD under repeated sampling:
  true_power[j]<-pow<-mean(ifelse(abs(drep/se)>2,1,0))
  ## observed power from *each* repeated study:
  obspower<-pnorm(qnull,mean=drep,sd=se,lower.tail=FALSE)+
  pnorm(-qnull,mean=drep,sd=se,lower.tail=TRUE)
  ## power inflation index:
  pii[j]<-mean(obspower/pow)
  ## uncertainty bounds of PII:
  pii_CI[j,]<-quantile(obspower/pow,probs=c(0.025,0.975))
}

powerinflation<-data.frame(x=true_power,y=pii,lower=pii_CI[,1],
           upper=pii_CI[,2])

powerinf<-ggplot(data=powerinflation,
                aes(x=x,y=y))+
  geom_errorbar(data=powerinflation,aes(ymin=lower,
                                        ymax=upper), 
                width=.01,colour="gray")+
    geom_point(size=1, shape=20, fill="white") +
  geom_hline(yintercept=1,linetype="dashed")+
  ylab("Ratio of Estimated Power to True Power")+
  xlab("True Power")+ggtitle("How power can be overestimated")+
  ## don't display posterior of theta:
#    annotate("pointrange", x = 67.2, y = post[2], ymin = post[1], ymax = post[3],colour = "red", size = .1)+
#    annotation_custom(g, ymin=-100, ymax=-50, xmin=64, xmax=65) +
    #coord_flip()  + 
  theme_bw()
powerinf

## ----fig10simulatedpval,echo=FALSE,fig.height=4,fig.width=5--------------
set.seed(987654321)
library(ggplot2)
ndraws <- 1000
mean1 <- 6
mean2 <- 5.9
mean3 <- 5.96
SD <- 0.6

pvalues<- plyr::ldply(1:1000, function(i){
    highpower<-rlnorm(ndraws,mean=mean1,sd=SD) - 
                rlnorm(ndraws,mean=mean2,sd=SD)

    lowpower<-rlnorm(ndraws,mean=mean1,sd=SD) - 
                rlnorm(ndraws,mean=mean3,sd=SD)

    null<-rlnorm(ndraws,mean=mean1,sd=SD) - 
                rlnorm(ndraws,mean=mean1,sd=SD)
 
 rbind(data.frame(p= t.test(highpower)[]$p.value,exp="High power"),
    data.frame(p= t.test(lowpower)[]$p.value,exp="Low power"),
   data.frame(p= t.test(null)[]$p.value,exp="Null is true"))
   
})

pvalues$exp <- factor(pvalues$exp, levels =c("Null is true","Low power","High power"))
summaryp <- summarise(group_by(pvalues,exp),power=mean(p<.05))

ggplot(pvalues,aes(x=p)) + geom_histogram(bins = 30, fill="white", colour="black", origin=0) + theme_bw() + facet_grid(.~exp) + scale_x_continuous(name="p-value",breaks=seq(0,1,.2))


## ----stoppingrules,echo=FALSE--------------------------------------------
##Standard:
pvals<-NULL
tstat_standard<-NULL
n<-10
nsim<-10000
## assume a standard dev of 1:
stddev<-1
mn<-0
for(i in 1:nsim){
  samp<-rnorm(n,mean=mn,sd=stddev)
  pvals[i]<-t.test(samp)$p.value
  tstat_standard[i]<-t.test(samp)$statistic
}

## ----echo=FALSE----------------------------------------------------------
## Type I error rate: about 5% as theory says:
#table(pvals<0.05)[2]/nsim

## ----stoppingrules2,echo=FALSE-------------------------------------------
pvals<-NULL
tstat<-NULL
## how many subjects can I run?
upper_bound<-n*6

## ----echo=FALSE----------------------------------------------------------
for(i in 1:nsim){
  significant<-FALSE 
  x<-rnorm(n,mean=mn,sd=stddev) ## take sample
while(!significant & length(x)<upper_bound){
  ## if not significant:
if(t.test(x)$p.value>0.05){
  x<-append(x,rnorm(n,mean=mn,sd=stddev)) ## get more data
} else {significant<-TRUE}   ## otherwise stop:
}
pvals[i]<-t.test(x)$p.value
tstat[i]<-t.test(x)$statistic
}

## ----echo=FALSE----------------------------------------------------------
## Type I error rate: much higher than 5%:
#table(pvals<0.05)[2]/nsim

## ----fig11stoppingrules3,echo=FALSE,fig.height=4-------------------------


normal <- with(density(tstat_standard,adjust=2,from=-6, to=6),data.frame(mu=x,density=y,dist="normal"))
bump <- with(density(tstat,adjust=2,from=-6, to=6),data.frame(mu=x,density=y,dist="bump"))

dens <- rbind(normal,bump)


ggplot(data=dens,aes(x=mu,y=density,color=dist)) +geom_line()+ scale_color_manual(values=c("black","gray"),guide=FALSE)+ theme_bw() + geom_vline(xintercept = c(-2,2),linetype="dashed")+
  ggtitle("Type I, II Error")+ scale_x_continuous(limits=c(-6,6),breaks=seq(-6,6,2))+xlab("")+ylab("Density") 


## ----gibsonwuttest,echo=FALSE,eval=TRUE,include=FALSE--------------------
dgw_hn <- subset(dgw, subset = region == "headnoun")
dgw_hn$cond <- ifelse(dgw_hn$type == "obj-ext", 1, -1)
head(dgw_hn)
library(lme4)
## "maximal" model as recommended by Barr et al:
mgw<-lmer(rt~cond+(1+cond|subj)+(1+cond|item),dgw_hn)
t_lmer<-summary(mgw)$coefficients[2,3]

## t-test analyses:
dgw_hn_bysubj<-aggregate(rt~subj+type,mean,
               data=dgw_hn)
dgw_hn_byitem<-aggregate(rt~item+type,mean,
               data=dgw_hn)
## published:
t_subj<-with(dgw_hn_bysubj,t.test(rt~type,paired=TRUE))$statistic
p_subj<-with(dgw_hn_bysubj,t.test(rt~type,paired=TRUE))$p.value

t_item<-with(dgw_hn_byitem,t.test(rt~type,paired=TRUE))$statistic
p_item<-with(dgw_hn_byitem,t.test(rt~type,paired=TRUE))$p.value

