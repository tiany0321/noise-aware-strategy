setwd("D:\\as-code\\1D\\noise_level estimation\\hete\\0.05")

library("DiceKriging")

train.size=0.15############Define the percentage size of training data
n.level=0.05########################the true noise level prefactor phi_real

#######Continuous Ranked Probability Score
CRPS=function(mean,sd,y){
  zz=(y-mean)/sd
  CRPS=sd*(1/(pi)^0.5-2*dnorm(zz[,1])-zz*(2*pnorm(zz[,1])-1))
  return(CRPS)
}

###########1D function
fundet <- function(x){
  return((sin(8*x)/(1+x)+2*cos(2*x)*x^3))
}
x_all <- seq(0, 1, by=0.01)
x_all=as.data.frame(x_all)
colnames(x_all)="x"

y_all=fundet(x_all)
y_all=as.data.frame(y_all)
colnames(y_all)="y"


############set heterogeneous noise to the true function value
noise=rep(n.level*(max(y_all)-min(y_all)),length(y_all))

set.seed(n.level*778)
noise.sd=rnorm(length(y_all),mean=noise,sd=0.07)

# Making noisy observations from 'fundet' function 
set.seed(584)
mea <- y_all + noise.sd*rnorm(nrow(x_all))
data_all=cbind(mea,x_all,noise.sd)
###############Define the size of training data
train.num=train.size*round(nrow(y_all))

############## Sample the training data from the all data
set.seed(train.num*11)
sample.num0<-vector()
for(i in 1:1000){
  s <-  sample(c(1:nrow(x_all)),60,replace=F,prob=NULL) 
  sample.num0<-as.data.frame(rbind(sample.num0,s))
}
sample.num<-sample.num0[,1:train.num]

############set different noise level prefactors phi
#n_level=c(0,0.01,0.02,0.03,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
n_level=c(10^(-5),0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7)
############Define data frame for calculated error
MSE_data=matrix(,200,length(n_level))
CRPS_data=matrix(,200,length(n_level))

i=1
jj=1
for(j in n_level){
  for (i in 1:200){#########error in 200 trails for each level of noise
    
    ############sample train.num pieces of data as the training dataset
    split=sample.num[i,]
    split=unlist(split)
    training.data <- data_all[split,]
    
    x=as.data.frame(training.data[,"x"])
    colnames(x)=c("x")
    
    y=as.data.frame(training.data[,"y"])
    colnames(y)="y"


    ########## kriging model with different noise level(noise variance)
   
    set.seed(123)
   # model <- km(y~1, design=x, response=y,
   #             covtype="gauss", coef.trend=0, coef.cov=1/sqrt(30), coef.var=1, 
   #             noise.var=(rep(j*(max(y_all)-min(y_all)),nrow(x)))^2)
    model <- km(design=x, response=y,
                covtype="gauss", 
                noise.var=(rep(j*(max(y_all)-min(y_all)),nrow(x)))^2)####j is the prefactor
    
    p <- predict(model, data.frame(x_all), type="UK")
    
    mean=as.data.frame(p$mean)
    sd=as.data.frame(p$sd)
    sd[which(sd[,1]==0),1]=10^-5
 
    ######error calculation
    MSE_data[i,jj]=colMeans((as.data.frame(y_all)-as.data.frame(mean))^2)
    CRPS_data[i,jj]=colMeans(abs(CRPS(mean,sd,y_all)))
  }
  jj=jj+1
}

####error matrix
MSE_data=as.data.frame((MSE_data))
CRPS_data=as.data.frame(CRPS_data)

########calculate mean of error 
MSE_data_mean=colMeans(MSE_data)
CRPS_data_mean=colMeans(CRPS_data)

MSE_data_mean=as.data.frame(MSE_data_mean)
CRPS_data_mean=as.data.frame(CRPS_data_mean)

########calculate sd of error 
sd_MSE=rep()
sd_CRPS=rep()
for(m in 1:ncol(MSE_data)){
  sd_MSE[m]=t.test(MSE_data[,m])$conf.int[2]-t.test(MSE_data[,m])$estimate
  sd_CRPS[m]=t.test(CRPS_data[,m])$conf.int[2]-t.test(CRPS_data[,m])$estimate
}

sd_MSE=as.data.frame(sd_MSE)
sd_CRPS=as.data.frame(sd_CRPS)

n_level_data=as.data.frame(n_level)
error=cbind(n_level_data,MSE_data_mean,sd_MSE,CRPS_data_mean,sd_CRPS)

write.csv(error,"error_true noise0.05.csv")
