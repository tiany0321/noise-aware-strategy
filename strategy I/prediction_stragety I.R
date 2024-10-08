setwd("D:\\as-code\\latent heat\\strategy I")
#####library
library(DiceKriging)

phi_opt=0#######the optimal noise level prefactor

##################################### Define function ##########################
######acquisition function
fn.utility=function(selector,training.data,model.result,train.error,model.result.all,vir.data){
  #Efficient Global optimization
  if (selector=="EGO"){ 
    fn.ego.ei = function(training.data,model.result)
    {
      ego = (model.result[,"mean"] - max(training.data[,"es"]))/model.result[,"sd"]
      z = ego
      ei.ego = model.result[,"sd"]*z*pnorm(z) + model.result[,"sd"]*dnorm(z) 
      ei.ego = data.frame(ei.ego)
      return (ei.ego)
    }
    EI= fn.ego.ei(training.data,model.result)
    EI[which(EI[,]=="NaN"),]=0
    EI=as.data.frame(EI)
  }
  #Sequential Kriging Optimization
  if(selector=="SKO"){
    lamda=qnorm(0.75)
    fn.SKO.ei = function(model.result,train.error,lamda,model.result.all)
    {
      epsilon=train.error
      value=model.result.all[,"mean"]-lamda*model.result.all[,"sd"]
      i1 <- which(value==max(model.result.all[,"mean"]-lamda*model.result.all[,"sd"]))
      SKO = (model.result[,"mean"] - model.result.all[i1,"mean"])/model.result[,"sd"]
      z = SKO
      #####epsilon,lamda
      ei.SKO = (1-epsilon^0.5/(epsilon+model.result[,"sd"]^2)^(1/2))*(model.result[,"sd"]*z*pnorm(z) + model.result[,"sd"]*dnorm(z)) 
      ei.SKO  = data.frame(ei.SKO)
      return (ei.SKO )
    }
    EI=fn.SKO.ei(model.result,train.error,lamda,model.result.all)
    EI=as.data.frame(EI)
    EI[which(EI[,]=="NaN"),]=0
  }
  return(EI)
}
##########Kriging model function
fn.gp = function(training.data.x, training.data.y,virtual.data.x,noise.expe){ 
  set.seed(23) 
  noise=noise.expe
  m <- km(formula<-~.,design=training.data.x,noise.var = noise, response=training.data.y, covtype="exp")
  set.seed(23) 
  p <- predict(m, data.frame(virtual.data.x), "UK")
  sd=as.data.frame(p$sd)
  mean=as.data.frame(p$mean)
  pr=cbind(mean,sd)
  colnames(pr)=c("mean","sd")
  return(pr)
}
#########maximize the EI
fn.selector = function(EI){
  num=which(EI[,]==max(EI[,]))
  return(num)
}

##################read data
data.training=read.csv("traindata_scale.csv")
data.training[,"es"]=-data.training[,"es"]
data.training=data.training[,-c(1:9)]
data=na.omit(data.training)
################ x
keep=c("VEC.mix","ou.mix","numa","delta.r.mix","H.mix")
train.x=data[,keep]
#####y
train.y=as.data.frame(data[,"es"])
colnames(train.y)="es"
######training data
data.train=cbind(train.x, train.y)
#########calculate the noise level
noise.sd=rep(phi_opt*(max(train.y)-min(train.y)),nrow(train.y))

for(ii in 1:nrow(data.train)){
  set.seed(ii)
  data.train[ii,"es"] = train.y[ii,]
  data.train[ii,"error"] = (noise.sd[ii])^2#########noise level
}

noise.var=as.data.frame(data.train[,"error"])
colnames(noise.var)="var"

#################read virtual space
vir=read.csv("virtualdata_scale.csv")
vir.comp=as.data.frame(vir[,2:ncol(vir)])
########x in virtual space
vir.x=as.data.frame(vir[,keep])
#############modeling and prediction
gp = fn.gp(train.x, train.y,vir.x,data.train[,"error"])
gp.total=gp
##############the noise variance used in SKO
err.data=as.data.frame(rep((mean(data.train[,"error"]^0.5))^2,nrow(vir.x)))
colnames(err.data)="error"
#################calculate utility values
EI= fn.utility("EGO",data.train,gp,err.data, gp.total,vir.x)
colnames(EI)="EI"
##############data frame combining x, utility value, predicted mean and uncertainties in virtual space
data.EGO=cbind(vir.comp,EI,gp)
data.EGO=data.EGO[order(-data.EGO[,"EI"]),]#######Sort the data from high to low according to the utility values
data.EGO=data.EGO[1:10000,]
###########the one ranked first is the recommended alloy
data.EGO[1,]

write.csv(data.EGO,"data.EGO.csv")

