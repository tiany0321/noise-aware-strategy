setwd("D:\\as-code\\latent heat\\noise evaluation")
library("DiceKriging")

#######Kriging model function
fn.gp = function(training.data.x, training.data.y,virtual.data.x,phi)
{ set.seed(23)
  noiselevel=phi*(max(data.training.enthalpy.lin[,"es"])-min(data.training.enthalpy.lin[,"es"]))
  noise=rep(noiselevel^2,nrow(training.data.x))
  m <- km(formula<-~.,design=training.data.x,response=training.data.y,noise.var=noise,covtype = "exp")
  set.seed(23)
  p <- predict(m, virtual.data.x, "UK")
  sd=as.data.frame(p$sd)
  mean=as.data.frame(p$mean)
  pr=cbind(mean,sd)
  colnames(pr)=c("mean","sd")
  return(pr)
}
#######Continuous Ranked Probability Score
CRPS=function(mean,sd,y){
  zz=(y-mean)/sd
  CRPS=sd*(1/(pi)^0.5-2*dnorm(zz)-zz*(2*pnorm(zz)-1))
  
  return(CRPS)
}

##########read data
data.training=read.csv("traindata_scale.csv")
data.training=data.training[,-c(1)]
data.training.enthalpy.lin=na.omit(data.training)
data=data.training.enthalpy.lin
data<-na.omit(data)

#############x
keep=c("VEC.mix","ou.mix","H.mix","delta.r.mix","numa")
data0=data[,keep]
############y
data.y0=as.data.frame(data[,"es"])

############set different noise level prefactors phi
PHI=c(0,0.05,0.1,0.15,0.2,0.25)
############Define data frame for calculated error
data.error.frame=matrix(,length(PHI),3)
colnames(data.error.frame)=c("MSE","MAPE","CRPS_v")
j=1
for (phi in PHI){
  
  enthalpy.lin.train.gauss=matrix(,nrow(data),3)
  for(i in 1:nrow(data))
   {#############leave one out method
  
    data.x=data0[-i,]#############x_-i as x of training data 
    data.y=data.y0[-i,]#############dy_-i as y of training data 
    data.vir.x=data0[i,]##########x_i as test data
    data.vir.y=data.y0[i,]#############y_i as y of test data 
    
    model.gp<-fn.gp(data.x,data.y,data.vir.x,phi)
    
    enthalpy.lin.train.gauss[i,1]=data.vir.y############y_i
    enthalpy.lin.train.gauss[i,2]=model.gp[,1]############prediction value of y_i
    enthalpy.lin.train.gauss[i,3]=model.gp[,2]############the uncertainty of the prediction
    }

    enthalpy.lin.train.gauss=as.data.frame(enthalpy.lin.train.gauss)
    colnames(enthalpy.lin.train.gauss)=c("measured","predicted","sd")
    enthalpy.lin.train.gauss=na.omit(enthalpy.lin.train.gauss)
   
    #############calculate MSE
    enthalpy.lin.train.gauss$abs_delta=abs(enthalpy.lin.train.gauss[,"predicted"]-enthalpy.lin.train.gauss[,"measured"])
    data.error.frame[j,"MSE"]=sum(enthalpy.lin.train.gauss$abs_delta^2)/nrow(enthalpy.lin.train.gauss)
    #############calculate MAPE
    enthalpy.lin.train.gauss$delta_ratio=abs((enthalpy.lin.train.gauss[,"predicted"]-enthalpy.lin.train.gauss[,"measured"])/enthalpy.lin.train.gauss[,"measured"])
    data.error.frame[j,"MAPE"]=sum(enthalpy.lin.train.gauss$delta_ratio)/nrow(enthalpy.lin.train.gauss)
    #############calculate CRPS
    enthalpy.lin.train.gauss$CRPS=CRPS(enthalpy.lin.train.gauss[,"predicted"],enthalpy.lin.train.gauss[,"sd"],enthalpy.lin.train.gauss[,"measured"])
    CRPS_v=mean(abs(enthalpy.lin.train.gauss$CRPS))
    data.error.frame[j,"CRPS_v"]=CRPS_v
    j=j+1
}
PHI=as.data.frame(PHI)
data.error.frame=cbind(PHI,data.error.frame)
write.csv(data.error.frame,"data.error.frame.csv")
