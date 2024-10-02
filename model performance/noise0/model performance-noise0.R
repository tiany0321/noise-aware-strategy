setwd("D:\\as-code\\latent heat\\model performance\\noise0")
library("DiceKriging")

phi=0#########noise level prefactor
train.num=50#########  the size of training data

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

#####plot function
fn.plot.prevsmea = function(pre, meas){
  data = cbind(pre,meas)
  data = data.frame(data)
  colnames(data) = c("pre", "meas")
  if(min(data$meas) > min(data$pre)){mmin = min(data$pre)}else{mmin = min(data$meas)}
  if(max(data$meas) < max(data$pre)){mmax = max(data$pre)}else{mmax = max(data$meas)}
  plot(data$pre ~ data$meas ,tck=0.01,font=2,font.lab=2,
       ylab = "Predicted values", xlab = "Measured values",
       pch = 21, col = "darkblue", cex = 1.5, bg="gray",
       xlim= c(-40,-5), ylim = c(-40, -5))
  abline(0, 1, lwd =2, col = "red")
}


#####read data
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

######################## Sample the training data from the all data
set.seed(599)
sample_num=sample(1:nrow(data),train.num,replace = F)
######training data
data_train_x=data0[sample_num,]
data_train_y=data.y0[sample_num,]
######test data
data_test_x=data0[-sample_num,]
data_test_y=data.y0[-sample_num,]

#####predict the test data
model.gp_test<-fn.gp(data_train_x,data_train_y,data_test_x,phi)
#######the predicted results of the test data
test=cbind(data_test_y,model.gp_test[,1],model.gp_test[,2])
colnames(test)=c("measured","predicted","sd")
#####predict the training data
model.gp_train<-fn.gp(data_train_x,data_train_y,data_train_x,phi)
#######the predicted results of the training data
train=cbind(data_train_y,model.gp_train[,1],model.gp_train[,2])
colnames(train)=c("measured","predicted","sd")

#######combine the predicted results of test data and training data
total=rbind(train,test)

plot(total[,2] ~ total[,1],  pch = 21, xlab = "Predicted values",ylab = "Measured values",col = "darkblue", cex = 1.5, bg="gray",xlim= c(-40,-5), ylim = c(-40, -5))
par(new=T)
plot(test[,2] ~ test[,1], xlab = "Predicted values",ylab = "Measured values",  pch = 21, col = "darkblue", cex = 1.5, bg="blue",xlim= c(-40,-5), ylim = c(-40, -5))
abline(0, 1, lwd =2, col = "red")

###calculate the MSE
MSE=sum((test[,1]-test[,2])^2)/nrow(test)
#####calculate the R_2
test=as.data.frame(test)
model <- lm(measured ~ predicted, data=test)
R_2=summary(model)$r.squared
R_2
#####calculate the CRPS
attach(test)
CRPS_result=CRPS(mean=predicted,sd=sd,y=measured)
CRPS_result=mean(abs(CRPS_result))
CRPS_result
###the errors
result_error=c(MSE,CRPS_result,R_2)
result_error=as.data.frame(t(as.data.frame(result_error)))
colnames(result_error)=c("MSE","CRPS","R_2")


write.csv(result_error,"result_error.csv")
write.csv(total,"total0.csv")