setwd("D:\\as-code\\1D\\specific\\homo")

library("DiceKriging")
library("RColorBrewer")
################ kriging model function
fn_KM=function(x_train,y_train,x_vir_total,set.level){
  set.seed(123)
 
  model <- km(design=x_train, response=y_train,
              covtype="gauss", 
              noise.var=(rep(set.level*(max(fundet(t))-min(fundet(t))),nrow(x_train)))^2)####j is the prefactor
  
  p_total0 <- predict.km(model, newdata=x_vir_total, type="UK")
  model.result.all=cbind(p_total0$mean,p_total0$sd)
  colnames(model.result.all)=c("mean","sd")
  
  p<- predict.km(model, newdata=x_vir, type="UK")
  model.result=cbind(p$mean,p$sd)
  colnames(model.result)=c("mean","sd")
  
  return(model.result.all)
}
###########1D function
fundet <- function(x){
  return((sin(8*x)/(1+x)+2*cos(2*x)*x^3))
}

n.level=0.1########################the true noise level prefactor phi_real
train.num <- 15#########################Define the size of training data

########################x
t <- seq(0, 1, by=0.01)

########################y
y_real=fundet(t) 
y_real=as.data.frame(y_real)
############set homogeneous noise to the true function value
noise.sd=rep(n.level*(max(fundet(t))-min(fundet(t))),length(t))
noise.var=noise.sd^2
noise.var=as.data.frame(noise.var)
# Making noisy observations from 'fundet' function 
set.seed(44)
mea <- fundet(t) + sqrt(noise.var)*rnorm(length(t))
mea=as.data.frame(mea)
t=as.data.frame(t)
##############order number
order.num=as.data.frame(rep(1:nrow(t)))
colnames(order.num)="order.num"

data_all=cbind(t,y_real,mea,noise.var,order.num)
colnames(data_all)=c("x","y_real","mea","noise.var","order.num")

############sample train.num=5 pieces of data as the training dataset
set.seed(train.num*11)
sample.num0<-vector()
for(i in 1:1000){
  s <-  sample(c(1:nrow(t)),60,replace=F,prob=NULL) 
  sample.num0<-as.data.frame(rbind(sample.num0,s))
}

sample.num<-sample.num0[,1:train.num]

##############randomly sample 5 data as the training data
i=445
split=sample.num[i,]
split=unlist(split)
data_train=data_all[split,]

x_train=data_train[,"x"]
x_train=as.data.frame(x_train)
colnames(x_train)="x"

y_train=data_train[,"mea"] 
y_train=as.data.frame(y_train)
colnames(y_train)="mea"

############virtual space
x_vir_total=data_all[,"x"]
x_vir_total=as.data.frame(x_vir_total)
colnames(x_vir_total)="x"

x_vir=data_all[-split,"x"]
x_vir=as.data.frame(x_vir)
colnames(x_vir)="x"

vir_data=data_all[-split,]

#################define data frame
data_MSE=matrix(,1,7)
colnames(data_MSE)=c("0","0.01","0.02","0.1","0.2","0.25","0.3")#######preset noise level prefactor phi

################ graphics###########################################################
display.brewer.all(type = "seq")
par(mfrow = c(1,5),mai=c(0.07,0.05,0.07,0.05),omi=c(1.7,0.3,1.5,0.1))

######phi=0
set.level=10^(-5)
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,1]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

######phi=0.01
set.level=0.01
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,2]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

#####phi=0.02
set.level=0.02
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,3]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

lower <- p_total[,"mean"]-p_total[,"sd"]
upper <- p_total[,"mean"]+p_total[,"sd"]

plot(unlist(data_all[,"x"]), p_total[,"mean"],type="l",xaxt="n", yaxt="n",  ylab="y",xlab="x",col="black", lwd=2, font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.lab=2)
axis(c(1),  tck = 0.01, cex.axis=2,ylim=c(-1,2))
axis(c(2),  tck = 0.01, cex.axis=2,ylim=c(-1,2))
polygon(c(unlist(data_all[,"x"]),rev(unlist(data_all[,"x"]))), c(lower, rev(upper)), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(unlist(data_all[,"x"]), p_total[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=2,xlim=c(0,1),ylim=c(-0.7,1))
par(new=T)
plot(unlist(x_vir_total), fundet(unlist(x_vir_total)),xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=2, tck=0.01,font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.axis=2,cex.lab=2)
points(unlist(x_train), unlist(y_train), pch=20,col=brewer.pal(9, "Oranges")[5],cex=3)

#####phi=0.1
set.level=0.1
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,4]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

lower <- p_total[,"mean"]-p_total[,"sd"]
upper <- p_total[,"mean"]+p_total[,"sd"]

plot(unlist(data_all[,"x"]), p_total[,"mean"],type="l",xaxt="n", yaxt="n",  ylab="y",xlab="x",col="black", lwd=2, font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.lab=2)
axis(c(1),  tck = 0.01, cex.axis=2,ylim=c(-1,2))
polygon(c(unlist(data_all[,"x"]),rev(unlist(data_all[,"x"]))), c(lower, rev(upper)), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(unlist(data_all[,"x"]), p_total[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=2,xlim=c(0,1),ylim=c(-0.7,1))
par(new=T)
plot(unlist(x_vir_total), fundet(unlist(x_vir_total)),xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=2, tck=0.01,font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.axis=2,cex.lab=2)
points(unlist(x_train), unlist(y_train), pch=20,col=brewer.pal(9, "Oranges")[5],cex=3)

#####phi=0.2
set.level=0.2
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,5]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

lower <- p_total[,"mean"]-p_total[,"sd"]
upper <- p_total[,"mean"]+p_total[,"sd"]

plot(unlist(data_all[,"x"]), p_total[,"mean"],type="l",xaxt="n", yaxt="n",  ylab="y",xlab="x",col="black", lwd=2, font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.lab=2)
axis(c(1),  tck = 0.01, cex.axis=2,ylim=c(-1,2))
polygon(c(unlist(data_all[,"x"]),rev(unlist(data_all[,"x"]))), c(lower, rev(upper)), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(unlist(data_all[,"x"]), p_total[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=2,xlim=c(0,1),ylim=c(-0.7,1))
par(new=T)
plot(unlist(x_vir_total), fundet(unlist(x_vir_total)),xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=2, tck=0.01,font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.axis=2,cex.lab=2)
points(unlist(x_train), unlist(y_train), pch=20,col=brewer.pal(9, "Oranges")[5],cex=3)

#####phi=0.25
set.level=0.25
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,6]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

lower <- p_total[,"mean"]-p_total[,"sd"]
upper <- p_total[,"mean"]+p_total[,"sd"]

plot(unlist(data_all[,"x"]), p_total[,"mean"],type="l",xaxt="n", yaxt="n",  ylab="y",xlab="x",col="black", lwd=2, font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.lab=2)
axis(c(1),  tck = 0.01, cex.axis=2,ylim=c(-1,2))
polygon(c(unlist(data_all[,"x"]),rev(unlist(data_all[,"x"]))), c(lower, rev(upper)), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(unlist(data_all[,"x"]), p_total[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=2,xlim=c(0,1),ylim=c(-0.7,1))
par(new=T)
plot(unlist(x_vir_total), fundet(unlist(x_vir_total)),xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=2, tck=0.01,font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.axis=2,cex.lab=2)
points(unlist(x_train), unlist(y_train), pch=20,col=brewer.pal(9, "Oranges")[5],cex=3)

#####phi=0.3
set.level=0.3
p_total=fn_KM(x_train,y_train,x_vir_total,set.level)
data_MSE[1,7]=colMeans((as.data.frame(data_all[,"y_real"])-as.data.frame(p_total[,"mean"]))^2)

lower <- p_total[,"mean"]-p_total[,"sd"]
upper <- p_total[,"mean"]+p_total[,"sd"]

plot(unlist(data_all[,"x"]), p_total[,"mean"],type="l",xaxt="n", yaxt="n",  ylab="y",xlab="x",col="black", lwd=2, font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.lab=2)
axis(c(1),  tck = 0.01, cex.axis=2,ylim=c(-1,2))
polygon(c(unlist(data_all[,"x"]),rev(unlist(data_all[,"x"]))), c(lower, rev(upper)), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(unlist(data_all[,"x"]), p_total[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=2,xlim=c(0,1),ylim=c(-0.7,1))
par(new=T)
plot(unlist(x_vir_total), fundet(unlist(x_vir_total)),xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=2, tck=0.01,font=2,font.lab=2,xlim=c(0,1),ylim=c(-0.7,1),cex.axis=2,cex.lab=2)
points(unlist(x_train), unlist(y_train), pch=20,col=brewer.pal(9, "Oranges")[5],cex=3)


#################################################################################

data_nosie_level=as.data.frame(c(0,0.01,0.02,0.1,0.2,0.25,0.3))#######preset noise level prefactor phi
colnames(data_nosie_level)="nosie_level"

data.MSE=as.data.frame(t(data_MSE))
colnames(data.MSE)="MSE"
data.MSE=cbind(data_nosie_level,data.MSE)
write.csv(data.MSE,"data_MSE_homo.csv")