setwd("D:\\as-code\\latent heat\\feature selection")
data_fea=read.csv("traindata_scale.csv")
data_fea=data_fea[,-1]
data.training.enthalpy.lin=data_fea[,-c(1:11)]


library(gbm)
set.seed(44)
boost.hp = gbm(es ~ ., 
               data = data.training.enthalpy.lin, distribution = "gaussian", n.trees = 50, interaction.depth = 5)
rel.inf = summary(boost.hp)
inf=rel.inf[1:15,]
inf
write.csv(inf,"GB.result.csv")
feature=as.character(inf[,1])
feature.names.keep=data.training.enthalpy.lin[,feature]
feature.names.keep=as.data.frame(scale(feature.names.keep))
aa=as.data.frame(data.training.enthalpy.lin$es)
colnames(aa)="es"
feature.names.keep=cbind(aa,feature.names.keep)
cor(feature.names.keep)

library("corrplot")
colnames(feature.names.keep)=c("target", "VEN", "ou","cs", "H","mass", "YM","r", "delta.r","S","Tm", "lambda","CE", "mismatch.x","volume","numa")
pdf("pearson.pdf")
corrplot(corr=cor(feature.names.keep))
dev.off()
