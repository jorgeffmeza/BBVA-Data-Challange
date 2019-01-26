#GenerarModelo

setwd("C:/Users/DELL/Desktop/BBVA2018")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\test1.RData")

#carga de librerias
#install.packages("e1071")
library(rpart)
library(C50)
library(e1071)
library(randomForest)

load("datos1_4.RData")
#datos1_4[,"Grupo"]<-as.factor(datos1_4[,"Grupo"])
str(datos1_4)
#Estab=8
Modelamiento_BBVA<-function(Estab)
{
tryCatch({
modelo<-list()
tmp<-subset(datos1_4,codEstab==Estab)
#X <- tmp[,"ranking"]
#Y <- tmp[,c("Nestab","Ngiro","Grupo","Wi_RM_Estab","Wi_Trx_Estab","Wi_RM_Giro","Wi_Trx_Giro","rangoEdad","rangoIngreso","flagGenero","rangoCtdProdAct","rangoCtdProdPas",
#"rangoCtdProdSeg","flagBxi","saldoTcEntidad1","saldoTcEntidad2","saldoTcEntidad3","saldoTcEntidad4")]
tmp<-tmp[,c("ranking","Nestab","Ngiro","Grupo","Wi_RM_Estab","Wi_Trx_Estab","Wi_RM_Giro","Wi_Trx_Giro","rangoEdad","rangoIngreso","flagGenero","rangoCtdProdAct","rangoCtdProdPas",
"rangoCtdProdSeg","flagBxi","saldoTcEntidad1","saldoTcEntidad2","saldoTcEntidad3","saldoTcEntidad4")]
n<-nrow(tmp)

modelo[[1]]<-rpart(ranking~.,data=tmp)
yobs<-tmp$ranking
predict1 <- as.numeric(predict(modelo[[1]]))
error1<-sqrt(sum(((yobs-predict1)/100)^2)/length(yobs))

error2<-Inf	
if(n>5){
modelo[[2]]<-randomForest(ranking~.,data=tmp)
#modelo[[1]]<-C50::C5.0(X,Y)

predict2 <- as.numeric(predict(modelo[[2]]))
error2<-sqrt(sum(((yobs-predict2)/100)^2)/length(yobs))
}
error3<-Inf

if(n>20)
{
modelo[[3]]<-svm(ranking~.,data=tmp,cost = 100, gamma = 1)
predict3 <- as.numeric(predict(modelo[[3]]))
error3<-sqrt(sum(((yobs-predict3)/100)^2)/length(yobs))
}
cat(Estab,"\n")
res<-modelo[[which.min(c(error1,error2,error3))]]
return(res)
},error=function(e){
modelo<-rpart(ranking~.,tmp)
#modelo<-randomForest(ranking~.,tmp,ntree=20)
return(modelo)}
)
}
#
Estab0<-sort(unique(datos1_4$codEstab))
Estab0<-Estab0[37170:length(Estab0)]
#Estab0<-1
SalidaModelo1<-lapply(Estab0,function(x)Modelamiento_BBVA(Estab=x))

library(parallel)
options <- parallel:::defaultClusterOptions
options$user
#Determinar el número de cores en los que se va a trabajar
cl <- makeCluster(detectCores()-1)

clusterExport(cl, "Modelamiento_BBVA")
clusterExport(cl, "Estab0")
clusterExport(cl, "datos1_4")
#clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(C50))
clusterEvalQ(cl, library(e1071))

SalidaModelo2<-parLapply(cl,Estab0,function(x)Modelamiento_BBVA(Estab=x))
stopCluster(cl)
#14:28
save(SalidaModelo2,file="SalidaModelo2.RData")



######################################################
####Prueba del modelo
estb1=1
Estab0<-sort(unique(datos1_4$codEstab))
Estab0<-Estab0[which(Estab0==estb1)]
SalidaModelo1<-lapply(Estab0,function(x)Modelamiento_BBVA(Estab=x))

#length(unique(subset(BaseTrx,codEstab==estb1)$codCliente))


tmp1<-subset(test1,codEstab==estb1)[,]
new1=tmp1[,c("Nestab","Ngiro","Grupo","Wi_RM_Estab","Wi_Trx_Estab","Wi_RM_Giro","Wi_Trx_Giro","rangoEdad","rangoIngreso","flagGenero","rangoCtdProdAct","rangoCtdProdPas",
"rangoCtdProdSeg","flagBxi","saldoTcEntidad1","saldoTcEntidad2","saldoTcEntidad3","saldoTcEntidad4")]

as.numeric(predict(SalidaModelo1[[1]],newdata=new1))

tmp1[,"ranking"]



#pruebas de pronostico
Estab0[26702]




test[,"D4"]<-paste(test$codCliente,test$codEstab,sep="@")
test[,"Cont"]<-1:nrow(test)
datos1_4[,"D4"]<-paste(datos1_4$codCliente,datos1_4$codEstab,sep="@")
test1<-merge(test[,c("Cont","D4")],datos1_4,by="D4",all.x = TRUE)

test1[,"FlagNuevo"]<-0
test1[na.action(na.omit(test1$codCliente)),"FlagNuevo"]<-1

Estab0<-sort(unique(datos1_4$codEstab))
x<-Estab0[1:37169]
tmp1<-data.frame(id=1:length(x),Modelo="Modelo1",codEstab=x)
x<-Estab0[(37169+1):length(Estab0)]
tmp2<-data.frame(id=1:length(x),Modelo="Modelo2",codEstab=x)
tmp3<-rbind(tmp1,tmp2)
rm(tmp1)
rm(tmp2)
rm(x)




