#setwd("C:/Users/CESAR/Documents/Cursos/BBVA Challenger/BBVA2018")
setwd("C:/Users/DELL/Desktop/BBVA2018")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\SalidaModelo2.Rdata")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\test1.Rdata")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\Estab0.Rdata")

library(C50)
library(randomForest)
library(rpart)
library(e1071)

 

load("test1.RData")

load("SalidaModelo1.RData")

load("SalidaModelo2.RData")

load("Estab0.RData")

 

 

Estab1<-Estab0
#x<-Estab1[1:37169]
#tmp1<-data.frame(id=1:length(x),Modelo="SalidaModelo1",codEstab=x)
x<-Estab1[(37169+1):length(Estab1)]
maestrodemodel<-data.frame(id=1:length(x),Modelo="SalidaModelo2",codEstab=x)
#maestrodemodel<-rbind(tmp1,tmp2)
#rm(tmp1)
#rm(tmp2)
#rm(x)
#codEstab1<-unique(test1$codEstab)
codEstab1<-x
#estab<-codEstab1[4]

write.table(t(c("codCliente","CodEstab","codClienteCodEstab","ratingMonto")),"salida.txt",row.names=F,col.names=F,sep="\t")

 
PrediccionBBVA<-function(estab)
{
tmp1<-subset(test1,codEstab==estab)
tmp2<-subset(maestrodemodel,codEstab==estab)

pred1<-0
if(nrow(tmp2)>0)
{
new1<-tmp1[,c("Nestab","Ngiro","Grupo","Wi_RM_Estab","Wi_Trx_Estab","Wi_RM_Giro","Wi_Trx_Giro","rangoEdad","rangoIngreso","flagGenero","rangoCtdProdAct","rangoCtdProdPas",
"rangoCtdProdSeg","flagBxi","saldoTcEntidad1","saldoTcEntidad2","saldoTcEntidad3","saldoTcEntidad4")]
text1<-paste0("pred1<-predict(",tmp2$Modelo,"[[",tmp2$id,"]],newdata=new1)")
try(eval(parse(text =text1)))
}

var1<-paste0(tmp1$codCliente,tmp1$codEstab)
salida<-data.frame(codCliente=tmp1$codCliente,codEstab=tmp1$codEstab,codClienteCodEstab=var1,ratingMonto=as.numeric(pred1)/100)
write.table(salida,"salida.txt",row.names=F,col.names=F,sep="\t",append=T)
}
#SalidaPrediccion<-lapply(codEstab1,function(x)PrediccionBBVA(estab=x))

 

library(parallel)

options <- parallel:::defaultClusterOptions

options$user

#Determinar el número de cores en los que se va a trabajar
cl <- makeCluster(3)
clusterExport(cl, "PrediccionBBVA")
clusterExport(cl, "SalidaModelo2")
clusterExport(cl, "codEstab1")
clusterExport(cl, "test1")
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(e1071))

SALIDA<-parLapply(cl,codEstab1,function(x)PrediccionBBVA(estab=x))
#19:31
stopCluster(cl)






library(parallel)
options <- parallel:::defaultClusterOptions
options$user
#Determinar el número de cores en los que se va a trabajar
cl <- makeCluster(detectCores()-1)
clusterExport(cl, "Modelamiento_BBVA")
clusterExport(cl, "Estab1")
clusterExport(cl, "test1")
#clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(C50))
clusterEvalQ(cl, library(e1071))

SalidaModelo2<-parLapply(cl,Estab0,function(x)Modelamiento_BBVA(Estab=x))
stopCluster(cl)
#14:28
save(SalidaModelo2,file="SalidaModelo2.RData")



SalidaPrediccion<-lapply(codEstab1,function(x)PrediccionBBVA(estab=x))



