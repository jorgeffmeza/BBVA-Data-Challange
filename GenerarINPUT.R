#Generación del Input
setwd("C:/Users/CESAR/Documents/Cursos/BBVA Challenger")
load("C:\\Users\\CESAR\\Documents\\Cursos\\BBVA Challenger\\dataBaseTrainTrxRec.RData")
load("C:\\Users\\CESAR\\Documents\\Cursos\\BBVA Challenger\\dataBasePerfilRec.RData")
load("C:\\Users\\CESAR\\Documents\\Cursos\\BBVA Challenger\\dataBaseTestKeyRec.RData")

fecha<-as.Date(substring(as.character(BaseTrx$fechaOpe),1,10))
nomdia=weekdays(fecha)
periodo1=as.integer(format(fecha,"%Y%m%d"))
periodo2=as.integer(format(fecha,"%Y%m"))

BaseTrx[,"Periodo1"]<-periodo1
BaseTrx[,"Periodo2"]<-periodo2
BaseTrx[,"nomdia"]<-nomdia

FuncionDias<-function(x)
{
  resultado<-as.numeric(as.Date(as.character(max(x)),"%Y%m%d")-as.Date(as.character(min(x)),"%Y%m%d"))+1
  return(resultado)
}

XX1<-lapply(with(BaseTrx,split(ctdTrx,codCliente)),sum)
XX2<-lapply(with(BaseTrx,split(codEstab,codCliente)),function(x)length(unique(x)))
XX3<-lapply(with(BaseTrx,split(codGiro,codCliente)),function(x)length(unique(na.omit(x))))
XX4<-lapply(with(BaseTrx,split(Periodo1,codCliente)),FuncionDias)

datos_1<-
  data.frame(
  codCliente=as.integer(as.character(names(XX1))),
  Trx=as.numeric(XX1),
  Nestab=as.numeric(XX2),
  Ngiro=as.numeric(XX3),
  Ndias=as.numeric(XX4),
  Trx1=as.numeric(XX1)/as.numeric(XX4)*365)

X_1<-datos_1$Nestab
X_2<-datos_1$Trx1
Y_1<-as.character(cut(X_1,breaks = c(0,10,30,Inf)))
Y_2<-as.character(cut(X_2,breaks = c(0,20,60,Inf)))

##añadir rangos 
datos_1[,"rankestab"]<-Y_1
datos_1[,"ranktrx"]<-Y_2
datos_1[,"Grupo"]<-paste(Y_1,Y_2,sep="-")

datos_2<-merge(datos_1,baseperfil,by="codCliente")
BaseTrx<-merge(BaseTrx,datos_2,by="codCliente")

maxgiro<-max(na.omit(BaseTrx$codGiro))#225
BaseTrx[na.action(na.omit(BaseTrx[,"codGiro"])),"codGiro"]<-maxgiro+BaseTrx[na.action(na.omit(BaseTrx[,"codGiro"])),"codEstab"]

#maestro de establecimientos
maestro<-BaseTrx[,c("codEstab","codGiro","ubigeoEstab")]
maestro<-maestro[!duplicated(maestro),]

XX1_1<-lapply(split(BaseTrx$ratingMonto,BaseTrx$codCliente),function(x)sum(na.omit(x)))
XX2_1<-lapply(split(BaseTrx$ctdTrx,BaseTrx$codCliente),function(x)sum(na.omit(x)))

G1<-paste(BaseTrx$codCliente,BaseTrx$codGiro,sep="@")
XX1_2<-lapply(split(BaseTrx$ratingMonto,G1),function(x)sum(na.omit(x)))
XX2_2<-lapply(split(BaseTrx$ctdTrx,G1),function(x)sum(na.omit(x)))


G2<-paste(BaseTrx$codCliente,BaseTrx$codGiro,BaseTrx$codEstab,sep="@")

#valor reprentativo
XX1_3<-lapply(split(BaseTrx$ratingMonto,G2),function(x)median(na.omit(x)))
XX2_3<-lapply(split(BaseTrx$ratingMonto,G2),function(x)sum(na.omit(x)))
XX3_3<-lapply(split(BaseTrx$ctdTrx,G2),function(x)sum(na.omit(x)))

datos1_2<-data.frame(codCliente=as.integer(as.character(names(XX1_1))),Wi_RM_Cli=as.numeric(XX1_1),Wi_Trx_Cli=as.numeric(XX2_1))

datos1_3<-data.frame(ID2=names(XX1_2),Wi_RM_Giro=as.numeric(XX1_2),Wi_Trx_Giro=as.numeric(XX2_2))
datos1_3[,"ID2"]<-as.character(datos1_3[,"ID2"])

datos1_4<-data.frame(ID3=names(XX1_3),ranking=as.numeric(XX1_3)*100)
datos1_4[,"ID3"]<-as.character(datos1_4[,"ID3"])
datos1_4[,"Wi_RM_Estab"]<-as.numeric(XX2_3)
datos1_4[,"Wi_Trx_Estab"]<-as.numeric(XX3_3)
XX4_1<-do.call("rbind",strsplit(datos1_4$ID3,"@"))
datos1_4[,"ID2"]<-paste(XX4_1[,1],XX4_1[,2],sep="@")
datos1_4[,"codCliente"]<-as.integer(XX4_1[,1])
datos1_4[,"codEstab"]<-as.integer(XX4_1[,3])
datos1_4[,"codGiro"]<-as.integer(XX4_1[,2])
datos1_4<-merge(datos1_4,datos1_3,by="ID2")
datos1_4<-merge(datos1_4,datos1_2,by="codCliente")
datos1_4[,"Wi_RM_Estab"]<-datos1_4[,"Wi_RM_Estab"]/datos1_4[,"Wi_RM_Giro"]*100
datos1_4[,"Wi_Trx_Estab"]<-datos1_4[,"Wi_Trx_Estab"]/datos1_4[,"Wi_Trx_Giro"]*100
datos1_4[,"Wi_RM_Giro"]<-datos1_4[,"Wi_RM_Giro"]/datos1_4[,"Wi_RM_Cli"]*100
datos1_4[,"Wi_Trx_Giro"]<-datos1_4[,"Wi_Trx_Giro"]/datos1_4[,"Wi_Trx_Cli"]*100
datos1_4<-merge(datos1_4,baseperfil,by="codCliente")
datos1_4<-merge(datos1_4,datos_1,by="codCliente")
datos1_4<-merge(datos1_4,maestro[,c("codEstab","ubigeoEstab")],by="codEstab")

datos1_4[,"Grupo"]<-as.factor(datos1_4[,"Grupo"])

#save(datos1_4,file="datos1_4.RData")

test[,"D4"]<-paste(test$codCliente,test$codEstab,sep="@")
test[,"Cont"]<-1:nrow(test)

datos1_4[,"D4"]<-paste(datos1_4$codCliente,datos1_4$codEstab,sep="@")
test1<-merge(test[,c("Cont","D4")],datos1_4,by="D4",all.x = TRUE)


test1[,"FlagNuevo"]<-0
test1[na.action(na.omit(test1$codCliente)),"FlagNuevo"]<-1

###
XX5_1<-do.call("rbind",strsplit(datos1_3$ID2,"@"))
datos1_3[,"codCliente"]<-as.integer(XX5_1[,1])
datos1_5<-merge(datos1_3,datos1_2,by="codCliente")
datos1_5[,"Wi_RM_Giro"]<-datos1_5[,"Wi_RM_Giro"]/datos1_5[,"Wi_RM_Cli"]*100
datos1_5[,"Wi_Trx_Giro"]<-datos1_5[,"Wi_Trx_Giro"]/datos1_5[,"Wi_Trx_Cli"]*100

pos_1<-as.numeric(na.action(na.omit(test1$codCliente)))
datos1_6<-data.frame(pos=pos_1,D4=test1[pos_1,"D4"])
datos1_6[,"D4"]<-as.character(datos1_6[,"D4"])
datos1_6[,"cont"]<-1:nrow(datos1_6)
XX6_1<-do.call("rbind",strsplit(datos1_6$D4,"@"))
datos1_6[,"codCliente"]<-as.integer(XX6_1[,1])
datos1_6[,"codEstab"]<-as.integer(XX6_1[,2])
datos1_6<-merge(datos1_6,maestro[,c("codEstab","codGiro")],by="codEstab",all.x = TRUE)
datos1_6[,"ID2"]<-paste(datos1_6$codCliente,datos1_6$codGiro,sep="@")
datos1_6<-merge(datos1_6,datos1_5[,c("ID2","Wi_RM_Giro","Wi_Trx_Giro")],by="ID2",all.x = TRUE)

datos1_6[as.numeric(na.action(na.omit(datos1_6$Wi_RM_Giro))),"Wi_RM_Giro"]<-0
datos1_6[as.numeric(na.action(na.omit(datos1_6$Wi_Trx_Giro))),"Wi_Trx_Giro"]<-0
datos1_6<-merge(datos1_6,datos_2,by="codCliente",all.x = TRUE)
datos1_6<-datos1_6[order(datos1_6$cont,decreasing=F),]

variablesselect<-c("codCliente","codEstab","Trx","Ndias","Trx1","Nestab","Ngiro","Grupo","Wi_RM_Giro","Wi_Trx_Giro","rangoEdad","rangoIngreso","flagGenero","rangoCtdProdAct","rangoCtdProdPas",
"rangoCtdProdSeg","flagBxi","saldoTcEntidad1","saldoTcEntidad2","saldoTcEntidad3","saldoTcEntidad4","flagLimaProvCliente","ubigeoCliente","rankestab","ranktrx")

test1[pos_1,variablesselect]<-datos1_6[,variablesselect]

pos_2<-as.numeric(na.action(na.omit(test1$Wi_RM_Estab)))
pos_3<-as.numeric(na.action(na.omit(test1$Wi_Trx_Estab)))

test1[pos_2,"Wi_RM_Estab"]<-0
test1[pos_3,"Wi_Trx_Estab"]<-0
test1[,"Grupo"]<-factor(test1[,"Grupo"],levels=levels(datos1_4$Grupo))


head(test1,4)
#save(test1,file="test1.RData")




###############################



test1[1:20,c("Nestab","Ngiro","Grupo","Wi_RM_Giro","Wi_Trx_Giro","rangoEdad","rangoIngreso","flagGenero","rangoCtdProdAct","rangoCtdProdPas",
"rangoCtdProdSeg","flagBxi","saldoTcEntidad1","saldoTcEntidad2","saldoTcEntidad3","saldoTcEntidad4")]










Estab0<-sort(unique(datos1_4$codEstab))
x<-Estab0[1:37169]
tmp1<-data.frame(id=1:length(x),Modelo="Modelo1",codEstab=x)
x<-Estab0[(37169+1):length(Estab0)]
tmp2<-data.frame(id=1:length(x),Modelo="Modelo2",codEstab=x)
tmp3<-rbind(tmp1,tmp2)
rm(tmp1)
rm(tmp2)
rm(x)













