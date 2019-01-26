setwd("C:/Users/DELL/Desktop/BBVA2018")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\test1.RData")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\SalidaModelo1.RData")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\SalidaModelo2.RData")
load("C:\\Users\\DELL\\Desktop\\BBVA2018\\Estab0.RData")

Estab1<-Estab0
x<-Estab1[1:37169]
tmp1<-data.frame(id=1:length(x),Modelo="SalidaModelo1",codEstab=x)
x<-Estab1[(37169+1):length(Estab1)]
tmp2<-data.frame(id=1:length(x),Modelo="SalidaModelo2",codEstab=x)
tmp3<-rbind(tmp1,tmp2)
rm(tmp1)

test1$D4