# P6.1 Calcule la media, desviacion estandar y error estandar de cada zona
Media1<-mean(muestreo_MR$B1)
Media2<-mean(muestreo_MR$B2)
Media3<-mean(muestreo_MR$B3)
Media4<-mean(muestreo_MR$B4)
Media5<-mean(muestreo_MR$B5)

Desv1<-sd(muestreo_MR$B1)
Desv2<-sd(muestreo_MR$B2)
Desv3<-sd(muestreo_MR$B3)
Desv4<-sd(muestreo_MR$B4)
Desv5<-sd(muestreo_MR$B5)

ES1<- Media1/sqrt(50)
ES2<- Media2/sqrt(50)
ES3<- Media3/sqrt(50)
ES4<- Media4/sqrt(50)
ES5<- Media5/sqrt(50)

# P6.2 Según sus calculos, qué zonas entran en alerta sanitaria?
# Entran en alerta sanitaria son la zona Bahia Martin (primera media) y Bahia Chincui (cuarto promedio)

# P6.3 Grafique las medias muéstrales y sus desviaciones estándar. Interprete 

ID<- c("B1", "B2","B3", "B4", "B5")
Promedio<- c(80.39, 76.48, 78.83, 84.11, 75.49)
DesviaciónEstandar<-c(5.77, 30.67, 5.61, 4.17, 20.68)
DesvPromedio<- Promedio-DesviaciónEstandar
DesvPromedio2<- Promedio+DesviaciónEstandar
Tabla<- cbind(ID, Promedio, DesviaciónEstandar,DesvPromedio, DesvPromedio2)

TablaFrame<- as.data.frame(Tabla)

ggplot(data= TablaFrame, aes(x=ID, y=Promedio))+
  geom_bar(stat= "identity", fill= "grey", width= .2)+
  geom_errorbar(aes(ymin=DesvPromedio, ymax=DesvPromedio2), width=.5, colour= "black")+
  ylab("Promedio")+ xlab("Muestras")

ID<- c("B1", "B2","B3", "B4", "B5")
Prom<- c(80.39, 76.48, 78.83, 84.11, 75.49)
sd<-c(5.77, 30.67, 5.61, 4.17, 20.68)

inf <- Prom-sd
sup <- Prom+sd

tabla <- as.data.frame(cbind(ID,Prom,inf,sup))

ggplot(data= TablaFrame, aes(x=ID, y=Prom))+
  geom_bar(stat= "identity", fill= "grey", width= .2)+
  geom_errorbar(aes(ymin=inf, ymax=sup), width=.5, colour= "black")+
  ylab("Promedio")+ xlab("Muestras")
