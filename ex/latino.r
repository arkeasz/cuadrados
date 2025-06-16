library(readxl)
cuadradoslatinos <- read_excel("C:/Users/Lenovo/Desktop/cuadradoslatinos.xlsx", 
                               col_types = c("numeric", "text", "text", 
                                             "text"))
View(cuadradoslatinos1)
cuadradoslatinos1
datos<-cuadradoslatinos1
datos
attach(datos)

names(datos)

class(respuesta)
class(tipodecarro)
class(posición)
class(latinomarcas)

#modelo
#respuesta~tipodecarro+posición+latinomarcas, data=datos

aov(respuesta~tipodecarro+posición+latinomarcas, data=datos)
summary(aov(respuesta~tipodecarro+posición+latinomarcas, data=datos))

anova1<-aov(respuesta~tipodecarro+posición+latinomarcas, data=datos)
anova1
residuals(anova1)
summary(residuals(anova1))
shapiro.test(residuals(anova1))
qqnorm(residuals(anova1))
qqline(residuals(anova1))

#prueba de independencia de los residuos(DURBIN WATSON), Ho: hay independencia de residuos, H1: no hay
#pv>0.05 no se rechaza HO
dwtest(respuesta~tipodecarro+posición+latinomarcas, data=datos)
# DURBIN WATSON(AUTOCORRELACION ENTRE RESIDUOS EN RL: DW=2 NO HAY AUTOC, DW<2 
#HAY AUTOCORRELACION POSITIVA, DW>2 HAY CORRELACION NEGATIVA, DW= 0 AUTOCORRELACIÓN 
#POSITIVA MUY FUERTE, DW= 4 AUTOCORRELACION NEGATIVA MUY FUERTE)
durbinWatsonTest(anova1)

plot(resid(anova1),type= "p", col= "blue", ylab= "residuos", xlab= "Observación", main= "Residuos vs Orden de Orservación")
abline(h=0, lty=2, col="red")

install.packages("ggplot2")
library(ggplot2)
acf(resid(anova1), main = "ACF DE LOS RESIDUOS")


install.packages("car")
library(car)

leveneTest(resid(anova1) ~ tipodecarro, data = datos, center=mean)
leveneTest(resid(anova1)~ posición, data= datos, center=mean )
leveneTest(resid(anova1) ~ latinomarcas, data = datos, center=mean)


#HERETOCEDASTICIDAD (RESIDUOS) EN REGRESIÓN LINEAL (BREUSCH-PAGAN)
install.packages("lmtest")
library(lmtest)
bptest(lm(respuesta~tipodecarro+posición+latinomarcas, data=datos))

# HOMOGENEIDAD EN REGRESIÓN LINEAL CON DATOS NORMALES BARTLETT  Y NO NORMALES LEVENE
bartlett.test(resid(anova1) ~ tipodecarro, data = datos)
bartlett.test(resid(anova1)~ posición, data= datos )
bartlett.test(resid(anova1) ~ latinomarcas, data = datos)

#Pruebas Post hoc
TukeyHSD(anova1)

