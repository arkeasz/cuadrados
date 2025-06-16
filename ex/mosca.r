install.packages("readxl")

library(readxl)

MOSCAS <- read_excel("C:/Users/Lenovo/Desktop/MOSCAS.xlsx", sheet = "Hoja1")
datos= MOSCAS

View(datos)

install.packages("car")
library(car)

attach(datos)
names(datos)

class(bloques)
class(marcas)
class(moscasmuertas)


residuals(aov(moscasmuertas~marcas+bloques, data= datos))
summary(residuals(aov(moscasmuertas~marcas+bloques, data= datos)))
shapiro.test(residuals((aov(moscasmuertas~marcas+bloques, data= datos))))

qqnorm(residuals(aov(moscasmuertas~marcas+bloques, data= datos)))
qqline(residuals(aov(moscasmuertas~marcas+bloques, data= datos)))

install.packages("lmtest")
library(lmtest)

#prueba de independencia de los residuos(DURBIN WATSON), Ho: hay independencia de residuos, H1: no hay
#pv>0.05 no se rechaza HO
dwtest(moscasmuertas~marcas+bloques, data= datos)
# DURBIN WATSON(AUTOCORRELACION ENTRE RESIDUOS EN RL: DW=2 NO HAY AUTOC, DW<2 
#HAY AUTOCORRELACION POSITIVA, DW>2 HAY CORRELACION NEGATIVA, DW= 0 AUTOCORRELACIÓN 
#POSITIVA MUY FUERTE, DW= 4 AUTOCORRELACION NEGATIVA MUY FUERTE)
durbinWatsonTest(aov(moscasmuertas~marcas+bloques, data= datos))

plot(resid(aov(moscasmuertas~marcas+bloques, data= datos)),type= "p", col= "blue", ylab= "residuos", xlab= "Observación", main= "Residuos vs Orden de Orservación")
abline(h=0, lty=2, col="red")

install.packages("ggplot2")
library(ggplot2)
acf(resid(aov(moscasmuertas~marcas+bloques, data= datos)), main = "ACF DE LOS RESIDUOS")

leveneTest(moscasmuertas~marcas, data =datos, center= mean)

leveneTest(moscasmuertas~bloques, data =datos, center= mean)


moscasmuertas ~ (marcas + bloques)
model<- aov(moscasmuertas~ (marcas+bloques))
model             
summary(model)

modelo=lm(moscasmuertas~ (marcas+bloques))
modelo
summary(modelo)

anova=aov(modelo)
anova
summary(anova)

aov(moscasmuertas~ marcas+bloques, data= datos)
summary(aov(moscasmuertas~marcas+bloques, data= datos))
