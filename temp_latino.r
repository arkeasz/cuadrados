# y ~ tratamiento + fila + columna
# 0) Paquetes
if(!require(car))     install.packages("car",     repos="https://cran.r-project.org/")
if(!require(lmtest))  install.packages("lmtest",  repos="https://cran.r-project.org/")
library(car); library(lmtest)

# 1) Importar datos
# datos <- read_excel("ruta/archivo.xlsx", sheet="Hoja1")
# o read.csv(…)

# 2) Asegurarse de que todo sea factor / numérico
datos$tratamiento <- factor(datos$latinomarcas)   # tratamiento
datos$fila        <- factor(datos$tipodecarro)    # bloqueo fila
datos$columna     <- factor(datos$posición)       # bloqueo columna
datos$y           <- as.numeric(datos$respuesta)  # variable respuesta

# 3) Supuestos PRE‑ANOVA

## 3.1 Normalidad por nivel de tratamiento
for (tr in levels(datos$tratamiento)) {
  xi <- datos$y[datos$tratamiento==tr]
  sw <- shapiro.test(xi)
  cat("Shapiro", tr, ": p =", round(sw$p.value,4), "\n")
}

## 3.2 Homogeneidad (Levene)
print(leveneTest(y ~ tratamiento, data=datos))

## 3.3 Independencia (Durbin–Watson)
print(dwtest(y ~ tratamiento + fila + columna, data=datos))
plot(rstandard(aov(y ~ tratamiento, data=datos)), type="b",
     main="Residuos vs orden", xlab="Obs.", ylab="Residuo")
abline(h=0, lty=2)

# 3.4 Aditividad (interacción)
int <- anova(aov(y ~ tratamiento * fila * columna, data=datos))
print(int["tratamiento:fila","Pr(>F)"])
print(int["tratamiento:columna","Pr(>F)"])

# Si todos p > 0.05, seguimos:

# 4) ANOVA Cuadrado Latino
modelo <- aov(y ~ tratamiento + fila + columna, data=datos)
summary(modelo)

# 5) Supuestos POST‐ANOVA

# 5.1 Normalidad definitiva
print(shapiro.test(residuals(modelo)))

# 5.2 Homogeneidad visual
boxplot(residuals(modelo) ~ datos$tratamiento, main="Residuos vs Tratamiento")
abline(h=0, lty=2)

# 5.3 Independencia visual
plot(rstandard(modelo), type="b", main="Residuos vs Obs.")
abline(h=0, lty=2)

# 6) Post‐hoc (si procede)
# TukeyHSD(modelo, "tratamiento")
