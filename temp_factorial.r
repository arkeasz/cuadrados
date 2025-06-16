# PLANTILLA Diseño Factorial Completo
# ─────────────────────────────────────────────────────────────────────────
# Ejemplo de datos (ASCII):
# +-----------+---------+---------+--------+
# | FactorA   | FactorB | Réplica | Respuesta |
# +-----------+---------+---------+--------+
# | A         | X       | 1       | 4.8      |
# | A         | X       | 2       | 5.0      |
# | A         | Y       | 1       | 5.2      |
# | A         | Y       | 2       | 5.1      |
# | B         | X       | 1       | 4.5      |
# | B         | X       | 2       | 4.7      |
# | B         | Y       | 1       | 5.0      |
# | B         | Y       | 2       | 5.2      |
# | C         | X       | 1       | 5.1      |
# | C         | X       | 2       | 5.3      |
# | C         | Y       | 1       | 5.5      |
# | C         | Y       | 2       | 5.4      |
# +-----------+---------+---------+--------+
#   FactorA   : Factor principal A (e.g. tratamiento)
#   FactorB   : Factor principal B (e.g. tiempo, dosis)
#   Réplica   : Número de réplica dentro de cada combinación
#   Respuesta : Variable respuesta medida (numérica)

# 0) Paquetes necesarios ---------------------------------------------------
if(!require(car))      install.packages("car",      repos="https://cran.r-project.org/")
if(!require(emmeans))  install.packages("emmeans",  repos="https://cran.r-project.org/")
if(!require(ggplot2))  install.packages("ggplot2",  repos="https://cran.r-project.org/")
library(car); library(emmeans); library(ggplot2)

# 1) Importar datos ---------------------------------------------------------
# datos <- read.csv("ruta/tu_archivo.csv", header = TRUE)
# o con readxl:
# library(readxl)
# datos <- read_excel("ruta/tu_archivo.xlsx", sheet = "Hoja1")

# 2) Definir factores y respuesta -------------------------------------------
datos$FactorA   <- factor(datos$FactorA)
datos$FactorB   <- factor(datos$FactorB)
datos$Respuesta <- as.numeric(datos$Respuesta)

# 3) Verificación de supuestos PRE‑ANOVA -----------------------------------
## 3.1 Normalidad de residuos preliminares
mod_pre <- aov(Respuesta ~ FactorA * FactorB, data = datos)
res_pre <- residuals(mod_pre)
sw1 <- shapiro.test(res_pre)
cat("Shapiro-Wilk (residuos prelim): W=",round(sw1$statistic,3)," p=",round(sw1$p.value,4),"\n")

## 3.2 Homogeneidad de varianzas (Levene)
lev1 <- leveneTest(Respuesta ~ FactorA * FactorB, data = datos)
print(lev1)

## 3.3 Independencia de residuos (Residuals vs Orden)
plot(res_pre, type="b", pch=19, main="Residuos vs Orden",
     xlab="Observación", ylab="Residuo")
abline(h=0, lty=2)

# 4) Ajustar ANOVA factorial completo --------------------------------------
modelo <- aov(Respuesta ~ FactorA * FactorB, data = datos)
cat("\n--- RESUMEN ANOVA Factorial Completo ---\n")
print(summary(modelo))

# 5) Diagnósticos POST‑ANOVA ------------------------------------------------
res_fin <- residuals(modelo)
## 5.1 Normalidad definitiva
print(shapiro.test(res_fin))

## 5.2 Homogeneidad visual: boxplots
boxplot(res_fin ~ datos$FactorA, main="Residuos vs FactorA")
abline(h=0,lty=2)
boxplot(res_fin ~ datos$FactorB, main="Residuos vs FactorB")
abline(h=0,lty=2)

## 5.3 Independencia visual: residuos estudentizados
plot(rstandard(modelo), type="b", pch=19,
     main="Resid estudentizados vs Orden")
abline(h=0,lty=2)

## 5.4 Interacción: si p<0.05 en FactorA:FactorB
# ya se ve en el summary

# 6) Efectos y gráficas ----------------------------------------------------
emm <- emmeans(modelo, ~ FactorA * FactorB)
summary(emm)

# Interaction plot base R
interaction.plot(datos$FactorA, datos$FactorB, datos$Respuesta,
                 fun=mean, type="b", pch=c(1,19),
                 col=c("blue","red"),
                 xlab="FactorA", ylab="Media Respuesta",
                 trace.label="FactorB")

# 7) Comparaciones múltiples post‑hoc --------------------------------------
# Si interacción no significativa, usar emmeans por factor
cat("\n--- Post-hoc FactorA ---\n")
print(pairs(emmeans(modelo, ~ FactorA), adjust="tukey"))
cat("\n--- Post-hoc FactorB ---\n")
print(pairs(emmeans(modelo, ~ FactorB), adjust="tukey"))

# 8) Interpretación ---------------------------------------------------------
# - Revisar pValores en summary para FactorA, FactorB e interacción.
# - Si interacción significativa, enfocar análisis en interacción.
# - Si no, interpretar efectos principales y post-hoc.
