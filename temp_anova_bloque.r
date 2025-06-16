# PLANTILLA ANOVA de DOS FACTORES CON BLOQUE (Diseño con Bloques)
# ─────────────────────────────────────────────────────────────────────────
# Ejemplo de datos (ASCII):
# +-----------+---------+---------+--------+---------+
# | Bloque    | FactorA | FactorB | Réplica| Respuesta|
# +-----------+---------+---------+--------+---------+
# | 1         | A       | X       | 1      | 4.8     |
# | 1         | A       | X       | 2      | 5.0     |
# | 1         | A       | Y       | 1      | 5.2     |
# | 1         | A       | Y       | 2      | 5.1     |
# | 1         | B       | X       | 1      | 4.5     |
# | 1         | B       | X       | 2      | 4.7     |
# | 1         | B       | Y       | 1      | 5.0     |
# | 1         | B       | Y       | 2      | 5.2     |
# | 2         | A       | X       | 1      | 5.1     |
# | 2         | A       | X       | 2      | 5.3     |
# | ...       | ...     | ...     | ...    | ...     |
# +-----------+---------+---------+--------+---------+
#   Bloque    : Factor de bloqueo (e.g. días, lotes, bloques)
#   FactorA   : Primero de interés (p.ej. fertilizante)
#   FactorB   : Segundo de interés (p.ej. riego)
#   Réplica   : réplicas dentro de cada combinación
#   Respuesta : variable de respuesta (numérica)

# 0) Paquetes necesarios ---------------------------------------------------
if(!require(car))      install.packages("car",      repos="https://cran.r-project.org/")
if(!require(emmeans))  install.packages("emmeans",  repos="https://cran.r-project.org/")
if(!require(ggplot2))  install.packages("ggplot2",  repos="https://cran.r-project.org/")
library(car); library(emmeans); library(ggplot2)

# 1) Importar datos ---------------------------------------------------------
# datos <- read.csv("ruta/archivo.csv")
# o usando readxl:
# library(readxl)
# datos <- read_excel("ruta/archivo.xlsx", sheet = "Hoja1")

# 2) Factores y respuesta ---------------------------------------------------
datos$Bloque    <- factor(datos$Bloque)
datos$FactorA   <- factor(datos$FactorA)
datos$FactorB   <- factor(datos$FactorB)
datos$Respuesta <- as.numeric(datos$Respuesta)

# 3) Supuestos PRE‑ANOVA ----------------------------------------------------
## 3.1 Normalidad de residuos preliminares (Shapiro-Wilk)
mod_pre <- aov(Respuesta ~ FactorA * FactorB + Error(Bloque), data = datos)
res_pre <- residuals(mod_pre[[2]])
shapiro <- shapiro.test(res_pre)
cat("Shapiro-Wilk (preliminar): W =", round(shapiro$statistic,3), " p =", round(shapiro$p.value,4), "\n")

## 3.2 Homogeneidad (Levene) sobre residuos preliminares
levene <- leveneTest(res_pre ~ datos$FactorA * datos$FactorB)
print(levene)

## 3.3 Independencia de residuos (graf. vs orden)
plot(res_pre, type = "b", pch = 19,
     main = "Residuos vs Orden (pre-ANOVA)", xlab = "Obs.", ylab = "Residuo")
abline(h = 0, lty = 2)

# 4) Ajustar ANOVA de dos factores con bloque --------------------------------
modelo <- aov(Respuesta ~ FactorA * FactorB + Bloque, data = datos)
cat("\n--- RESUMEN ANOVA 2 Factores con Bloque ---\n")
print(summary(modelo))

# 5) Diagnósticos POST‑ANOVA ------------------------------------------------
## Residuos definitivos
res_fin <- residuals(modelo)

## 5.1 Normalidad definitiva
print(shapiro.test(res_fin))

## 5.2 Homogeneidad visual
boxplot(res_fin ~ datos$FactorA, main = "Residuos vs FactorA")
abline(h = 0, lty = 2)
boxplot(res_fin ~ datos$FactorB, main = "Residuos vs FactorB")
abline(h = 0, lty = 2)

## 5.3 Independencia visual
plot(rstandard(modelo), type = "b", pch = 19,
     main = "Residuos estudentizados vs Obs.")
abline(h = 0, lty = 2)

# 6) Efectos e interacción --------------------------------------------------
emm <- emmeans(modelo, ~ FactorA * FactorB)
summary(emm)

# Diagrama de interacción
interaction.plot(datos$FactorA, datos$FactorB, datos$Respuesta,
                 fun = mean, type = "b", pch = c(1,19),
                 col = c("blue","red"),
                 xlab = "Factor A", ylab = "Media Respuesta",
                 trace.label = "Factor B")

# 7) Post‑hoc Tukey ---------------------------------------------------------
# Comparaciones de FactorA
print(pairs(emmeans(modelo, ~ FactorA), adjust = "tukey"))
# Comparaciones de FactorB
print(pairs(emmeans(modelo, ~ FactorB), adjust = "tukey"))

# 8) Interpretación ---------------------------------------------------------
# - Revisar pValores en summary(modelo) para Bloque, FactorA, FactorB, FactorA:FactorB
# - Si interacción significativa (p<0.05), enfocar en interacción.
# - Si no, interpretar efectos principales.
# - Usar pairs(...) para identificar niveles que difieren.
