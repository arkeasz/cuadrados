# TEMPLATE ANOVA de DOS FACTORES (con interacción y ejemplo ASCII)
# ─────────────────────────────────────────────────────────────────────────
# Ejemplo de datos (ASCII):
# +-----------+---------+---------+---------+
# | Fertiliz.| Riego   | Réplica | Rendim. |
# +-----------+---------+---------+---------+
# | A         | Bajo    | 1       | 4.8     |
# | A         | Bajo    | 2       | 5.0     |
# | A         | Alto    | 1       | 5.2     |
# | A         | Alto    | 2       | 5.1     |
# | B         | Bajo    | 1       | 4.5     |
# | B         | Bajo    | 2       | 4.7     |
# | B         | Alto    | 1       | 5.0     |
# | B         | Alto    | 2       | 5.2     |
# | C         | Bajo    | 1       | 5.1     |
# | C         | Bajo    | 2       | 5.3     |
# | C         | Alto    | 1       | 5.5     |
# | C         | Alto    | 2       | 5.4     |
# +-----------+---------+---------+---------+
#   Fertiliz.: Factor A (A, B, C)
#   Riego    : Factor B (Bajo, Alto)
#   Réplica  : Réplicaciones dentro de cada combinación
#   Rendim.  : Variable respuesta (numérica)

# 0) Paquetes necesarios ---------------------------------------------------
if(!require(car))      install.packages("car",      repos="https://cran.r-project.org/")
if(!require(emmeans))  install.packages("emmeans",  repos="https://cran.r-project.org/")
if(!require(ggplot2))  install.packages("ggplot2",  repos="https://cran.r-project.org/")
library(car); library(emmeans); library(ggplot2)

# 1) Importar datos ---------------------------------------------------------
# Ejemplo CSV:
# datos <- read.csv("ruta/tu_archivo.csv", header = TRUE)
# Ejemplo Excel:
# library(readxl)
# datos <- read_excel("ruta/tu_archivo.xlsx", sheet = "Hoja1")

# 2) Convertir factores -----------------------------------------------------
datos$Fertiliz <- factor(datos$Fertiliz)    # Factor A
datos$Riego    <- factor(datos$Riego)       # Factor B

# 3) Verificación de supuestos PRE‑ANOVA -----------------------------------
## 3.1 Normalidad de residuos
mod_pre <- aov(Rendim ~ Fertiliz * Riego, data = datos)
res_pre <- residuals(mod_pre)
shapiro <- shapiro.test(res_pre)
cat("Shapiro-Wilk: W =", round(shapiro$statistic,3), " p =", round(shapiro$p.value,4), "\n")

## 3.2 Homogeneidad de varianzas (Levene)
levene <- leveneTest(Rendim ~ Fertiliz * Riego, data = datos)
print(levene)

## 3.3 Independencia de residuos
plot(res_pre, type = "b", pch = 19,
     xlab = "Índice de obs.", ylab = "Residuo",
     main = "Residuos vs Orden (pre‑ANOVA)")
abline(h = 0, lty = 2)

# 4) Ajustar ANOVA de dos factores ------------------------------------------
modelo2 <- aov(Rendim ~ Fertiliz * Riego, data = datos)
cat("\n--- RESUMEN ANOVA 2 FACTORES ---\n")
print(summary(modelo2))

# 5) Diagnósticos POST‑ANOVA ------------------------------------------------
## 5.1 Normalidad definitiva
res_fin <- residuals(modelo2)
print(shapiro.test(res_fin))

## 5.2 Homogeneidad visual
boxplot(res_fin ~ datos$Fertiliz, main="Residuos vs Fertiliz")
abline(h = 0, lty = 2)
boxplot(res_fin ~ datos$Riego, main="Residuos vs Riego")
abline(h = 0, lty = 2)

## 5.3 Independencia visual
plot(rstandard(modelo2), type = "b", pch = 19,
     main = "Residuos estudentizados vs Obs.")
abline(h = 0, lty = 2)

# 6) Efectos principales y gráficas -----------------------------------------
emm <- emmeans(modelo2, ~ Fertiliz * Riego)
summary(emm)

interaction.plot(x.factor   = datos$Fertiliz,
                 trace.factor = datos$Riego,
                 response      = datos$Rendim,
                 fun           = mean,
                 type          = "b",
                 pch           = c(1,19),
                 col           = c("blue","red"),
                 xlab          = "Fertiliz",
                 ylab          = "Media de Rendim.",
                 trace.label   = "Riego")

# 7) Post‑hoc de comparaciones múltiples ------------------------------------
# Por factor Fertiliz
pairs(emmeans(modelo2, ~ Fertiliz), adjust = "tukey")
# Por factor Riego
pairs(emmeans(modelo2, ~ Riego), adjust = "tukey")

# 8) Interpretación ---------------------------------------------------------
# - Revisar p de interacción en summary(modelo2).
# - Si interacción p<0.05 -> interpretar curvas de interacción.
# - Si no, interpretar efectos principales.
# - Usar pairs(...) para ver comparaciones significativas.
