# ─────────────────────────────────────────────────────────────────────────
#            PLANTILLA ANOVA de DOS FACTORES (con interacción)
# ─────────────────────────────────────────────────────────────────────────

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
datos$<factorA> <- factor(datos$<factorA>)
datos$<factorB> <- factor(datos$<factorB>)

# 3) Verificación de supuestos PRE‑ANOVA -----------------------------------

## 3.1 Normalidad de residuos
mod_pre <- aov(<response> ~ <factorA> * <factorB>, data = datos)
res_pre <- residuals(mod_pre)
shapiro <- shapiro.test(res_pre)
cat("Shapiro-Wilk: W =", round(shapiro$statistic,3),
    " p =", round(shapiro$p.value,4), "\n")

## 3.2 Homogeneidad de varianzas (Levene)  
levene <- leveneTest(<response> ~ <factorA> * <factorB>, data = datos)
print(levene)

## 3.3 Independencia de residuos  
plot(res_pre, type = "b", pch = 19,
     xlab = "Índice de obs.", ylab = "Residuo",
     main = "Residuos vs Orden (pre‑ANOVA)")
abline(h = 0, lty = 2)

# Si p>0.05 en Shapiro y Levene, puedes seguir.

# 4) Ajustar ANOVA de dos factores ------------------------------------------
modelo2 <- aov(<response> ~ <factorA> * <factorB>, data = datos)
cat("\n--- RESUMEN ANOVA 2 FACTORES ---\n")
print(summary(modelo2))

# 5) Diagnósticos POST‑ANOVA ------------------------------------------------

## 5.1 Normalidad definitiva
res_fin <- residuals(modelo2)
print(shapiro.test(res_fin))

## 5.2 Homogeneidad visual  
boxplot(res_fin ~ datos$<factorA>, main="Residuos vs Factor A")
abline(h = 0, lty = 2)
boxplot(res_fin ~ datos$<factorB>, main="Residuos vs Factor B")
abline(h = 0, lty = 2)

## 5.3 Independencia visual  
plot(rstandard(modelo2), type="b", pch=19,
     main="Residuos estudentizados vs Obs.")
abline(h=0, lty=2)

# 6) Efectos principales y gráficas -----------------------------------------

# Medias marginales con error estándar
emm <- emmeans(modelo2, ~ <factorA> * <factorB>)
summary(emm)

# Interaction plot
interaction.plot(x.factor = datos$<factorA>,
                 trace.factor = datos$<factorB>,
                 response = datos$<response>,
                 fun = mean,
                 type = "b",
                 pch = c(1,19),
                 col = c("blue","red"),
                 xlab = "Factor A",
                 ylab = "Media de <response>",
                 trace.label = "Factor B")

# 7) Post‑hoc de comparaciones múltiples ------------------------------------

# Por factor A
pairs(emmeans(modelo2, ~ <factorA>), adjust = "tukey")

# Por factor B
pairs(emmeans(modelo2, ~ <factorB>), adjust = "tukey")

# 8) Interpretación ---------------------------------------------------------
# - Revisa el interaction term en summary(modelo2). Si p<0.05 → hay interacción.
# - Si no hay interacción, interpreta efectos principales de FactorA y FactorB.
# - Utiliza los resultados de pairs(...) para ver qué niveles difieren.
