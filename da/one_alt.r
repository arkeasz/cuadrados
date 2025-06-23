# 0) Cargar paquetes --------------------------------------------------------
if (!require(car))      install.packages("car", repos = "https://cran.r-project.org/")
if (!require(emmeans))  install.packages("emmeans", repos = "https://cran.r-project.org/")
if (!require(ggplot2))  install.packages("ggplot2", repos = "https://cran.r-project.org/")
library(car); library(emmeans); library(ggplot2)

# 1) Cargar y preparar datos ------------------------------------------------
Flujo <- c(88, 89, 91, 91, 87, 88, 92, 94, 87, 90, 95, 93)
Presion <- factor(rep(c("1000", "1100", "1200"), each = 2, times = 2))
Gas <- factor(rep(c("N2", "ArN2"), each = 6))

datos <- data.frame(Flujo, Presion, Gas)

# 2) Convertir factores -----------------------------------------------------
datos$Presion <- factor(datos$Presion)  # Factor A
datos$Gas     <- factor(datos$Gas)      # Factor B

# 3) Supuestos PRE‑ANOVA ----------------------------------------------------
## 3.1 Normalidad de residuos
mod_pre <- aov(Flujo ~ Presion * Gas, data = datos)
res_pre <- residuals(mod_pre)
shapiro <- shapiro.test(res_pre)
cat("Shapiro-Wilk: W =", round(shapiro$statistic, 3),
    " p =", round(shapiro$p.value, 4), "\n")

## 3.2 Homogeneidad de varianzas (Levene)
levene <- leveneTest(Flujo ~ Presion * Gas, data = datos)
print(levene)

## 3.3 Independencia de residuos (visual)
plot(res_pre, type = "b", pch = 19,
     xlab = "Índice de obs.", ylab = "Residuo",
     main = "Residuos vs Orden (pre‑ANOVA)")
abline(h = 0, lty = 2)

# 4) ANOVA de dos factores --------------------------------------------------
modelo2 <- aov(Flujo ~ Presion * Gas, data = datos)
cat("\n--- RESUMEN ANOVA 2 FACTORES ---\n")
print(summary(modelo2))

# 5) Diagnósticos POST‑ANOVA ------------------------------------------------
## 5.1 Normalidad definitiva
res_fin <- residuals(modelo2)
print(shapiro.test(res_fin))

## 5.2 Homogeneidad visual
boxplot(res_fin ~ datos$Presion, main = "Residuos vs Presión")
abline(h = 0, lty = 2)
boxplot(res_fin ~ datos$Gas, main = "Residuos vs Gas")
abline(h = 0, lty = 2)

## 5.3 Independencia visual
plot(rstandard(modelo2), type = "b", pch = 19,
     main = "Residuos estudentizados vs Obs.")
abline(h = 0, lty = 2)

# 6) Efectos principales e interacción --------------------------------------
emm <- emmeans(modelo2, ~ Presion * Gas)
print(summary(emm))

interaction.plot(x.factor = datos$Presion,
                 trace.factor = datos$Gas,
                 response = datos$Flujo,
                 fun = mean,
                 type = "b",
                 pch = c(1,19),
                 col = c("blue","red"),
                 xlab = "Presión",
                 ylab = "Media de Flujo",
                 trace.label = "Gas")

# 7) Comparaciones múltiples (Tukey) ----------------------------------------
pairs(emmeans(modelo2, ~ Presion), adjust = "tukey")
pairs(emmeans(modelo2, ~ Gas), adjust = "tukey")

# 8) Interpretación ---------------------------------------------------------
# - Revisa si hay interacción significativa (p < 0.05).
# - Si la hay, interpreta con la gráfica de interacción.
# - Si no la hay, interpreta efectos principales con el post-hoc.
