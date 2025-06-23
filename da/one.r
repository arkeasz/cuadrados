# Datos
Flujo <- c(88, 89, 91, 91, 87, 88, 92, 94, 87, 90, 95, 93)
Presion <- factor(rep(c("1000", "1100", "1200"), each = 2, times = 2))
Gas <- factor(rep(c("N2", "ArN2"), each = 6))

df <- data.frame(Flujo, Presion, Gas)

# ANOVA de dos factores con interacción
modelo_con_interaccion <- aov(Flujo ~ Presion * Gas, data = df)
summary(modelo_con_interaccion)

# Verificación de supuestos
# 1. Homocedasticidad (Test de Levene)
library(car)
leveneTest(Flujo ~ interaction(Presion, Gas), data = df)

# 2. Normalidad (Shapiro-Wilk)
residuos <- residuals(modelo_con_interaccion)
shapiro.test(residuos)

# Gráficos de diagnóstico
par(mfrow = c(1, 2))
plot(modelo_con_interaccion, which = 1)  # Residuos vs. Ajustados
plot(modelo_con_interaccion, which = 2)  # QQ-Plot

# ANOVA sin interacción (si no es significativa)
modelo_sin_interaccion <- aov(Flujo ~ Presion + Gas, data = df)
summary(modelo_sin_interaccion)

# Prueba post-hoc para presión (Tukey HSD)
# if (summary(modelo_con_interaccion)[[1]]$"Pr(>F)"[3] > 0.05) {
  TukeyHSD(modelo_sin_interaccion, "Presion")
# }