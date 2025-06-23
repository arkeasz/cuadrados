# Datos
Flujo <- c(88, 91, 87, 92, 87, 95, 89, 91, 88, 94, 90, 93)
Operador <- factor(rep(c("Op1", "Op2"), each = 6))
Presion <- factor(rep(rep(c("1000", "1100", "1200"), each = 2), times = 2))
Gas <- factor(rep(c("N2", "ArN2"), each = 3, times = 2))

df <- data.frame(Flujo, Operador, Presion, Gas)

# Modelo ANOVA con bloque (operador) e interacción
modelo <- aov(Flujo ~ Operador + Presion * Gas, data = df)
summary(modelo)

# Verificación de supuestos
library(car)

# 1. Normalidad (Shapiro-Wilk)
shapiro.test(residuals(modelo))

# 2. Homocedasticidad (Levene)
leveneTest(Flujo ~ interaction(Presion, Gas, Operador), data = df)

# Gráficos de diagnóstico
par(mfrow = c(1, 2))
plot(modelo, which = 1)  # Residuos vs. Ajustados
plot(modelo, which = 2)  # QQ-Plot