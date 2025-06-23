# Datos
Lumenes <- c(132.5, 117.1, 154.2, 105.9, 146.7, 130.5, 143.6, 126.4)
Polvo <- factor(rep(1:4, times = 2))
Turno <- factor(rep(c("Turno1", "Turno2"), each = 4))

df <- data.frame(Lumenes, Polvo, Turno)

# ANOVA de bloques
modelo <- aov(Lumenes ~ Polvo + Turno, data = df)
summary(modelo)

# Verificación de supuestos
library(car)

# 1. Normalidad (Shapiro-Wilk)
shapiro.test(residuals(modelo))  # p > 0.05 cumple

# 2. Homocedasticidad (Levene)
leveneTest(Lumenes ~ interaction(Polvo, Turno), data = df)  # p > 0.05 cumple

# Gráficos de diagnóstico
par(mfrow = c(1, 2))
plot(modelo, which = 1)  # Residuos vs. Ajustados
plot(modelo, which = 2)  # QQ-Plot