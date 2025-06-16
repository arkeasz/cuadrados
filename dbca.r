# instalacion de paquetes necesarios
if (!require(readxl)) install.packages("readxl")
if (!require(car)) install.packages("car")
if (!require(ggplot2)) install.packages("ggplot2")

library(readxl)
library(car)
library(ggplot2)

# Load the data from the Excel file
data <- read_xlsx("./MOSCAS.xlsx")

data$BLOQUES <- as.factor(data$BLOQUES)
data$TRATAMIENTOS <- as.factor(data$TRATAMIENTOS)
data$MOSCAS <- as.numeric(data$MOSCAS)

# Hipotesis del diseño
# H_0: No hay diferencias significativas entre los tratamientos.
# H_a: Al menos un tratamiento es diferente.

# supuestos antes de anova
# Normalidad de cada tratamiento
sha_A <- shapiro.test(data$MOSCAS[data$TRATAMIENTOS == "A"])
sha_B <- shapiro.test(data$MOSCAS[data$TRATAMIENTOS == "B"])
sha_C <- shapiro.test(data$MOSCAS[data$TRATAMIENTOS == "C"])

for (sha in list(sha_A, sha_B, sha_C)) {
  if (sha$p.value < 0.05) {
    print(paste("Normalidad no se cumple para tratamiento:", sha$data.name))
  } else {
    print(paste("Normalidad se cumple para tratamiento:", sha$data.name))
  }
}

# Homogeneidad de varianzas
levene_test <- leveneTest(MOSCAS ~ TRATAMIENTOS, data = data)
p_levene <- levene_test[3]$Pr[1]

if (p_levene < 0.05) {
  print("Homogeneidad de varianzas no se cumple.")
} else {
  print("Homogeneidad de varianzas se cumple.")
}

# Independencia (grafico residuos estudentizados vs orden)
mode_prelim <- aov(MOSCAS ~ TRATAMIENTOS, data = data)
stud_res <- rstudent(mode_prelim)
plot(stud_res, type = "b", pch = 19,
    xlab = "Indice de observación",
    ylab = "Residuos estudentizados",
    main = "Residuos estudentizados vs Orden de observación"
)
abline(h = 0, col = "red")

# Interacción entre bloques y tratamientos
mode_int <- aov(MOSCAS ~ BLOQUES * TRATAMIENTOS, data = data)
cat("\nInteracción entre bloques y tratamientos:\n")
summary(mode_int)

# ANOVA con bloques
modelo <- aov(MOSCAS ~ BLOQUES + TRATAMIENTOS, data = data)
cat("\nANOVA con bloques:\n")
summary(modelo)

res <- residuals(modelo)
leveneTest(res ~ TRATAMIENTOS, data = data)

boxplot(res ~ data$TRATAMIENTOS,
        data = data,
        xlab = "Tratamientos",
        ylab = "Residuos",
        main = "Residuos vs Tratamientos",
        col = c("#E69F00","#56B4E9","#009E73"))
abline(h = 0, lty = 2)

# – Las cajas tienen rangos y bigotes de longitud comparable, sin un spread claramente distinto en ningún tratamiento.
# – Esto visualmente confirma que ningún grupo muestra dispersión de residuos anormalmente mayor o menor.

# Grafica de medias con error estándar
tuk <- TukeyHSD(modelo, "TRATAMIENTOS")
cat("\nTukey HSD:\n")
tuk

# extraer p-values de Tukey
# los nombres seran "A-B", "A-C", "B-C"

p_values <- tuk$TRATAMIENTOS[, "p adj"]

#           diff        lwr       upr     p adj
# B-A -10.333333 -21.934911  1.268245 0.0813437
# C-A  -6.666667 -18.268245  4.934911 0.2999465
# C-B   3.666667  -7.934911 15.268245 0.6723754

# – B vs A: p = 0.0813 > 0.05 → no hay diferencia significativa.
# – C vs A: p = 0.2999 > 0.05 → no hay diferencia significativa.
# – C vs B: p = 0.6724 > 0.05 → no hay diferencia significativa.

# Conclusion:
# No hay diferencias significativas entre los tratamientos A, B y C.
# Es consistente con anova con bloques, que no muestra diferencias significativas entre tratamientos.
# No hay evidencia suficiente para rechazar la hipótesis nula de que no hay diferencias significativas entre los tratamientos.

# Gráfico de medias con error estándar
ggplot(data, aes(x = TRATAMIENTOS, y = MOSCAS, fill = TRATAMIENTOS)) +
  stat_summary(fun.data = mean_se, geom = "bar", position = "dodge") +
  labs(title = "Medias de Moscas por Tratamiento",
       x = "Tratamientos",
       y = "Número de Moscas") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")