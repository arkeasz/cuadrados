# ══════════════════════════════════════════════════════════════
#  TEMPLATE ANÁLISIS DBCA
#  – Leer datos
#  – Verificar supuestos
#  – Ajustar ANOVA con bloques
#  – Diagnósticos
# ══════════════════════════════════════════════════════════════

# 0) Paquetes mínimos -------------------------------------------------------
if(!require(car))      install.packages("car",      repos="https://cran.r-project.org/")
if(!require(lmtest))   install.packages("lmtest",   repos="https://cran.r-project.org/")
if(!require(ggplot2))  install.packages("ggplot2",  repos="https://cran.r-project.org/")
library(car); library(lmtest); library(ggplot2)

# 1) Importar datos ---------------------------------------------------------
# Ejemplo: desde Excel
# library(readxl)
# datos <- read_excel("ruta/al/archivo.xlsx", sheet = "Hoja1")

# O desde CSV:
# datos <- read.csv("ruta/al/archivo.csv", header = TRUE)

# Supón que tu data.frame final se llama:
# datos
# y tiene al menos estas columnas:
#   - <response>     (numérica; e.g. "moscasmuertas", "rendimiento", ...)
#   - <treatment>    (factor; tratamientos, e.g. "marcas", "fertilizante")
#   - <block>        (factor; bloques, e.g. "dias", "bloques")

# 2) Asegurar factores ------------------------------------------------------
datos$<treatment> <- factor(datos$<treatment>)
datos$<block>     <- factor(datos$<block>)

# 3) Supuestos ANTES de ANOVA -----------------------------------------------

## 3.1 Normalidad por tratamiento (Shapiro–Wilk)
for (tr in levels(datos$<treatment>)) {
  xi <- datos$<response>[datos$<treatment> == tr]
  sw <- shapiro.test(xi)
  cat("Shapiro ", tr, ": W =", round(sw$statistic,3),
      " p =", round(sw$p.value,4), "\n")
}

## 3.2 Homogeneidad de varianzas (Levene)
lev <- leveneTest(<response> ~ <treatment>, data = datos)
print(lev)

## 3.3 Independencia (Durbin–Watson)
# Ajustamos un modelo simple solo con tratamiento para extraer residuos
mod_pre <- aov(<response> ~ <treatment>, data = datos)
dw  <- dwtest(<response> ~ <treatment> + <block>, data = datos)
cat("Durbin-Watson: DW =", round(dw$statistic,3),
    " p =", round(dw$p.value,4), "\n")

# Gráfico residuos vs orden
stud_res_pre <- rstandard(mod_pre)
plot(stud_res_pre, type="b", pch=19,
     main="Residuos estudentizados vs orden (preliminar)",
     xlab="Observación", ylab="Residuo")
abline(h=0, lty=2)

## 3.4 Aditividad (no interacción)
mod_int <- aov(<response> ~ <treatment> * <block>, data = datos)
cat("\nANOVA interacción bloque×tratamiento:\n")
print(anova(mod_int)["<treatment>:<block>",])

# Si todos los tests de supuestos p>0.05 y la interacción no es significativa,
# procedemos al ANOVA final.

# 4) ANOVA con bloques completos al azar ------------------------------------
modelo <- aov(<response> ~ <treatment> + <block>, data = datos)
cat("\n--- RESULTADO ANOVA DBCA ---\n")
print(summary(modelo))

# 5) Diagnósticos POST‑ANOVA -----------------------------------------------

## 5.1 Normalidad definitiva (Shapiro sobre residuos del modelo)
resid_fin <- residuals(modelo)
sw2 <- shapiro.test(resid_fin)
cat("Shapiro residuos final: W =", round(sw2$statistic,3),
    " p =", round(sw2$p.value,4), "\n")

## 5.2 Homogeneidad (residuos vs tratamientos)
boxplot(resid_fin ~ datos$<treatment>,
        main="Residuos vs Tratamiento (post‑ANOVA)",
        xlab="Tratamiento", ylab="Residuo")
abline(h=0, lty=2)

## 5.3 Independencia (residuos vs orden)
stud_res_fin <- rstandard(modelo)
plot(stud_res_fin, type="b", pch=19,
     main="Residuos estudentizados vs orden (post‑ANOVA)",
     xlab="Observación", ylab="Residuo")
abline(h=0, lty=2)

## 5.4 Interacción definitiva (opcional)
# Ya visto antes, pero si quieres repetir:
mod_int2 <- aov(<response> ~ <treatment> * <block>, data = datos)
cat("\nANOVA interacción final:\n")
print(anova(mod_int2)["<treatment>:<block>",])

# 6) (Opcional) Post-hoc Tukey HSD ------------------------------------------
# tuk <- TukeyHSD(modelo, "<treatment>")
# print(tuk)

# 7) Interpretación final ---------------------------------------------------
# – Si Pr(>F) de tratamiento < α: diferencias entre tratamientos.
# – Bloques: debe ser no significativo (o relevante solo como control).
# – Supuestos comprobados p>0.05 → análisis válido.
