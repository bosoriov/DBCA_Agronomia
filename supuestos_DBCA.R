# 1. Construcción del dataset del ensayo en DBCA (4 tratamientos x 5 bloques)
bloque <- factor(rep(paste0("Bloque_", 1:5), each = 4))
tratamiento <- factor(rep(c("Testigo_Convencional",
                            "NPK_Balanceado",
                            "NPK_Mas_Magnesio",
                            "Enmienda_Organica_NPK"), times = 5))

# Rendimiento (kg/ha de cacao seco) con gradiente entre bloques y efecto de tratamientos
rendimiento <- c(
  1120, 1280, 1360, 1410,  # Bloque_1
  1150, 1310, 1400, 1450,  # Bloque_2
  1190, 1350, 1440, 1490,  # Bloque_3
  1210, 1380, 1470, 1520,  # Bloque_4
  1240, 1410, 1500, 1560   # Bloque_5
)

datos_cacao <- data.frame(bloque, tratamiento, rendimiento)

# 2. Ajuste del modelo lineal aditivo del DBCA
modelo_cacao <- lm(rendimiento ~ bloque + tratamiento, data = datos_cacao)

# 3. Tabla ANOVA
anova(modelo_cacao)

# 4. Coeficiente de Variación (CV) con agricolae y cálculo manual
if (!require(agricolae)) install.packages("agricolae")
library(agricolae)

cv_agricolae <- cv.model(modelo_cacao)
print(cv_agricolae)

# 5. Comparaciones múltiples de Tukey con letras de significancia (emmeans + multcomp)
if (!require(emmeans)) install.packages("emmeans")
if (!require(multcomp)) install.packages("multcomp")
library(emmeans)
library(multcomp)

medias_trt <- emmeans(modelo_cacao, ~ tratamiento)
letras_tukey <- cld(medias_trt, adjust = "tukey", Letters = letters)
print(letras_tukey)




# Continuando con el modelo DBCA del ensayo de cacao (4 tratamientos x 5 bloques)
modelo_cacao <- lm(rendimiento ~ bloque + tratamiento, data = datos_cacao)

# 1. Diagnóstico gráfico base (residuos vs. ajustados y Q-Q plot)
par(mfrow = c(1, 2))
plot(modelo_cacao, which = 1) # Residuos vs. Valores ajustados
plot(modelo_cacao, which = 2) # Normal Q-Q plot
par(mfrow = c(1, 1))

# 2. Prueba formal de normalidad (Shapiro-Wilk sobre los residuos)
residuos_dbca <- residuals(modelo_cacao)
shapiro.test(residuos_dbca)

# 3. Prueba de homocedasticidad (Levene sobre residuos o variable respuesta)
if (!require(car)) install.packages("car")
library(car)
leveneTest(rendimiento ~ tratamiento, data = datos_cacao)
