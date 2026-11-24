# 1. Simulación de datos en DBCA (4 tratamientos, 5 bloques)
tratamiento <- factor(rep(c("Variedad_A", "Variedad_B", "Variedad_C", "Variedad_D"), times = 5))
bloque <- factor(rep(1:5, each = 4))
rendimiento <- c(4.1, 5.2, 3.8, 4.5,
                 4.3, 5.5, 4.0, 4.8,
                 4.6, 5.8, 4.2, 5.1,
                 4.8, 6.0, 4.4, 5.3,
                 5.0, 6.2, 4.6, 5.5)

datos_dbca <- data.frame(bloque, tratamiento, rendimiento)

# 2. Ajuste del modelo lineal para DBCA
modelo_dbca <- lm(rendimiento ~ bloque + tratamiento, data = datos_dbca)

# 3. Tabla ANOVA
anova(modelo_dbca)

# 4. Cálculo automático del Coeficiente de Variación (CV) con agricolae
if(!require(agricolae)) install.packages("agricolae")
library(agricolae)
cv.model(modelo_dbca)

# 5. Comparación de medias con Tukey
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)
library(multcomp)
medias_dbca <- emmeans(modelo_dbca, ~ tratamiento)
letras_tukey<- cld(medias_dbca,adjust = "tukey", Letters = letters)
print(letras_tukey)










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
