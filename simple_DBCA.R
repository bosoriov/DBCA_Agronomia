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
