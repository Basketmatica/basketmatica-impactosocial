library(tidyverse)

# Cargar los datos
players_df <- read.csv("C:/Users/Yago/Documents/Basketmática/Social_Agosto2024/players_complete_stats_clean.csv")

# Seleccionar columnas relevantes para el análisis y eliminar las filas con valores nulos
df <- players_df %>% 
  select(PAGEVIEWS, TWITTER_FAVORITE_COUNT, TWITTER_RETWEET_COUNT, 
         FGA, W, MP, POINTS, BLK, TOV, POSITION, SALARY_MILLIONS, WINS_RPM) %>%
  na.omit()

# Construcción de modelos de regresión lineal
# Modelo 1: Relaciona las visitas en Wikipedia con las demás variables (excluyendo favoritos y retweets en Twitter)
model_views <- lm(PAGEVIEWS ~ . - TWITTER_FAVORITE_COUNT - TWITTER_RETWEET_COUNT, data=df)
summary(model_views)

# Modelo 2: Relaciona los retweets en Twitter con las demás variables (excluyendo visitas en Wikipedia y favoritos en Twitter)
model_retweets <- lm(TWITTER_RETWEET_COUNT ~ . - TWITTER_FAVORITE_COUNT - PAGEVIEWS, data=df)
summary(model_retweets)

# Modelo 3: Relaciona los favoritos en Twitter con las demás variables (excluyendo visitas en Wikipedia y retweets en Twitter)
model_likes <- lm(TWITTER_FAVORITE_COUNT ~ . - PAGEVIEWS - TWITTER_RETWEET_COUNT, data=df)
summary(model_likes)

# Visualización de los resultados
# Instalar y cargar la librería ggplot2 si no está instalada
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Visualización 1: Importancia de características para las vistas en Wikipedia (PAGEVIEWS)
# Obtener los coeficientes del modelo y organizar los datos para visualización.
coef_wiki <- summary(model_views)$coefficients
coef_wiki_df <- data.frame(
  Variable = rownames(coef_wiki),
  Coeficiente = coef_wiki[, "Estimate"],
  P_Value = coef_wiki[, "Pr(>|t|)"]
)

# Agregar una columna que define el color del gráfico en función del p-valor
# (rojo para significancia estadística < 0.05, azul para no significancia).
coef_wiki_df <- coef_wiki_df %>%
  mutate(Color = ifelse(P_Value < 0.05, "red", "skyblue"))

# Crear un gráfico de barras ordenado por la magnitud de los coeficientes.
ggplot(coef_wiki_df, aes(x = reorder(Variable, Coeficiente), y = Coeficiente, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(title = "Importancia de Características para Vistas en Wikipedia") +
  coord_flip() +
  theme_minimal()

# Visualización 2: Importancia de características para retweets en Twitter (TWITTER_RETWEET_COUNT)
# Obtener los coeficientes del modelo y organizar los datos para visualización.
coef_retweets <- summary(model_retweets)$coefficients
coef_retweets_df <- data.frame(
  Variable = rownames(coef_retweets),
  Coeficiente = coef_retweets[, "Estimate"],
  P_Value = coef_retweets[, "Pr(>|t|)"]
)

# Agregar una columna que define el color del gráfico en función del p-valor
# (rojo para significancia estadística < 0.05, azul para no significancia).
coef_retweets_df <- coef_retweets_df %>%
  mutate(Color = ifelse(P_Value < 0.05, "red", "skyblue"))

# Crear un gráfico de barras ordenado por la magnitud de los coeficientes.
ggplot(coef_retweets_df, aes(x = reorder(Variable, Coeficiente), y = Coeficiente, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(title = "Importancia de Características para Twitter Retweets") +
  coord_flip() +
  theme_minimal()

# Visualización 3: Importancia de características para favoritos en Twitter (TWITTER_FAVORITE_COUNT)
# Obtener los coeficientes del modelo y organizar los datos para visualización.
coef_likes <- summary(model_likes)$coefficients
coef_likes_df <- data.frame(
  Variable = rownames(coef_likes),
  Coeficiente = coef_likes[, "Estimate"],
  P_Value = coef_likes[, "Pr(>|t|)"]
)

# Agregar una columna que define el color del gráfico en función del p-valor
# (rojo para significancia estadística < 0.05, azul para no significancia).
coef_likes_df <- coef_likes_df %>%
  mutate(Color = ifelse(P_Value < 0.05, "red", "skyblue"))

# Crear un gráfico de barras ordenado por la magnitud de los coeficientes.
ggplot(coef_likes_df, aes(x = reorder(Variable, Coeficiente), y = Coeficiente, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(title = "Importancia de Características para Twitter Likes") +
  coord_flip() +
  theme_minimal()
