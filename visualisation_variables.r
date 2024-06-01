# Importation des librairies
library(readxl)
library(ggplot2)
library(reshape2)

# Répertoire de travail
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Importation des données
var_endo <- read_excel("data.xlsx", sheet = "Variables Expliquées")
var_exo <- read_excel("data.xlsx", sheet = "Variables Explicatives")

# Visualisation - variables exogènes
print(head(var_exo))
print(summary(var_exo))
plot_variables("Output/Variables/Exogènes", var_exo)

# Visualisation - variables endogènes
print(head(var_endo))
print(summary(var_endo))
plot_variables("Output/Variables/Endogènes", var_endo)

# Etude des corrélations - variables endogènes et exogènes
cor_matrix <- cor(var_exo[, -1], var_endo[, -1])
cor_data <- melt(cor_matrix)

cor_data$Var1 <- rownames(cor_matrix)[cor_data$Var1]
cor_data$Var2 <- colnames(cor_matrix)[cor_data$Var2]

p <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       mid = "white", midpoint = 0) +
  theme_bw() +
  labs(title = "Corrélation entre les variables", x = "Variables exogènes",
       y = "Variables endogènes", fill = "Correlation")

chemin_fichier <- file.path("Output/Corrélations",
                            "Corrélation_variables_exo_endo.png")

ggsave(filename = chemin_fichier, plot = p, width = 10, height = 6)

# Etude des corrélations - variables exogènes
cor_matrix_exo <- cor(var_exo[, -1])
cor_data_exo <- melt(cor_matrix_exo)

cor_data_exo$Var1 <- rownames(cor_matrix_exo)[cor_data_exo$Var1]
cor_data_exo$Var2 <- colnames(cor_matrix_exo)[cor_data_exo$Var2]

p_exo <- ggplot(cor_data_exo, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       mid = "white", midpoint = 0) +
  theme_bw() +
  labs(title = "Corrélation entre les variables", x = "Variables exogènes",
       y = "Variables exogènes", fill = "Correlation")

chemin_fichier2 <- file.path("Output/Corrélations",
                             "Corrélation_variables_exo.png")

ggsave(filename = chemin_fichier2, plot = p_exo, width = 10, height = 6)
