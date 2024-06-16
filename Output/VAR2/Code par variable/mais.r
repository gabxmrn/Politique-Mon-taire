# Importation des librairies
library(readxl)
library(urca)
library(vars)
library(lmtest)

# Répertoire de travail
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Importation des données
var_endo <- read_excel("data.xlsx", sheet = "Variables Expliquées")
var_exo <- read_excel("data.xlsx", sheet = "Variables Explicatives")
nom_variable <- "Mais"
var_exo_noms  <- list("FED_ER", "USDEUR")

# Stationnarisation - variables endogènes
for (col_name in names(var_endo)) {
  if (col_name != "Cacao" && col_name != "Blé") {
    var_endo[[col_name]] <- c(NA, diff(var_endo[[col_name]], differences = 1))
  }
}

# Stationnarisation - variables exogènes
for (col_name in names(var_exo)) {
  if (col_name != "FED_LB" && col_name != "FED_UB" &&
        col_name != "FED_ER" && col_name != "CPI_CORE" &&
        col_name != "NFP" && col_name != "VIX") {
    var_exo[[col_name]] <- c(NA, diff(var_exo[[col_name]], differences = 1))
  }
}

# On retire les valeurs nulles et sélectionne la variable endogène
var_endo <- var_endo[-1, nom_variable]
var_exo <- var_exo[-1, unlist(var_exo_noms)]

cat("\n\nEtude de la variable endogène :", nom_variable, "\n\n")

# Sélection du Nombre de Retards Optimaux
lag_selection <- VARselect(y = var_endo, lag.max = 10,
                           type = "none", exogen = var_exo)
# pour afficher l'ensemble des résultats: print(lag_selection$criteria)

aic_valeurs <- lag_selection$criteria["AIC(n)", ]
optimal_lags <- which.min(aic_valeurs)
cat("Nombre de retards optimal :", optimal_lags)

# Modèle VAR
data <- cbind(var_endo, var_exo)
model_var <- VAR(y = data, p = optimal_lags, type = "none")

# Résultats
cat("\nCoefficients de la variable :\n")
print(summary(model_var))

# Validation du modèle
residuals <- residuals(model_var)
model_var_validation(residuals[, nom_variable],
                     nom_variable, "Output/VAR2/")
