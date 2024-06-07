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
nom_variable <- "Café"
var_exo_noms  <- list("FED_ER", "FED_BS", "M2", "NFP", "VIX")

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

"On garde uniquement les séries I(1) donc on retire systématiquement
    - CPI_CORE
    - FED_LB / UB / ER
    - NFP
    - VIX
    - Blé
    - Cacao
"

data_statio_coint <- data[, c(nom_variable, "M2", "FED_BS")]

jotest <- ca.jo(data_statio_coint, type = "eigen")
print(summary(jotest))

# Test de causalité de Granger
cat("\nTest de causalité de Granger sur le Café :\n\n")
granger_vix <- grangertest(data[, "Café"] ~ data[, "FED_ER"])
print(summary(granger_vix))

granger_ic <- grangertest(data[, "Café"] ~ data[, "FED_BS"])
print(summary(granger_ic))

granger_nfp <- grangertest(data[, "Café"] ~ data[, "M2"])
print(summary(granger_nfp))

granger_vix <- grangertest(data[, "Café"] ~ data[, "NFP"])
print(summary(granger_vix))

granger_nfp <- grangertest(data[, "Café"] ~ data[, "VIX"])
print(summary(granger_nfp))

cat("\nTest de causalité de Granger du Café sur les variables :\n\n")
granger_vix <- grangertest(data[, "FED_ER"] ~ data[, "Café"])
print(summary(granger_vix))

granger_ic <- grangertest(data[, "FED_BS"] ~ data[, "Café"])
print(summary(granger_ic))

granger_nfp <- grangertest(data[, "M2"] ~ data[, "Café"])
print(summary(granger_nfp))

granger_vix <- grangertest(data[, "NFP"] ~ data[, "Café"])
print(summary(granger_vix))

granger_nfp <- grangertest(data[, "VIX"] ~ data[, "Café"])
print(summary(granger_nfp))