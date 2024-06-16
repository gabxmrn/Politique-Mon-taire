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
nom_variable <- "WTI"
var_exo_noms  <- list("US10Y", "NFP")

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
    - FED_ER
    - NFP
    - VIX
    - Blé
    - Cacao
"

data_statio_coint <- data[, c(nom_variable, "US10Y")]

jotest <- ca.jo(data_statio_coint, type = "eigen")
print(summary(jotest))

# Test de causalité de Granger
cat("\nTest de causalité de Granger sur le Cacao :\n\n")
granger_vix <- grangertest(data[, "WTI"] ~ data[, "US10Y"])
print(summary(granger_vix))

granger_nfp <- grangertest(data[, "WTI"] ~ data[, "NFP"])
print(summary(granger_nfp))

cat("\nTest de causalité de Granger sur le US10Y :\n\n")
granger_vix <- grangertest(data[, "US10Y"] ~ data[, "WTI"])
print(summary(granger_vix))

granger_nfp <- grangertest(data[, "US10Y"] ~ data[, "NFP"])
print(summary(granger_nfp))

cat("\nTest de causalité de Granger sur NFP :\n\n")
granger_vix <- grangertest(data[, "NFP"] ~ data[, "WTI"])
print(summary(granger_vix))

granger_nfp <- grangertest(data[, "NFP"] ~ data[, "NFP"])
print(summary(granger_nfp))
