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
nom_variable <- "Cuivre"
var_exo_noms  <- list("FED_ER", "CPI_CORE", "M2", "NFP", "VIX")

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

data_statio_coint <- data[, c(nom_variable, "M2")]

jotest <- ca.jo(data_statio_coint, type = "eigen")
print(summary(jotest))


# Test de causalité de Granger
cat("\nTest de causalité de Granger sur le Cuivre :\n\n")
granger_er <- grangertest(data[, "Cuivre"] ~ data[, "FED_ER"])
print(summary(granger_er))

granger_cpi <- grangertest(data[, "Cuivre"] ~ data[, "CPI_CORE"])
print(summary(granger_cpi))

granger_m2 <- grangertest(data[, "Cuivre"] ~ data[, "M2"])
print(summary(granger_m2))

granger_nfp <- grangertest(data[, "Cuivre"] ~ data[, "NFP"])
print(summary(granger_nfp))

granger_vix <- grangertest(data[, "Cuivre"] ~ data[, "VIX"])
print(summary(granger_vix))

cat("\nTest de causalité de Granger sur FED_ER :\n\n")
granger_er <- grangertest(data[, "FED_ER"] ~ data[, "Cuivre"])
print(summary(granger_er))

granger_cpi <- grangertest(data[, "FED_ER"] ~ data[, "CPI_CORE"])
print(summary(granger_cpi))

granger_m2 <- grangertest(data[, "FED_ER"] ~ data[, "M2"])
print(summary(granger_m2))

granger_nfp <- grangertest(data[, "FED_ER"] ~ data[, "NFP"])
print(summary(granger_nfp))

granger_vix <- grangertest(data[, "FED_ER"] ~ data[, "VIX"])
print(summary(granger_vix))

cat("\nTest de causalité de Granger CPI_CORE :\n\n")
granger_cpi <- grangertest(data[, "CPI_CORE"] ~ data[, "Cuivre"])
print(summary(granger_cpi))

granger_er <- grangertest(data[, "CPI_CORE"] ~ data[, "FED_ER"])
print(summary(granger_er))

granger_m2 <- grangertest(data[, "CPI_CORE"] ~ data[, "M2"])
print(summary(granger_m2))

granger_nfp <- grangertest(data[, "CPI_CORE"] ~ data[, "NFP"])
print(summary(granger_nfp))

granger_vix <- grangertest(data[, "CPI_CORE"] ~ data[, "VIX"])
print(summary(granger_vix))

cat("\nTest de causalité de Granger sur M2 :\n\n")
granger_cpi <- grangertest(data[, "M2"] ~ data[, "Cuivre"])
print(summary(granger_cpi))

granger_er <- grangertest(data[, "M2"] ~ data[, "FED_ER"])
print(summary(granger_er))

granger_m2 <- grangertest(data[, "M2"] ~ data[, "CPI_CORE"])
print(summary(granger_m2))

granger_nfp <- grangertest(data[, "M2"] ~ data[, "NFP"])
print(summary(granger_nfp))

granger_vix <- grangertest(data[, "M2"] ~ data[, "VIX"])
print(summary(granger_vix))

cat("\nTest de causalité de Granger sur le NFP :\n\n")

granger_nfp <- grangertest(data[, "NFP"] ~ data[, "Cuivre"])
print(summary(granger_nfp))

granger_er <- grangertest(data[, "NFP"] ~ data[, "FED_ER"])
print(summary(granger_er))

granger_cpi <- grangertest(data[, "NFP"] ~ data[, "CPI_CORE"])
print(summary(granger_cpi))

granger_m2 <- grangertest(data[, "NFP"] ~ data[, "M2"])
print(summary(granger_m2))

granger_vix <- grangertest(data[, "NFP"] ~ data[, "VIX"])
print(summary(granger_vix))

cat("\nTest de causalité de Granger sur le VIX :\n\n")

granger_vix <- grangertest(data[, "VIX"] ~ data[, "Cuivre"])
print(summary(granger_vix))

granger_er <- grangertest(data[, "VIX"] ~ data[, "FED_ER"])
print(summary(granger_er))

granger_cpi <- grangertest(data[, "VIX"] ~ data[, "CPI_CORE"])
print(summary(granger_cpi))

granger_m2 <- grangertest(data[, "VIX"] ~ data[, "M2"])
print(summary(granger_m2))

granger_nfp <- grangertest(data[, "VIX"] ~ data[, "NFP"])
print(summary(granger_nfp))
