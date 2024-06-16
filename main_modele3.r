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
var_exo_noms  <- list("FED_ER", "FED_BS", "M3")

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
                     nom_variable, "Output/VAR3/")

# Test de cointégration de Johansen
"On garde uniquement les séries I(1) donc ne peut pas être effectué sur :
    - FED_ER
    - Blé
    - Cacao
"

if (nom_variable != "Blé" && nom_variable != "Cacao") {
  data_statio_coint <- data[, c(nom_variable, "FED_BS", "M3")]
  jotest <- ca.jo(data_statio_coint, type = "eigen")
  print(summary(jotest))
} else {
  print(paste(nom_variable, "test de cointégration impossible."))
}

# Test de causalité de Granger
cat("\nTest de causalité de Granger sur la MP (FED_ER / FED_BS / M3) :\n")

granger1 <- grangertest(data[, nom_variable] ~ data[, "FED_ER"])
print(summary(granger1))

granger2 <- grangertest(data[, nom_variable] ~ data[, "FED_BS"])
print(summary(granger2))

granger3 <- grangertest(data[, nom_variable] ~ data[, "M3"])
print(summary(granger3))

cat("\nTest de causalité de Granger sur FED_ER (FED_BS / M3 / MP) :\n")

granger4 <- grangertest(data[, "FED_ER"] ~ data[, "FED_BS"])
print(summary(granger4))

granger5 <- grangertest(data[, "FED_ER"] ~ data[, "M3"])
print(summary(granger5))

granger6 <- grangertest(data[, "FED_ER"] ~ data[, nom_variable])
print(summary(granger6))

cat("\nTest de causalité de Granger sur FED_BS (FED_ER / M3 / MP) :\n")

granger7 <- grangertest(data[, "FED_BS"] ~ data[, "FED_ER"])
print(summary(granger7))

granger8 <- grangertest(data[, "FED_BS"] ~ data[, "M3"])
print(summary(granger8))

granger9 <- grangertest(data[, "FED_BS"] ~ data[, nom_variable])
print(summary(granger9))

cat("\nTest de causalité de Granger sur M3 (FED_ER / FED_BS / MP) :\n")

granger10 <- grangertest(data[, "M3"] ~ data[, "FED_ER"])
print(summary(granger10))

granger11 <- grangertest(data[, "M3"] ~ data[, "FED_BS"])
print(summary(granger11))

granger12 <- grangertest(data[, "M3"] ~ data[, nom_variable])
print(summary(granger12))