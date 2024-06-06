# Importation des librairies
library(readxl)
library(urca)
library(vars)

# Répertoire de travail
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Importation des données
var_endo <- read_excel("data.xlsx", sheet = "Variables Expliquées")
var_exo <- read_excel("data.xlsx", sheet = "Variables Explicatives")

# Stationnarisation - variables endogènes
statio_var_endo <- sapply(var_endo[-1], adf_test_results, simplify = "var_endo")
cat("\nRésultats - Stationnarité des variables endogènes\n\n")
print(statio_var_endo)

" Résultats - Variables endogènes :
    Or -> 0.62
    Argent -> 0.27
    Cuivre -> 0.31
    Caco -> 0.01 : stationnaire au seuil de 5%
    Blé -> 0.03 : stationnaire au seuil de 5%
    Soja -> 0.29
    Maïs -> 0.60
    Café -> 0.07
    Brent -> 0.36
    WTI -> 0.34
    Gas -> 0.17
    Indice -> 0.34
"

# Stationnarisation par différence première
for (col_name in names(var_endo)) {
  if (col_name != "Cacao" && col_name != "Blé") {
    var_endo[[col_name]] <- c(NA, diff(var_endo[[col_name]], differences = 1))
  }
}

# Nouveau test
statio_var_endo_2 <- sapply(var_endo[-1], adf_test_results,
                            simplify = "var_endo")
cat("\nRésultats - Stationnarité des variables endogènes 
         après différence première\n\n")
print(statio_var_endo_2)

" Résultats - Variables endogènes :
    Or -> 0.01
    Argent -> 0.01
    Cuivre -> 0.01
    Cacao -> série originelle stationnaire au seuil de 5%
    Blé -> série originelle stationnaire au seuil de 5%
    Soja -> 0.01
    Maïs -> 0.0101
    Café -> 0.01
    Brent -> 0.01
    WTI -> 0.01
    Gas -> 0.01
    Indice -> 0.01
"

# Stationnarisation - variables exogènes
statio_var_exo <- sapply(var_exo[-1], adf_test_results, simplify = "var_exo")
cat("\nRésultats - Stationnarité des variables exogènes\n\n")
print(statio_var_exo)

" Résultats - Variables exogènes :
    US10Y -> 0.40
    FED_LB -> 0.05 : stationnaire au seuil de 5%
    FED_UB -> 0.04 : stationnaire au seuil de 5%
    FED_ER -> 0.04 : stationnaire au seuil de 5%
    FED_BS -> 0.34
    CPI_CORE -> 0.04 : stationnaire au seuil de 5%
    M2 -> 0.93
    M3 -> 0.99
    USDEUR -> 0.48
    NFP -> 0.01 : stationnaire au seuil de 5%
    VIX -> 0.03 : stationnaire au seuil de 5%
    Indice -> 0.34
"

# Stationnarisation par différence première
for (col_name in names(var_exo)) {
  if (col_name != "FED_LB" && col_name != "FED_UB" &&
        col_name != "FED_ER" && col_name != "CPI_CORE" &&
        col_name != "NFP" && col_name != "VIX") {
    var_exo[[col_name]] <- c(NA, diff(var_exo[[col_name]], differences = 1))
  }
}

# Nouveau test
statio_var_exo_2 <- sapply(var_exo[-1], adf_test_results, simplify = "var_exo")
cat("\nRésultats - Stationnarité des variables exogènes
         après différence première\n\n")
print(statio_var_exo_2)

" Résultats - Variables exogènes :
    US10Y -> 0.01 : stationnaire au seuil de 5%
    FED_LB -> série originelle stationnaire au seuil de 5%
    FED_UB -> série originelle stationnaire au seuil de 5%
    FED_ER -> série originelle stationnaire au seuil de 5%
    FED_BS -> 0.01 : stationnaire au seuil de 5% 
    CPI_CORE -> série originelle stationnaire au seuil de 5%
    M2 -> 0.08
    M3 -> 0.08
    USDEUR -> 0.01 : stationnaire au seuil de 5%
    NFP -> série originelle stationnaire au seuil de 5%
    VIX -> série originelle stationnaire au seuil de 5%
    Indice -> 0.01 : stationnaire au seuil de 5%
"

# On retire les valeurs nulles
var_endo <- var_endo[-1, ]
var_exo <- var_exo[-1, ]

# Premier modèle VAR - Prends toutes les variables explicatives en compte
optimal_lags <- numeric(ncol(var_endo) - 1)

for (i in 2:ncol(var_endo)) {
  # Information
  cat("\n\nEtude de la variable :", names(var_endo)[i], "\n\n")

  # Sélection du Nombre de Retards Optimaux
  lag_selection <- VARselect(y = var_endo[i], lag.max = 10,
                             type = "none", exogen = var_exo)
  # pour afficher l'ensemble des résultats: print(lag_selection$criteria)

  aic_valeurs <- lag_selection$criteria["AIC(n)", ]
  optimal_lags[i - 1] <- which.min(aic_valeurs)
  cat("Nombre de retards optimal :", optimal_lags[i - 1])

  # Modèle VAR
  data <- cbind(var_endo[i], var_exo[-1])
  model_var <- VAR(y = data, p = optimal_lags[i - 1], type = "none")

  # Résultats
  specific_eqn_result <- coef(model_var)[[names(var_endo)[i]]]

  cat("\nCoefficients de la variable :\n")
  print(specific_eqn_result)

  # Validation du modèle
  #residuals <- residuals(model_var)
  #model_var_validation(residuals[, names(var_endo)[i]],
                       #names(var_endo)[i], "Output/VAR1/")
}
