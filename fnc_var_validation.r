library(vars)
library(LSTS)

model_var_validation <- function(residuals, col_name, chemin_fichier) {
  # Fonction qui qui permet de valider un modèle VAR en graphant : les résidus, l'ACF, le PCF, et les stats / p-values de Ljung Box sur 12 retards

  # Graphique des résidus
  chemin <- paste0(chemin_fichier, col_name, "/graph_residus.png")
  png(chemin)
  plot(residuals, type = "l", main = paste("Graphique des résidus -", col_name))
  dev.off()

  # Graphique ACF
  chemin <- paste0(chemin_fichier, col_name, "/graph_acf.png")
  png(chemin)
  acf(residuals, main = paste("ACF -", col_name))
  dev.off()

  # Graphique PACF
  chemin <- paste0(chemin_fichier, col_name, "/graph_pacf.png")
  png(chemin)
  pacf(residuals, main = paste("PACF -", col_name))
  dev.off()

  # Graphique des statistiques de test et des p-values du test de Ljung Box
  png(file = paste0(chemin_fichier, col_name, "/ljung_box_plot.png"))
  test_stats <- sapply(1:12, function(l) {
    Box.test(residuals, type = "Ljung-Box", lag = l)$statistic
  })
  p_values <- sapply(1:12, function(l) {
    Box.test(residuals, type = "Ljung-Box", lag = l)$p.value
  })

  plot(1:12, test_stats, type = "o", pch = 20,
       main = paste("Ljung-Box Test Statistic -", col_name),
       xlab = "Lag", ylab = "Test Statistic", col = "blue")
  dev.off()

  png(file = paste0(chemin_fichier, col_name, "/ljung_box_pvalues.png"))
  plot(1:12, p_values, type = "o", pch = 20,
       main = paste("Ljung-Box Test P-values -", col_name),
       xlab = "Lag", ylab = "P-Value", col = "red")
  dev.off()

}
