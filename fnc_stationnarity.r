library(tseries)

adf_test_results <- function(column) {
  # Fonction qui renvoie la t-stat et la p-value d'un test ADF pouir une série de données
  column <- na.omit(column)
  adf_test <- adf.test(column, alternative = "stationary")
  return(c(Statistic = adf_test$statistic, p.Value = adf_test$p.value))
}