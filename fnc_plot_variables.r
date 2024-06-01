library(ggplot2)

plot_variables <- function(chemin_dossier, df) {

  for (i in 2:ncol(df)) {
    p <- ggplot(data = df,
                aes_string(x = names(df)[1], y = names(df)[i])) +
      geom_line(color = "steelblue") +
      labs(x = "Date", y = names(df)[i],
           title = paste("Représentation de la variable : ", names(df)[i])) +
      theme_bw()

    chemin_fichier <- file.path(chemin_dossier, paste0("Graphique_",
                                                       names(df)[i], ".png"))

    ggsave(filename = chemin_fichier, plot = p, width = 10, height = 6)
  }
}
