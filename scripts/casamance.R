#
# quelques fonction pour la casamance
# auteur: Marc Gauthier
#
mga  <- function() {
  source("geo/scripts/casamance.R");
}
#
# fonctions packages
# ==================
#
install <- function() {
  require(devtools)
  install.packages("rgrass7", repos = "http://R-Forge.R-project.org")
}

#
# lecture des especes
# source("geo/scripts/casamance.R");especes_lire_xls()
especes_lire_xls <- function() {
  library(readxl)
  dsn <- sprintf("%s/CONF/especes.xls", varDir)
  df <- read_excel(dsn) %>%
    glimpse()
  return(invisible(df))
}
nom_doubles <- function() {
  library(dplyr)
  df <- donnees_lire()
  df$Nom <- df$NomScientifique
  nom_double(df)
  df$Nom <- tolower(iconv(df$NomFrancais, from = "UTF-8", to = "ASCII//TRANSLIT//IGNORE"))
  nom_double(df)
}
nom_double <- function(df) {
  library(knitr)
  df1 <- df %>%
    group_by(Nom, NomFrancais) %>%
    summarize(n = n())
  df2 <- df1 %>%
    group_by(Nom) %>%
    summarize(n = n())
  df3 <- df2[df2$n > 1, ]
  df4 <- df3 %>% left_join(df1, by = c("Nom"))
  print(kable(df4))
}

#
# les commandes permettant le lancement
Drive <- substr(getwd(), 1, 2)
baseDir <- sprintf("%s/web", Drive)
cfgDir <- sprintf("%s/web/geo/CASAMANCE", Drive);
varDir <- sprintf("%s/web.heb/casamance/casamance", Drive);
texDir <- cfgDir
webDir <- sprintf("%s/web.heb/casamance", Drive);
setwd(baseDir)
source("geo/scripts/mga.R");
source("geo/scripts/misc.R");
source("geo/scripts/misc_atlas.R");
source("geo/scripts/misc_db.R");
source("geo/scripts/misc_ggplot.R");
source("geo/scripts/misc_mysql.R");
source("geo/scripts/misc_tex.R");
source("geo/scripts/casamance_atlas.R");
source("geo/scripts/casamance_alain.R");
source("geo/scripts/casamance_donnees.R");
source("geo/scripts/casamance_grille.R");
source("geo/scripts/casamance_sql.R");
if (interactive()) {
  print(sprintf("casamance.R interactif"))
# un peu de nettoyage
  graphics.off()
} else {
  print(sprintf("casamance.R console"))
}
