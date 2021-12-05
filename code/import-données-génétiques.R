#
# Import du fichier des données génétiques
#
# Fichier obtenu par OCR à partir du fichier :
# https://www.loupfrance.fr/wp-content/uploads/FLASH_DONNEES_GENETIQUES_2021.pdf
#
library(tidyverse)

raw_input <- read_lines("dat/FLASH_DONNEES_GENETIQUES_2021.txt",
                        n_max = -1)
# Analyse syntaxique simplifiée pour retrouver les colonnes
pattern_indice = "(.{2,4}) (../../....) (F|P|D|U|S) (\\w{1,3}\\d{7}) ([\\w|É|-]+) ([\\w|é|_|\\.]+) (.+) (OUI|NON) ?(../../....)? ?(\\w* ?\\w*)? ?([\\w|-]*)? ?(\\w{0,2})? ?([\\w|-]*)? ?([\\d|,]*)?"
indices <- tibble(ligne = raw_input) %>% 
            tidyr::extract(
              ligne, 
              c("N_dpt", "Date", "Type", "N_ref", "Nom", "Organisme", "Commune",
                "Analysé", "G_session", "Espèce", "Lignée", "Sexe", "Génotype", "IQ"), 
              pattern_indice, 
              remove = FALSE
            )

# Vérification de complétude de l'analyse
indices[is.na(indices["N_dpt"]),]

# Save to CSV file
write.csv(indices, "dat/données_genetiques_2021.csv")

#
# Géocodage des communes
#
# Lecture de la liste des centroïdes des communes (issu de IGN ADMIN EXPRESS)
communes <- read.csv("/tmp/communes.csv")

# Nettoyage des départements mal reconnus
indices["N_dpt"][indices["N_dpt"] == "O1"] <- "01"
head(indices)

# Liste des départements
dept <- unique(indices[c("N_dpt")])

# Mise au format proche de l'INSEE de la commune indice
modif_commune <- function(nom) {
  com <- str_replace(nom, "^PNR Luberon ", "")
  com <- str_replace(nom, "^ST ", "SAINT-")
  com <- str_replace_all(com, " ", "-")
  com <- str_replace(com, "^LE-", "LE ")
  com <- str_replace(com, "^LES-", "LES ")
  com <- str_replace(com, "^LA-", "LA ")
  return(com)
}

indices <- indices %>% 
  mutate(nom_insee = modif_commune(Commune))

# Recherche dans la liste INSEE
cherche_commune <- function(nom, dept) {
  xy <- select(filter(communes, NOM_M == nom, INSEE_DEP == dept), X, Y)
  if (nrow(xy) < 1) {
    xy = tibble(X=NA, Y=NA)
  }
  return(xy)
}

cherche_commune("HOSTIAS", "01")

indices_geo <- tibble()
# for(i in 1:nrow(indices)) {
for(i in 1:50) {
  row <- indices[i,]
  print(paste(i, row["nom_insee"]))
  xy <- cherche_commune(as.character(row["nom_insee"]), as.character(row["N_dpt"]))
  indices_geo <- bind_rows(indices_geo, bind_cols(data_frame(row), xy))
}

indices_geo
