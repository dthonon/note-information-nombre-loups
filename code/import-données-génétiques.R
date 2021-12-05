#
# Import du fichier des données génétiques
#
# Fichier obtenu par OCR à partir du fichier :
# https://www.loupfrance.fr/wp-content/uploads/FLASH_DONNEES_GENETIQUES_2021.pdf
#
library(tidyverse)
library(readxl)

raw_input <- read_lines("dat/FLASH_DONNEES_GENETIQUES_2021.txt",
                        n_max = -1)
# Analyse syntaxique simplifiée pour retrouver les colonnes
pattern_indice <-
  "(.{2,4}) (../../....) (F|P|D|U|S) (\\w{1,3}\\d{7}) ([\\w|É|-]+) ([\\w|é|_|\\.]+) (.+) (OUI|NON) ?(../../....)? ?(\\w* ?\\w*)? ?([\\w|-]*)? ?(\\w{0,2})? ?([\\w|-]*)? ?([\\d|,]*)?"
indices <- tibble(ligne = raw_input) %>%
  tidyr::extract(
    ligne,
    c(
      "N_dpt",
      "Date",
      "Type",
      "N_ref",
      "Nom",
      "Organisme",
      "Commune",
      "Analysé",
      "G_session",
      "Espèce",
      "Lignée",
      "Sexe",
      "Génotype",
      "IQ"
    ),
    pattern_indice,
    remove = FALSE
  )

# Vérification de complétude de l'analyse
indices[is.na(indices["N_dpt"]),]


# Save to CSV file
write.csv(indices, "dat/données_génétiques_2021.csv")

#
# Géocodage des communes
#
# Lecture des indices corrigés dans Excel
indices2 <-
  (read_xlsx(
    "dat/Données_génétiques_2021.xlsx",
    "données_génétiques_2021"
  ))

# Lecture de la liste des centroïdes des communes (issu de IGN ADMIN EXPRESS)
# communes <- read.csv("/tmp/communes.csv")
# saveRDS(communes, file = "dat/communes.rds")
communes <- readRDS(file = "dat/communes.rds")

# Nettoyage des départements mal reconnus
indices2["N_dpt"][indices2["N_dpt"] == "O1"] <- "01"
head(indices2)

# Liste des départements
dept <- unique(indices2[c("N_dpt")])

# Mise au format proche de l'INSEE de la commune indice
modif_commune <- function(nom) {
  com <- nom
  com <- str_replace(com, "^ST ", "SAINT-")
  com <- str_replace(com, "^STE ", "SAINTE-")
  com <- str_replace(com, " ST ", "-SAINT-")
  com <- str_replace(com, " STE ", "-SAINTE-")
  com <- str_replace_all(com, " ", "-")
  com <- str_replace(com, "^LE-", "LE ")
  com <- str_replace(com, "^LES-", "LES ")
  com <- str_replace(com, "^LA-", "LA ")
  com <- str_replace(com, "^ABRIES$", "ABRIES-RISTOLAS")
  com <- str_replace(com, "^AGNIERES-EN-DEVOLUY$", "DEVOLUY")
  com <- str_replace(com, "^HAUTES-DUVES$", "HAUTES-DUYES")
  com <- str_replace(com, "^HOSTIAS$", "PLATEAU D'HAUTEVILLE")
  com <- str_replace(com, "^CHAPELLE-EN-VALGAUDEMAR$", "LA CHAPELLE-EN-VALGAUDEMAR")
  com <- str_replace(com, "^CHATEAUROUX$", "CHATEAUROUX-LES-ALPES")
  com <- str_replace(com, "^L-EPINE$", "L'EPINE")
  com <- str_replace(com, "^L-HOSPITALET$", "L'HOSPITALET")
  com <- str_replace(com, "^LA CLUSE$", "DEVOLUY")
  com <- str_replace(com, "^LARCHE$", "VAL D'ORONAYE")
  com <- str_replace(com, "^MEOLANS$", "MEOLANS-REVEL")
  com <- str_replace(com, "^MEYRONNES$", "VAL D'ORONAYE")
  com <- str_replace(com, "^MONTMORIN$", "VALDOULE")
  com <- str_replace(com, "^RISTOLAS$", "ABRIES-RISTOLAS")
  com <- str_replace(com, "^SAINT-DISDIER$", "DEVOLUY")
  com <- str_replace(com, "^SAINT-ETIENNE-EN-DEVOLUY$", "DEVOLUY")
  com <- str_replace(com, "^VALLOUISE$", "VALLOUISE-PELVOUX")
  com <- str_replace(com, "^VILLENEUVE-D-ENTRAUNE$", "VILLENEUVE-D'ENTRAUNE")
  return(com)
}

indices2 <- indices2 %>%
  mutate(nom_insee = modif_commune(Commune))

# Recherche dans la liste INSEE
cherche_commune <- function(nom, dept) {
  # print(paste("Cherche", nom, "dans", dept))
  xy <-
    select(filter(communes, NOM_M == nom, INSEE_DEP == dept), X, Y)
  if (nrow(xy) < 1) {
    xy <- tibble(X = NA, Y = NA)
  }
  # print(xy)
  return(xy)
}

cherche_commune("VAL D'ORONAYE", "04")

indices_geo <- tibble()
# for(i in 1:nrow(indices)) {
for (i in 1:1000) {
  row <- indices2[i, ]
  xy <-
    cherche_commune(as.character(row["nom_insee"]), sprintf("%02d", as.integer(row["N_dpt"])))
  # print(paste(i, row["nom_insee"], xy))
  indices_geo <-
    bind_rows(indices_geo, bind_cols(tibble(row), xy))
}

indices_geo %>%
  filter(is.na(X)) %>%
  select(N_dpt, Commune, nom_insee)
