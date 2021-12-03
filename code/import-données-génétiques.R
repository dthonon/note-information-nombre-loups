#
# Import du fichier des données génétiques
#
# Fichier obtenu par OCR à partir du fichier :
# https://www.loupfrance.fr/wp-content/uploads/FLASH_DONNEES_GENETIQUES_2021.pdf
#
library(tidyverse)

raw_input <- read_lines("dat/FLASH_DONNEES_GENETIQUES_2021.txt",
                        n_max = -1)
# raw_input

pattern_indice = "(.{2,4}) (../../....) (F|P|D|U|S) (\\w{1,3}\\d{7}) ([\\w|É|-]+) ([\\w|é|_|\\.]+) (.+) (OUI|NON) ?(../../....)? ?(\\w* ?\\w*)? ?([\\w|-]*)? ?(\\w{0,2})? ?([\\w|-]*)? ?([\\d|,]*)?"
indices <- tibble(ligne = raw_input) %>% 
            tidyr::extract(
              ligne, 
              c("N_dpt", "Date", "Type", "N_ref", "Nom", "Organisme", "Commune",
                "Analysé", "G_session", "Espèce", "Lignée", "Sexe", "Génotype", "IQ"), 
              pattern_indice, 
              remove = FALSE
            )

indices[is.na(indices["N_dpt"]),]

write.csv(indices, "dat/données_genetiques_2021.csv")
