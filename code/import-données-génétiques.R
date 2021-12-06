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
  com <- str_replace(com, "^ALLEMONT$", "ALLEMOND")
  com <- str_replace(com, "^ANGOUSTRINE-VILLENEUVE-DE$", "ANGOUSTRINE-VILLENEUVE-DES-ESCALDES")
  com <- str_replace(com, "^AUMONT-AUBRAC$", "PEYRE EN AUBRAC")
  com <- str_replace(com, "^AUTRANS$", "AUTRANS-MEAUDRE EN VERCORS")
  com <- str_replace(com, "^BARRET-LE-BAS$", "BARRET-SUR-MEOUGE")
  com <- str_replace(com, "^BELLENTRE$", "LA PLAGNE TARENTAISE")
  com <- str_replace(com, "^BONNEVAL$", "BONNEVAL-SUR-ARC")
  com <- str_replace(com, "^BRAMANS$", "VAL-CENIS")
  com <- str_replace(com, "^BUISSON$", "LE BUISSON")
  com <- str_replace(com, "^HAUTES-DUVES$", "HAUTES-DUYES")
  com <- str_replace(com, "^CAILAR$", "LE CAILAR")
  com <- str_replace(com, "^CHAUVAC$", "CHAUVAC-LAUX-MONTAUX")
  com <- str_replace(com, "^COLOMBEY-LES-DEUX-EGLISES$", "COLOMBEY LES DEUX EGLISES")
  com <- str_replace(com, "^CORDEAC$", "CHATEL-EN-TRIEVES")
  com <- str_replace(com, "^COUVERTOIRADE$", "LA COUVERTOIRADE")
  com <- str_replace(com, "^CHAMPLAURENT$", "CHAMP-LAURENT")
  com <- str_replace(com, "^CHANTELOUVE$", "CHANTEPERIER")
  com <- str_replace(com, "^CHAPELLE-EN-VALGAUDEMAR$", "LA CHAPELLE-EN-VALGAUDEMAR")
  com <- str_replace(com, "^CHATEAUROUX$", "CHATEAUROUX-LES-ALPES")
  com <- str_replace(com, "^CHAVAGNAC$", "NEUSSARGUES EN PINATELLE")
  com <- str_replace(com, "^CHEMERY-SUR-BAR$", "CHEMERY-CHEHERY")
  com <- str_replace(com, "^ESTABLES$", "MONTS-DE-RANDON")
  com <- str_replace(com, "^FAGE-SAINT-JULIEN$", "LA FAGE-SAINT-JULIEN")
  com <- str_replace(com, "^FALGOUX$", "LE FALGOUX")
  com <- str_replace(com, "^FAU$", "LE FAU")
  com <- str_replace(com, "^FAVERGES$", "FAVERGES-SEYTHENEX")
  com <- str_replace(com, "^FERTE-SAINT-CYR$", "LA FERTE-SAINT-CYR")
  com <- str_replace(com, "^FEISSONS-SUR-ISERE$", "LA LECHERE")
  com <- str_replace(com, "^GRANDE-MOTTE$", "LA GRANDE-MOTTE")
  com <- str_replace(com, "^GRANIER$", "AIME-LA-PLAGNE")
  com <- str_replace(com, "^HERMILLON$", "LA TOUR-EN-MAURIENNE")
  com <- str_replace(com, "^HOSPITALET-DU-LARZAC$", "L'HOSPITALET-DU-LARZAC")
  com <- str_replace(com, "^HOSTIAS$", "PLATEAU D'HAUTEVILLE")
  com <- str_replace(com, "^L-EPINE$", "L'EPINE")
  com <- str_replace(com, "^L-HOSPITALET$", "L'HOSPITALET")
  com <- str_replace(com, "^LA BREOLE$", "UBAYE-SERRE-PONCON")
  com <- str_replace(com, "^LA CLUSE$", "DEVOLUY")
  com <- str_replace(com, "^LA CHAPELLE-D-ABONDANCE$", "LA CHAPELLE-D'ABONDANCE")
  com <- str_replace(com, "^LA FERRIERE$", "LE HAUT-BREDA")
  com <- str_replace(com, "^LA PLAGNE$", "LA PLAGNE TARENTAISE")
  com <- str_replace(com, "^LANSLEBOURG-MONT-CENIS$", "VAL-CENIS")
  com <- str_replace(com, "^LANSLEVILLARD$", "VAL-CENIS")
  com <- str_replace(com, "^LARCHE$", "VAL D'ORONAYE")
  com <- str_replace(com, "^LAVAL-D-AIX$", "LAVAL-D'AIX")
  com <- str_replace(com, "^LE BOURG-D-OISANS$", "LE BOURG-D'OISANS")
  com <- str_replace(com, "^LE FRENEY-D-OISANS$", "LE FRENEY-D'OISANS")
  com <- str_replace(com, "^LE PERIER$", "CHANTEPERIER")
  com <- str_replace(com, "^LE PETIT-BORNAND-LES-GLIE$", "GLIERES-VAL-DE-BORNE")
  com <- str_replace(com, "^MALUAI$", "MALIJAI")
  com <- str_replace(com, "^MAS-D'ORCIERES$", "MONT LOZERE ET GOULET")
  com <- str_replace(com, "^MEAUDRE$", "AUTRANS-MEAUDRE EN VERCORS")
  com <- str_replace(com, "^MEOLANS$", "MEOLANS-REVEL")
  com <- str_replace(com, "^MEYRONNES$", "VAL D'ORONAYE")
  com <- str_replace(com, "^MONTAUBAN-SUR-L-OUVEZE$", "MONTAUBAN-SUR-L'OUVEZE")
  com <- str_replace(com, "^MONASTIER-SUR-GAZEILLE$", "LE MONASTIER-SUR-GAZEILLE")
  com <- str_replace(com, "^MONESTIER-D-AMBEL$", "MONESTIER-D'AMBEL")
  com <- str_replace(com, "^MONTAIMONT$", "SAINT FRANCOIS LONGCHAMP")
  com <- str_replace(com, "^MONTMORIN$", "VALDOULE")
  com <- str_replace(com, "^MONT-DE-LANS$", "LES DEUX ALPES")
  com <- str_replace(com, "^MORETEL-DE-MAILLES$", "CRETS EN BELLEDONNE")
  com <- str_replace(com, "^PANOUSE$", "LA PANOUSE")
  com <- str_replace(com, "^PINSOT$", "LE HAUT-BREDA")
  com <- str_replace(com, "^PLAN-D-AUPS-SAINTE-BAUME$", "PLAN-D'AUPS-SAINTE-BAUME")
  com <- str_replace(com, "^PONT-DE-MONTVERT$", "PONT DE MONTVERT - SUD MONT LOZERE")
  com <- str_replace(com, "^PONTAMAFREY-MONTPASCAL$", "LA TOUR-EN-MAURIENNE")
  com <- str_replace(com, "^RANDENS$", "VAL-D'ARC")
  com <- str_replace(com, "^RIEUTORT-DE-RANDON$", "MONTS-DE-RANDON")
  com <- str_replace(com, "^RISTOLAS$", "ABRIES-RISTOLAS")
  com <- str_replace(com, "^RIVES$", "LES RIVES")
  com <- str_replace(com, "^SAINTE-ENIMIE$", "GORGES DU TARN CAUSSES")
  com <- str_replace(com, "^SAINTE-GENEVIEVE-SUR-ARGENCE$", "ARGENCES EN AUBRAC")
  com <- str_replace(com, "^SAINT-ALBAN-DES-HURTIERES$", "SAINT-ALBAN-D'HURTIERES")
  com <- str_replace(com, "^SAINT-CHRISTOPHE-LE-JAJOLET$", "BOISCHAMPRE")
  com <- str_replace(com, "^SAINT-DISDIER$", "DEVOLUY")
  com <- str_replace(com, "^SAINT-ETIENNE-EN-DEVOLUY$", "DEVOLUY")
  com <- str_replace(com, "^SAINT-JEAN-D-AULPS$", "SAINT-JEAN-D'AULPS")
  com <- str_replace(com, "^SAINT-GENIEZ-D'OLT$", "SAINT GENIEZ D'OLT ET D'AUBRAC")
  com <- str_replace(com, "^SAINT-MARTIN$", "SAINT-MARTIN-DE-PALLIERES")
  com <- str_replace(com, "^SAINT-MARTIN-D-ARC$", "SAINT-MARTIN-D'ARC")
  com <- str_replace(com, "^SAINT-MARTIN-D-ENTRAUNES$", "SAINT-MARTIN-D'ENTRAUNES")
  com <- str_replace(com, "^SAINT-MARTIN-D-URIAGE$", "SAINT-MARTIN-D'URIAGE")
  com <- str_replace(com, "^SAINT-MARTIN-DE-BELLEVILLE$", "LES BELLEVILLE")
  com <- str_replace(com, "^SAINT-MARTIN-DE-QUEYRIERE$", "SAINT-MARTIN-DE-QUEYRIERES")
  com <- str_replace(com, "^SAINT-PAUL$", "SAINT-PAUL-SUR-UBAYE")
  com <- str_replace(com, "^SAINT-PIERRE-D-ALBIGNY$", "SAINT-PIERRE-D'ALBIGNY")
  com <- str_replace(com, "^SAINT-PIERRE-DE-BOEUF$", "SAINT-PIERRE-DE-BŒUF")
  com <- str_replace(com, "^SALAGRIFFON$", "SALLAGRIFFON")
  com <- str_replace(com, "^SEGALASSIERE$", "LA SEGALASSIERE")
  com <- str_replace(com, "^SEILLONS-SOURCE-DARGENS$", "SEILLONS-SOURCE-D'ARGENS")
  com <- str_replace(com, "^SERVANCE$", "SERVANCE-MIELLIN")
  com <- str_replace(com, "^SOLLIERES-SARDIERES$", "VAL-CENIS")
  com <- str_replace(com, "^TERMIGNON$", "VAL-CENIS")
  com <- str_replace(com, "^THORENS-GLIERES$", "FILLIERE")
  com <- str_replace(com, "^TIEULE$", "LA TIEULE")
  com <- str_replace(com, "^TOURETTE-SUR-LOUP$", "TOURRETTES-SUR-LOUP")
  com <- str_replace(com, "^TRESCHENU-CREYERS$", "CHATILLON-EN-DIOIS")
  com <- str_replace(com, "^VALLOUISE$", "VALLOUISE-PELVOUX")
  com <- str_replace(com, "^VENOSC$", "LES DEUX ALPES")
  com <- str_replace(com, "^VILLEDIEU$", "MONTS-DE-RANDON")
  com <- str_replace(com, "^VILLENEUVE-D-ENTRAUNE$", "VILLENEUVE-D'ENTRAUNES")
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

# cherche_commune("VAL D'ORONAYE", "04")

indices_geo <- tibble()
for(i in 1:nrow(indices)) {
#for (i in 2500:3000) {
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
