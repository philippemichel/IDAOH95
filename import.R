

#  ------------------------------------------------------------------------
#
# Title : Import IDAOH95
#    By : PhM
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------
library(tidyverse)
library(labelled)
library(readODS)
library(baseph)
#
#   Directeurs
#
dire <- read_ods("data/idaoh95.ods", sheet = "directeur") |>
  janitor::clean_names() |>
  mutate(across(where(is.character), as.factor)) |>
   mutate(temps_medecin_coordonnateur = factor(cut(temps_medecin_coordonnateur,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 0.05, 0.5, 0.7, 1),
  labels = c("0", "< 0,5", "0,5-0,6","1")
)))
# rename_with(~ paste0("dir_", .x, recycle0 = TRUE))
bn <- read_ods("data/idaoh95.ods", sheet = "bnom_dir")
var_label(dire) <- bn$nom
dire <- dire |>
mutate(nb_lits_rec = cut(nb_lits,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 80,  300),
  labels = c("< 80 lits", "80 lits et +")
))
#
#   Médecins coordonnateurs
#
medco <- read_ods("data/idaoh95.ods", sheet = "medco") |>
  janitor::clean_names() |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(across(
    starts_with("ars"),
    \(x)
    fct_relevel(
      x,
      "Il vous est déjà arrivé de déléguer en totalité  la mise en œuvre de cette mission",
      "Il vous est déjà arrivé de déléguer partiellement la mise en œuvre de cette mission",
      "Vous souhaitez déléguer mais vous ne le faites pas",
      "Vous ne souhaiteriez pas déléguer cette mission"
    )
  )) |>
mutate(age = fct_relevel(age,
    "Moins de 40 ans", "Entre 40 et 49 ans", "Entre 50 et 59 ans",
    "Entre 60 et 69 ans")) |>
# Réordonnancement de medco$nb_d_annees
mutate(nb_d_annees = fct_relevel(nb_d_annees,
    "0-5 ans", "6-10 ans", "11-15 ans", "16-20 ans", "Plus de 20 ans"
  )) |>
# Recodage de medco$presence_effective en medco$presence_effective
  mutate(presence_effective =  cut(presence_effective,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 5, 6, 10, 20),
  labels = c("< 5", "5", "6-9", "10 et +")
)) |>
## Recodage de medco$sous_quelle_forme en medco$sous_quelle_forme_rec
mutate(sous_quelle_forme = fct_recode(sous_quelle_forme,
                                      "En semaine" = "En semaine (lundi au vendredi);",
                                      "En semaine avec astreintes" = "En semaine (lundi au vendredi);Astreintes nuit;",
                                      "En semaine avec astreintes" = "En semaine (lundi au vendredi);Astreintes nuit;Astreintes week-end;",
                                      "En semaine avec astreintes" = "En semaine (lundi au vendredi);Astreintes week-end;"))

bn <- read_ods("data/idaoh95.ods", sheet = "bnom_medco")
var_label(medco) <- bn$Nom
#
#   IDEC
#

idec <- read_ods("data/idaoh95.ods", sheet = "idec") |>
  janitor::clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("data/idaoh95.ods", sheet = "bnom_idec")



bn <- read_ods("data/idaoh95.ods", sheet = "bnom_idec")
var_label(idec) <- bn$nom
#
#  Save
#
save(dire, medco, idec, file = "data/idaoh.RData")
load("data/idaoh.RData")

