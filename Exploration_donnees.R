# ERPS : Taxe carbone et microsimulation ----------------------------------

library(haven)
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)
library(readr)

# Importation des bases "emissions" et "TES"
emissions <- read_csv("Donnees/emissions.csv")
# View(emissions)

tes_francais <- read_csv("Donnees/tes_francais.csv")
# View(tes_francais)

dim(emissions)
dim(tes_francais)

# On ne s'intéresse qu'aux émissions issues de la production française, on ignore
# les importations
tes_fr <- tes_francais |>
  filter(sply_country == "FR")

dim(tes_fr)

# Calcul de la production totale intérieure par branche d'activité (filtre FR)
production_par_prod <- tes_fr |>
  group_by(sply_product) |>
  summarise(prod_tot = sum(value))
production_par_prod
dim(production_par_prod)
View(production_par_prod)

# Codes des emplois finals
empl_fin <- c("P3_S13", "P3_S14", "P3_S15", "P51G", "P5M", "P6")

# Création des CI produites en FR à partir du TES par branche
tes_ci <- tes_fr |>
  filter(!(use_type %in% empl_fin)) |>
  group_by(sply_product) |>
  summarise(CI = sum(value))
dim(tes_ci)

# Création des emplois finals (EF) produits en FR à partir du TES par branche
tes_ef <- tes_fr |>
  filter(use_type %in% empl_fin) |>
  group_by(sply_product) |>
  summarise(EF = sum(value))
dim(tes_ef)

# Fusion des bases de la CI intérieure et des EF intérieurs
tes_ci_ef <- tes_ci |>
  left_join(tes_ef, by = "sply_product")
tes_ci_ef
#View(tes_ci_ef)

# Vérification de si CI+EF=Production Totale
tes_verif <- tes_ci_ef |> left_join(production_par_prod)
tes_verif <- tes_verif |>
  mutate(cons_int = prod_tot - EF) |>
  mutate(verif = CI - cons_int)
tes_verif |> summarise(moy = mean(verif))
# On s'aperçoit que les CI sont identiques (la différence est quasi nulle)

nbre_branche <- tes_fr |>
  summarise(nbre_use = n_distinct(use_type), n_branche = n_distinct(sply_product))
nbre_branche
# Nous avons 6 emplois finals et tous les autres codes correspondent à des CI

# Nous calculons maintenant les CI par branches provenant de la FR et du RM
tes_ci_fr_rm <- tes_francais |>
  filter(!use_type %in% empl_fin) |>
  group_by(sply_product) |>
  group_by(use_type) |>
  summarise(CI_FR_RM = sum(value))

# Pour fusionner les bases contenant la production totale intérieure et les CI
# totales provenant de la FR et du RM on crée une clé de fusion
# qui correspond à sply_product et à use_type respectivement

tes_ci_fr_rm <- tes_ci_fr_rm |>
  mutate(cle = use_type)
dim(tes_ci_fr_rm)
tes_ci_ef <- tes_verif |>
  mutate(cle = sply_product)
dim(tes_ci_ef)
data <- tes_ci_fr_rm |>
  left_join(tes_ci_ef, by = "cle")

# On supprime les colonnes cle, verif et cons_int
data <- data |>
  select(-cle, -cons_int, -verif)
data

# On calcule la matrice comportant les coefficients techniques
Aij <- data |>
  mutate(coefficient = CI_FR_RM / prod_tot)
Aij

# Fusion de la base des coefficients techniques et des emissions
print(unique(emissions$gas_type))
emis_CO2 <- emissions |>
  filter(gas_type == "CO2")  
emis_CO2 <- emis_CO2 |>
  mutate(cle=emis_industry)
dim(emis_CO2)
emis_CO2


#
A <- Aij |>
  mutate(cle=sply_product)

data_emission <- A |>
  left_join(emis_CO2,by="cle")
dim(data_emission)

# données avec les émissions de CO2
data_emission <- data_emission |>
  select(-cle,-gas_type,-emis_industry)
data_emission

