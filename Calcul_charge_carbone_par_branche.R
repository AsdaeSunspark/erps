library(tidyverse)

# Lire les données
prod_induite <- read_csv2("Donnees/prod_induite.csv")
emissions <- read_csv("Donnees/emissions.csv")

# Pour les émissions, on retient l'équivalent CO2 de l'ensemble des gaz
emissions <- emissions |>
  filter(gas_type == "GHG") |>
  select(-c("gas_type"))

# Les trois branches relevant du secteur énergétique sont B (industries
# extractives), C19 (cokéfaction et raffinage), et B (production d'électricité,
# de gaz et de vapeur d'eau)
branches_energies <- c("B", "C19", "D35")

# On calcule l'intensité carbone de chaque branche, puis celle induite par la
# demande finale, et celle induite seulement par l'énergie
intensite_carbone <- prod_nationale |>
  left_join(emissions, by = join_by(sply_product == emis_industry)) |>
  mutate(intensite_carbone = if_else(prod != 0, emissions / prod, 0)) |>
  select(c("sply_product", "intensite_carbone"))

intensite_carbone_induite <- prod_induite |>
  left_join(intensite_carbone, by = join_by(sply_product)) |>
  group_by(use_type) |>
  mutate(
    emis_carbone = value * intensite_carbone,
    emis_carbone_energie = if_else(sply_product %in% branches_energies, emis_carbone, 0)
  ) |>
  summarise(
    intensite_carbone_induite = sum(emis_carbone),
    intensite_carbone_induite_energie = sum(emis_carbone_energie)
  )

# La taxation des émissions issues de l'énergie peut servir de proxy pour
# les émissions totales. En revanche, les émissions induites par l'énergie ne
# sont responsables que d'une faible part des émissions totales
intensite_carbone_induite |>
  filter(!(use_type %in% branches_energies)) |>
  ggplot(aes(x = intensite_carbone_induite, y = intensite_carbone_induite_energie)) +
  geom_point() +
  geom_smooth(method = "lm")

# Quels postes sont les plus intensifs en carbone?
intensite_carbone_induite |>
  ggplot(aes(x = fct_reorder(use_type, intensite_carbone_induite), y = intensite_carbone_induite)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

intensite_carbone_induite |>
  ggplot(aes(x = fct_reorder(use_type, intensite_carbone_induite_energie), y = intensite_carbone_induite_energie)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90)
  )
