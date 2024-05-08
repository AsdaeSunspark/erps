library(tidyverse)

# Lire les données
prod_induite <- read_csv2("Donnees/prod_induite.csv")
emissions <- read_csv("Donnees/emissions.csv")
tes_francais <- read_csv("Donnees/tes_francais.csv")

# On calcule la production française pour chaque branche
prod_nationale <- tes_francais |>
  filter(sply_country == "FR") |>
  group_by(sply_product) |>
  summarise(prod = sum(value))

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

# Quels postes sont les plus intensifs en carbone?
intensite_carbone_induite |>
  ggplot(aes(x = fct_reorder(use_type, intensite_carbone_induite), y = intensite_carbone_induite)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  xlab("Branche de production") +
  ylab("Intensité carbone induite") +
  ylim(0, 1.5)

intensite_carbone_induite |>
  ggplot(aes(x = fct_reorder(use_type, intensite_carbone_induite_energie), y = intensite_carbone_induite_energie)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  xlab("Branche de production") +
  ylab("Intensité carbone induite par\nles trois branches énergétiques") +
  ylim(0, 0.5)

# On corrèle les rangs de chacune des branches, pour vérifier l'alignement
fisc_carbone <- intensite_carbone_induite |>
  select(use_type, intensite_carbone_induite, intensite_carbone_induite_energie) 

cor(rank(data$intensite_carbone_induite), rank(data$intensite_carbone_induite_energie))

# On considère trois manières de fixer le prix relatif de chacune des trois branches:
# 1) selon l'intensité carbone de chaque branche :
# pente de 15,68, poids pas important, R² environ 55%
intensite_carbone_induite |>
  filter(!(use_type %in% branches_energies)) |>
  left_join(prod_nationale, by = join_by(use_type == sply_product)) |>
  ggplot(aes(x = intensite_carbone_induite, y = intensite_carbone_induite_energie)) +
  geom_point(aes(size = prod)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Intensité carbone induite") +
  ylab("Intensité carbone prédite") +
  labs(size = "Production") +
  theme(legend.position = "none")

coef_naifs <- lm(
  intensite_carbone_induite ~ intensite_carbone_induite_energie,
  data = intensite_carbone_induite |>
  filter(!(use_type %in% branches_energies)) |>
  left_join(prod_nationale, by = join_by(use_type == sply_product))#,
  # weights = prod
)

summary(coef_naifs)

# Calcul des coefficients
# Le coefficient sur intensite_carbone_induite_energie indique le prix à fixer sur
# les émissions issues de l'énergie pour obtenir une taxe unitaire sur les émissions
# totale. Pour obtenir le prix sur une unité (en valeur) produite, on multiplie par
# l'intensité carbone (la quantité émise par la production d'une unité en valeur).
# Les masses de gaz sont en kT mais les productions sont en millions d'euros.
intensite_carbone |>
  filter(sply_product %in% branches_energies) |>
  mutate(taxe_unitaire = coef_naifs$coef[["intensite_carbone_induite_energie"]] * intensite_carbone * 100  / 1000)

# 2) en fixant des prix relatifs proches de ceux utilisés en pratique
# càd même prix pour B et C19 et prix nul pour D35 (gaz/électricité)
#
energie_carbone_induite <- prod_induite |>
  filter(sply_product %in% branches_energies) |>
  pivot_wider(id_cols = use_type, names_from = sply_product, values_from = value) |>
  left_join(intensite_carbone_induite, by = join_by(use_type)) |>
  left_join(prod_nationale, by = join_by(use_type == sply_product))

energie_carbone_induite |>
  filter(!(use_type %in% branches_energies)) |>
  mutate(
    intensite_carbone_predite_energie = B + C19
  ) |>
  ggplot(aes(x = intensite_carbone_induite, y = intensite_carbone_predite_energie)) +
  geom_point(aes(size = prod)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Intensité carbone induite") +
  ylab("Intensité carbone prédite") +
  labs(size = "Production") +
  theme(legend.position = "none")

coef_actuels <- lm(
  intensite_carbone_induite ~ intensite_carbone_predite_energie,
  data = energie_carbone_induite |>
    filter(!(use_type %in% branches_energies)) |>
    mutate(
      intensite_carbone_predite_energie = B + C19
    )#,
  # weights = prod
)
summary(coef_actuels)

# Calcul des coefficients
# Ici, le coefficient sur intensite_carbone_induite_energie indique le montant à fixer
# directement sur le produit de chaque branche pour obtenir une taxe unitaire sur
# les émissions totale. Les masses de gaz sont en kT mais les productions sont
# en millions d'euros.
coef_actuels$coef[["intensite_carbone_predite_energie"]] * 100 / 1000

# 3) en choisissant les prix relatifs pour prédire au mieux
coef_carbone <- lm(
  intensite_carbone_induite ~ B + C19 + D35,
  data = energie_carbone_induite |> filter(!(use_type %in% branches_energies))
)

summary(coef_carbone)

coef_carbone_rwt <- lm(
  intensite_carbone_induite ~ B + C19 + D35,
  data = energie_carbone_induite |> filter(!(use_type %in% branches_energies)),
  weights = prod
)

summary(coef_carbone_rwt)

energie_carbone_induite |>
  filter(!(use_type %in% branches_energies)) |>
  mutate(
    intensite_carbone_predite_energie =
      B * coef_carbone$coef["B"] +
      C19 * coef_carbone$coef["C19"] +
      D35 * coef_carbone$coef["D35"],
    intensite_carbone_predite_rwt_energie =
      B * coef_carbone_rwt$coef["B"] +
      C19 * coef_carbone_rwt$coef["C19"] +
      D35 * coef_carbone_rwt$coef["D35"],
  ) |>
  pivot_longer(cols = starts_with("intensite_carbone_predite"), names_to = "rwt", values_to = "intensite_carbone_predite") |>
  mutate(rwt = if_else(str_detect(rwt, "rwt"), "Régression repondérée", "Régression standard")) |>
  ggplot(aes(x = intensite_carbone_induite, y = intensite_carbone_predite)) +
  geom_point(aes(size = prod)) +
  facet_wrap(~rwt) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Intensité carbone induite") +
  ylab("Intensité carbone prédite") +
  labs(size = "Production") +
  theme(legend.position = "top")

# Calcul des coefficients
# Ici, le coefficient sur intensite_carbone_induite_energie indique le montant à fixer
# directement sur le produit de chaque branche pour obtenir une taxe unitaire sur
# les émissions totale. Les masses de gaz sont en kT mais les productions sont
# en millions d'euros.
coef_carbone$coef[c("B", "C19", "D35")] * 100 / 1000
coef_carbone_rwt$coef[c("B", "C19", "D35")] * 100 / 1000