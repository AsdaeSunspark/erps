library(tidyverse)


# Importation du TES
tes_francais <- read_csv("Donnees/tes_francais.csv")

# Liste des emplois finals
empl_fin <- c("P3_S13", "P3_S14", "P3_S15", "P51G", "P5M", "P6")

# On fusionne consommations intermédiaires de sources nationale et importée
conso_intermediaire <- tes_francais |>
  filter(!(use_type %in% empl_fin)) |>
  group_by(sply_product, use_type) |>
  summarise(value = sum(value), .groups = "drop")

# On extrait les consommations finales de sources nationale et importée
conso_finale <- tes_francais |>
  filter((use_type %in% empl_fin)) |>
  group_by(sply_product) |>
  summarise(value = sum(value))

# On calcule la production française pour chaque branche
prod_nationale <- tes_francais |>
  filter(sply_country == "FR") |>
  group_by(sply_product) |>
  summarise(prod = sum(value))

# On calcule les consommations intermédiaires par unité produite, et on en déduit
# la matrice des coefficients techniques
coeff_techniques <- conso_intermediaire |>
  left_join(prod_nationale, by = join_by(use_type == sply_product)) |>
  mutate(value = value / prod,
         value = if_else(is.na(value), 0, value)
  ) |>
  select(-prod) |>
  arrange(sply_product, use_type) |>
  pivot_wider(names_from = use_type, values_from = value) |>
  column_to_rownames("sply_product") |>
  as.matrix()

# On inverse I - A pour obtenir la production induite par la demande finale (où
# A est la matrice des coefficients techniques)
prod_induite <- solve(diag(64) - coeff_techniques, diag(64))
prod_induite <- as_tibble(prod_induite)
rownames(prod_induite) <- rownames(coeff_techniques)
colnames(prod_induite) <- colnames(coeff_techniques)

prod_induite <- prod_induite |>
  rownames_to_column("sply_product") |>
  pivot_longer(cols = !c("sply_product"), names_to = "use_type")

# prod_induite indique la quantité (en valeur) de sply_product nécessaire à
# produire pour permettre une demande finale d'une unité (en valeur) de use_type
prod_induite |> write_csv2("Donnees/prod_induite.csv")