library(tidyverse)
library(readxl)

# Lire les données
conso_decile_revenu <- read_excel(
  path = "Donnees/Ventilation_conso.xlsx",
  sheet = 3,
  col_names = c("Detail", "sply_product", paste0("D", 1:10)),
  skip = 1
)

tes_francais <- read_csv("Donnees/tes_francais.csv")

prod_induite <- read_csv2("Donnees/prod_induite.csv")

conso_age <- read_excel(
  path = "Donnees/Ventilation_conso.xlsx",
  sheet = 2,
  col_names = c("Detail", "sply_product", "_29", "30_39", "40_49", "50_64", "65_"),
  skip = 1
)

conso_typmen <- read_excel(
  path = "Donnees/Ventilation_conso.xlsx",
  sheet = 1,
  col_names = c("Detail", "sply_product", "avec_enfants", "sans_enfant", "monoparentale", "complexe", "seul"),
  skip = 1
)

# On convertit la nomenclature de l'un à l'autre manuellement
table_conversion <- tibble(
  nomenclature_sale = conso_decile_revenu |> pull(sply_product),
  nomenclature_propre = c("C10T12", "C10T12","C10T12", "C10T12", "C13T15",
                          "C13T15", "L", "L", "L", "D35", "C31_32", "C13T15",
                          "C27", "C23", "C28", "C20", "C31_32", "Q86", "Q86",
                          "Q86", "C29", "C19", "H49", "J61", "C26", "J59_60",
                          "C31_32", "R90T92", "J58", "R90T92", "P85", "I", "I",
                          "S96", "C31_32", "Q87_88", "K65", "K64", "S")
)

# Il y a beaucoup d'erreurs, mais tant pis
conso_decile_revenu |>
  left_join(table_conversion, join_by(sply_product == nomenclature_sale)) |>
  select(-c("sply_product", "Detail")) |>
  select(sply_product = nomenclature_propre, everything()) |>
  pivot_longer(cols = starts_with("D"), names_to = "Decile", values_to = "value") |>
  group_by(sply_product, Decile) |>
  summarise(value = sum(value)) |>
  summarise(value = sum(value)) |>
  left_join(tes_francais |>
  filter(str_starts(use_type, "P")) |>
  group_by(sply_product) |>
  summarise(value2 = sum(value) * 1000000)) |>
  pivot_longer(cols = c("value", "value2")) |>
  ggplot(aes(x = sply_product, fill = name, y = value)) +
  geom_col(position = "dodge")

# On convertit la nomenclature
conso_decile_revenu <- conso_decile_revenu |>
  left_join(table_conversion, join_by(sply_product == nomenclature_sale)) |>
  select(-c("sply_product", "Detail")) |>
  select(sply_product = nomenclature_propre, everything()) |>
  pivot_longer(cols = starts_with("D"), names_to = "Decile", values_to = "value")

conso_age <- conso_age |>
  left_join(table_conversion, join_by(sply_product == nomenclature_sale)) |>
  select(-c("sply_product", "Detail")) |>
  select(sply_product = nomenclature_propre, everything()) |>
  pivot_longer(cols = !starts_with("s"), names_to = "age_group", values_to = "value")

conso_typmen <- conso_typmen |>
  left_join(table_conversion, join_by(sply_product == nomenclature_sale)) |>
  select(-c("sply_product", "Detail")) |>
  select(sply_product = nomenclature_propre, everything()) |>
  pivot_longer(cols = !starts_with("sply"), names_to = "type_menage", values_to = "value")

# On récupère la consommation totale pour chaque catégorie
conso_totale_decile_revenu <- conso_decile_revenu |>
  group_by(Decile) |>
  summarise(value = sum(value))

conso_totale_age <- conso_age |>
  group_by(age_group) |>
  summarise(value = sum(value))

conso_totale_typemen <- conso_typmen |>
  group_by(type_menage) |>
  summarise(value = sum(value))

# On calcule l'incidence d'une augmentation de la taxe carbone, absolue et en
# proportion de la consommation
incidence_absolue_decile_revenu <- conso_decile_revenu |>
  left_join(
    prod_induite |>
      rename(consd_product = sply_product) |>
      rename(sply_product = use_type) |>
      filter(consd_product %in% c("B", "C19")),
    by = "sply_product",
    relationship = "many-to-many"
  ) |>
  filter(!is.na(consd_product)) |>
  group_by(Decile, consd_product) |>
  summarise(value = sum(value.x * value.y)) |>
  summarise(value = sum(value) * 0.53, .groups = "drop")

incidence_absolue_decile_revenu |>
  left_join(conso_totale_decile_revenu, by = join_by(Decile)) |>
  mutate(incidence_relative = value.x / value.y) |>
  rename(incidence_absolue = value.x) |>
  select(Decile, incidence_absolue, incidence_relative) |>
  pivot_longer(cols = c("incidence_absolue", "incidence_relative"), names_to = "Incidence", values_to = "value") |>
  mutate(
    Incidence = if_else(str_ends(Incidence, "absolue"), "Absolue", "Relative"),
    Decile = parse_number(Decile)
  ) |>
  ggplot(aes(x = Decile, y = value)) +
  geom_col() +
  facet_wrap(~ Incidence, scales = "free_y") +
  ylab("Incidence") +
  scale_x_continuous(breaks = 1:10)

incidence_absolue_age <- conso_age |>
  left_join(
    prod_induite |>
      rename(consd_product = sply_product) |>
      rename(sply_product = use_type) |>
      filter(consd_product %in% c("B", "C19")),
    by = "sply_product",
    relationship = "many-to-many"
  ) |>
  filter(!is.na(consd_product)) |>
  group_by(age_group, consd_product) |>
  summarise(value = sum(value.x * value.y)) |>
  summarise(value = sum(value) * 0.53, .groups = "drop")

incidence_absolue_age |>
  left_join(conso_totale_age, by = join_by(age_group)) |>
  mutate(
    age_group = case_when(
      str_equal(age_group, "_29") ~ "<30 ans",
      str_equal(age_group, "30_39") ~ "30-39 ans",
      str_equal(age_group, "40_49") ~ "40-49 ans",
      str_equal(age_group, "50_64") ~ "50-64 ans",
      str_equal(age_group, "65_") ~ ">64 ans",
    )
  ) |>
  mutate(incidence_relative = value.x / value.y) |>
  rename(incidence_absolue = value.x) |>
  select(age_group, incidence_absolue, incidence_relative) |>
  pivot_longer(cols = c("incidence_absolue", "incidence_relative"), names_to = "Incidence", values_to = "value") |>
  mutate(
    Incidence = if_else(str_ends(Incidence, "absolue"), "Absolue", "Relative")
  ) |>
  ggplot(aes(x = fct_reorder(.f = age_group, .x = age_group, .fun = function(x) parse_number(x)[1]), y = value)) +
  geom_col() +
  facet_wrap(~ Incidence, scales = "free_y") +
  ylab("Incidence") +
  xlab("Âge") +
  theme(
    axis.text.x = element_text(angle = 20)
  )

incidence_absolue_typmen <- conso_typmen |>
  left_join(
    prod_induite |>
      rename(consd_product = sply_product) |>
      rename(sply_product = use_type) |>
      filter(consd_product %in% c("B", "C19")),
    by = "sply_product",
    relationship = "many-to-many"
  ) |>
  filter(!is.na(consd_product)) |>
  group_by(type_menage, consd_product) |>
  summarise(value = sum(value.x * value.y)) |>
  summarise(value = sum(value) * 0.53, .groups = "drop")

incidence_absolue_typmen |>
  left_join(conso_totale_typemen, by = join_by(type_menage)) |>
  mutate(incidence_relative = value.x / value.y) |>
  rename(incidence_absolue = value.x) |>
  select(type_menage, incidence_absolue, incidence_relative) |>
  pivot_longer(cols = c("incidence_absolue", "incidence_relative"), names_to = "Incidence", values_to = "value") |>
  mutate(
    Incidence = if_else(str_ends(Incidence, "absolue"), "Absolue", "Relative"),
    type_menage = case_when(
      str_equal(type_menage, "avec_enfants") ~ "Avec enfants",
      str_equal(type_menage, "complexe") ~ "Complexe",
      str_equal(type_menage, "sans_enfant") ~ "Sans enfant",
      str_equal(type_menage, "seul") ~ "Seul",
      str_equal(type_menage, "monoparentale") ~ "Monoparentale"
    )
  ) |>
  ggplot(aes(x = type_menage, y = value)) +
  geom_col() +
  facet_wrap(~ Incidence, scales = "free_y") +
  ylab("Incidence") +
  xlab("Type de ménage") +
  theme(
    axis.text.x = element_text(angle = 20)
  )

# Redistribution: forfaitaire, uniforme
revenu_etat <- incidence_absolue_decile_revenu |>
  summarise(value = sum(value)) |>
  pull(value)

conso_totale_decile_revenu |>
  left_join(incidence_absolue_decile_revenu, join_by(Decile)) |>
  mutate(
    incidence_absolue = {{revenu_etat}} / 10 - value.y,
    incidence_relative = incidence_absolue / value.x
  ) |>
  select(Decile, incidence_absolue, incidence_relative) |>
  pivot_longer(cols = c("incidence_absolue", "incidence_relative"), names_to = "Incidence", values_to = "value") |>
  mutate(
    Incidence = if_else(str_ends(Incidence, "absolue"), "Absolue", "Relative"),
    Decile = parse_number(Decile)
  ) |>
  ggplot(aes(x = Decile, y = value)) +
  geom_col() +
  facet_wrap(~ Incidence, scales = "free_y") +
  ylab("Incidence") +
  scale_x_continuous(breaks = 1:10)

# Redistribution sur les trois déciles les plus pauvres
conso_totale_decile_revenu |>
  left_join(incidence_absolue_decile_revenu, join_by(Decile)) |>
  mutate(
    incidence_absolue = {{revenu_etat}} / 3 * (Decile %in% c("D1", "D2", "D3")) - value.y,
    incidence_relative = incidence_absolue / value.x
  ) |>
  select(Decile, incidence_absolue, incidence_relative) |>
  pivot_longer(cols = c("incidence_absolue", "incidence_relative"), names_to = "Incidence", values_to = "value") |>
  mutate(
    Incidence = if_else(str_ends(Incidence, "absolue"), "Absolue", "Relative"),
    Decile = parse_number(Decile)
  ) |>
  ggplot(aes(x = Decile, y = value)) +
  geom_col() +
  facet_wrap(~ Incidence, scales = "free_y") +
  ylab("Incidence") +
  scale_x_continuous(breaks = 1:10)
