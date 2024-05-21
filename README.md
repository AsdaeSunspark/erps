# Économie de la redistribution et de la protection sociale

Ce github correspond au mémoire de validation du cours *Économie de la redistribution et de la protection sociale*, proposée en troisième année de l'ENSAE, et enseigné par Lionel WILNER et Mathias ANDRE. Il s'agit d'un mémoire de microsimulation portant sur la taxation carbone et ses effets (hétérogènes) sur la population. Il est réalisé par Maël LAOUFI et Souleymane Kane DIALLO. Pour de plus amples informations, consulter le document memoire.pdf.

## Données

Avant de faire tourner les codes, les fichiers .csv *emissions.csv*, *tes_francais.csv* devront être ajoutés à un dossier *Donnees* à la racine du projet.

## Rapport

Le rapport est disponible en tant que "memoire.pdf"

## Codes

On détaille ici la fonction des différents fichiers .R, ils sont listés dans l'ordre dans lequel ils doivent être exécutés :
- "Exploration_donnees.R" réalise une exploration initiale des dossiers.
- "Construction_demande_induite_par_consommation_finale.R" utile la technique de Leontieff pour calculer la production induite par une unité de demande finale dans chaque branche.
- "Calcul_charge_carbone_par_branche.R" utilise les données concernant les émissions carbone de chaque brancghe ainsi que les productions induites calculées dans le fichier précédent pour calculer les émissions carbone induites tout au long du processus de production d'une unité de demande finale, pour chaque branche. Les émissions induites sont calculées à la fois globalement, et seulement pour les branches énergétiques.
- "Calcul_incidence_taxe.R" utilise les émissions carbone induites par branche, calculées dans le fichier précédent, pour microsimuler l'incidence d'une augmentation de la taxe carbone sur les énergies fossiles le long de différentes catégories de foyer. Les dimensions considérées (séparèment) sont l'âge, le niveau de vie et la composition du foyer.
