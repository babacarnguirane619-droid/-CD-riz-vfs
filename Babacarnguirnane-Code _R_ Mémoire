# =========================================================
# MÉMOIRE : Babacar NGUIRANE
# Simulation réaliste des données climatiques
# Vallée du Fleuve Sénégal (VFS)
# Période : 2010 - 2025
# =========================================================

library(tidyverse)
library(lubridate)

set.seed(123)


annees <- 2010:2025

departements <- c(
  "Dagana",
  "Podor",
  "Matam"
)

saisons <- c(
  "Hivernage",
  "Contre_saison"
)

varietes <- c(
  "ISRIZ15",
  "Sahel108"
)


n_parcelles <- 23


base <- expand.grid(
  annee = annees,
  departement = departements,
  saison = saisons,
  variete = varietes,
  parcelle = 1:n_parcelles
)

base <- as_tibble(base)


base <- base %>%
  mutate(
    
    precipitation_totale = case_when(
      
      saison == "Hivernage" ~
        rnorm(n(), mean = 350, sd = 120),
      
      saison == "Contre_saison" ~
        rnorm(n(), mean = 15, sd = 10)
    ),
    
    precipitation_totale =
      pmax(precipitation_totale, 0)
  )


base <- base %>%
  mutate(
    
    nb_jours_pluie = case_when(
      
      saison == "Hivernage" ~
        round(rnorm(n(), mean = 28, sd = 8)),
      
      saison == "Contre_saison" ~
        round(rnorm(n(), mean = 2, sd = 2))
    ),
    
    nb_jours_pluie =
      pmax(nb_jours_pluie, 0)
  )


base <- base %>%
  mutate(
    
    tmin = case_when(
      
      saison == "Hivernage" ~
        rnorm(n(), mean = 25, sd = 2),
      
      saison == "Contre_saison" ~
        rnorm(n(), mean = 18, sd = 3)
    ),
    
    tmax = case_when(
      
      saison == "Hivernage" ~
        rnorm(n(), mean = 37, sd = 2),
      
      saison == "Contre_saison" ~
        rnorm(n(), mean = 32, sd = 3)
    )
  )


base <- base %>%
  mutate(
    
    etp = case_when(
      
      saison == "Hivernage" ~
        rnorm(n(), mean = 5.5, sd = 1),
      
      saison == "Contre_saison" ~
        rnorm(n(), mean = 7.5, sd = 1)
    )
  )


base <- base %>%
  mutate(
    
    disponibilite_eau =
      precipitation_totale * 0.35 +
      rnorm(n(), mean = 350, sd = 60)
  )


base <- base %>%
  mutate(
    
    vitesse_vent =
      rnorm(n(), mean = 3.5, sd = 1),
    
    vitesse_vent =
      pmax(vitesse_vent, 0.5)
  )



base <- base %>%
  mutate(
    
    nb_jours_vent_violent =
      round(
        pmax(
          rnorm(n(), mean = 3, sd = 2),
          0
        )
      )
  )


base <- base %>%
  mutate(
    
    probabilite_inondation = case_when(
      
      precipitation_totale > 500 ~ 0.55,
      precipitation_totale > 400 ~ 0.35,
      TRUE ~ 0.08
    ),
    
    inondation =
      rbinom(
        n(),
        size = 1,
        prob = probabilite_inondation
      ),
    
    inondation =
      ifelse(inondation == 1, "Oui", "Non")
  )


base <- base %>%
  mutate(
    
    nb_jours_sup_38 =
      round(
        pmax(
          (tmax - 36) * 4 +
            rnorm(n(), 2, 2),
          0
        )
      )
  )


moyenne_historique <- 34

base <- base %>%
  mutate(
    
    anomalie_temperature =
      tmax - moyenne_historique
  )


base <- base %>%
  mutate(
    
    spi_3mois = precipitation_totale  
    
  )


base <- base %>%
  mutate(
    
    stress_hydrique =
      etp / (disponibilite_eau + 1)
  )


base <- base %>%
  mutate(
    
    cumul_deg_jours =
      pmax(
        ((tmin + tmax)/2 - 10) * 120,
        0
      )
  )


types_sol <- c(
  "Argileux",
  "Limoneux",
  "Sablo_limoneux"
)

base$type_sol <- sample(
  types_sol,
  size = nrow(base),
  replace = TRUE,
  prob = c(0.45, 0.35, 0.20)
)


base <- base %>%
  mutate(
    
    distance_fleuve =
      round(
        runif(n(), 0.5, 25),
        1
      )
  )


base <- base %>%
  mutate(
    
    mois_semis = case_when(
      
      saison == "Hivernage" ~
        sample(
          c("Juillet", "Août"),
          n(),
          replace = TRUE
        ),
      
      TRUE ~
        sample(
          c("Novembre", "Décembre"),
          n(),
          replace = TRUE
        )
    )
  )

glimpse(base)

summary(base)

write.csv(
  base,
  "base_climatique_vfs.csv",
  row.names = FALSE
)


cat(
  "\nBase climatique simulée créée avec succès !\n"
)

cat(
  "\nNombre d'observations : ",
  nrow(base),
  "\n"
)# =========================================================
# BLOC 2 — VARIABLES AGRONOMIQUES
# + RENDEMENT
# + REVENUS AGRICOLES
# =========================================================

# =========================================================
# 1. SUPERFICIE CULTIVÉE (ha)
# =========================================================

base <- base %>%
  mutate(
    
    superficie =
      round(
        runif(n(), 0.5, 8),
        2
      )
  )

# =========================================================
# 2. DOSE D'ENGRAIS (kg/ha)
# =========================================================

base <- base %>%
  mutate(
    
    dose_engrais = case_when(
      
      variete == "ISRIZ15" ~
        rnorm(n(), mean = 260, sd = 40),
      
      TRUE ~
        rnorm(n(), mean = 230, sd = 45)
    ),
    
    dose_engrais =
      pmax(dose_engrais, 50)
  )

# =========================================================
# 3. PRÉSENCE DE PLANTES INVASIVES
# =========================================================

base <- base %>%
  mutate(
    
    probabilite_invasives =
      case_when(
        
        saison == "Hivernage" ~ 0.35,
        TRUE ~ 0.18
      ),
    
    plantes_invasives =
      rbinom(n(), 1, probabilite_invasives),
    
    plantes_invasives =
      ifelse(
        plantes_invasives == 1,
        "Oui",
        "Non"
      )
  )

# =========================================================
# 4. STADE PHÉNOLOGIQUE
# =========================================================

stades <- c(
  "Tallage",
  "Montaison",
  "Epiaison",
  "Maturation"
)

base$stade_phenologique <- sample(
  stades,
  size = nrow(base),
  replace = TRUE,
  prob = c(0.30, 0.25, 0.20, 0.25)
)

# =========================================================
# 5. VARIABLES SATELLITAIRES
# =========================================================

# ---------------------------------------------------------
# 5.1 NDVI
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    ndvi =
      0.35 +
      (dose_engrais / 600) +
      (disponibilite_eau / 2500) -
      (nb_jours_sup_38 / 120) +
      rnorm(n(), 0, 0.05),
    
    ndvi =
      pmin(
        pmax(ndvi, 0.2),
        0.95
      )
  )

# ---------------------------------------------------------
# 5.2 EVI
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    evi =
      ndvi * 0.75 +
      rnorm(n(), 0, 0.03),
    
    evi =
      pmin(
        pmax(evi, 0.1),
        0.8
      )
  )

# =========================================================
# 6. PERTES ESTIMÉES (%)
# =========================================================

base <- base %>%
  mutate(
    
    pertes_estimees =
      2 +
      (nb_jours_sup_38 * 0.8) +
      (ifelse(inondation == "Oui", 12, 0)) +
      (ifelse(plantes_invasives == "Oui", 7, 0)) +
      rnorm(n(), 0, 3),
    
    pertes_estimees =
      pmin(
        pmax(pertes_estimees, 0),
        80
      )
  )

# =========================================================
# 7. ANNÉES CLIMATIQUES EXTRÊMES
# =========================================================

# Sécheresse sévère
annees_secheresse <- c(2011, 2014, 2021)

# Inondation sévère
annees_inondation <- c(2012, 2020)

base <- base %>%
  mutate(
    
    choc_climatique = case_when(
      
      annee %in% annees_secheresse ~
        "Sécheresse",
      
      annee %in% annees_inondation ~
        "Inondation",
      
      TRUE ~
        "Normal"
    )
  )

# =========================================================
# 8. CONSTRUCTION RÉALISTE DU RENDEMENT
# =========================================================

base <- base %>%
  mutate(
    
    # -----------------------------------------------------
    # RENDEMENT THÉORIQUE
    # -----------------------------------------------------
    
    rendement_paddy =
      
      5.5 +
      
      # Effet variété
      ifelse(variete == "ISRIZ15", 0.7, 0.3) +
      
      # Effet saison
      ifelse(saison == "Contre_saison", 0.9, 0) +
      
      # Effet engrais
      (dose_engrais / 320) +
      
      # Effet NDVI
      (ndvi * 2.5) +
      
      # Effet disponibilité eau
      (disponibilite_eau / 1200) -
      
      # Stress thermique
      (nb_jours_sup_38 * 0.08) -
      
      # Vent violent
      (nb_jours_vent_violent * 0.04) -
      
      # Inondation
      ifelse(inondation == "Oui", 1.1, 0) -
      
      # Mauvaises herbes
      ifelse(plantes_invasives == "Oui", 0.7, 0) -
      
      # Pertes
      (pertes_estimees * 0.03) -
      
      # Sécheresse sévère
      ifelse(
        choc_climatique == "Sécheresse",
        1.3,
        0
      ) -
      
      # Inondation sévère
      ifelse(
        choc_climatique == "Inondation",
        1,
        0
      ) +
      
      # Bruit aléatoire
      rnorm(n(), 0, 0.7)
  )

# ---------------------------------------------------------
# LIMITES BIOLOGIQUES
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    rendement_paddy =
      pmin(
        pmax(rendement_paddy, 1),
        12
      )
  )

# =========================================================
# 9. VARIABLES ÉCONOMIQUES
# =========================================================

# ---------------------------------------------------------
# 9.1 PRIX DU RIZ
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    prix_riz =
      round(
        rnorm(n(), mean = 185000, sd = 15000),
        0
      )
  )

# ---------------------------------------------------------
# 9.2 COÛTS DE PRODUCTION
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    cout_engrais =
      dose_engrais * 350,
    
    cout_irrigation =
      superficie * runif(n(), 45000, 95000),
    
    cout_main_oeuvre =
      superficie * runif(n(), 70000, 160000)
  )

# ---------------------------------------------------------
# 9.3 REVENU BRUT
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    revenu_brut =
      rendement_paddy *
      superficie *
      prix_riz
  )

# ---------------------------------------------------------
# 9.4 REVENU NET
# ---------------------------------------------------------

base <- base %>%
  mutate(
    
    revenu_net =
      revenu_brut -
      (
        cout_engrais +
          cout_irrigation +
          cout_main_oeuvre
      )
  )

# =========================================================
# 10. VÉRIFICATIONS
# =========================================================

summary(base$rendement_paddy)

summary(base$revenu_net)

# =========================================================
# 11. SAUVEGARDE
# =========================================================

write.csv(
  base,
  "base_complete_riz_vfs.csv",
  row.names = FALSE
)

# =========================================================
# 12. MESSAGE FINAL
# =========================================================

cat(
  "\nBloc 2 terminé avec succès !\n"
)

cat(
  "\nVariables agronomiques, rendement et revenus générés.\n"
)# =========================================================
# BLOC 3 — EXPLORATION DES DONNÉES (EDA)
# ANALYSES DESCRIPTIVES ET VISUALISATIONS
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(tidyverse)
library(corrplot)
library(GGally)

# =========================================================
# 2. APERÇU GÉNÉRAL
# =========================================================

# Structure
str(base)

# Dimensions
dim(base)

# Résumé statistique
summary(base)

# =========================================================
# 3. VALEURS MANQUANTES
# =========================================================

colSums(is.na(base))

# =========================================================
# 4. DISTRIBUTION DU RENDEMENT
# =========================================================

ggplot(base, aes(x = rendement_paddy)) +
  
  geom_histogram(
    bins = 30,
    fill = "darkgreen",
    color = "white"
  ) +
  
  labs(
    title = "Distribution du rendement en riz paddy",
    x = "Rendement (t/ha)",
    y = "Fréquence"
  ) +
  
  theme_minimal()

# =========================================================
# 5. RENDEMENT PAR SAISON
# =========================================================

ggplot(
  base,
  aes(
    x = saison,
    y = rendement_paddy,
    fill = saison
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Rendement selon la saison",
    x = "Saison",
    y = "Rendement (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 6. RENDEMENT PAR VARIÉTÉ
# =========================================================

ggplot(
  base,
  aes(
    x = variete,
    y = rendement_paddy,
    fill = variete
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Rendement selon la variété",
    x = "Variété",
    y = "Rendement (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 7. EFFET DES CHOCS CLIMATIQUES
# =========================================================

ggplot(
  base,
  aes(
    x = choc_climatique,
    y = rendement_paddy,
    fill = choc_climatique
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Impact des chocs climatiques sur le rendement",
    x = "Type de choc",
    y = "Rendement (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 8. RELATION TEMPÉRATURE - RENDEMENT
# =========================================================

ggplot(
  base,
  aes(
    x = tmax,
    y = rendement_paddy
  )
) +
  
  geom_point(alpha = 0.4, color = "red") +
  
  geom_smooth(
    method = "loess",
    color = "blue"
  ) +
  
  labs(
    title = "Relation Tmax et rendement",
    x = "Température maximale (°C)",
    y = "Rendement (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 9. RELATION NDVI - RENDEMENT
# =========================================================

ggplot(
  base,
  aes(
    x = ndvi,
    y = rendement_paddy
  )
) +
  
  geom_point(alpha = 0.4, color = "darkgreen") +
  
  geom_smooth(
    method = "lm",
    color = "black"
  ) +
  
  labs(
    title = "Relation NDVI et rendement",
    x = "NDVI",
    y = "Rendement (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 10. MATRICE DE CORRÉLATION
# =========================================================

variables_numeriques <- base %>%
  
  select(
    precipitation_totale,
    tmin,
    tmax,
    etp,
    disponibilite_eau,
    vitesse_vent,
    nb_jours_sup_38,
    spi_3mois,
    ndvi,
    evi,
    dose_engrais,
    pertes_estimees,
    rendement_paddy,
    revenu_net
  )

# Corrélation
matrice_corr <- cor(variables_numeriques)

# Affichage
corrplot(
  matrice_corr,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.7
)

# =========================================================
# 11. IMPORTANCE VISUELLE DES VARIABLES
# =========================================================

ggpairs(
  variables_numeriques %>%
    sample_n(400)
)

# =========================================================
# 12. ÉVOLUTION TEMPORELLE DES RENDEMENTS
# =========================================================

evolution <- base %>%
  
  group_by(annee) %>%
  
  summarise(
    rendement_moyen =
      mean(rendement_paddy)
  )

ggplot(
  evolution,
  aes(
    x = annee,
    y = rendement_moyen
  )
) +
  
  geom_line(
    color = "blue",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "red",
    size = 2
  ) +
  
  labs(
    title = "Évolution temporelle du rendement moyen",
    x = "Année",
    y = "Rendement moyen (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 13. RENDEMENT PAR DÉPARTEMENT
# =========================================================

ggplot(
  base,
  aes(
    x = departement,
    y = rendement_paddy,
    fill = departement
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Rendement selon le département",
    x = "Département",
    y = "Rendement (t/ha)"
  ) +
  
  theme_minimal()

# =========================================================
# 14. SAUVEGARDE DES STATISTIQUES
# =========================================================

write.csv(
  summary(base),
  "resume_statistique.csv"
)

# =========================================================
# 15. MESSAGE FINAL
# =========================================================

cat(
  "\nEDA terminé avec succès !\n"
)

cat("\nLes graphiques descriptifs ont été générés.\n")

# =========================================================
# BLOC 4 CORRIGÉ — PRÉTRAITEMENT SANS FUITE DE DONNÉES
# =========================================================

library(tidyverse)
library(caret)

donnees <- base

# Conversion des facteurs (inchangé)
donnees <- donnees %>%
  mutate(
    departement       = as.factor(departement),
    saison            = as.factor(saison),
    variete           = as.factor(variete),
    inondation        = as.factor(inondation),
    type_sol          = as.factor(type_sol),
    mois_semis        = as.factor(mois_semis),
    plantes_invasives = as.factor(plantes_invasives),
    stade_phenologique= as.factor(stade_phenologique),
    choc_climatique   = as.factor(choc_climatique)
  )

# Encodage one-hot (inchangé)
variables_facteurs <- donnees %>% select(where(is.factor))
matrice_dummies    <- as.data.frame(model.matrix(~ . -1, data = variables_facteurs))

donnees_finales <- bind_cols(
  donnees %>% select(where(is.numeric)),
  matrice_dummies
)

# ─────────────────────────────────────────────────────────
# CORRECTIF 1 : split AVANT normalisation
# ─────────────────────────────────────────────────────────
train <- donnees_finales %>% filter(annee <= 2022)
test  <- donnees_finales %>% filter(annee >= 2023)

# Variables cibles
y_train <- train$rendement_paddy
y_test  <- test$rendement_paddy

vars_a_normaliser <- c(
  "precipitation_totale", "tmin", "tmax", "etp",
  "disponibilite_eau", "vitesse_vent", "nb_jours_sup_38",
  "spi_3mois", "stress_hydrique", "cumul_deg_jours",
  "distance_fleuve", "superficie", "dose_engrais",
  "ndvi", "evi", "pertes_estimees"
)

# ─────────────────────────────────────────────────────────
# CORRECTIF 2 : fit du scaler sur train uniquement,
#               puis apply sur test
# ─────────────────────────────────────────────────────────
preproc <- preProcess(
  train[, vars_a_normaliser],   # ← train seulement !
  method = c("center", "scale")
)

train[, vars_a_normaliser] <- predict(preproc, train[, vars_a_normaliser])
test[, vars_a_normaliser]  <- predict(preproc, test[, vars_a_normaliser])

# Variables explicatives
x_train <- train %>% select(-rendement_paddy, -revenu_brut, -revenu_net)
x_test  <- test  %>% select(-rendement_paddy, -revenu_brut, -revenu_net)

# =========================================================
# BLOC 5 — MODÈLE LASSO
# MODÈLE DE RÉFÉRENCE (BASELINE)
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(glmnet)
library(Metrics)

# =========================================================
# 2. CONVERSION EN MATRICES
# =========================================================

x_train_mat <- as.matrix(x_train)

x_test_mat <- as.matrix(x_test)

# =========================================================
# 3. VALIDATION CROISÉE LASSO
# =========================================================

cv_lasso <- cv.glmnet(
  x         = x_train_mat,
  y         = y_train,
  alpha     = 1,
  nfolds    = 10,
  standardize = TRUE   # ← changer FALSE en TRUE
)

# =========================================================
# 4. MEILLEUR LAMBDA
# =========================================================

lambda_optimal <- cv_lasso$lambda.min

cat(
  "\nLambda optimal : ",
  lambda_optimal,
  "\n"
)

# =========================================================
# 5. ENTRAÎNEMENT DU MODÈLE FINAL
# =========================================================

modele_lasso <- glmnet(
  
  x = x_train_mat,
  
  y = y_train,
  
  alpha = 1,
  
  lambda = lambda_optimal
)

# =========================================================
# 6. PRÉDICTIONS
# =========================================================

pred_lasso <- predict(
  
  modele_lasso,
  
  s = lambda_optimal,
  
  newx = x_test_mat
)

pred_lasso <- as.numeric(pred_lasso)

# =========================================================
# 7. MÉTRIQUES DE PERFORMANCE
# =========================================================

# ---------------------------------------------------------
# RMSE
# ---------------------------------------------------------

rmse_lasso <- rmse(
  y_test,
  pred_lasso
)

# ---------------------------------------------------------
# MAE
# ---------------------------------------------------------

mae_lasso <- mae(
  y_test,
  pred_lasso
)

# ---------------------------------------------------------
# R²
# ---------------------------------------------------------

pred_lasso <- as.numeric(pred_lasso)

r2_lasso <- 1 - (
  
  sum((y_test - pred_lasso)^2) /
    
    sum((y_test - mean(y_test))^2)
)

r2_lasso
# =========================================================
# 8. AFFICHAGE DES RÉSULTATS
# =========================================================

cat(
  "\n========== PERFORMANCE LASSO ==========\n"
)

cat(
  "\nRMSE : ",
  round(rmse_lasso, 3)
)

cat(
  "\nMAE : ",
  round(mae_lasso, 3)
)

cat(
  "\nR² : ",
  round(r2_lasso, 3),
  "\n"
)

# =========================================================
# 9. COEFFICIENTS IMPORTANTS
# =========================================================

coef_lasso <- coef(
  modele_lasso
)

print(coef_lasso)

# =========================================================
# 10. VARIABLES SÉLECTIONNÉES
# =========================================================

variables_importantes <- rownames(coef_lasso)[
  
  coef_lasso[,1] != 0
]

cat(
  "\nVariables sélectionnées par le LASSO :\n"
)

print(variables_importantes)

# =========================================================
# 11. VISUALISATION VALIDATION CROISÉE
# =========================================================

plot(cv_lasso)

title(
  "Validation croisée du modèle LASSO"
)

# =========================================================
# 12. OBSERVÉ VS PRÉDIT
# =========================================================

resultats_lasso <- data.frame(
  
  Observé = y_test,
  
  Prédit = pred_lasso
)

library(ggplot2)

ggplot(
  resultats_lasso,
  aes(
    x = Observé,
    y = Prédit
  )
) +
  
  geom_point(
    color = "darkgreen",
    alpha = 0.5
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  
  labs(
    title = "LASSO : Observé vs Prédit",
    x = "Rendement observé",
    y = "Rendement prédit"
  ) +
  
  theme_minimal()

# =========================================================
# 13. ANALYSE DES RÉSIDUS
# =========================================================

residus_lasso <- y_test - pred_lasso

ggplot(
  data.frame(residus_lasso),
  aes(x = residus_lasso)
) +
  
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  
  labs(
    title = "Distribution des résidus — LASSO",
    x = "Résidus",
    y = "Fréquence"
  ) +
  
  theme_minimal()

# =========================================================
# 14. SAUVEGARDE DES RÉSULTATS
# =========================================================

performances_lasso <- data.frame(
  
  Modele = "LASSO",
  
  RMSE = rmse_lasso,
  
  MAE = mae_lasso,
  
  R2 = r2_lasso
)

write.csv(
  performances_lasso,
  "performance_lasso.csv",
  row.names = FALSE
)

# =========================================================
# 15. MESSAGE FINAL
# =========================================================

cat(
  "\nModèle LASSO terminé avec succès !\n"
)

# =========================================================
# BLOC 6 — RANDOM FOREST
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(randomForest)
library(Metrics)
library(ggplot2)

# =========================================================
# 2. ENTRAÎNEMENT DU MODÈLE RANDOM FOREST
# =========================================================

set.seed(123)

modele_rf <- randomForest(
  
  x = x_train,
  
  y = y_train,
  
  ntree = 500,
  
  mtry = floor(sqrt(ncol(x_train))),
  
  importance = TRUE
)

# =========================================================
# 3. RÉSUMÉ DU MODÈLE
# =========================================================

print(modele_rf)

# =========================================================
# 4. PRÉDICTIONS
# =========================================================

pred_rf <- predict(
  
  modele_rf,
  
  newdata = x_test
)

# =========================================================
# 5. MÉTRIQUES DE PERFORMANCE
# =========================================================

# ---------------------------------------------------------
# RMSE
# ---------------------------------------------------------

rmse_rf <- rmse(
  y_test,
  pred_rf
)

# ---------------------------------------------------------
# MAE
# ---------------------------------------------------------

mae_rf <- mae(
  y_test,
  pred_rf
)

# ---------------------------------------------------------
# R²
# ---------------------------------------------------------

pred_rf <- as.numeric(pred_rf)

r2_rf <- 1 - (
  
  sum((y_test - pred_rf)^2) /
    
    sum((y_test - mean(y_test))^2)
)

r2_rf

# =========================================================
# 6. AFFICHAGE DES RÉSULTATS
# =========================================================

cat(
  "\n========== PERFORMANCE RANDOM FOREST ==========\n"
)

cat(
  "\nRMSE : ",
  round(rmse_rf, 3)
)

cat(
  "\nMAE : ",
  round(mae_rf, 3)
)

cat(
  "\nR² : ",
  round(r2_rf, 3),
  "\n"
)

# =========================================================
# 7. IMPORTANCE DES VARIABLES
# =========================================================

importance_rf <- importance(modele_rf)

print(importance_rf)

# =========================================================
# 8. VISUALISATION IMPORTANCE VARIABLES
# =========================================================

varImpPlot(
  
  modele_rf,
  
  main = "Importance des variables — Random Forest"
)

# =========================================================
# 9. TOP VARIABLES IMPORTANTES
# =========================================================

importance_df <- data.frame(
  
  Variable = rownames(importance_rf),
  
  Importance = importance_rf[,1]
)

importance_df <- importance_df %>%
  
  arrange(desc(Importance))

top10_rf <- importance_df %>%
  
  slice(1:10)

ggplot(
  top10_rf,
  aes(
    x = reorder(Variable, Importance),
    y = Importance
  )
) +
  
  geom_col(fill = "forestgreen") +
  
  coord_flip() +
  
  labs(
    title = "Top 10 variables importantes — RF",
    x = "Variables",
    y = "Importance"
  ) +
  
  theme_minimal()

# =========================================================
# 10. OBSERVÉ VS PRÉDIT
# =========================================================

resultats_rf <- data.frame(
  
  Observé = y_test,
  
  Prédit = pred_rf
)

ggplot(
  resultats_rf,
  aes(
    x = Observé,
    y = Prédit
  )
) +
  
  geom_point(
    color = "darkblue",
    alpha = 0.5
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  
  labs(
    title = "Random Forest : Observé vs Prédit",
    x = "Rendement observé",
    y = "Rendement prédit"
  ) +
  
  theme_minimal()

# =========================================================
# 11. ANALYSE DES RÉSIDUS
# =========================================================

residus_rf <- y_test - pred_rf

ggplot(
  data.frame(residus_rf),
  aes(x = residus_rf)
) +
  
  geom_histogram(
    bins = 30,
    fill = "orange",
    color = "white"
  ) +
  
  labs(
    title = "Distribution des résidus — RF",
    x = "Résidus",
    y = "Fréquence"
  ) +
  
  theme_minimal()

# =========================================================
# 12. ERREUR OOB
# =========================================================

plot(
  modele_rf,
  main = "Erreur OOB — Random Forest"
)

# =========================================================
# 13. SAUVEGARDE DES PERFORMANCES
# =========================================================

performances_rf <- data.frame(
  
  Modele = "Random Forest",
  
  RMSE = rmse_rf,
  
  MAE = mae_rf,
  
  R2 = r2_rf
)

write.csv(
  performances_rf,
  "performance_random_forest.csv",
  row.names = FALSE
)

# =========================================================
# 14. COMPARAISON LASSO VS RF
# =========================================================

comparaison <- rbind(
  
  performances_lasso,
  
  performances_rf
)

print(comparaison)

# =========================================================
# 15. MESSAGE FINAL
# =========================================================

cat(
  "\nModèle Random Forest terminé avec succès !\n"
)

# =========================================================
# BLOC 7 — XGBOOST
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(xgboost)
library(Metrics)
library(ggplot2)

# =========================================================
# 2. CONVERSION EN MATRICES
# =========================================================

x_train_xgb <- as.matrix(x_train)

x_test_xgb <- as.matrix(x_test)

# =========================================================
# 3. OBJETS DMatrix
# =========================================================

dtrain <- xgb.DMatrix(
  
  data = x_train_xgb,
  
  label = y_train
)

dtest <- xgb.DMatrix(
  
  data = x_test_xgb,
  
  label = y_test
)

# =========================================================
# 4. PARAMÈTRES XGBOOST
# =========================================================

params <- list(
  
  objective = "reg:squarederror",
  
  eval_metric = "rmse",
  
  eta = 0.05,
  
  max_depth = 6,
  
  subsample = 0.8,
  
  colsample_bytree = 0.8,
  
  min_child_weight = 3
)

# =========================================================
# 5. VALIDATION CROISÉE
# =========================================================

set.seed(123)

cv_xgb <- xgb.cv(
  
  params = params,
  
  data = dtrain,
  
  nrounds = 500,
  
  nfold = 5,
  
  early_stopping_rounds = 20,
  
  verbose = 1
)

# =========================================================
# 6. MEILLEUR NOMBRE D'ARBRES
# =========================================================

best_nrounds <- cv_xgb$best_iteration

cat(
  "\nMeilleur nombre d'itérations : ",
  best_nrounds,
  "\n"
)

# =========================================================
# 7. ENTRAÎNEMENT DU MODÈLE FINAL
# =========================================================

modele_xgb <- xgb.train(
  
  params = params,
  
  data = dtrain,
  
  nrounds = best_nrounds,
  
  verbose = 1
)

# =========================================================
# 8. PRÉDICTIONS
# =========================================================

pred_xgb <- predict(
  
  modele_xgb,
  
  newdata = dtest
)

# =========================================================
# 9. MÉTRIQUES DE PERFORMANCE
# =========================================================

# ---------------------------------------------------------
# RMSE
# ---------------------------------------------------------

rmse_xgb <- rmse(
  y_test,
  pred_xgb
)

# ---------------------------------------------------------
# MAE
# ---------------------------------------------------------

mae_xgb <- mae(
  y_test,
  pred_xgb
)

# ---------------------------------------------------------
# R²
# ---------------------------------------------------------

pred_xgb <- as.numeric(pred_xgb)

r2_xgb <- 1 - (
  
  sum((y_test - pred_xgb)^2) /
    
    sum((y_test - mean(y_test))^2)
)

r2_xgb

# =========================================================
# 10. AFFICHAGE DES RÉSULTATS
# =========================================================

cat(
  "\n========== PERFORMANCE XGBOOST ==========\n"
)

cat(
  "\nRMSE : ",
  round(rmse_xgb, 3)
)

cat(
  "\nMAE : ",
  round(mae_xgb, 3)
)

cat(
  "\nR² : ",
  round(r2_xgb, 3),
  "\n"
)

# =========================================================
# 11. IMPORTANCE DES VARIABLES
# =========================================================

importance_xgb <- xgb.importance(
  
  feature_names = colnames(x_train_xgb),
  
  model = modele_xgb
)

print(importance_xgb)

# =========================================================
# 12. VISUALISATION IMPORTANCE VARIABLES
# =========================================================

xgb.plot.importance(
  
  importance_matrix = importance_xgb,
  
  top_n = 15,
  
  main = "Importance des variables — XGBoost"
)

# =========================================================
# 13. OBSERVÉ VS PRÉDIT
# =========================================================

resultats_xgb <- data.frame(
  
  Observé = y_test,
  
  Prédit = pred_xgb
)

ggplot(
  resultats_xgb,
  aes(
    x = Observé,
    y = Prédit
  )
) +
  
  geom_point(
    color = "purple",
    alpha = 0.5
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  
  labs(
    title = "XGBoost : Observé vs Prédit",
    x = "Rendement observé",
    y = "Rendement prédit"
  ) +
  
  theme_minimal()

# =========================================================
# 14. ANALYSE DES RÉSIDUS
# =========================================================

residus_xgb <- y_test - pred_xgb

ggplot(
  data.frame(residus_xgb),
  aes(x = residus_xgb)
) +
  
  geom_histogram(
    bins = 30,
    fill = "darkred",
    color = "white"
  ) +
  
  labs(
    title = "Distribution des résidus — XGBoost",
    x = "Résidus",
    y = "Fréquence"
  ) +
  
  theme_minimal()

# =========================================================
# 15. COMPARAISON DES MODÈLES
# =========================================================

performances_xgb <- data.frame(
  
  Modele = "XGBoost",
  
  RMSE = rmse_xgb,
  
  MAE = mae_xgb,
  
  R2 = r2_xgb
)

comparaison_finale <- rbind(
  
  performances_lasso,
  
  performances_rf,
  
  performances_xgb
)

print(comparaison_finale)

# =========================================================
# 16. VISUALISATION COMPARAISON RMSE
# =========================================================

ggplot(
  comparaison_finale,
  aes(
    x = Modele,
    y = RMSE,
    fill = Modele
  )
) +
  
  geom_col() +
  
  labs(
    title = "Comparaison RMSE des modèles",
    x = "Modèle",
    y = "RMSE"
  ) +
  
  theme_minimal()

# =========================================================
# 17. VISUALISATION COMPARAISON R²
# =========================================================

ggplot(
  comparaison_finale,
  aes(
    x = Modele,
    y = R2,
    fill = Modele
  )
) +
  
  geom_col() +
  
  labs(
    title = "Comparaison R² des modèles",
    x = "Modèle",
    y = "R²"
  ) +
  
  theme_minimal()

# =========================================================
# 18. SAUVEGARDE DES RÉSULTATS
# =========================================================

write.csv(
  comparaison_finale,
  "comparaison_modeles.csv",
  row.names = FALSE
)

# =========================================================
# 19. MESSAGE FINAL
# =========================================================

cat(
  "\nModèle XGBoost terminé avec succès !\n"
)

cat(
  "\nComparaison finale des modèles effectuée.\n"
)

# =========================================================
# BLOC 10 — SVR (Support Vector Regression)
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(e1071)
library(Metrics)
library(ggplot2)

# =========================================================
# 2. ENTRAÎNEMENT — NOYAU RBF (radial)
# =========================================================

set.seed(123)

modele_svr <- svm(
  x         = x_train,
  y         = y_train,
  type      = "eps-regression",
  kernel    = "radial",
  cost      = 1,
  epsilon   = 0.1,
  gamma     = 1 / ncol(x_train)   # défaut recommandé
)

# =========================================================
# 3. RÉSUMÉ DU MODÈLE
# =========================================================

print(modele_svr)

# =========================================================
# 4. PRÉDICTIONS
# =========================================================

pred_svr <- predict(
  modele_svr,
  newdata = x_test
)

pred_svr <- as.numeric(pred_svr)

# =========================================================
# 5. MÉTRIQUES DE PERFORMANCE
# =========================================================

rmse_svr <- rmse(y_test, pred_svr)

mae_svr  <- mae(y_test, pred_svr)

r2_svr <- 1 - (
  sum((y_test - pred_svr)^2) /
    sum((y_test - mean(y_test))^2)
)

cat("\n========== PERFORMANCE SVR ==========\n")
cat("\nRMSE : ", round(rmse_svr, 3))
cat("\nMAE  : ", round(mae_svr,  3))
cat("\nR²   : ", round(r2_svr,   3), "\n")

# =========================================================
# 6. OBSERVÉ VS PRÉDIT
# =========================================================

resultats_svr <- data.frame(
  Observé = y_test,
  Prédit  = pred_svr
)

ggplot(
  resultats_svr,
  aes(x = Observé, y = Prédit)
) +
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linewidth = 1) +
  labs(
    title = "SVR : Observé vs Prédit",
    x = "Rendement observé (t/ha)",
    y = "Rendement prédit (t/ha)"
  ) +
  theme_minimal()

# =========================================================
# 7. ANALYSE DES RÉSIDUS
# =========================================================

residus_svr <- y_test - pred_svr

ggplot(
  data.frame(residus_svr),
  aes(x = residus_svr)
) +
  geom_histogram(
    bins = 30,
    fill = "blue",
    color = "white"
  ) +
  labs(
    title = "Distribution des résidus — SVR",
    x = "Résidus",
    y = "Fréquence"
  ) +
  theme_minimal()

# =========================================================
# 8. COMPARAISON FINALE DES 4 MODÈLES
# =========================================================

performances_svr <- data.frame(
  Modele = "SVR",
  RMSE   = rmse_svr,
  MAE    = mae_svr,
  R2     = r2_svr
)

comparaison_finale <- rbind(
  performances_lasso,
  performances_svr,
  performances_rf,
  performances_xgb
)

comparaison_finale <- comparaison_finale %>%
  arrange(RMSE) %>%
  mutate(
    RMSE = round(RMSE, 3),
    MAE  = round(MAE,  3),
    R2   = round(R2,   3)
  )

print(comparaison_finale)

# =========================================================
# 9. VISUALISATION COMPARATIVE
# =========================================================

ggplot(
  comparaison_finale,
  aes(x = reorder(Modele, RMSE), y = R2, fill = Modele)
) +
  geom_col() +
  geom_text(
    aes(label = R2),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Comparaison R² — 4 modèles",
    x = "Modèle",
    y = "R²"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# =========================================================
# 10. SAUVEGARDE
# =========================================================

write.csv(
  comparaison_finale,
  "comparaison_4_modeles.csv",
  row.names = FALSE
)

cat("\nSVR terminé avec succès !\n")

# =========================================================
# BLOC 8 — INTERPRÉTABILITÉ DES MODÈLES
# ANALYSE SHAP — XGBOOST
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(SHAPforxgboost)
library(ggplot2)
library(dplyr)

# =========================================================
# 2. CALCUL DES VALEURS SHAP
# =========================================================

shap_values <- shap.values(
  
  xgb_model = modele_xgb,
  
  X_train = x_train_xgb
)

# =========================================================
# 3. MATRICE SHAP
# =========================================================

shap_score <- shap_values$shap_score

# =========================================================
# 4. IMPORTANCE GLOBALE DES VARIABLES
# =========================================================

importance_shap <- shap.importance(
  
  shap_contrib = shap_score
)

print(importance_shap)

# =========================================================
# 5. TOP VARIABLES LES PLUS INFLUENTES
# =========================================================

top_variables <- importance_shap %>%
  
  slice(1:15)

print(top_variables)

# =========================================================
# 6. VISUALISATION IMPORTANCE GLOBALE
# =========================================================

shap.plot.summary(
  
  shap_score,
  
  x_train_xgb
)

# =========================================================
# 7. BARPLOT DES VARIABLES IMPORTANTES
# =========================================================

ggplot(
  top_variables,
  aes(
    x = reorder(variable, mean_shap_score),
    y = mean_shap_score
  )
) +
  
  geom_col(fill = "darkgreen") +
  
  coord_flip() +
  
  labs(
    title = "Importance globale des variables (SHAP)",
    x = "Variables",
    y = "Score SHAP moyen"
  ) +
  
  theme_minimal()

# =========================================================
# 8. COMPARAISON IMPORTANCE RF vs XGB
# =========================================================

importance_rf_df <- importance_df %>%
  
  rename(
    importance_rf = Importance
  )

importance_xgb_df <- importance_xgb %>%
  
  select(
    Feature,
    Gain
  ) %>%
  
  rename(
    Variable = Feature,
    importance_xgb = Gain
  )

comparaison_importance <- merge(
  
  importance_rf_df,
  
  importance_xgb_df,
  
  by = "Variable",
  
  all = TRUE
)

print(comparaison_importance)

# =========================================================
# 9. VISUALISATION COMPARÉE
# =========================================================

comparaison_long <- comparaison_importance %>%
  
  pivot_longer(
    
    cols = c(
      importance_rf,
      importance_xgb
    ),
    
    names_to = "Modele",
    
    values_to = "Importance"
  )

ggplot(
  comparaison_long %>%
    filter(!is.na(Importance)) %>%
    slice_max(Importance, n = 20),
  
  aes(
    x = reorder(Variable, Importance),
    y = Importance,
    fill = Modele
  )
) +
  
  geom_col(
    position = "dodge"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Comparaison importance variables RF vs XGB",
    x = "Variables",
    y = "Importance"
  ) +
  
  theme_minimal()

# =========================================================
# 10. SAUVEGARDE DES IMPORTANCES
# =========================================================

write.csv(
  importance_shap,
  "importance_shap.csv",
  row.names = FALSE
)

write.csv(
  comparaison_importance,
  "comparaison_importance_variables.csv",
  row.names = FALSE
)

# =========================================================
# 11. MESSAGE FINAL
# =========================================================

cat(
  "\nAnalyse SHAP terminée avec succès !\n"
)

cat(
  "\nLes variables climatiques influentes ont été identifiées.\n"
)

# =========================================================
# BLOC 9 — ANALYSES STATISTIQUES COMPLÉMENTAIRES
# TESTS, CORRÉLATIONS ET VALIDATIONS
# =========================================================

# =========================================================
# 1. LIBRAIRIES
# =========================================================

library(tidyverse)
library(car)
library(lmtest)
library(caret)

# =========================================================
# 2. CORRÉLATION DE SPEARMAN
# =========================================================

variables_corr <- base %>%
  
  select(
    rendement_paddy,
    precipitation_totale,
    tmin,
    tmax,
    etp,
    disponibilite_eau,
    vitesse_vent,
    nb_jours_sup_38,
    spi_3mois,
    ndvi,
    evi,
    dose_engrais,
    pertes_estimees,
    revenu_net
  )

# Matrice Spearman
corr_spearman <- cor(
  
  variables_corr,
  
  method = "spearman"
)

print(corr_spearman)

# =========================================================
# 3. VISUALISATION CORRÉLATION
# =========================================================

library(corrplot)

corrplot(
  
  corr_spearman,
  
  method = "color",
  
  type = "upper",
  
  tl.col = "black",
  
  tl.cex = 0.7
)

# =========================================================
# 4. TEST DE KRUSKAL-WALLIS
# =========================================================

# ---------------------------------------------------------
# Rendement selon variété
# ---------------------------------------------------------

kruskal_variete <- kruskal.test(
  
  rendement_paddy ~ variete,
  
  data = base
)

print(kruskal_variete)

# ---------------------------------------------------------
# Rendement selon saison
# ---------------------------------------------------------

kruskal_saison <- kruskal.test(
  
  rendement_paddy ~ saison,
  
  data = base
)

print(kruskal_saison)

# ---------------------------------------------------------
# Rendement selon département
# ---------------------------------------------------------

kruskal_departement <- kruskal.test(
  
  rendement_paddy ~ departement,
  
  data = base
)

print(kruskal_departement)

# =========================================================
# 5. BOXPLOTS DES GROUPES
# =========================================================

ggplot(
  base,
  aes(
    x = variete,
    y = rendement_paddy,
    fill = variete
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Test Kruskal — Variété",
    x = "Variété",
    y = "Rendement"
  ) +
  
  theme_minimal()

# =========================================================
# 6. RÉGRESSION LINÉAIRE SIMPLE
# =========================================================

modele_lm <- lm(
  
  rendement_paddy ~
    precipitation_totale +
    tmax +
    ndvi +
    dose_engrais +
    pertes_estimees,
  
  data = base
)

summary(modele_lm)

# =========================================================
# 7. TEST BREUSCH-PAGAN
# =========================================================

bp_test <- bptest(modele_lm)

print(bp_test)

# =========================================================
# 8. TEST DURBIN-WATSON
# =========================================================

dw_test <- dwtest(modele_lm)

print(dw_test)

# =========================================================
# 9. TEST SHAPIRO-WILK
# =========================================================

# Attention :
# échantillon limité à 5000 max

residus_lm <- residuals(modele_lm)

shapiro_test <- shapiro.test(
  
  sample(
    residus_lm,
    size = min(5000, length(residus_lm))
  )
)

print(shapiro_test)

# =========================================================
# 10. MULTICOLINÉARITÉ (VIF)
# =========================================================

vif_modele <- vif(modele_lm)

print(vif_modele)

# =========================================================
# 11. VALIDATION CROISÉE
# =========================================================

controle_cv <- trainControl(
  
  method = "cv",
  
  number = 10
)

modele_cv <- train(
  
  rendement_paddy ~
    precipitation_totale +
    tmax +
    ndvi +
    dose_engrais +
    pertes_estimees,
  
  data = base,
  
  method = "lm",
  
  trControl = controle_cv
)

print(modele_cv)

# =========================================================
# 12. ANALYSE DES RÉSIDUS
# =========================================================

par(mfrow = c(2,2))

plot(modele_lm)

# =========================================================
# 13. IMPORTANCE DES VARIABLES
# =========================================================

coeficients <- summary(modele_lm)$coefficients

importance_lm <- data.frame(
  
  Variable = rownames(coeficients),
  
  Estimate = coeficients[,1],
  
  p_value = coeficients[,4]
)

print(importance_lm)

# =========================================================
# 14. VISUALISATION IMPORTANCE
# =========================================================

ggplot(
  importance_lm %>%
    filter(Variable != "(Intercept)"),
  
  aes(
    x = reorder(Variable, abs(Estimate)),
    y = abs(Estimate)
  )
) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(
    title = "Importance des variables — Régression",
    x = "Variables",
    y = "Effet absolu"
  ) +
  
  theme_minimal()

# =========================================================
# 15. TABLEAU FINAL DES PERFORMANCES
# =========================================================

tableau_final <- comparaison_finale %>%
  
  arrange(RMSE)

print(tableau_final)

# =========================================================
# 16. VISUALISATION COMPARATIVE GLOBALE
# =========================================================

ggplot(
  tableau_final,
  aes(
    x = reorder(Modele, RMSE),
    y = RMSE,
    fill = Modele
  )
) +
  
  geom_col() +
  
  labs(
    title = "Classement final des modèles",
    x = "Modèle",
    y = "RMSE"
  ) +
  
  theme_minimal()

# =========================================================
# 17. SAUVEGARDE DES RÉSULTATS
# =========================================================

write.csv(
  tableau_final,
  "classement_final_modeles.csv",
  row.names = FALSE
)

write.csv(
  importance_lm,
  "importance_regression_lineaire.csv",
  row.names = FALSE
)

# =========================================================
# 18. MESSAGE FINAL
# =========================================================

cat(
  "\nAnalyses statistiques complémentaires terminées !\n"
)

cat(
  "\nLes hypothèses statistiques ont été vérifiées.\n"
)
