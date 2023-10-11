set.seed(1234)

donnee <- read.delim("Chapters\\biostat_projet_1\\addhealth_long.txt", header = TRUE)
#donnee <- read.delim("addhealth_long.txt", header = TRUE)

drops <- c("X","Z", "outcome", "visit", "")
rownames(donnee) <- NULL
donnee <- donnee[ , !(names(donnee) %in% drops)]

donnee$smoking <- as.factor(donnee$smoking)
donnee$age <- as.numeric(donnee$age)
donnee$sex <- as.factor(donnee$sex)
donnee$SES <- as.factor(donnee$SES)
donnee$weight <- as.numeric(donnee$weight)

donnee_separe <- split(donnee, donnee$time_discrete)

# Question 1: Toepliz

# Question 2: Comparer les trois modèles en terme de la corrélation résiduelle
#Je choisis le modèle avec smoking pour 2i
modeles_lineaires_aucune <- list()
residues_aucune <- rep(1,4)
for (i in 1:4){
    modele_i <- lm(donnee_separe[[i]]$weight ~ 1)
    modeles_lineaires_aucune <- append(modeles_lineaires_aucune, modele_i)
    residues_aucune[i] <- mean(residuals(modele_i))
}

modeles_lineaires_age_sex <- list()
residues_age_sex <- rep(1,4)
for (i in 1:4){
    modele_i <- lm(donnee_separe[[i]]$weight ~ donnee_separe[[i]]$age + donnee_separe[[i]]$sex)
    modeles_lineaires_age_sex <- append(modeles_lineaires_age_sex, modele_i)
    residues_age_sex[i] <- mean(residuals(modele_i))
}

modeles_lineaires_age_SES_sex_smoking <- list()
residues_age_SES_sex_smoking <- rep(1,4)
for (i in 1:4){
    modele_i <- lm(donnee_separe[[i]]$weight ~ donnee_separe[[i]]$age + donnee_separe[[i]]$sex + 
donnee_separe[[i]]$SES + donnee_separe[[i]]$smoking)
    modeles_lineaires_age_SES_sex_smoking <- append(
modeles_lineaires_age_SES_sex_smoking,  modele_i)
    residues_age_SES_sex_smoking[i] <- mean(residuals(modele_i))
}

print(cor(residues_aucune, residues_age_sex)) # -0.5644551
print(cor(residues_age_sex, residues_age_SES_sex_smoking)) # -0.9982123
print(cor(residues_age_SES_sex_smoking, residues_aucune)) # 0.5159088

# Question 3:
# Comme discuté, on utilise SES comme une variable explicative pour la variable de réponse weight

# 3.1) l'indépendence entre toutes les observations
modele_lineaire <- lm(weight ~ SES, data = donnee)
print(summary(modele_lineaire))

#3.2) Modèle linéaire généralisé (normale) où on assume une matrice de corrélation interchangeable
# pas complet
library(nlme)
#glm_normal <- glm(weight ~ SES, data = donnee, correlation = corSymm(form = ~ 1 | SES))
#print(summary(glm_normal))

#3.3) GEE + interchangeable
require(geepack)
GEE <- summary ( geese ( weight ~ SES , id = ID, data = donnee , corstr = 'exchangeable' ) )
print(summary(GEE))

#3.4) modèle mixte une ordonnée à l'origine et une pente aléatoire pour chaque individu et des erreurs indépendents
library(lme4)

modele_mixte <- lmer(weight ~ 1 + (1 | SES), data = donnee)
print(summary(modele_mixte))