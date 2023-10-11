library(rsq)

data("hcrabs")
attach (hcrabs)

#Question a: régression de poisson et surdispersion

modele_poisson <- glm ( num.satellites ~ width + spine + color, family = poisson ( link = log ) ,data = hcrabs ) 

#On fait puis le test de surdispersion et évaluer le paramèter de sur-dispersion
library (AER)
print(dispersiontest(modele_poisson))
#Le test donne que le paramètre de dispersion est 3.143975 avec p-valeur 4.07e-08
#Par conséquent, la sur-dispersion est significative

#Question b:  modèle binomiale négatif

#Question c: 2^4-1=15 modèles possibles, il faut choisir une critière pour sélectionner le meilleur modèle
#Disons, on choisit le critère AIC pour sélectionner le meilleur modèle
#Rappel : si on a k modèles, qui ont les valeurs d'AIC  AIC_1, AIC_2, ..., AIC_k resptivement
#Les valeurs exp(-0.5*(AIC_i - AIC_min)) sont les probabilités que le modèle i 
#minimise la perte d'informations

modele_color <- glm ( num.satellites ~ color, family = poisson ( link = log ) ,data = hcrabs ) 
modele_spine <- glm ( num.satellites ~ spine, family = poisson ( link = log ) ,data = hcrabs )
modele_width <- glm ( num.satellites ~ width, family = poisson ( link = log ) ,data = hcrabs )
modele_weight <- glm ( num.satellites ~ weight, family = poisson ( link = log ) ,data = hcrabs )
modele_color_spine <- glm ( num.satellites ~ color + spine, family = poisson ( link = log ) ,data = hcrabs )
modele_color_width <- glm ( num.satellites ~ color + width, family = poisson ( link = log ) ,data = hcrabs )
modele_color_weight <- glm ( num.satellites ~ color + weight, family = poisson ( link = log ) ,data = hcrabs )
modele_spine_width <- glm ( num.satellites ~ spine + width, family = poisson ( link = log ) ,data = hcrabs )
modele_spine_weight <- glm ( num.satellites ~ spine + weight, family = poisson ( link = log ) ,data = hcrabs )
modele_width_weight <- glm ( num.satellites ~ width + weight, family = poisson ( link = log ) ,data = hcrabs )
modele_color_spine_width <- glm ( num.satellites ~ color + spine + width, family = poisson ( link = log ) ,data = hcrabs )
modele_color_spine_weight <- glm ( num.satellites ~ color + spine + weight, family = poisson ( link = log ) ,data = hcrabs )
modele_color_width_weight <- glm ( num.satellites ~ color + width + weight, family = poisson ( link = log ) ,data = hcrabs )
modele_spine_width_weight <- glm ( num.satellites ~ spine + width + weight, family = poisson ( link = log ) ,data = hcrabs )
modele_color_spine_width_weight <- glm ( num.satellites ~ color + spine + width + weight, family = poisson ( link = log ) ,data = hcrabs )

modeles <- list(modele_color, modele_spine, modele_width, modele_weight, 
modele_color_spine, modele_color_width, modele_color_weight, modele_spine_width, 
modele_spine_weight, modele_width_weight, modele_color_spine_width, 
modele_color_spine_weight, modele_color_width_weight, 
modele_spine_width_weight, modele_color_spine_width_weight)

AICs <- rep(0, 15)
for (i in 1:15) {
  AICs[i] <- AIC(modeles[[i]])
}

print(AICs)

min_AIC <- min(AICs)
proba <- rep(0, 15)
for (i in 1:15) {
  proba[i] <- exp(0.5*(min_AIC- AICs[i]))
}

print(proba)
print(which.max(proba)) # = 7, le modèle color-weight est le meilleur modèle

#Question d: Analyse de résidus pour le modèle color-weight
#Disons que, comme dans les diapos, on utilise le résidus d'anscombe https://www.sfu.ca/sasdoc/sashtml/insight/chap39/sect57.htm

library(surveillance)
plot(anscombe.residuals(modele_color_weight, phi =1))
