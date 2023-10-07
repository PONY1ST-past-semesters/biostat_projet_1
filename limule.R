library(rsq)

data("hcrabs")
attach (hcrabs)

#Question a: régression de poisson et surdispersion

modele_poisson <- glm ( num.satellites ~ width + spine + color, family = poisson ( link = log ) ,data = hcrabs ) 

print(modele_poisson)
#generic_frame <- data.frame(width = 10, spine = as.factor(1), color = as.factor(2))
#print(predict(modele_poisson, newdata = generic_frame, type = "response"))
