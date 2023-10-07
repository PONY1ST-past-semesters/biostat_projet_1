set.seed(1234)

donnee <- read.delim("Chapters\\biostat_projet_1\\resource_content_1_addhealth.txt", header = TRUE)
#donnee <- read.delim("resource_content_1_addhealth.txt", header = TRUE)

attach(donnee)

#On commence par le netoyage
donnee$feeling_depressed <- as.factor(donnee$feeling_depressed)
donnee$feeling_depressed[is.na(donnee$feeling_depressed)] <- as.factor(floor(runif(sum(is.na(donnee$feeling_depressed)), min = 1, max = 4.9999)))

donnee$smoking <- as.factor(donnee$smoking)

donnee$weight <- as.numeric(donnee$weight)
donnee$weight[is.na(donnee$weight)] <- mean(donnee$weight, na.rm = TRUE)

donnee$time <- as.factor(donnee$time) #identiquement 1

donnee$age <- as.numeric(donnee$age)
donnee$age[is.na(donnee$age)] <- as.factor(floor(mean(donnee$age, na.rm = TRUE)))

donnee$sex <- as.factor(donnee$sex)
donnee$sex[is.na(donnee$sex)] <- as.factor(floor(runif(sum(is.na(donnee$sex)), min = 1, max = 2.9999)))

donnee$SES <- as.factor(donnee$SES)
donnee$SES[is.na(donnee$SES)] <- as.factor(floor(runif(sum(is.na(donnee$SES)), min = 2, max = 10.9999)))

attach(donnee)

print(head(donnee, 30))