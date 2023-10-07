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

# Question 1:

# Question 2:

# Question 3: