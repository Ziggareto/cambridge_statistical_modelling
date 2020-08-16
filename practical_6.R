file_path <- "https://raw.githubusercontent.com/AJCoca/SM19/master/"
Myopia <- read.csv(paste0(file_path, "Myopia.csv"))
Myopia[1:3, ]
attach(Myopia)

MyopiaLogReg0 <- glm(myopic ~ 1, family=binomial)
MyopiaLogReg1 <- glm(myopic ~ ., data = Myopia, family = binomial)
summary(MyopiaLogReg1)

MyopiaLogReg2 <- glm(myopic ~ . + mumMyopic:dadMyopic, data = Myopia, family = binomial)
anova(glm(myopic ~ 1, family=binomial), MyopiaLogReg1, MyopiaLogReg2, test="LR")

MyopiaLogReg3 <- glm(myopic ~ gender + sportHR + readHR + studyHR + mumMyopic + dadMyopic, family=binomial)

mumPlusdadMyopic <- (dadMyopic == "Yes") + (mumMyopic == "Yes")
MyopiaLogReg4 <- glm(myopic ~ gender + sportHR + readHR + studyHR + mumMyopic + mumPlusdadMyopic, family=binomial)
summary(MyopiaLogReg4)


detach(Myopia)
Smoking <- read.csv(paste(file_path, "Smoking.csv", sep =""))
attach(Smoking)

total <- Survived + Died
propDied <- Died / total
plot(propDied[Smoker == "Yes"] ~ Age.group[Smoker == "Yes"], xlab = "Age group", ylab = "Proportion died")
points(propDied[Smoker == "No"] ~ Age.group[Smoker == "No"], pch = 4)

logit <- function(p) log(p/(1-p))
plot(logit(propDied)[Smoker == "Yes"] ~ Age.group[Smoker == "Yes"], xlab = "Age group", ylab = "Empirical logit")
points(logit(propDied)[Smoker == "No"] ~ Age.group[Smoker == "No"], pch = 4)

SmokingLogReg1 <- glm(propDied ~ Age.group + Smoker, family = binomial, weights = total)
summary(SmokingLogReg1)

SmokingLogReg2 <- glm(propDied ~ Age.group + I(Age.group^2) + Smoker, family = binomial,weights = total)
SmokingLogReg3 <- glm(propDied ~ factor(Age.group) + Smoker, family = binomial, weights = total)
SmokingLogReg4 <- glm(propDied ~ Age.group + Smoker, family = binomial(link=probit),weights = total)
SmokingLogReg5 <- glm(propDied ~ Age.group + Smoker, family = binomial(link=cloglog),weights = total)

