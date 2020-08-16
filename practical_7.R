file_path <- "https://raw.githubusercontent.com/AJCoca/SM19/master/"
Smoking <- read.csv(paste0(file_path, "Smoking.csv"))
attach(Smoking)
total <- Survived + Died
propDied <- Died / total
SmokingLogReg2 <- glm(propDied ~ Age.group + I(Age.group^2) + Smoker, family = binomial,weights = total)

newdata <- data.frame("Age.group" = rep(21:80, times=2),"Smoker" = gl(2, 60, labels = levels(Smoker)))
newdata[1:3, ]

PredProp <- predict(SmokingLogReg2, newdata, type = "response")

plot(propDied[Smoker == "Yes"] ~ Age.group[Smoker == "Yes"],xlab = "Age group", ylab = "Proportion died", col = "red")
points(propDied[Smoker == "No"] ~ Age.group[Smoker == "No"], pch = 4)
lines(21:80, PredProp[1:60], xlab = "Age", ylab = "Prob of dying", type = "l")
lines(21:80, PredProp[61:120], col = "red")

PredLin <- predict(SmokingLogReg2, newdata, se.fit = TRUE, type = "link")
str(PredLin)

invlogit <- function(x) exp(x) / (1+exp(x))
lines(21:80, invlogit(PredLin$fit[1:60] + 1.96 * PredLin$se.fit[1:60]), lty = 2)
lines(21:80, invlogit(PredLin$fit[1:60] - 1.96 * PredLin$se.fit[1:60]), lty = 2)
lines(21:80, invlogit(PredLin$fit[61:120] + 1.96 * PredLin$se.fit[61:120]),lty = 2, col = "red")
lines(21:80, invlogit(PredLin$fit[61:120] - 1.96 * PredLin$se.fit[61:120]),lty = 2, col = "red")


detach(Smoking)
football2014 <- read.csv(paste0(file_path, "football2014.csv"))
football2014[1:3, ]

football2014$By <- relevel(football2014$By, "Leicester")
football2014$Against <- relevel(football2014$Against, "Leicester")

attach(football2014)
LogLinMod <- glm(GoalsScored ~ HomeAway + By + Against, family = poisson)
summary(LogLinMod)

attack_strength <- exp(sort(coef(LogLinMod)[3:21], decreasing = TRUE))
barplot(rev(attack_strength), las=2, horiz=TRUE, cex.names=0.75)

detach(football2014)
fixtures_remaining <- read.csv(paste0(file_path, "fixtures_remaining.csv"))

Pred <- predict(LogLinMod, newdata=fixtures_remaining, type="response")
cbind(fixtures_remaining, Pred)
