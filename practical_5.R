file_path <- "https://raw.githubusercontent.com/AJCoca/SM19/master/"
EssayMarks <- read.csv(paste0(file_path, "EssayMarks.csv"))
attach(EssayMarks)
levels(Quality)
plot(Mark ~ Quality)
plot(Mark ~ Photo)

EssayMarksLM1 <- lm(Mark ~ Quality)
EssayMarksLM2 <- lm(Mark ~ Quality + Photo)
EssayMarksLM3 <- lm(Mark ~ Quality*Photo)

model.matrix(EssayMarksLM2)

anova(EssayMarksLM1, EssayMarksLM2)

interaction.plot(Photo, Quality, Mark)
interaction.plot(Quality, Photo, Mark)

interaction.plot(Photo, Quality, fitted.values(EssayMarksLM2))
interaction.plot(Quality, Photo, fitted.values(EssayMarksLM2))

anova(EssayMarksLM2, EssayMarksLM3)

Photo_grp <- Photo
levels(Photo_grp) <- c("Contr+Attract", "Contr+Attract", "Unnatractive")

EssayMarksLM4 <- lm(Mark ~ Quality*Photo_grp)
EssayMarksLM5 <- lm(Mark ~ Quality + Photo + Quality:Photo_grp)

AIC(EssayMarksLM1, EssayMarksLM2, EssayMarksLM3, EssayMarksLM4, EssayMarksLM5)