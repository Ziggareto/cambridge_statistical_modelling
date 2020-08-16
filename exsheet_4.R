wins = c(2, 1, 3, 2, 6, 3, 7, 1, 3, 3)
tot = c(5, 1, 6, 2, 9, 3, 8, 3, 5, 4)
sel = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
graf = c(-1, 0, 0, 0, 1, 1, 1, 0, 0, 0)
saba = c(0, -1, 0, 0, -1, 0, 0, 1, 1, 0)
navr = c(0, 0, -1, 0, 0, -1, 0, -1, 0, 1)
sanc = c(0, 0, 0, -1, 0, 0, -1, 0, -1, -1)

fit <- glm(wins/tot ~ sel + graf + saba + navr - 1, binomial, weights=tot)
fit2 <- glm(wins/tot ~ sel + graf + saba + navr + sanc - 1, binomial, weights=tot)


file_path <- "https://raw.githubusercontent.com/AJCoca/SM19/master/"
SD_data <- read.csv(paste0(file_path, "SD_go_out_gender_subj.csv"))
attach(SD_data)



interaction.plot(gender, subject, go_out)
interaction.plot(go_out, gender, Freq)

go_out2 = (go_out == "> 2/week")*4 + (go_out == "2/week")*3 + (go_out == "1/week")*2 + (go_out == "< 1/week")*1

mod <- glm(Freq ~ go_out2+gender+subject, family=poisson)

mod1 <- glm(go_out2 ~ gender, family=poisson, weights=Freq)
mod2 <- glm(go_out2 ~ subject, family=poisson, weights=Freq)
summary(mod1)
summary(mod2)
xtabs(Freq ~ subject + go_out2)
