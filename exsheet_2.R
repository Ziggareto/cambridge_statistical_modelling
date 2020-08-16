###
#Q53
###

file_path <- "http://www.statslab.cam.ac.uk/~rds37/teaching/statistical_modelling/"
BrainSize <- read.csv(paste0(file_path, "BrainSize.csv"))
attach(BrainSize)
BrainSizeLM2 <- lm(PIQ ~ MRI_Count + Height)

library(ellipse)
# 95% confidence interval ellipse of the MRI_Count and Height betahat coefficients
plot(ellipse(BrainSizeLM2, c(2, 3)), type = "l")
cfint1 <- confint(BrainSizeLM2)

cfint2 <- confint(BrainSizeLM2, level=1-0.05/3)

# Add in horizontal and vertical lines for individual 95% confidence intervals of each coefficient
abline(v=c(cfint1[2, 1],cfint1[2, 2]), col=c("red", "red"))
abline(h=c(cfint1[3, 1],cfint1[3, 2]), col=c("red", "red"))

# 97.5% conf interval cuboid - alpha/p as in Example Sheet 1 Q8
abline(v=c(cfint2[2, 1],cfint2[2, 2]), col=c("blue", "blue"))
abline(h=c(cfint2[3, 1],cfint2[3, 2]), col=c("blue", "blue"))

###
#Q6
###

library(MASS)

# Find the Cooks distance outlier and correct it
lr = lm(hills[,c(3)] ~ hills[,c(1)] + hills[,c(2)])
cooks_distance = cooks.distance(lr)
plot(cooks_distance)
hills[cooks_distance > 1,]
hills[cooks_distance > 1, 'time'] = hills[cooks_distance > 1, 'time'] - 60

lr = lm(time ~ dist + climb, data=hills)
lr2 = lm(hills$time ~ ., data=hills)
newdata <- data.frame("dist" = 5.3, "climb" = 1100)
prediction = predict(lr, newdata=newdata)

# 95% confidence interval for predictions
predict(lr, newdata=newdata, interval="confidence")

hills$distlog = log(hills$dist)
hills$climblog = log(hills$climb)
lr_log = lm(time ~ log(dist) + log(climb), data=hills)
log_prediction = predict(lr_log, newdata=newdata)


###
#Q9
###

mammals_lr = lm(log(brain) ~ log(body), data=mammals)
plot(log(mammals$body), log(mammals$brain))

file_path <- "http://www.statslab.cam.ac.uk/~rds37/teaching/statistical_modelling/"
Colleges <- read.csv(paste0(file_path, "Colleges.csv"))
Colleges_lr <- lm(PercFirsts ~ log(WineBudget), data=Colleges)
plot(log(Colleges$WineBudget), Colleges$PercFirsts)
text(log(Colleges$WineBudget), Colleges$PercFirsts, rownames(Colleges), cex=0.6, pos=3)
abline(Colleges_lr)

