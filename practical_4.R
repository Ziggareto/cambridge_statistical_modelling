file_path <- "https://raw.githubusercontent.com/AJCoca/SM19/master/"
HousePrices <- read.csv(paste0(file_path, "HousePrices.csv"))
HousePricesLM1 <- lm(Sale.price ~ ., data = HousePrices)
summary(HousePricesLM1)

attach(HousePrices)
HousePricesLM2 <- lm(Sale.price ~ Bathrooms + Living.area + Year.built)
summary(HousePricesLM2)

anova(lm(Sale.price ~ 1), HousePricesLM2, HousePricesLM1)
l1 = lm(Sale.price ~ Bathrooms)
l2 = lm(Sale.price ~ Bathrooms + Living.area)
l3 = lm(Sale.price ~ Bathrooms + Living.area + Year.built)
l4 = lm(Sale.price ~ Bathrooms + Living.area + Year.built + Lot.size)


stepAIC(lm(Sale.price ~ 1, data = HousePrices), scope =
          + Sale.price ~ Bedrooms + Bathrooms + Living.area + Lot.size + Year.built + Property.tax,
        direction = "forward") 

set.seed(2)
n <- nrow(HousePrices)
n_reps <- 1000
Sale.price_mat <- fitted.values(HousePricesLM2) +
  summary(HousePricesLM2)$sigma * matrix(rnorm(n*n_reps), n, n_reps)

confint_Bdrm <- function(y) {
  LinMod <- lm(y ~ Bedrooms + Bathrooms + Living.area + Lot.size + Year.built + Property.tax)
  return(confint(LinMod)["Bedrooms", ])
}


detach(HousePrices)
Movies <- read.csv(paste(file_path, "Movies.csv", sep =""))
Movies
attach(Movies)

par(mfrow = c(2,2))
plot(Total.Gross ~ Opening)
plot(Total.Gross ~ Screens)
plot(Total.Gross ~ RT)
plot(Total.Gross ~ Budget)

plot(log(Total.Gross) ~ log(Opening))
plot(log(Total.Gross) ~ log(Screens))
plot(log(Total.Gross) ~ log(RT))
plot(log(Total.Gross) ~ log(Budget))

plot(Total.Gross ~ log(Opening))
plot(Total.Gross ~ log(Screens))
plot(Total.Gross ~ log(RT))
plot(Total.Gross ~ log(Budget))

boxcox(lm(Total.Gross ~ log(Opening) + Screens + RT + log(Budget)))

MoviesLM <- lm(log(Total.Gross) ~ log(Opening) + Screens + RT + log(Budget))
summary(MoviesLM)

MoviesLM2 <- lm(log(Total.Gross) ~ log(Opening) + RT + log(Budget))
summary(MoviesLM2)
