file_path <- "http://www.statslab.cam.ac.uk/~rds37/teaching/statistical_modelling/"
BrainSize <- read.csv(paste0(file_path, "BrainSize.csv"))

attach(BrainSize)
BrainSizeLM1 <- lm(PIQ ~ MRI_Count)
BrainSizeLM2 <- lm(PIQ ~ MRI_Count + Height)

set.seed(1)
x <- c(rnorm(24), 4)
y <- 1 + x + rnorm(25)
plot(y ~ x)
LinMod1 <- lm(y ~ x)
abline(LinMod1)
(lev1 <- hatvalues(LinMod1)) #leverages of each y value 
which(lev1 > 3*2/25) # pi > 3p/n

# cdf of each y value's cooks distance in F_{p, n-p}
# cause for concern if < 0.5 
pf(cooks.distance(LinMod1), 2, 23, lower.tail=FALSE)

y2 <- y
y2[25] <- y[25] - 5

detach(BrainSize)
(HousePrices <- read.csv(paste0(file_path, "HousePrices.csv")))
HousePricesLM <- lm(Sale.price ~ ., data = HousePrices)
newdata <- data.frame("Bedrooms" = 5, "Bathrooms" = 2, "Living.area" = 1400, "Lot.size" = 7000, "Year.built" = 1950, "Property.tax"=9000)
