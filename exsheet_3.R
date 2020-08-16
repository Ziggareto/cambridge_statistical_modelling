library(MASS)
attach(cabbages)
# Planting date effect on head weight?

plot(HeadWt ~ Date)
cabbagesLM = lm(HeadWt ~ Date)
cabbagesLM2 = lm(HeadWt ~ Date + VitC + Cult)

plot(VitC ~ Date)


# Initial thinking says yes Date does have an effect on head weight. However maybe it's more like this effect
# can be explained by other variables. Eg perhaps different cultivar and VitC for different dates.

