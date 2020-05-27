state <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/state.csv")
mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])

weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
library("matrixStats")
weightedMedian(state[["Murder.Rate"]], w=state[["Population"]])

sd(state[["Population"]])
IQR(state[["Population"]])
mad(state[["Population"]])

quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))

boxplot(state[["Population"]]/1000000, ylab="Population (millions)")

breaks <- seq(from=min(state[["Population"]]),
                to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks,
                right=TRUE, include.lowest = TRUE)
table(pop_freq)

hist(state[["Population"]], breaks=breaks)

hist(state[["Murder.Rate"]], freq=FALSE)
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")


dfw <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/dfw_airline.csv")
barplot(as.matrix(dfw)/6, cex.axis=.5)

sp500_px <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/sp500_px.csv")
sp500_sym <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/sp500_sym.csv", stringsAsFactors = FALSE)

etfs <- sp500_px[row.names(sp500_px)>"2012-07-01",
                 sp500_sym[sp500_sym$sector=="etf", 'symbol']]
library(corrplot)
corrplot(cor(etfs), method = "ellipse")

telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")

kc_tax <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/kc_tax.csv")

kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving>100 &
           SqFtTotLiving<3500)
nrow(kc_tax0)

library(ggplot2)
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) +
  stat_binhex(colour="white") +
  theme_bw() +
  scale_fill_gradient(low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")

ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() +
  geom_point( alpha=0.1) +
  geom_density2d(colour="white") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")

library(descr)
lc_loans <- read.csv("/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/lc_loans.csv")
x_tab <- CrossTable(lc_loans$grade, lc_loans$status,
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

airline_stats <- read.csv(file= "/home/af/Dokumenter/Programs/PracticalStatisticsforDataScientists-R40/data/airline_stats.csv")
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0, 50))

ggplot(data=airline_stats, aes(airline, pct_carrier_delay)) +
  ylim(0, 50) +
  geom_violin() +
  labs(x="", y="Daily % of Delayed Flights")

ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
         aes(x=SqFtTotLiving, y=TaxAssessedValue)) +
  stat_binhex(colour="white") +
  theme_bw() +
  scale_fill_gradient( low="white", high="blue") +
  labs(x="Finished Square Feet", y="Tax Assessed Value") +
  facet_wrap("ZipCode")
