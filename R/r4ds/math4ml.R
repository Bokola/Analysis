# Define columns
columns <- c("trip_distance", "total_amount", "passenger_count")

# Create summary function
taxis_summary <- function(col, data) {
  c(
    mean = mean(data[[col]]), 
    sd = sd(data[[col]]),
    quantile(data[[col]], c(.25, .5, .75))
  )
}

# Use sapply to summarize columns
sapply(columns, taxis_summary, taxis)

a = rbind(c(2, 3), c(1, 4))
b = c(23, 24)
solve(a, b)

library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Define the function
k = function(x) -10 * x^2 + 100 * x + 3

df1 = data.frame(x = seq(0,10))
df1$f = k(df1$x)

ggplot(df1, aes(x,f)) +
  geom_line(color = 'green', size = 1) +
  ylab('k(x) (height in feet)') + 
  xlab('x (time in seconds)')

a = c(184, 180, 181, 184, 182, 181, 182, 182, 183, 182, 181, 182, 182, 180, 183, 181, 184, 183, 182, 182, 183, 182, 183, 181, 180, 181, 183)
mean(a)
median(a)
table(a)
sd(a)

### LOAD PACKAGES ####
install.packages('pwr')
install.packages('effsize')
library(pwr)
library(effsize)
library(ggplot2)

#### LOAD PACKAGES ####
install.packages('psych')
library(psych)
library(ggplot2)
install.packages('Rmisc')
library(Rmisc)

#### LOAD DATA in Axure Notebook environment####
wd = paste(getwd(),"/datasets/cupsdat.csv", sep = "") 
print(wd)
dat <- read.csv(wd)

names(dat)

head(dat)
summary(dat$count)
table(dat$count)
as.matrix(table(dat$count))
table(dat$count) / sum(!is.na(dat$count))

tab1 <- cbind(
  table(dat$count),
  table(dat$count) / sum(!is.na(dat$count))
)

colnames(tab1) <- c("Count", "%")

tab1

# running total
cumsum(table(dat$count)) 

#as percent
cumsum(table(dat$count)) / sum(!is.na(dat$count))

#added to tab1
tab1 <- cbind(
  table(dat$count),
  table(dat$count) / sum(!is.na(dat$count)),
  cumsum(table(dat$count)) / sum(!is.na(dat$count))
)

colnames(tab1) <- c("Count", "%", "cum. %")

tab1

#### LOAD DATA ####
dat <- read.csv("datasets/loyaltydata.csv")
summary(dat)
cor(dat[,-1])
round(cor(dat[,-1]), 2)

#### LOAD DATA ####
dat <- read.csv("datasets/causal.csv")

# Inspect data
names(dat)

str(dat)

head(dat)

summary(dat)

# Means
t(tapply(dat$prod, dat$group, mean, na.rm=T))

# SDs
t(tapply(dat$prod, dat$group, sd, na.rm=T))

t.test(dat$prod ~ dat$group)

t.test(prod ~ group, data=dat)

#### LOAD DATA ####
dat <- read.csv("datasets/measurement.csv")
summary(dat)
str(dat)
dat <- dat[, -1]
names(dat)
round(cor(dat), 2)
fa.parallel(dat, fm = 'minres', fa = 'fa')
fa(dat, nfactors = 1, fm="minres")$loadings
psych::alpha(dat)

dat$sentiment <- rowSums(data.frame(dat$friendly, dat$inviting, dat$awesome, dat$pleasant))

hist(dat$sentiment)
#### LOAD DATA ####
dat <- read.csv("datasets/validity.csv")
#drop the ID variable
dat <- dat[,-1]

#correlations
round(cor(dat), 2)
round( psych::corr.test(dat)$p, 3)

#####Module 5, Lab 1 - Correlation
#### LOAD PACKAGES ####
install.packages('corrplot')
install.packages('psych')
library(ggplot2)
library(corrplot)
library(psych)

#### LOAD DATA ####
dat <- read.csv("datasets/regionalhappy.csv")
names(dat) <- c("Happiness", "GDP", "Family", "Life.Expect", "Freedom", "Generosity", "Trust.Gov", "Dystopia")
cor(dat$Happiness, dat$Life.Expect)

ggplot(data=dat, aes(x=Happiness, y=Life.Expect))+
  geom_point()+
  theme_light()

cor.test(dat$Happiness, dat$Life.Expect)
hist(dat$Happiness)
hist(dat$Life.Expect)
psych::skew(dat$Life.Expect)
dat$Life.Expect2 <- max(dat$Life.Expect) + 1 - dat$Life.Expect
hist(dat$Life.Expect2)

dat$Life.Expect2 <- sqrt(dat$Life.Expect2)
dat$Life.Expect2 <- max(dat$Life.Expect2) + 1 - dat$Life.Expect2

cor.test(dat$Happiness, dat$Life.Expect2)
cor(data.frame(dat$Happiness, dat$Life.Expect, dat$GDP, dat$Generosity))
corr.test(data.frame(dat$Happiness, dat$Life.Expect, dat$GDP, dat$Generosity))$p

# save cor matrix
cors <- cor(dat[,-9])

#heatmap
heatmap(cors, symm = TRUE)

corrplot(cors)
corrplot.mixed(cors, order="hclust")

corrplot.mixed(cors,
               p.mat=corr.test(dat[,-9])$p, sig.level = .05)

####Module 5, Lab 2 - Regression
#### LOAD PACKAGES ####
install.packages('lm.beta')
install.packages('corrplot')
library(ggplot2)
library(lm.beta)
library(corrplot)
library(psych)

#### LOAD DATA ####
dat <- read.csv("datasets/regionalhappy.csv")
names(dat) <- c("Happiness", "GDP", "Family", "Life.Expect", "Freedom", "Generosity", "Trust.Gov", "Dystopia")
lm(Happiness ~ GDP, data=dat)
mod2 <- lm(Happiness ~ GDP + Freedom, data=dat)
summary(mod2)

# slope standardization
1.8736 * sd(dat$GDP) / sd(dat$Happiness)
##### Module 5, Lab 3: Between Subjects Experiments
#### LOAD PACKAGES ####
install.packages('pwr')
install.packages('psych')
install.packages('effsize')
library(ggplot2)
library(pwr)
library(psych)
library(effsize)
#### LOAD DATA ####
dat <- read.csv("datasets/logos.csv", header=T)
summary(dat)
round(
  cor(dat[,2:6])
  ,2)
#data.frame method
dat$sentiment <- rowMeans(
  data.frame(dat$friendly, dat$inviting, dat$interesting, dat$positive, dat$pleasant))

#subset method
dat$sentiment <- rowMeans(
  subset(dat, select=c("friendly", "inviting", "interesting", "positive", "pleasant"))
)

mean(dat$sentiment)

sd(dat$sentiment)

ggplot(data=dat, aes(x=sentiment))+
  geom_histogram(color="black", fill="light blue", bins=15)+
  theme_light()+
  scale_x_continuous(name="Sentiment")+
  ggtitle("Histogram of Sentiment")

psych::alpha(
  data.frame(dat$friendly, dat$inviting, dat$interesting, dat$positive, dat$pleasant)
)$total

ggplot(data=dat, aes(x=logo, y=sentiment, fill=logo))+
  geom_boxplot(alpha=.20, color="black")+
  geom_jitter(alpha=.5, color="black", fill="grey90", width=.20)+
  theme_light()+
  scale_y_continuous(name="Sentiment")+
  scale_x_discrete(name="Logo")+
  scale_fill_discrete(name="Logo")

ggplot(data=dat[!is.na(dat$logo), ], aes(x=logo, y=sentiment, fill=logo))+
  geom_boxplot(alpha=.20, color="black")+
  geom_jitter(alpha=.5, color="black", fill="grey90", width=.20)+
  theme_light()+
  scale_y_continuous(name="Sentiment")+
  scale_x_discrete(name="Logo")+
  scale_fill_discrete(name="Logo")
dat.complete = dat[complete.cases(dat),]
# Means
t(tapply(dat$friendly, dat$logo, mean, na.rm=T))

# SDs
t(tapply(dat$friendly, dat$logo, sd, na.rm=T))

#Make Mean and SD Table
tab.1 <- rbind(
  round(tapply(dat$sentiment, dat$logo, mean, na.rm=T), 2),
  round(tapply(dat$sentiment, dat$logo, sd, na.rm=T), 2)
)

tab.1 <- t(tab.1)
colnames(tab.1) <- c("M", "SD")
tab.1
dat2 <- dat[(dat$logo == "Logo A" | dat$logo=="Logo B"), ]
t.test(dat2$sentiment ~ dat2$logo)

t.test(sentiment ~ logo, data=dat2)
t.test(dat2$sentiment ~ dat2$logo, var.equal=TRUE)

ggplot(data=dat2[!is.na(dat2$logo), ], aes(x=logo, y=sentiment, fill=logo))+
  geom_boxplot(alpha=.20, color="black")+
  geom_jitter(alpha=.5, color="black", fill="grey90", width=.20)+
  theme_light()+
  scale_y_continuous(name="Sentiment")+
  scale_x_discrete(name="Logo")+
  scale_fill_discrete(name="Logo")+
  ggtitle("Sentiment: Logo A vs. Logo B")
# First, ask R for our sample sizes
table(dat2$logo)

# Feed information inot pwr.t2n.test()
pwr::pwr.t2n.test(n1=33, n2=32, power=.80)
pwr::pwr.t.test(d=.20, power=.80)
model1 <- aov(dat$sentiment ~ dat$logo)
summary(model1)
TukeyHSD(model1)

#Make Mean and SD Table
tab.1 <- rbind(
  round(tapply(dat$sentiment, dat$logo, mean, na.rm=T), 2),
  round(tapply(dat$sentiment, dat$logo, sd, na.rm=T), 2)
)

tab.1 <- t(tab.1)
colnames(tab.1) <- c("M", "SD")
tab.1
#####Module 5, Lab 4 - Factorial Designs
#### LOAD PACKAGES ####
ipk <- function(pkg) {
  new.pkg <- list.of.pkg[!(list.of.pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
list.of.pkg <- c("effsize", "psych", "ez", "phia", "tidyverse")
ipk(list.of.pkg)



#### LOAD DATA ####
dat <- read.csv("datasets/logos.csv", header=T)

#### SCORE SENTIMENT SCALE ####
dat$sentiment <- rowMeans(
  data.frame(dat$friendly, dat$inviting, dat$interesting, dat$positive, dat$pleasant)
)

#### VIEW DATA #####
head(dat)

ggplot(data=dat[!is.na(dat$logo), ], aes(x=logo, y=sentiment, fill=sex))+
  geom_boxplot(alpha=.20, color="black")+
  geom_jitter(alpha=.5, color="black", fill="grey90", width=.20)+
  theme_light()+
  scale_y_continuous(name="Sentiment")+
  scale_x_discrete(name="Logo")+
  scale_fill_discrete(name="Sex")

# Means
tapply(dat$friendly, list(dat$logo, dat$sex), mean, na.rm=T)

# SDs
tapply(dat$friendly, list(dat$logo, dat$sex), sd, na.rm=T)

#generate id variable for each subject as a factor variable
dat$idvar <- as.factor(1:nrow(dat))

ezANOVA(data=dat, 
        wid=.(idvar), 
        dv=.(sentiment), 
        between=.(sex, logo),
        type=3)

mod <- ezANOVA(data=dat, 
               wid=.(idvar), 
               dv=.(sentiment), 
               between=.(sex, logo),
               type=3,
               return_aov=TRUE)
mod$aov

TukeyHSD(mod$aov)
