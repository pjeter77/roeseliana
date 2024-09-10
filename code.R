library("lme4")
library("DHARMa")
library("MuMIn")
library("performance")
library("car")
library("jtools")
library("sjPlot")
library("sjmisc")
library("ggplot2")
library("patchwork")
library("tidyverse")
library("hrbrthemes")

####################
# growth_estimate  #
####################

# years since introduction

years = c(1,2,3,4,5,6,7,8)

# census

pop022 = c(2,0,5,0,1,57,23,38)
pop020 = c(8,3,0,4,6,480,281,NA)
pop005 = c(24,0,7,284,8,2000,2138,NA)
pop314 = c(4,0,0,17,0,838,1020,NA)
pop025 = c(21,2,5,68,7,500,156,380)
pop318 = c(2,5,0,1,1,66,8,165)

data <- data.frame(years,pop022,pop020,pop005,pop314,pop025,pop318)
head(data)

m1 <- lm(pop022~years)
summary(m1)

m2 <- lm(pop020~years)
summary(m2)

m3 <- lm(pop005~years)
summary(m3)

m4 <- lm(pop314~years)
summary(m4)

m5 <- lm(pop025~years)
summary(m5)

m6 <- lm(pop318~years)
summary(m6)

####################
# data table       #
####################

dat<-read.table("data.txt", header=TRUE)
attach(dat)
pop <- factor(pop)
names(dat)

####################
# LMs              #
####################

# heterozygosity vs population growth rate

m1 <- lm(Hs_obs ~ growth_estimate * sex, data=dat) # final to ms
m1 <- lm(Hs_obs ~ growth_estimate * sex, data=dat, subset =-74) # excluding F id 74
check_model(m1)
summ(m1)
Anova(m1, type="III", icontrasts=c("contr.sum", "contr.poly"))

p1 <- plot_model(m1, type = "pred", terms = c("growth_estimate", "sex"), title = "",
      show.data =T, axis.title=c("population growth", "heterozygosity")) + 
      theme_ipsum()       

# body size vs growth rate

m2 <- lm(femur ~ growth_estimate*sex, data=dat)
check_model(m2)
summ(m2)
Anova(m2, type="III", icontrasts=c("contr.sum", "contr.poly"))

p2 <- plot_model(m2, type = "pred", terms = c("e", "sex"), title = "",
      show.data =T, jitter=0.01, axis.title=c("population growth", "body size")) +
      theme_ipsum()

####################
# LMM              #
####################

# body size vs heterozygosity

m3 <- lmer(femur ~ Hs_obs * sex+ (1 | pop_cat), data=dat) 
check_model(m3)
Anova(m3, type="III", icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(m3)
summ(m3)
summary(m3)

p3 <- plot_model(m3, type = "pred", terms = c("Hs_obs", "sex"), title = "",
      show.data =T, jitter=0.01, axis.title=c("heterozygosity", "body size")) +
      theme_ipsum()

p1 + p2 + p3



