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
library("psych")
library("pastecs")
library("Hmisc")
library("PerformanceAnalytics")

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
# pop gen means    #
####################

means <-read.table("data1.txt", sep=",", header=TRUE)
means <- means[, c(2:8)]

m1 <- lm(means$growth~means$HO)
summary(m1)

m2 <- lm(means$growth~means$HE)
summary(m2)

m3 <- lm(means$growth~means$Pi)
summary(m3)

m4 <- lm(means$growth~means$FIS)
summary(m4)

m5 <- lm(means$growth~means$Ne)
summary(m5)

m6 <- lm(means$growth~means$TajimaD)
summary(m6)


####################
# data table       #
####################

dat<-read.table("data2.txt", sep=",", header=TRUE)
attach(dat)
pop <- factor(pop)
names(dat)

traits <- dat[, c(6:9)]
chart.Correlation(traits, histogram=F, pch=19)
indices_HQ <- dat[, c(10:14)]
chart.Correlation(indices_HQ, histogram=F, pch=19)
indices_LQ <- dat[, c(15:19)]
chart.Correlation(indices_LQ, histogram=F, pch=19)

########################################
# Models HQ dataset                    #
########################################

####################
# LMM              #
####################

# body size vs heterozygosity

m1 <- lmer(femur ~ Hs_obs_HQ * sex + (1 | pop), data=dat) 
check_model(m1)
Anova(m1, type="III", icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(m1)
summ(m1)
summary(m1)

p1 <- plot_model(m1, type = "pred", terms = c("Hs_obs_HQ", "sex"), title = "",
        show.data =T, colors = c("#ff00ff","#0055d4"), axis.title=c("heterozygosity", "body size")) +
        coord_cartesian(xlim = c(0.6,1.2), ylim = c(14,19)) +
        theme_ipsum()

p1h <- ggplot(dat, aes(Hs_obs_HQ,sex)) + geom_boxplot() + xlim(0.6,1.2)
p1b <- ggplot(dat, aes(sex,femur)) + geom_boxplot() + ylim(14,19)

####################
# LMs              #
####################

# population growth rate vs heterozygosity

m2 <- lm(growth_estimate ~ Hs_obs_HQ * sex, data=dat)
check_model(m2)
summ(m2)
Anova(m2, type="III", icontrasts=c("contr.sum", "contr.poly"))

p2 <- plot_model(m2, type = "pred", terms = c("Hs_obs_HQ", "sex"), title = "", 
      show.data =T, colors = c("#0055d4","#ff00ff"), axis.title=c("heterozygosity","population growth")) + 
      coord_cartesian(xlim = c(0.6,1.2), ylim = c(0,400)) +    
      theme_ipsum()

# population growth rate vs body size

m3 <- lm(growth_estimate ~ femur*sex, data=dat)
check_model(m3)
summ(m3)
Anova(m3, type="III", icontrasts=c("contr.sum", "contr.poly"))

p3 <- plot_model(m3, type = "pred", terms = c("femur", "sex"), title = "",
      show.data =T, axis.title=c("body size","population growth")) +
      theme_ipsum() 

(p1h / p1) | (p1b / p2)

########################################
# Models LQ dataset                    #
########################################

####################
# LMM              #
####################

# body size vs heterozygosity

m1 <- lmer(femur ~ Hs_obs_LQ * sex + (1 | pop), data=dat) 
check_model(m1)
Anova(m1, type="III", icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(m1)
summ(m1)
summary(m1)

p1 <- plot_model(m1, type = "pred", terms = c("Hs_obs_HQ", "sex"), title = "",
      show.data =T, axis.title=c("heterozygosity", "body size")) +
      theme_ipsum()

####################
# LM              #
####################

# population growth rate vs heterozygosity

m2 <- lm(growth_estimate ~ Hs_obs_LQ * sex, data=dat)
check_model(m2)
summ(m2)
Anova(m2, type="III", icontrasts=c("contr.sum", "contr.poly"))

p2 <- plot_model(m2, type = "pred", terms = c("Hs_obs_HQ", "sex"), title = "",
      show.data =T, axis.title=c("heterozygosity","population growth")) +
      theme_ipsum()

p1 + p2

