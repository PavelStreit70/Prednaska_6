library(stats)

#############
#Experimental design Power analysis, Linear models, Mixed effects models

#How many observations do we need to prove a treatment effect as significant, if we assume:

  #average value in control group is 4.5
  #average value in experimental group is 6.5
  #SD in both groups 1.5
  #normal distribution of values in both groups
  #significance level = 5%
  #power = 80% 

#power.t.test(n = NULL, delta = 2.0, sd = 1.5, sig.level = 0.05, power = 0.8,
             #type = c("one.sample"), alternative = c("two.sided"))

power.t.test(n = NULL, delta = 2.0, sd = 1.5, sig.level = 0.05, power = 0.8,
             type = c("two.sample"), alternative = c("two.sided"))

#Vyzkoušení funkce rnorm - generuje to random čísla, pak to vrazíme do funkce

rnorm(10, mean = 4.5, sd = 1.5)

df <- data.frame(
  value = c(rnorm(n = 10, mean = 4.5, sd = 1.5), rnorm(10, mean = 6.5, sd = 1.5)),
  group = rep(1:2, each = 10))
View(df)

t.test(value ~ group, data = df, var.equal = TRUE)
t.test(value ~ group, data = df)

#p-hodnota může být rozdílná, nebude vždy statisticky významné
#set.seed nám zajístí stejný výsledek, takže:

set.seed(123)
df <- data.frame(
  value = c(rnorm(n = 10, mean = 4.5, sd = 1.5), rnorm(10, mean = 6.5, sd = 1.5)),
  group = rep(1:2, each = 10))

#Zacyklení, spustíme to vícekrát a nemusíme to dělat ručně:
#Nicméně kód z nějakého důvodu nefunguje - opraveno ChatGPT, ten jede a je níže:

p_val <- NULL
for(i in 1:10) {
  df <- data.frame(
    value = c(rnorm(10, mean = 4.5, sd = 1.5), rnorm(10, mean = 6.5, sd = 1.5)),
    group = rep(1:2, each = 10)
  )
  t <- t.test(value ~ group, data = df)
  p_val[i] <- t$p.value
}
p_val

summary(p_val)

#ChatGPT - funguje!

# Vytvoříme vektor o délce 10, vyzkoušet přepsat "for" z 1:10 na 1:1000!

p_val <- numeric(10)  

for(i in 1:1000) {
  df <- data.frame(
    value = c(rnorm(10, mean = 4.5, sd = 1.5), rnorm(10, mean = 6.5, sd = 1.5)),
    group = rep(1:2, each = 10))
  t <- t.test(value ~ group, data = df)
  p_val[i] <- t$p.value  
}

# Výpis výsledků

print(p_val)  
sum(p_val<0.05)
mean(p_val)

#Změna, použití Wilcoxona, srovnat rozdíl:

p_val <- numeric(10)  

for(i in 1:1000) {
  df <- data.frame(
    value = c(rnorm(10, mean = 4.5, sd = 1.5), rnorm(10, mean = 6.5, sd = 1.5)),
    group = rep(1:2, each = 10))
  t <- wilcox.test(value ~ group, data = df)
  p_val[i] <- t$p.value  
}

# Výpis výsledků

print(p_val)  
sum(p_val<0.05)
mean(p_val)


##############

library(readxl)
library(dplyr)
library(tidyr)
library(lme4)# lmer
library(lmerTest)
library(multcomp) # Attaching package: ‘MASS’, dplyr::select masked
library(emmeans) # emmeans
library(effects) # allEffects
library(ggplot2)
library(nortest)  # normality testing 
library(car)   # Levene test

library(glht) #nefunguje to! - není to knihovna - je schována v "multcomp"
library(remotes)



library(readxl)
M <- read_excel("modely_data.xlsx", sheet = "MV")
View(M)

M <- M %>%
  pivot_longer(MV0:MV48, names_to = "Time", values_to = "MV") %>%
  mutate(Time = factor(gsub("MV", "", Time), levels = c(0,6,12,24,48)),
         Replicate = factor(Replicate))

boxplot(MV ~ Time, M)

M <- M %>% mutate(MV_log = log10(MV))

m1 <- lm(MV_log ~ Time, M)
summary(m1)
anova(m1)
summary(glht(m1))
emmeans(m1, pairwise ~ Time)


########

#Verze 2

m2 <- lmer(MV_log ~ Time + (1|Replicate), M)
anova(m2)
summary(m2)
emmeans(m2, pairwise ~ Time)

#m2 <- lmer(MV_log ~ Time + (1|Replicate), M)
#library(glht)
#summary(glht(m2)) # comparison with reference category
#library(emmeans)
#emmeans(m2, pairwise ~ Time) # comparison of all pairs
#emmeans(m2, consec ~ Time) # comparison of consecutive pairs

########

#Verze 3

M <- M %>% 
  mutate(Time_num = gsub("MV","",Time) %>% 
           as.numeric())

m3 <- lmer(MV_log ~ Time_num + (1|Replicate), M)
summary(m3)
plot(MV_log ~ Time_num,M)

#Intercept - hodnota na baseline, hodnota říká její sklon MV_log: 0.82+0.03
#Další vzájmená porovnání nemají v tomto případě význam

#Zde ještě vizualizace:

plot(allEffects(m3))
plot(allEffects(m3, resid = TRUE))
plot(allEffects(m3), multiline=TRUE, confint=TRUE, ci.style="bars")

ggplot(M, aes(x = Time_num, y = MV_log, colour = Replicate)) +
  geom_point(size=3) +
  theme(legend.position = "none") +
  geom_line(aes(y = predict(m3)), size=.25) 



