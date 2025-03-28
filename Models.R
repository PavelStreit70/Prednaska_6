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

