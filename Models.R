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
