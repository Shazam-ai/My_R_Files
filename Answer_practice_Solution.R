library(MASS)
summary(birthwt)
#I want to look at my data to see if it is homoscedastic and if I 
#can use one line to fit all or use quantile regressions
plot(birthwt$age, birthwt$bwt) # in my opinion, it is not homoscedastic
my_linear <- lm(bwt~age, data=birthwt)
summary(my_linear)
Call:
  lm(formula = bwt ~ age, data = birthwt)
library(quantreg)
#fitting a quantile regression for 20th quantile

my_quant <- rq(bwt~age, data=birthwt, tau=0.2)
summary(my_quant)
Call: rq(formula = bwt ~ age, tau = 0.2, data = birthwt)
#fitting a quantile regression for 50th quantile
my_quant <- rq(bwt~age, data=birthwt, tau=0.5)
summary(my_quant)
Call: rq(formula = bwt ~ age, tau = 0.5, data = birthwt)
#fitting a quantile regression for 80th quantile
my_quant <- rq(bwt~age, data=birthwt, tau=0.8)
summary(my_quant)

Call: rq(formula = bwt ~ age, tau = 0.8, data = birthwt)



