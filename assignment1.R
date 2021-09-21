setwd("~/Documents/Documents - gebruiker’s MacBook Pro/MSc Data Science/SSO/Assignment 1")
bw=read.table('birthweight.txt', header=TRUE)
weights = as.numeric(as.character(bw$birthweight))
###Ex 1 
#check normality
hist(weights)
qqnorm(weights)
shap = shapiro.test(weights)
shap


#90% confidence interval for mu
n=length(weights)
m = mean(weights); m
s = sd(weights); s
t = qt(0.95,df=n-1); t
ci = c(m-t*s/sqrt(n),m+t*s/sqrt(n))       
ci

#hypothesis testing H0 mu <= 2800
t.test(weights, mu=2800, alt='g')

###Ex 2
#point estimate for p
140/200

#99% CI for p=0.7
qnorm(0.995)*sqrt((0.7*0.3)/200)

#binomial test 2c
binom.test(140,n=200,p=0.75)
#different confidence intervals
binom.test(140,200,p=0.75, conf.level=0.99)
binom.test(140,200,p=0.75, conf.level=0.8)



-

