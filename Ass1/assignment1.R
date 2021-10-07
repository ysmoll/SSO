
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


###Ex 3
weather=read.table('weather.txt', header=TRUE)
weather
#summaries + qq-plot
#lens
length(weather$humidity)
length(weather$temperature)
#means
mean(weather$humidity)
mean(weather$temperature)
#sample stdv
sd(weather$humidity)
sd(weather$temperature)
#histogram
hist(weather$humidity)
hist(weather$temperature)
#scatterplot
plot(weather)
#boxplot
boxplot(weather)
#qq-plot
qqnorm(weather$humidity)
qqnorm(weather$temperature)
#correlation
cor(weather$humidity,weather$temperature)

#3c
n2 = length(weather$temperature); n2
s2 = sd(weather$temperature) ; s2
m2 = mean(weather$temperature); m2
t2 = qt(0.95,df=n2-1); t2
ci2 = c(m2-t2*s2/sqrt(n2),m2+t2*s2/sqrt(n2)) ; ci2 
  
#3d
za = qnorm(1-(0.05/2)); za
s3 = sd(weather$humidity); s3
e = 0.01; e
n3 = (za^2*s3^2)/e^2; n3

### Ex 4
austen = read.table('austen.txt', header=TRUE)
austen


# 4b
z=chisq.test(austen[, c(1,3,2)]); z


# 4c
z=chisq.test(austen); z

