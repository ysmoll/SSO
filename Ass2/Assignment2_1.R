# Ass 2: 2.1 


sat = read.table("sat.txt", header = TRUE, sep=" ", dec = ".", )

#a 

#a.1: Get lm of sat with total as response variable 
totallm = lm(total~expend+ratio+salary+takers, data=sat)
summary(totallm)

#a.2.1: Find first variable for Step Up strategy
summary(lm(total~expend,data=sat)) # R^2 = 0.1448    significant
summary(lm(total~ratio,data=sat))  # R^2 = 0.006602  not significant
summary(lm(total~salary,data=sat)) # R^2 = 0.1935    significant
summary(lm(total~takers,data=sat)) # R^2 = 0.787     significant

# we take takers as first variable

#a.2.2: Find second variable for Step Up strategy
summary(lm(total~takers+expend,data=sat))  # R^2 = 0.8195  significant
summary(lm(total~takers+ratio,data=sat))   # R^2 = 0.7991  not significant
summary(lm(total~takers+salary,data=sat))  # R^2 = 0.8056  significant

# we take expend as second variable

#a.2.3: Find third variable for Stup Up strategy
summary(lm(total~takers+expend+ratio,data=sat))   # R^2 = 0.8227 not significant
summary(lm(total~takers+expend+salary,data=sat))  # R^2 = 0.8078 not significant

# We do not take a third variable, as neither are significant.
# Resulting model:
summary(lm(total~takers+expend,data=sat))  # R^2 = 0.8195  significant
# Total = 993.8317 - 2.8509*takers + 12.2865*expend + error
# with R^2 = 0.8195 and hat(sigma) = 32.46


#a.3.1: Find first variable to remove for Step Down strategy
summary(lm(total~expend+ratio+salary+takers, data=sat))
# expend, ratio and salary are not significant, we remove expend, which has the highest p-value

#a.3.2: Find second variable to remove for Step Down strategy
summary(lm(total~ratio+salary+takers, data=sat))
#All remaining variables are significant. 

# Resulting model:
# Total = 1057.8982 - 4.6394*ratio + 2.5525*salary - 2.9134*takers + error
# With R^2 = 0.8239 and hat(sigma) = 32.41

# SECOND ONE IS BETTER: HIGHER R^2 and lower Sigma and same number of variables
# BUT FIRST ONE HAS ONLY 2 VARIABLES SO IS BETTER


#b.1: expend dataset with takers^2
sat$takers2=(sat$takers)^2
sat

# WELL HERE WE FucKING GO AGAIN

# STEP UP
#b.2.1: Find first variable for Step Up strategy
summary(lm(total~expend,data=sat))  # R^2 = 0.1448    significant
summary(lm(total~ratio,data=sat))   # R^2 = 0.006602  not significant
summary(lm(total~salary,data=sat))  # R^2 = 0.1935    significant
summary(lm(total~takers,data=sat))  # R^2 = 0.787     significant
summary(lm(total~takers2,data=sat)) # R^2 = 0.6578    significant
# we take takers as first variable

#b.2.2: Find second variable for Step Up strategy
summary(lm(total~takers+expend,data=sat))  # R^2 = 0.8195  significant
summary(lm(total~takers+ratio,data=sat))   # R^2 = 0.7991  not significant
summary(lm(total~takers+salary,data=sat))  # R^2 = 0.8056  significant
summary(lm(total~takers+takers2,data=sat)) # R^2 = 0.8732  significant
# we take takers2 as second variable

#b2.3: Find third variable for Step Up strategy
summary(lm(total~takers+takers2+expend,data=sat))  # R^2 = 0.8859  significant
summary(lm(total~takers+takers2+ratio,data=sat))   # R^2 = 0.8738  not significant
summary(lm(total~takers+takers2+salary,data=sat))  # R^2 = 0.8858  significant
# we take expend as third variable

#b2.4: Find fourth variable for Steup Up strategy
summary(lm(total~takers+takers2+expend+ratio,data=sat))  # R^2 = 0.8887  not significant
summary(lm(total~takers+takers2+expend+salary,data=sat)) # R^2 = 0.8873  not significant
# We do not take a third variable, as neither are significant.

# Resulting model:
summary(lm(total~takers+takers2+expend,data=sat))  # R^2 = 0.8859  significant
# Total = 1052 -6.381*takers + 0.04741*takers2 + 7.914*expend + error
# With R^2 = 0.8859 and hat(sigma) = 26.08


#b3.1 Find first variable to remove for Step Down strategy
summary(lm(total~expend+ratio+salary+takers+takers2, data=sat))
# expend, ratio and salary are not significant, we remove salary, which has the highest p-value

#b.3.2: Find second variable to remove for Step Down strategy
summary(lm(total~expend+ratio+takers+takers2, data=sat))
# Only ratio is not significant, so we remove ratio

#b.3.3: Find third variable to remove for Step Down strategy
summary(lm(total~expend+takers+takers2, data=sat))
#All remaining variables are significant. 

# Resulting model:
# Total = 1052 + 7.914*expend - 6.381*takers + 0.04741*takers2 + error
# With R^2 = 0.8859 and hat(sigma) = 26.08

# HOLY MOLY THEY ARE THE SAME!

#c 
# We prefer the second solution as the R^2 is much higher (0.8859 vs 0.8195),
best_model =lm(total~expend+takers+takers2, data=sat)
# even though one more variable is used
# Takers is most significant and negatively correlated. Takers squared is also 
# very significant and has a positive correlation, this is possibly to compensate for takers.
attach(sat)
exp_lm = lm(total~expend, data=sat)
plot(expend,total)
abline(exp_lm)

tak_lm = lm(total~takers)
plot(takers,total)
abline(tak_lm)

tak2_lm = lm(total~takers2)
plot(takers2,total)
abline(tak2_lm)



#d.1 add
fitted(best_model)
newxdata = data.frame(expend=5, takers=25, takers2=625)
predict(best_model,newxdata,interval="confidence",level=0.95)
