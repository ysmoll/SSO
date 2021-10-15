# import dataset
trees=read.table('treeVolume.txt', header=TRUE,  colClasses = c("numeric", "numeric", "numeric", "character"))
summary(trees)
trees

# 2.a perform anova, print summary and full results
anova <- aov(volume ~ type, data = trees)
summary(anova)
predict(anova, interval='confidence')

# sanity check to see if the predict function indeed just outputs the mean (it does).
tmp = trees[trees[, "type"] == "beech",];
mean(tmp[, c(3)])
tmp = trees[trees[, "type"] == "oak",]
mean(tmp[, c(3)])


# 2.b
anova = aov(volume ~ type + diameter + height, data = trees)
summary(anova)

# split the dataset into the two tree types
beeches = trees[trees[, "type"] == "beech",]
oaks = trees[trees[, "type"] == "oak",]

# use the summary function to get the mean height and width for beech and oak
summary(beeches)
summary(oaks)

# get the mean diameter and volume for beech and oak
mean_beech = list("diameter"=13.25, "height"=76, "volume"=30.17, "type" = "beech")
mean_oak = list("diameter"=14.64, "height"=75.68, "volume"=35.25, "type" = "oak")

# predict the volume for each of the average trees
predict(anova, newdata = mean_beech)
predict(anova, newdata = mean_oak)


diameter_beech = beeches[,"diameter"]
volume_beech = beeches[, "volume"]
diameter_oak = oaks[,"diameter"]
volume_oak = oaks[, "volume"]

plot(diameter_beech, volume_beech, xlab="Diameter", ylab="Volume")
plot(diameter_oak, volume_oak, xlab="Diameter", ylab="Volume")


# 2.c
anova = aov(volume ~ type + diameter * height, data = trees)
summary(anova)
predict(anova, newdata = mean_beech)
predict(anova, newdata = mean_oak)

anova = aov(volume ~ type + diameter + height, data = trees)
summary(anova)
predict(anova, newdata = mean_beech)
predict(anova, newdata = mean_oak)

predict(anova, trees, interval="confidence" )

# Set Seed so that same sample can be reproduced in future also
set.seed(101) 
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]


