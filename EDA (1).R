a <- c(1,2,3,100)
mean(a)
median(a)

# Assignment Operator alt - or = 
#x = read.csv("E:\\Datasets\\Datasets_BA 2\\mba.csv")
mba <- read.csv("C:/Users/ROHIT/anaconda3/projects/mba.csv")
# Path needs to be modified to either / or \\
# mba <- read.csv(file.choose())
#Measures of Central Tendency
mean(mba$gmat)
median(mba$gmat)


#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(mba$gmat)

# Another way to find the mode:modeest package
# dependency packages are genefilter and BiocManager
install.packages("BiocManager")
library("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
BiocManager::install("modeest")
library(modeest)
mlv(mba$gmat,method = "mfv")
?mlv

#Measures of Dispersion
var(mba$gmat)
sd(mba$gmat)
range(mba$gmat)
rangevalue <- function(x){max(x)-min(x)}
rangevalue(mba$gmat)

#Measures of skewness
install.packages("moments")
library(moments)

#Measures of skewness
skewness(mba$gmat)


#Measures of Kurtosis 
kurtosis(mba$gmat)

#Graphical R,epresentation
#Boxplot
#Histogram
#Barplot

x = boxplot(mba$gmat,horizontal = FALSE)
x$out
hist(mba$gmat)
barplot(mba$gmat)
library(moments)
skewness(mba$gmat)
data = c(15,24,38,54)
names = c("one","two","three","four")
pie(data,labels = names,radius =1 )


str(mba)
x = c(1,1,1,2,1,2,1,2,1,2,1,2,1,2,1)
y = as.factor(x)
str(y)
mba$datasrno <- as.factor(mba$datasrno)

#install.packages(psych)
library(psych)
describe(mba)
?describe

pnorm(740,711,29)-pnorm(697,711,29)

# to calculate Z score
qnorm(0.950)#90%
qnorm(0.975)#95%
qnorm(0.995)

#to calculate t score
qt(0.950,139)#0.90%

#qqplot
qqnorm(mba$gmat)
qqline(mba$gmat)

# Standardisation
summary(mba)
standard_values = scale(mba)
summary(standard_values)
summary(mba)

normalize = function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

normalized_data = as.data.frame(lapply(mba,normalize))
summary(normalized_data)
# transformations of data

qqnorm(mba$workex)
qqline(mba$workex)

x = log(mba$workex)
qqnorm(x)
qqline(x)

hist(mba$workex)
hist(x)


df <- read.table(text =
                   "x y
4     71.88
20    65.80
40    63.92
60    63.47", header = T);

library(ggplot2)
ggplot(df, aes(as.factor(y), x)) + 
  geom_point() + 
  labs(y = "Percentage correct", x = "Categorical variable")
?ggplot
# geom_jitter
plot(df$x,df$y)




















