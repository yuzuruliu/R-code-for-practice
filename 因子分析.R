install.packages("psych")

data <- read.csv('C:/Users/windows/Desktop/100.csv', header = T)
fa(data)
library(psych)  
fa.parallel(data[, -1],n.obs=289,fa="both",n.iter=100, main="Scree plots with parallel analysis")  
library(' GPArotation')
fa.promax <- fa(data, nfactors = 6, rotate = "promax", fm = "pa")
fa.promax
factor.plot(fa.promax, labels = rownames(fa.promax$loadings))

