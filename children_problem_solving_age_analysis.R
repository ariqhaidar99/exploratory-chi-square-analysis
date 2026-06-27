options(scipen=999)
d2 <- read.csv( "child problem solving analysis.csv", stringsAsFactors = T)
# Final prep b4 analysis
View(d2)
str(d2)
# 1x1 plotting plane
par(mfrow=c(1, 1))
#main plot v1
#pch = point symbol
#age on x-axis, problem solving on y-axis
plot(d2$Age, d2$ProblemSolve, col="red", pch = 19)
# main plot v2
library(ggplot2)
ggplot(d2, aes(x=Age, y=ProblemSolve)) + geom_point() + geom_smooth(method=lm) +theme_classic()
modelproblemsolve <- with(d2, lm(ProblemSolve ~ Age))
summary(modelproblemsolve)
#each age increase of 1 year, problem solving will increase by 1.96795 (1.968) in theory
#creates new category, assigned to new variable in d2 called "AgeCat"
d2$AgeCat <- cut(d2$Age, c(0, 6.9, 8.9, 11.2), label = c("P1-2", "P3-4", "P5-7"), ordered_results=T)
View(d2)
boxplot(d2$ProblemSolve~d2$AgeCat, col = 5, xlab="Age Category", ylab="Problem Solving Ability Score")
summary(d2, n.arm=T)
tapply(d2$ProblemSolve, d2$AgeCat, summary)