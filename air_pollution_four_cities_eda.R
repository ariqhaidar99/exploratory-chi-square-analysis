options(scipen=999)
list.files()
d1 <- read.csv("4 cities air pollution.csv" , stringsAsFactors = T)
# Final prep b4 analysis
View(d1)
str(d1)
#Check data inputs
is.na(d1)
#Check data inputs (inverted)
!is.na(d1)
# 2x2 plotting plane
par(mfrow=c(2, 2))
# Air Quality analysis of the 4 cities
# Histogram Per City
hist(d1$London, col = "red", xlab="Pollution Level", ylab="Tally", main="London")
hist(d1$Beijing, col = "green", xlab="Pollution Level", ylab="Tally", main="Beijing")
hist(d1$Moscow, col = "blue", xlab="Pollution Level", ylab="Tally", main="Moscow")
hist(d1$LosAngeles, col = "purple", xlab="Pollution Level", ylab="Tally", main="Los Angeles")
# GGPLOT version
ggplot(d1, aes (x= Beijing)) + geom_histogram(binwidth = 20) +
  scale_x_continuous(breaks = scales::breaks_width(10)) +theme_classic() +
  labs(x = "Air Pollution (PPM) in Beijing", y = "Tally")
ggplot(d1, aes (x= LosAngeles)) + geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = scales::breaks_width(5)) +theme_classic() +
  labs(x = "Air Pollution (PPM) in Los Angeles", y = "Tally") + 
  scale_y_continuous(breaks = scales::breaks_width(2))
# n.arm 
summary(d1, n.arm=T)
# Range
max(d1$Beijing, na.rm = T) - min(d1$Beijing, na.rm = T)
# Standard Deviation
sd(d1$Beijing, na.rm = T)
# InterQuartile Range
IQR(d1$Beijing, na.rm = T)
# 1x1 Box and whisker plot of all
par(mfrow=c(1, 1))
boxplot(d1, col=rainbow (4), xlab="City", ylab="PollutionLevel", 
        main="Air Pollution Level in 4 Big Cities")
max(d1$London, na.rm = T) - min(d1$London, na.rm = T)
max(d1$LosAngeles, na.rm = T) - min(d1$LosAngeles, na.rm = T)
max(d1$Moscow, na.rm = T) - min(d1$Moscow, na.rm = T)