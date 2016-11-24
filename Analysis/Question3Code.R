head(Orange)

aggregate(Orange[3], list(Orange$Tree), mean)
aggregate(Orange[3], list(Orange$Tree), median)

plot(Orange$age, Orange$circumference, pch=c(16,17,18,14,15)[as.numeric(Orange$Tree)], main='Circumference of Tree by age'
      , col=c('red','blue','green', 'purple', 'black')[as.numeric(Orange$Tree)],xlab = 'Tree Age(days since 12/31/1968)', ylab = 'Circumference(mm)')


##boxplots
boxplot(Orange$circumference~Orange$Tree, main='Tree Circumference by Tree', xlab='Tree Number', ylab='Circumference(mm)')
