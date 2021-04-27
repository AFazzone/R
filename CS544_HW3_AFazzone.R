#Part 1

data <- read.csv("http://people.bu.edu/kalathur/datasets/myPrimes.csv")

#a.)

head(data)
tail(data)

table(data$LastDigit)
barplot(table(data$LastDigit),
        col = "blue", ylim=c(0,310),
        xlab = "Last Digit", ylab = " Frequency")
#b.)


table(data$FirstDigit)
barplot(table(data$FirstDigit),
        col = "green", ylim=c(0,310),
        xlab = "First Digit", ylab = " Frequency")


#Part 2

#a.)

us_quarters <- read.csv("http://people.bu.edu/kalathur/datasets/us_quarters.csv")

head(us_quarters)

d_max <- max(us_quarters$DenverMint)
d_min <- min(us_quarters$DenverMint)
p_max <- max(us_quarters$PhillyMint)
p_min <- min(us_quarters$PhillyMint)

print("Max Denver")
us_quarters[us_quarters$DenverMint == d_max, c(1,2)]
print("Min Denver")
us_quarters[us_quarters$DenverMint == d_min, c(1,2)]
print("Max Philly")
us_quarters[us_quarters$PhillyMint == p_max, c(1,3)]
print("Min Philly")
us_quarters[us_quarters$PhillyMint == p_min, c(1,3)]


#b.)
d_sum <-sum(us_quarters$DenverMint)
p_sum <-sum(us_quarters$PhillyMint)


paste("The value of the quarters in dollars is $",
     (d_sum + p_sum)/4)

#c.)

barplot(us_quarters$PhillyMint,
        us_quarters$DenverMint,
        col = c('red', 'blue'), beside = TRUE, 
        legend.text = TRUE, ylim=c(0,900000),
        names.arg = us_quarters$State)

legend("topright", legend = c("Denver", "Philly"), 
       fill = c("red", "blue"), horiz = TRUE, cex=0.8)


#d.)

dotchart(us_quarters$PhillyMint,us_quarters$DenverMint,
         col = c("green", "purple"), 
         labels = us_quarters$State)


#e.)

boxplot(us_quarters[2:3], col = rainbow(2))


#f.)
#Philly 5 number
f1 <- fivenum(us_quarters$PhillyMint)
f1


philly_max = (f1[4]+ (1.5*(f1[4]-f1[2])))
philly_max
#Philly Upper outliers
subset(us_quarters[us_quarters$PhillyMint > philly_max, c(1,3)])

#Philly Lower outliers
philly_min = (f1[2] - (1.5*(f1[4]-f1[2])))
subset(us_quarters[us_quarters$PhillyMint < philly_min, c(1,3)])

#Denver 5 number
f2 <-fivenum(us_quarters$DenverMint)
f2

Denver_max = (f2[4] + (1.5*(f2[4]-f2[2])))
Denver_max

#Denver Upper outliers
subset(us_quarters[us_quarters$DenverMint > Denver_max, c(1,2)])

#Denver Lower outliers
Denver_min = (f2[2] - (1.5*(f2[4]-f2[2])))
subset(us_quarters[us_quarters$DenverMint < Denver_min, c(1,3)])



#Part 3
stocks <- read.csv("http://people.bu.edu/kalathur/datasets/faang.csv")
head(stocks)
#a.)
pairs(stocks[, 2:6])

#b.) 
cor(stocks[, 2:6])


#Part 4
#a.)
scores <- read.csv("http://people.bu.edu/kalathur/datasets/scores.csv")

head(scores)

hist(scores$Score, xlab = "score", 
                    ylab = "# of students",
                    main = "Histogram of scores")

scores_hist <- hist(scores$Score)

paste(scores_hist$count[1],"Students in range (",
      scores_hist$breaks[1],scores_hist$breaks[2], "]")


paste(scores_hist$count[2],"Students in range (",
      scores_hist$breaks[2],scores_hist$breaks[3], "]")


paste(scores_hist$count[3],"Students in range (",
      scores_hist$breaks[3],scores_hist$breaks[4], "]")


paste(scores_hist$count[4],"Students in range (",
      scores_hist$breaks[4],scores_hist$breaks[5], "]")


paste(scores_hist$count[5],"Students in range (",
      scores_hist$breaks[5],scores_hist$breaks[6], "]")


paste(scores_hist$count[6],"Students in range (",
      scores_hist$breaks[6],scores_hist$breaks[7], "]")

paste(scores_hist$count[7],"Students in range (",
      scores_hist$breaks[7],scores_hist$breaks[8], "]")

paste(scores_hist$count[8],"Students in range (",
      scores_hist$breaks[8],scores_hist$breaks[9], "]")

paste(scores_hist$count[9],"Students in range (",
      scores_hist$breaks[9],scores_hist$breaks[10], "]")

paste(scores_hist$count[10],"Students in range (",
      scores_hist$breaks[10],scores_hist$breaks[11], "]")

#b.)
x2 <-hist(scores$Score, breaks = c(30, 50, 70, 90))
plot(x2)

x2$counts

paste(x2$count[1],"Students in C grade range (",
      x2$breaks[1],x2$breaks[2], "]")

paste(x2$count[2],"Students in B grade range (",
      x2$breaks[2],x2$breaks[3], "]")

paste(x2$count[3],"Students in A grade range (",
      x2$breaks[3],x2$breaks[4], "]")

