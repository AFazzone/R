#Part 1

#a.)


par(mfrow=c(3,2), oma = c(0, 0, 2, 0))
heights1 <- dbinom(0:5, size = 5, prob = .5)
heights1

# PMF Plot for p = 0.5
heights1 <- dbinom(0:5, size = 5, prob = .5)
plot(0:5, heights1, type ="h",
     main = "Spike plote of .5", xlab ='x', ylab = "PMF")
points(0:5, heights1, pch = 16)

# CDF Plot for p = 0.5

cdf <- c(0, cumsum(heights1))
cdfplot <- stepfun(0:5, cdf)
plot(cdfplot, pch = 16,
     main = "Step Plot .5", xlab = "x", ylab = "CDF")



#b.)

#Exactly 2 at .5
dbinom(2, size =5, prob = .5)

x <- c(0,1,2,3,4,5)
f <- c(1/32, 5/32, 10/32, 10/32, 5/32, 1/32)

# values  A = perfect, B = not Perfect

AABBB = .5 *.5 *.5*.5*.5
BAABB = .5 *.5 *.5*.5*.5
BBAAB = .5 *.5 *.5*.5*.5
BBBAA = .5 *.5 *.5*.5*.5
ABABB = .5 *.5 *.5*.5*.5
BABAB = .5 *.5 *.5*.5*.5
BBABA = .5 *.5 *.5*.5*.5
ABBAB = .5 *.5 *.5*.5*.5
BABBA = .5 *.5 *.5*.5*.5
ABBBA = .5 *.5 *.5*.5*.5


explicit1 <- 10*((.5)^2)*((.5)^3)
explicit1 

#c.)
#At least 2 at .5

sum(dbinom(2:5, size =5, prob = .5))

#d.)
# PMF Plot for p = 0.75
heights2 <- dbinom(0:5, size = 5, prob = .75)
plot(0:5, heights2, type ="h",
     main = "Spike plote of .75", xlab ='x', ylab = "PMF")
points(0:5, heights2, pch = 16)


# CDF Plot for p = 0.75

cdf <- c(0, cumsum(heights2))
cdfplot <- stepfun(0:5, cdf)
plot(cdfplot, pch = 16,
     main = "Step Plot .75", xlab = "x", ylab = "CDF")

explicit2 <- 10*((.75)^2)*((.25)^3)
explicit2 



#Exactly 2 at .75
dbinom(2, size =5, prob = .75)

#At least 2 at .75
sum(dbinom(2:5, size =5, prob = .75))



#e.)

# PMF Plot for p = 0.25
heights3 <- dbinom(0:5, size = 5, prob = .25)
plot(0:5, heights3, type ="h",
     main = "Spike plote of .25", xlab ='x', ylab = "PMF")
points(0:5, heights3, pch = 16)



# CDF Plot for p = 0.25

cdf <- c(0, cumsum(heights3))
cdfplot <- stepfun(0:5, cdf)
plot(cdfplot, pch = 16,
     main = "Step Plot .5", xlab = "x", ylab = "CDF")

#Exactly 2 at .25
dbinom(2, size =5, prob = .25)

explicit1 <- 10*((.25)^2)*((.75)^3)
explicit1 

#At least 2 at .25
dbinom(2, size =5, prob = .25)+
dbinom(3, size =5, prob = .25)+
dbinom(4, size =5, prob = .25)+
dbinom(5, size =5, prob = .25)



#g
aa <- rbinom(1000, size=5, prob = .5)
table(aa)

barplot (table(aa), main ="Probability of .5", xlab = c(".5", ".75", ".25"))
         
         
bb <- rbinom(1000, size=5, prob = .75)
table(bb)

barplot (table(bb), main ="Probability of .75", xlab = "Number of perfect scores")

cc <- rbinom(1000, size=5, prob = .25)
table(cc)

barplot (table(cc), main ="Probability of .25", xlab = "Number of perfect scores")


barplot (cbind(table(aa),table(bb),table(cc)), beside=TRUE, 
         main ="Probabilities",col=c("aquamarine3","coral","red", "blue","yellow","black"),
         names.arg=c(".5",".75",".25"))

legend("topleft", c("0","1","2","3","4","5"), pch=15, 
       col=c("aquamarine3","coral","red", "blue","yellow","black"), 
       bty="n")


#Part 2
#a)
# 10 total tries means up to 7 failures
# PMF Plot for p = 0.5
heights1 <- dnbinom(0:7, size = 3, prob = .5)
heights1
plot(0:7, heights1, type ="h",
     main = "Spike plote of .5", xlab ='x', ylab = "PMF")
points(0:7, heights1, pch = 16)

#CDF Plot for p = 0.5
cdf <- c(0, cumsum(heights1))
cdfplot <- stepfun(0:7, cdf)
plot(cdfplot, pch = 16,
     main = "Step Plot .5", xlab = "x", ylab = "CDF")



#b) 3 perfect, 4 failures
dnbinom(4, size = 3, prob = .5)

ffffsss = .5*.5*.5*.5*.5*.5*.5
fffssfs = .5*.5*.5*.5*.5*.5*.5
ffssffs = .5*.5*.5*.5*.5*.5*.5
fssfffs = .5*.5*.5*.5*.5*.5*.5
ssffffs = .5*.5*.5*.5*.5*.5*.5
fffsfss = .5*.5*.5*.5*.5*.5*.5
ffsffss = .5*.5*.5*.5*.5*.5*.5
fsfffss = .5*.5*.5*.5*.5*.5*.5
sffffss = .5*.5*.5*.5*.5*.5*.5
ffsfsfs = .5*.5*.5*.5*.5*.5*.5
fsfsffs = .5*.5*.5*.5*.5*.5*.5
sfsfffs = .5*.5*.5*.5*.5*.5*.5
fsffsfs = .5*.5*.5*.5*.5*.5*.5
sfffsfs = .5*.5*.5*.5*.5*.5*.5
sffsffs = .5*.5*.5*.5*.5*.5*.5


explicit <- 15*((.5)^4)*((.5)^3)
explicit


#c.)
#3 perfect with at most 4 failures

sum(dnbinom(0:4, size =3, prob = .5))

#d)
# 10 total tries means up to 7 failures
# PMF Plot for p = 0.75
heights2 <- dnbinom(0:7, size = 3, prob = .75)
heights2
plot(0:7, heights2, type ="h",
     main = "Spike plote of .75", xlab ='x', ylab = "PMF")
points(0:7, heights2, pch = 16)

# CDF Plot for p = 0.75
cdf <- c(0, cumsum(heights2))
cdfplot <- stepfun(0:7, cdf)
plot(cdfplot, pch = 16,
     main = "Step Plot .75", xlab = "x", ylab = "CDF")

#e
# 10 total tries means up to 7 failures
# PMF Plot for p = 0.25
heights3 <- dnbinom(0:7, size = 3, prob = .25)
heights3
plot(0:7, heights3, type ="h",
     main = "Spike plote of .25", xlab ='x', ylab = "PMF")
points(0:7, heights3, pch = 16)

# CDF Plot for p = 0.75
cdf <- c(0, cumsum(heights3))
cdfplot <- stepfun(0:7, cdf)
plot(cdfplot, pch = 16,
     main = "Step Plot .25", xlab = "x", ylab = "CDF")

#f.)
aa <- rnbinom(1000, size=3, prob = .5)
table(aa)
barplot (table(aa), xlim = c(0,8), main ="Probability of .5")

bb <- rnbinom(1000, size=5, prob = .75)
table(bb)
barplot (table(bb), xlim = c(0,8), main ="Probability of .75")


cc <- rnbinom(1000, size=5, prob = .25)
table(cc)
barplot (table(cc), xlim = c(0,8), main ="Probability of .25")



#Part 3
#a.)
pmf <- dhyper(0:20, m = 60, n = 40, k=20)
pmf
plot(0:20, pmf, type ="h",
     main = "20 questions out of 60 multiple/40 programming",
     xlab ='x', ylab = "PMF")
points(0:20, pmf, pch = 16)

cdf <- phyper(0:20, m = 60, n = 40, k = 20)


#b.)
dhyper(10, m= 60, n = 40, k =20)
explicit <- (2^20)*((.6)^10)*((.4)^10)
explicit 


#c.)
sum(dhyper(10:20, m = 60, n = 40, k = 20))

#d.)
pmf <- dhyper(0:20, m = 40, n = 60, k =20)
pmf
plot(0:20, pmf, type ="h",
     main = "20 questions out of 40 multiple/60 programming", xlab ='x', ylab = "PMF")
points(0:20, pmf, pch = 16)

#exactly 10
dhyper(10, 40, 60, 20)
explicit <- (2^20)*((.6)^10)*((.4)^10)
explicit 

#at least 10
sum(dhyper(10:20, m = 40, n = 60, k = 20))

#e.)
pmf <- dhyper(0:20, m = 20, n =80, k= 20)
pmf
plot(0:20, pmf, type ="h",
     main = "20 questions out of 20 multiple/80 programming", xlab ='x', ylab = "PMF")
points(0:20, pmf, pch = 16)

#exactly 10
dhyper(10, m= 20, n = 80, k = 20)
explicit <- (2^20)*((.2)^10)*((.8)^10)
explicit 

#at least 10
sum(dhyper(10:20, m = 20, n = 80, k =20))

#f.)
aa = rhyper(1000,m = 60, n = 40, k = 20 )
table(aa)
barplot (table(aa), main ="Questions from 60 Multiple/40 Programming", xlab = "Multiple Choice")

bb = rhyper(1000,m = 40, n = 60, k = 20 )
table(bb)
barplot (table(bb), main ="Questions from 40 Multiple/60 Programming", xlab = "Multiple Choice")


cc = rhyper(1000,m = 20, n = 80, k = 20 )
table(cc)
barplot (table(cc), main ="Questions from 20 Multiple/80 Programming", xlab = "Multiple Choice")




#Part 4

#a.)
dpois(8, lambda = 10)

#b.)
ppois(8, lambda = 10)

#c.)
ppois(12, lambda =10) - ppois(6, lambda = 10)

#d.)
pmf = dpois(0:20, lambda = 10)
pmf
plot(0:20, pmf, type ="h",
     main = "First 20 questions", xlab ='x', ylab = "PMF")
points(0:20, pmf, pch = 16)


#e.)
aba <- rpois(50, lambda = 10)
aba
table(rpois(50, lambda = 10))

barplot (table(rpois(50, lambda = 10)), main ="Frequecy of questions over 50 days")
boxplot(aba)

#Part 5
#a.)
x <- seq(70,130)
x
pdf <- dnorm(x, mean = 100, sd = 10)
pdf

plot(x, pdf, type="l", col="red", 
     main="Souvineirs Spending", xlab="Dollars", ylab="PDF")

#b.)
1 -pnorm(120, 100, 10)

#c.)
pnorm(90, 100, 10)-pnorm(80, 100, 10)

#d)
#One Standard Deviation  90 - 110
pnorm(110, 100, 10)-pnorm(90, 100, 10)

#Two Standard Deviations  80 - 120
pnorm(120, 100, 10)-pnorm(80, 100, 10)

#Three Standard Deviations  70 - 130
pnorm(130, 100, 10)-pnorm(70, 100, 10)

#e.)
#Middle 90% between 5% - 95%
qnorm(.05, 100, 10)
qnorm(.95, 100, 10)


#f.)
# Above 95%, so 95.1%
qnorm(.951, 100, 10)

#g.)
visitors = runif(10000, min = 70, max = 130 )
boxplot(visitors, main = "Visitors Souviner Spending")

