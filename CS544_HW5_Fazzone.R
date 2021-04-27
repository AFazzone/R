library(sampling)

boston <- read.csv(
  "http://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv",
  colClasses = c("character", "character", "character", "integer", "character"))

#Part 1
#a.)
earnings <- boston$Earnings
earnings
b <- seq(40000, 400000, by = 20000)
b
bb <- length(b)
bb
hist(earnings, breaks = b, names = levels(b))

mean(earnings)
sd(earnings)







#b
aa =sample(earnings, size =10)
aa


sample1 <- replicate(5000,sample(earnings, size =10))
                    

hist(sample1, main = "Sample Size 10")

mean(sample1)
sd(sample1)     


xbar1 <- numeric(5000)

for (i in 1:5000){
  xbar1[i] <- mean(sample(aa, size = 10, replace = FALSE))
}

hist(xbar1, prob =  TRUE, main = "Sample Means Size 10")


mean(xbar1)
sd(xbar1)




#c)

sample2 <- replicate(5000,sample(earnings, size =40))


hist(sample2, main = "Sample Size 40")

mean(sample2)
sd(sample2)     


xbar2 <- numeric(5000)

for (i in 1:5000){
  xbar2[i] <- mean(sample(aa, size = 40, replace = FALSE))
}

hist(xbar2, prob =  TRUE, main = "Sample Means Size 40")


mean(xbar2)
sd(xbar2)





#d.)
means <- c(mean(earnings),mean(sample1), mean(sample2))

means

standards <- c(sd(earnings),sd(sample1),sd(sample2))

standards



#Part2
#a.)

aa <- rnbinom(5000, size=3, prob = .5)
barplot(table(aa))

#b.)

par(mfrow=c(3,2), oma = c(0, 0, 2, 0))
heights1 <- dbinom(0:5, size = 5, prob = .5)
heights1

nbsample1  <- replicate(5000, sample(aa, size=10, replace=FALSE))

nb1xba <- numeric(5000)

for (i in 1:5000){
  nb1xbar[i] <- mean(sample(aa, size = 10, replace = FALSE))
}

hist(nb1xbar, prob =  TRUE, main = "Negative Binomial Sample Size 10")

nbsample2  <- replicate(5000, sample(aa, size=20, replace=FALSE))

nb2xbar <- numeric(5000)

for (i in 1:5000){
  nb2xbar[i] <- mean(sample(aa, size = 20, replace = FALSE))
}

hist(nb2xbar, prob =  TRUE, main = "Negative Binomial Sample Size 20")


nbsample3  <- replicate(5000, sample(aa, size=30, replace=FALSE))

nb3xbar <- numeric(5000)

for (i in 1:5000){
  nb3xbar[i] <- mean(sample(aa, size = 30, replace = FALSE))
}

hist(nb3xbar, prob =  TRUE, main = "Negative Binomial Sample Size 30")



nbsample4  <- replicate(5000, sample(aa, size=40, replace=FALSE))



nb4xbar <- numeric(5000)

for (i in 1:5000){
  nb4xbar[i] <- mean(sample(aa, size = 30, replace = FALSE))
}

hist(nb4xbar, prob =  TRUE, main = "Negative Binomial Sample Size 40")





#c.)


mean(aa)
mean(nbsample1)
mean(nbsample2)
mean(nbsample3)
mean(nbsample4)


sd(aa)
sd(nbsample1)
sd(nbsample2)
sd(nbsample3)
sd(nbsample4)


#Part 3

dept <- boston$Department

table(dept)
tail(sort(table(dept)))

deptsub <-subset(boston, boston$Department %in% c("Boston Police Department", "Boston Fire Department", 
                                       "BPS Special Education", "BPS Facility Management",
                                       "Boston Public Library"))

nrow(deptsub)

#BU ID 4361

#a)

s <- srswor(50, nrow(deptsub))
s[s!=0]

rows <-(1:nrow(deptsub))[s!=0]
rows <- rep(rows, s[s!=0] )
rows

srs_sample <- deptsub[rows,]
srs_sample


#Frequency 
table(srs_sample$Department)


#Percentage Boston Fire Department 

10/50

#Percentage Boston Police Department

25/50


#Percentage  Boston Public Library  

4/50

#Percentage BPS Facility Management 


5/50

#Percentage BPS Special Education 


6/50



#b.)

N <- nrow(deptsub)
n <- 50
k = round(N/n)
k

r <- sample(k,1)
r
ss <-seq(r, by = k, length = n)
ss

ss_sample <- deptsub[ss,]
ss_sample

table(ss_sample$Department)





#Percentage Boston Fire Department 

10/50

#Percentage Boston Police Department

25/50


#Percentage  Boston Public Library  

5/50

#Percentage BPS Facility Management 


5/50

#Percentage BPS Special Education 


5/50


#c.)

pik <- inclusionprobabilities(deptsub$Earnings,50)
length(pik)
sum(pik)

sys <- UPsystematic(pik)

inc_sample <- deptsub[sys!=0,]
inc_sample

table(inc_sample$Department)




#Percentage Boston Fire Department 

14/50

#Percentage Boston Police Department

29/50


#Percentage  Boston Public Library  

2/50

#Percentage BPS Facility Management 


3/50

#Percentage BPS Special Education 


2/50


#d.)

deptsub2 <- sort(deptsub$Department)

sections <- c("Boston Police Department", "Boston Fire Department", 
              "BPS Special Education", "BPS Facility Management",
              "Boston Public Library")


st1 <- strata(deptsub2, stratnames = sections, method = "srswor")

st1


#e.)

mean(srs_sample$Earnings)
mean(ss_sample$Earnings)
mean(inc_sample$Earnings)


sd(srs_sample$Earnings)
sd(ss_sample$Earnings)
sd(inc_sample$Earnings)

