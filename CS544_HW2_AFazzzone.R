

#Part 1 
#a.)

over_30 <- (1062+ 1710 + 656 + 189)/ 10000
over_30

#b. #c. #d. #e

group1 <- 4250/10000
group2 <- 2850/10000
group3 <- 1640/10000
group4 <- 1260/10000

group1
group2
group3
group4

group1_bmi <- 1062/4250
group2_bmi <- 1710/2850
group3_bmi <- 656/1640
group4_bmi <- 189/1260

group1_bmi
group2_bmi
group3_bmi
group4_bmi



bayes <- function (prior, likelihood, event){
  numerators <- prior * likelihood
  return(numerators/event)}


#b
bayes(group1, group1_bmi,over_30)

#c
bayes(group2, group2_bmi,over_30)

#d
bayes(group3, group3_bmi,over_30)

#e
bayes(group4, group4_bmi,over_30)


#Part 2
#a.)  
S <- rolldie(3, makespace = TRUE)
d1 <-subset(S, X1 + X2 + X3 >10)
d1
sum(d1$probs)
  
#b.)
d2 <- subset(S, X1 == X2 & X2 == X3)
d2
sum(d2$probs)

#c.)
d3 <-subset(S, (X1 == X2 & X2 != X3)|(X2 == X3 & X2 != X1)
            | (X1 == X3 & X3 != X2))
d3
sum(d3$probs)

#d.)
d4 <- subset(S, X1 != X2 & X2 != X3 & X1 != X3)
d4
sum(d4$probs)

#e.)

d5 <-subset(S, (X1 == X2 & X2 != X3)|(X2 == X3 & X2 != X1)
            | (X1 == X3 & X3 != X2) 
            & sum(X1,X2,X3)> 10)
d5
sum(d5$probs)

#Part 3
#With loop

sum_of_first_n_odd_squares <- function (n){
  m = seq(n*2)
  a = 0
  for (i in m){
    if (i %% 2 != 0)
    {a = a + i**2 }
  }
  return(a)
}
sum_of_first_n_odd_squares(2)
sum_of_first_n_odd_squares(5)
sum_of_first_n_odd_squares(10)

#Without Loop

sum_of_first_n_odd_squares_V2 <- function (n){
  n = seq(n*2)
  a = sum((n[n%%2 != 0])**2)
  return(a)
}

sum_of_first_n_odd_squares_V2(2)
sum_of_first_n_odd_squares_V2(5)
sum_of_first_n_odd_squares_V2(10)

#Part 4
#a

dow <- read.csv('http://people.bu.edu/kalathur/datasets/DJI_2020.csv')

sm <- summary(dow$Close)
names(sm) <- c("Min", "Q1", "Q2","Mean","Q3","Max")
sm

low <- 18592
q1 <- 23466
q2 <- 24826
q3 <- 28862
high <- 29551



paste("First quartile variation is", q1-low)
paste("Second quartile variation is", q2 -q1)
paste("Third quartile variation is", q3 - q2) 
paste("Fourth quartile variation is", high - q3) 



#b.)
paste(" The minimum dow value of ", min(dow$Close), 
      " is at row ", which.min(dow$Close), " on ",
      dow$Date[which.min(dow$Close)]) 

#c.)
#Create subset of all row dates above the min
dow2 = dow[which.min(dow$Close):nrow(dow),]

#Maximum  value above the min   
sellvalue = dow2$Close[which.max(dow2$Close)] 

#Date of maximum value above the min
selldate = dow2$Date[which.max(dow2$Close)] 

#Gain Calculation
gain = (sellvalue - min(dow$Close))/min(dow$Close)
gain
                                  
paste("I would sell on ", selldate,
      "when the dow is at", sellvalue, 
      " for a gain of ", round(gain*100,2), "%")
 
#d.) 
#Put close in a vector
close <- c(dow$Close)
close
#make a vector with the differences in close, 0 in front
close_diff <- c(0,diff(close) )
close_diff      

#add column to dataframe
dow$Diffs = close_diff
head(dow)

#e
paste(sum(dow$Diffs > 0), "Days dow closed higher than previous day")
paste(sum(dow$Diffs < 0), "Days dow closed lower than previous day")

#f
subset(dow, dow$Diffs > 1000)

