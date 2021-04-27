
#Part 1
scores <- c(59, 46, 76, 60, 49, 65, 82, 68, 99, 52)

#a.)
length(scores)
scores[1:2]
scores[c(1, length(scores))]
scores[c(length(scores)/2,(length(scores)/2)+1)]

#b.)
x <- median(scores)
x
a <- scores <= x
a
b <- scores > x
b
sum(a)
sum(b)

#c.)
scores[a]
scores[b]

#d.)
scores[c(TRUE, FALSE)]
scores[c(FALSE, TRUE)]


#e.)
scores[seq(from = 1, to = length(scores), by = 2)]
scores[seq(from = 2, to = length(scores), by = 2)]

#f.)
scores.matrix <- matrix(scores,
                 nrow = 2, byrow = TRUE)
scores.matrix

#g.)
scores.matrix[,c(1,ncol(scores.matrix))]

#h.)

dimnames(scores.matrix) <- list(
  c("Quiz_1", "Quiz_2"), 
  c("Student_1", "Student_2", "Student_3", 
    "Student_4", "Student_5"))

scores.matrix

#i.)
scores.matrix[,c(1,ncol(scores.matrix))]

#Part 2
#a.)

Month <- c("Jan", "Feb", "Mar", "Apr", "May")
Open <- c(28639, 28320, 25591, 21227, 24121)
High <- c(29374, 29569, 27102, 24765, 24350)
Low <- c(28170, 24681, 18214, 20735, 23361)
Close <- c(28256, 25409, 21917, 24346, 24331)

dow <- data.frame(Month, Open, High, Low, Close)

dow

#b.)
summary(dow)

#c.)
dow[c("Month","Open","Close")]

#d.)
dow[c(1,nrow(dow)),]

#e.)
dow[c(1,nrow(dow)),c("Month","High","Low")]

#f.)
subset(dow, Low > 22000)

dow[Low > 22000, 1:5]


#g.)
subset(dow, Low > 25000 & Open > 25000)

dow[Low > 25000 & Open > 25000, 1:5]


#h.)
dow$Volatility <- dow$High - dow$Low
dow

#i.)
subset(dow, dow$Volatility == max(dow$Volatility))

#j.)
dow[dow$Volatility == min(dow$Volatility), 1:6]

