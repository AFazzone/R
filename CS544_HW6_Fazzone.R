
library(stringi)
library(stringr)
library(tidyverse)
library(dbplyr)

file <- "http://people.bu.edu/kalathur/datasets/lincoln.txt"
words <- scan(file, what=character())
words

#Part 1
#a.)
punct <- "[.,!-':;?]"

punct_words <- list(words[str_detect(words, punct)])
punct_words

#b.)
#Take out punctuation
words <- str_replace_all(words, "[.,!-':;?]", " ")

#Take out empty words
words <- stri_remove_empty(words)

#Make lower case
words <- tolower(words)

words

#c.)
head(sort(table(words), decreasing = TRUE))
table(words)
#d.)
table(str_length(words))

plot(table(str_length(words)), main = "Word Length Frequency")

#e.)
#prints all words longer than 14 characters
words[str_length(words) > 14]


#f.)

words[str_detect(words, "^p")]

#g.)

words[str_detect(words, "r$")]

#h.)

p <-words[str_detect(words, "^p")]

p[str_detect(p,"r$")]

stopfile <- "http://people.bu.edu/kalathur/datasets/stopwords.txt"
stopwords <- scan(file, what=character())
stopwords

no_stop_words <- words[!(words %in% stopwords)]

table(no_stop_words)
sort(table(no_stop_words), decreasing = TRUE)


head(sort(table(no_stop_words), decreasing = TRUE))

table(str_length(no_stop_words))

plot(table(str_length(no_stop_words)), main = "No Stop Words")


#Part 2
print(getwd())

df <-read.csv(file = 'C:/Users/HP/Documents/544/CS544_HW6_Fazzone/usa_daily_avg_temps.csv')


#a.)

usaDailyTemps <- as_tibble(df)
usaDailyTemps

#b)
#Find years
glimpse(usaDailyTemps)

year_max <- usaDailyTemps %>% group_by(year) %>% summarise(max(avgtemp))
year_max

plot(year_max, main = "Yearly Maximum Temperatures")

#c.)
state_max <- usaDailyTemps %>% group_by(state) %>% summarise(max(avgtemp))
state_max
plot(state_max$`max(avgtemp)`, xlab = "States",
     main = "Maximum Temp By State", col = "red")

#d.)
bostonDailyTemps = filter(usaDailyTemps, city == "Boston")
bostonDailyTemps

#e.)
bos_month_max <-bostonDailyTemps %>% group_by(month) %>%  summarise (max(avgtemp))
bos_month_max
plot(bos_month_max, main = "Boston's monthly Max Temp", col = "purple")
