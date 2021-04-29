library(tidyverse)

x <- 100

sqrt(x)
x %>% sqrt
x %>% sqrt()

log(x, base = 10)
x %>% log(base = 10)

log10(sqrt(x))
x %>% sqrt %>% log10
x %>% sqrt %>% log10()


log(sqrt(x), base = 10)
x %>% sqrt %>% log(base = 10)

# As a placeholder

start <- 10
end <- 50
step <- 5
seq(start, end, by = step)

start %>% seq(end, by = step)
end %>% seq(start, ., by = step)
step %>% seq(start, end, by = .)



###############

## https://www.r-bloggers.com/2016/02/japans-ageing-population-animated-with-r/
## https://github.com/walkerke/idbr


library(idbr)
library(tidyverse)
library(animation)
library(ggthemes)

idb_api_key('9c6cecdca5a6185f1c969d35cc4f69f4e78908e0')

india_data <- get_idb(
  country = "India",
  year = 2025,
  age = 0:100,
  sex = c("male", "female")
) 

india_data

india_data %>%
  mutate(pop = ifelse(sex == "Male", pop * -1, pop)) %>%
  ggplot(aes(x = pop, y = as.factor(age), fill = sex)) + 
  geom_col(width = 1) + 
  theme_minimal(base_size = 15) + 
  scale_x_continuous(labels = function(x) paste0(abs(x / 1000000), "m")) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 10)) + 
  scale_fill_manual(values = c("red", "gold")) + 
  labs(title = "Population structure of India in 2025",
       x = "Population",
       y = "Age") 



####


years <- 1990:2050

male <- get_idb(country = 'Japan', years, sex = 'male') %>%
  mutate(pop = pop * -1)

female <- get_idb(country = 'Japan', years, sex = 'female') 

japan <- rbind(male, female) %>%
  mutate(abs_pop = abs(pop))

saveGIF({
  
  for (i in years) {
    
    title <- as.character(i)
    
    year_data <- filter(japan, year == i)
    
    g1 <- ggplot(year_data, 
                 aes(x = age, y = pop, fill = sex, width = 1)) +
      coord_fixed() + 
      coord_flip() +
      annotate('text', x = 98, y = -800000, 
               label = 'Data: US Census Bureau IDB; idbr R package', size = 3) + 
      geom_bar(data = subset(year_data, sex == "Female"), stat = "identity") +
      geom_bar(data = subset(year_data, sex == "Male"), stat = "identity") +
      scale_y_continuous(breaks = seq(-1000000, 1000000, 500000),
                         labels = paste0(as.character(c(seq(1, 0, -0.5), c(0.5, 1))), "m"), 
                         limits = c(min(japan$pop), max(japan$pop))) +
      theme_economist(base_size = 14) + 
      scale_fill_manual(values = c('#ff9896', '#d62728')) + 
      ggtitle(paste0('Population structure of Japan, ', title)) + 
      ylab('Population') + 
      xlab('Age') + 
      theme(legend.position = "bottom", legend.title = element_blank()) + 
      guides(fill = guide_legend(reverse = TRUE))
    
    print(g1)
    
  }
  
}, movie.name = 'japan_pyramid.gif', interval = 0.1, ani.width = 700, ani.height = 600)



##


# Get data on life expectancy at birth for all countries in 2021 and
# make a map with ggplot2

lex <- get_idb(
  country = "all",
  year = 2021,
  variables = c("name", "e0"),
  geometry = TRUE
)

ggplot(lex, aes(fill = e0)) +
  theme_bw() +
  geom_sf() +
  coord_sf(crs = 'ESRI:54030') +
  scale_fill_viridis_c() +
  labs(fill = "Life expectancy at birth (2021)")






