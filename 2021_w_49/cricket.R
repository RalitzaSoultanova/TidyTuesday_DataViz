#libraries
library(qwraps2)
library(tidyverse)
require(ggplot2)
require(gganimate)
library(gifski)


# import data 

data <- read_csv2(file = "data/sim_match2.csv")
data <- data %>% separate(match_date, into = c("date_month", "year"), sep = ", ")
data <- data %>% transform(year = as.numeric(year))
#View(data)



data_win <- data %>% group_by(winner, year) %>% summarise(winner, year) %>% count(winner)
#View(data_win)
data_loose <- data %>% group_by(looser, year)%>% summarise(looser, year) %>% count(looser)
#View(data_loose)

#write_csv(data_win, "data_win.csv")
#write_csv(data_loose, "data_loose.cvs")

merged_data <- read_csv2("win_loos.csv")
merged_data <- merged_data %>% 
  mutate(winner = factor(winner)) %>% 
  mutate(pourcentage_win = n_wins/(n_wins+ n_loose)*100)%>% arrange(year)

best_icc <- merged_data %>% filter (winner =="New Zealand" | winner =="India" |winner =="Australia"| winner =="England" | winner =="Pakistan"| winner =="South Africa")

#head(best_icc)


#plot best 6 countries ICC
D <- ggplot(
  best_icc, aes(year, pourcentage_win, color=winner)) +
  geom_point( alpha = 0.7) +
  geom_path() + 
  transition_reveal(along = year)+
  shadow_trail(distance = 0.01, alpha = 0.4)+
  ylim(0, 90) +
  facet_wrap(~winner) + 
  labs( x = "year", y = "% wins")+
  labs(title = "Year: {frame_time}")+
  ggtitle("Evolution of pourcentage of winning kricket games", subtitle = "Year: {as.integer(frame_time)}")+
  transition_time(year)  

#  "Top 6 ICC countries"

b <- animate(D, renderer = gifski_renderer())

anim_save("animation.mp4", b, renderer = gifski_renderer())

