
# Load libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(stm) #topic modeling 
library(textdata)
library(stringr)
library(ggplot2)
library(waffle)
library(forcats)





# Get the Data

tuesdata <- tidytuesdayR::tt_load('2021-12-14')


studio_album_tracks <- tuesdata$studio_album_tracks
related_artist <- tuesdata$related_artists
lyrics <- tuesdata$lyrics


# most frequent words  

lyrics %>% distinct(album_name, song_name)

tidy_lyrics<- lyrics %>% 
  mutate(song_name = str_replace(song_name, "\x92", "'")) %>%
  unnest_tokens(word, line) %>%
  anti_join(get_stopwords())

#View(tidy_lyrics)

#summarisation and data cleaning

tidy_lyrics %>% 
  count(word, sort=TRUE)


#count number of words per song
total_words_song <- tidy_lyrics %>% 
  group_by(song_name)%>%
 count(song_name, sort=TRUE) %>%
  mutate(words_in_song = n) %>%
  select(-n)


#count number of repetitions of words per song

repetive <- tidy_lyrics %>% 
  count(word, song_name, album_name,  sort=TRUE) %>%
  mutate(repetitive_per_song = n) %>%
  select(-n)

#full frame song stats 
full_song <- left_join(total_words_song, repetive) %>%
    arrange(desc(repetitive_per_song)) %>%
    mutate(proportion = (repetitive_per_song/words_in_song)*100)%>%
    arrange(desc(proportion))

most_repetitive_9 <- full_song %>% 
  arrange(song_name, desc(proportion)) %>%
  filter(proportion>14.9)

#View(most_repetitive_9)

cant_dance <- most_repetitive_9 %>% 
  filter(song_name=="If U Can't Dance")%>%
  mutate(non_rep=words_in_song-repetitive_per_song)



library(gridExtra)


df <- read.csv2("if_cant2.csv")
move <- read.csv2("move_over3.csv")
never_give <- read_csv2("never_give2.csv")

sathurday <- read_csv2("sathurday_night.csv")

# palette background rgb(118, 102, 103) #766667 
#paletteSpice <- c("204566", "#3A78B1", "#67b4f7", "#cbe1f4")
hsize <- 4

df <- df %>% 
  mutate(x = hsize)

#If U can't dance  
dance <- ggplot(df, aes(x = hsize, y = repetitive_per_song, fill = word)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.6, hsize + 0.8))+ 
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="If U Can't Dance", hjust = 5.5)+ 
  geom_text(aes(label = repetitive_per_song),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Accent")+
  theme_void() 

dance
#move Over
move<- move %>% 
  mutate(x = hsize)

move <- ggplot(move, aes(x = hsize, y = repetitive_per_song, fill = word)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.6, hsize + 0.8))+ 
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Move Over", hjust = 5.5)+ 
  geom_text(aes(label = repetitive_per_song),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Accent")+
  theme_void() 

move
#never give

never_give <- never_give %>% 
  mutate(x=hsize)

give <- ggplot(never_give, aes(x = hsize, y = repetitive_per_song, fill = word)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.6, hsize + 0.8))+ 
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Never give up")+ 
  geom_text(aes(label = repetitive_per_song),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Accent")+
  theme_void() 
give
# sathurday night 
sathurday <- sathurday %>% 
  mutate(x=hsize)

sathurday <- ggplot(sathurday, aes(x = hsize, y = repetitive_per_song, fill = word)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.6, hsize + 0.8))+ 
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Saturday Night Divas", hjust = 5.5)+ 
  geom_text(aes(label = repetitive_per_song),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Accent")+
  theme_void() 


sathurday

grid.arrange(dance, move, give, sathurday, ncol=2) %>% ggsave("plot.png")

theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
