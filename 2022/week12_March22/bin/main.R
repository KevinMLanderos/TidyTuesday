############     -    Tidy Tuestady Week 12    -     ############
############    --------   Baby names    --------    ############

# By Kevin Meza Landeros

# Long version: 1
# Sub-version: 0

### -------------------------- Description -------------------------- ###
# This code shows how to create figures based on a table that has the name
# frequency of babies (males and females) borned from 1880 to 2017.
# Plots generated: cloud words and line plots.

cat('\n\n')
### --------------------------- Libraries --------------------------- ###
cat('### --------------------------- Libraries --------------------------- ###\n')
cat('Importing libraries...\n\n')
library(tidyverse)
library(data.table)
library(optparse)
library(ggplot2)
library(hrbrthemes)
library(here)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(gganimate)
library(gifski)
cat('Libraries imported!\n')
set.seed(1234)

# ---------- Set TidyTuesday date
year <- "2022"
week <- "week12_March22"

# ---------- Read data and save it
setwd("/Users/ljiuser/Documents/Github/TidyTuesday/")
here::here()

# ---------- Read data and save it
# Read data
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')
# Save data
readr::write_csv(babynames, file = here(year, week, "data", "babynames.csv"))

# Split dataset by sex
females <- babynames %>% filter(sex == "F")
males <- babynames %>% filter(sex == "M")
# Get the age range of the individuals
range(babynames$year)

# ---------- Tables

# Most common names for males and females
freq_female <- females %>% group_by(name) %>% summarize(freq = sum(n)) %>% arrange(desc(freq))
summary(freq_female$freq)
freq_male <- males %>% group_by(name) %>% summarize(freq = sum(n)) %>% arrange(desc(freq))
summary(freq_male$freq)

# Changes in frequency over time for the 5 most abundant names
top_names_female <- freq_female$name[1:5]
top_names_year_female <- females %>% filter(name %in% top_names_female) %>% select(year, name, n)
top_names_year_female$name <- factor(top_names_year_female$name, levels = top_names_female)

top_names_male <- freq_male$name[1:5]
top_names_year_male <- males %>% filter(name %in% top_names_male) %>% select(year, name, n)
top_names_year_male$name <- factor(top_names_year_male$name, levels = c(top_names_male, "Kevin"))

# Changes in frequency over time ffor the name Kevin
top_names_kevin_year_male <- males %>% filter(name %in% c(top_names_male, "Kevin")) %>% select(year, name, n)
top_names_kevin_year_male$name <- factor(top_names_kevin_year_male$name, levels = c(top_names_male, "Kevin"))


# ---------- Plots

# Most common names for males and females
pdf(here(year, week, "results", "cloudWord_all_female.pdf"))
wordcloud(words = freq_female$name, freq = freq_female$freq, min.freq = 50, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
pdf(here(year, week, "results", "cloudWord_all_male.pdf"))
wordcloud(words = freq_male$name, freq = freq_male$freq, min.freq = 50, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Changes in frequency over time for the 5 most abundant names
# Female
pdf(here(year, week, "results", "linePlot_top5_female.pdf"))
p1 <- top_names_year_female %>% ggplot( aes(x=year, y=n, group=name, color=name)) +
    geom_line() +
    labs(color='Top 5 names') +
    ggtitle("Popularity of female names from 1880 to 2017") +
    theme_classic() +
    ylab("Number of babies born") + xlab("Year")
p1
dev.off()
graph2.animation <- p1 +
  transition_reveal(year) +
  view_follow(fixed_y = TRUE)
animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10,
  res = 100, renderer = gifski_renderer())
anim_save(here(year, week, "results", "linePlot_top5_female.gif"))

# Male
pdf(here(year, week, "results", "linePlot_top5_male.pdf"))
p2 <- top_names_year_male %>% ggplot( aes(x=year, y=n, group=name, color=name)) +
    geom_line() +
    labs(color='Top 5 names') +
    ggtitle("Popularity of male names from 1880 to 2017") +
    theme_classic() +
    ylab("Number of babies born") + xlab("Year")
p2
dev.off()
graph2.animation <- p2 +
  transition_reveal(year) +
  view_follow(fixed_y = TRUE)
animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10,
  res = 100, renderer = gifski_renderer())
anim_save(here(year, week, "results", "linePlot_top5_male.gif"))

# Changes in frequency over time for the name Kevin
x_sum <- 18; y_sum <- 1500
pdf(here(year, week, "results", "linePlot_top5_male_kevin.pdf"))
p3 <- top_names_kevin_year_male %>% ggplot( aes(x=year, y=n, group=name, color=name)) +
    geom_line() +
    labs(color='Top 5 names + Kevin') +
    ggtitle("Popularity of male names from 1880 to 2017") +
    ylab("Number of babies born") + xlab("Year") +
    theme_classic()
p3 + scale_color_manual(values=c("#E7E7E7", "#E7E7E7", "#E7E7E7", "#E7E7E7", "#E7E7E7", "#F7451F")) +
  annotate("segment", x = 1930, xend = 1955, y = 20000, yend = 17000,
         colour = "black", size = 0.2, arrow = arrow()) +
  annotate(geom = "text", x = 1930 - x_sum, y = 20000 + y_sum, label = "Kevin Costner (1955)", hjust = "left", size = 2.3) +
  annotate("segment", x = 1930, xend = 1958, y = 30000, yend = 25000,
         colour = "black", size = 0.2, arrow = arrow()) +
  annotate(geom = "text", x = 1930 - x_sum, y = 30000 + y_sum, label = "Kevin Bacon (1958)", hjust = "left", size = 2.3) +
  annotate("segment", x = 1978, xend = 1959, y = 40000, yend = 27000,
         colour = "black", size = 0.2, arrow = arrow()) +
  annotate(geom = "text", x = 1978 - x_sum, y = 40000 + y_sum, label = "Kevin Spacey (1959)", hjust = "left", size = 2.3) +
  annotate("segment", x = 2000, xend = 2000, y = 26500, yend = 12500,
         colour = "black", size = 0.2, arrow = arrow()) +
  annotate(geom = "text", x = 2000 - 5, y = 26500 + y_sum, label = "Me (2000)", hjust = "left", size = 2.3)
dev.off()
