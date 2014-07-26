setwd('/Users/Runze/Github/artsy')
library(ggplot2)
library(scales)

load(file = 'followers.RData')

#analyze followers' friends and followers counts
friends_hist =
  ggplot(followers, aes(x = as.numeric(friends_count), y = ..density..)) +
  geom_histogram(binwidth = 200, fill = '#6baed6', col = '#bdd7e7', size = .2) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 3500, 500), labels = comma) +
  scale_y_continuous(labels = comma) +
  xlab('Friends Count') + ylab('Density') + 
  ggtitle('Histogram of Followers\' Friends\n') + 
  theme(plot.title = element_text(size = rel(1.5), face = 'bold'), legend.position = 'bottom')
   
ggsave(filename = 'friends.jpeg', plot = friends_hist, width = 8, height = 8)

followers_hist = 
  ggplot(followers, aes(x = as.numeric(followers_count), y = ..density..)) +
  geom_histogram(binwidth = 300, fill = '#f768a1', col = '#fbb4b9', size = .2) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 5000, 500), labels = comma) +
  scale_y_continuous(labels = comma) +
  xlab('Followers Count') + ylab('Density') + 
  ggtitle('Histogram of Followers\' Followers\n') + 
  theme(plot.title = element_text(size = rel(1.5), face = 'bold'), legend.position = 'bottom')

ggsave(filename = 'followers.jpeg', plot = followers_hist, width = 8, height = 8)

#analyze followers' twitter ages
created_at = as.character(followers$created_at)
year = substr(created_at, nchar(created_at) - 3, nchar(created_at))
month = substr(created_at, 5, 7)
day = substr(created_at, 9, 10)
born = as.Date(paste(year, month, day, sep = '-'), '%Y-%b-%d')
followers$age = as.numeric(Sys.Date() - born)

age_hist = 
  ggplot(followers, aes(x = age, y = ..density..)) +
  geom_histogram(binwidth = 200, fill = '#9e9ac8', col = '#bcbddc', size = .2) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 3000, 500), labels = comma) +
  scale_y_continuous(labels = comma) +
  xlab('Age in Days') + ylab('Density') + 
  ggtitle('Histogram of Followers\' Twitter Ages\n') + 
  theme(plot.title = element_text(size = rel(1.5), face = 'bold'), legend.position = 'bottom')
        
ggsave(filename = 'age.jpeg', plot = age_hist, width = 8, height = 8)