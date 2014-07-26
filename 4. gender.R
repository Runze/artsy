setwd('/Users/Runze/Github/artsy')
library(XML)
library(ggplot2)
library(scales)

load(file = 'followers.RData')

#web crawl names
link = 'http://www.ssa.gov/cgi-bin/popularnames.cgi'
male = data.frame(male = as.character())
female = data.frame(female = as.character())
for(y in c(2000, 2010)) {
  raw = postForm(link, year = y, top = 1000, number = 'n', style = 'post')
  tables = readHTMLTable(raw, header = F)
  temp = data.frame(tables[[3]][-1, ])
  male = unique(rbind(male, data.frame(temp$V2)))
  female = unique(rbind(female, data.frame(temp$V4)))
}
male = tolower(as.vector(t(male)))
female = tolower(as.vector(t(female)))

#clean names for matching
name = gsub('[^[:alpha:]]', ' ', followers$name)
name = strsplit(name, ' ')

#extract first name
name1 = lapply(name, function(x) x[1])
name1 = tolower(unlist(name1))

#function to perform name matching
match_name = function(x) {
  if (is.na(x)) {
    return('Unknown')
  }
  else {
    male_m = length(grep(x, male))
    female_m = length(grep(x, female))
    m = male_m - female_m
    g = ifelse(m > 0, 'Male', ifelse(m < 0, 'Female', 'Unknown'))
    return(g) 
  }
}

#note it takes a while to run
followers$gender = rep('', nrow(followers))
for(i in 1:nrow(followers)) {
  followers$gender[i] = match_name(name1[i])
}

save(followers, file = 'followers_w_gender.RData')

#chart gender distribution
gender_agg = aggregate(name ~ gender, subset(followers, gender != 'Unknown'), length)
gender = 
  ggplot(gender_agg, aes(x = gender, y = name)) + geom_bar(stat = 'identity', col = '#6baed6', fill = '#6baed6') +
  xlab('Gender') + ylab('Number of Followers') +
  scale_y_continuous(labels = comma)
ggsave(gender, file = 'gender.jpeg', width = 8, height = 8)