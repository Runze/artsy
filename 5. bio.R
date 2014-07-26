setwd('/Users/Runze/Github/artsy')
library(ggplot2)
library(scales)
library(tm)
library(slam)
library(plyr)
library(wordcloud)
library(skmeans)

load(file = 'data/followers.RData')

#analysis bio
#clean bio
bio = tolower(followers$description)
bio = gsub('[^[:alpha:]]', ' ', bio)
bio = gsub('^ +', '', bio)
bio = gsub(' +$', '', bio)
bio = gsub(' +', ' ', bio)
bio = subset(bio, bio != '' & nchar(bio) >= 3)

c = Corpus(VectorSource(bio))
c_clean = tm_map(c, removeWords, c(stopwords('SMART'), 'http', 'https'))

wordcloud(c_clean, max.words = 300, random.order = F, col = brewer.pal(8, "Dark2"))

#dtm (min word length set to be 3)
dtm = DocumentTermMatrix(c_clean, control = list(minWordLength = 3))

#find and remove "documents" (i.e., bios) that contain no words longer than 3 letters
dtm_rollup = as.matrix(rollup(dtm, 2, na.rm = T, FUN = sum))
ind = which(dtm_rollup == 0)
dtm_non0 = dtm[-ind, ]
c_clean_non0 = c_clean[-ind]

#spherical k-means
set.seed(1)
bio_sk = skmeans(dtm_non0, 4)

#find the corpus correpsonding to each cluster
c1 = c_clean_non0[bio_sk$cluster == 1]
c2 = c_clean_non0[bio_sk$cluster == 2]
c3 = c_clean_non0[bio_sk$cluster == 3]
c4 = c_clean_non0[bio_sk$cluster == 4]

wordcloud(c1, max.words = 200, random.order = F, col = brewer.pal(8, "Dark2"))
wordcloud(c2, max.words = 200, random.order = F, col = brewer.pal(8, "Dark2"))
wordcloud(c3, max.words = 200, random.order = F, col = brewer.pal(8, "Dark2"))
wordcloud(c4, max.words = 200, random.order = F, col = brewer.pal(8, "Dark2"))

#count the number of bios per each cluster
c_count = data.frame(table(bio_sk$cluster))
names(c_count) = c('cluster', 'freq')

cluster = 
  ggplot(c_count, aes(x = cluster, y = freq)) + geom_bar(stat = 'identity', col = '#fa9fb5', fill = '#fa9fb5') +
  xlab('Cluster') + ylab('Number of Followers') +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels=c('People who love everything', 'Artist', 'Art-lover', 'Creative Professionals\n and Students'))

ggsave(cluster, file = 'graph/cluster.jpeg',width = 8, height = 8)